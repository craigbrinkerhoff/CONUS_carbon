#_targets.R file
library(targets)
library(tarchetypes)
library(tibble)
library(future)
library(future.batchtools)

#necessary functions
source("src/utils.R")
source("src/model.R")
source("src/run_model.R")
source("src/calibrate_model.R")
source("src/setup_hydrography.R")
source('src/run_raymond.R')

#parallelization and pipeline settings
plan(batchtools_slurm, template = "slurm_future.tmpl") #for parallelization via futures transient workers
tar_option_set(packages = c('tidyr', 'plyr', 'dplyr', 'readr', 'cowplot', 'colorspace', 'ggplot2', 'GA', 'lubridate', 'Metrics', 'roxygen2', 'doParallel', 'terra', 'sf')) #required packages
tar_option_set(format = "qs") #qs compressed/serialized r objects. Much faster read/write than rds (uncompressed)
tar_option_set(resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = 1))))) #default num_cores setting (is changed for calibration later)
cores_req <- 25 #cores requested for calibration (overides the defualt set right above this)

######################
#### PIPELINE PARAMETERS------------------------------------------------------------------
######################
#meta parameters
path_to_data <- '/nas/cee-water/cjgleason/craig/CONUS_ephemeral_data' #path to data repo (separate from code repo)
path_to_dataRaymond <- '/nas/cee-water/cjgleason/craig/CONUS_CO2_data' #path to raymond data (seperate from code repo)
glorich_data <- readr::read_csv('data/HUC4_calibration.csv') #glorish CO2 values for each HUC4 basin (for calibration)
raymond_coscat_lookup <- readr::read_csv('data/raymond_coscat.csv') #raymond coscat lookup table
lookUpTable <- readr::read_csv('data/HUC4_lookup.csv') #basin routing lookup table

#constant parameters
C_atmosphere <- 400 #[ppm]
emergenceQ <- 0.000105 #[m3/s]: Allen etal 2018 width of 30cm plugged into our HG equation for width --> smallest headwater Q
C_groundwater <- 16000  #[ppm]: held constant and not calibrated

#calibrated parameters (these are all set to be 2 orders of magntiude wide)
lowerCBZ_riv <- 0 #[ppm]
lowerCBZ_lake <- 0 #[ppm]
lowerFWC_riv <- 0 #[ppm/s]
lowerFWC_lake <- 0 #[ppm/s] #internally changed to negative for regions with below-atmospheric CO2 (HUC04 and HUC16)

upperCBZ_riv <- 2000 #[ppm]
upperCBZ_lake <- 2000 #[ppm]
upperFWC_riv <- 1e-2 #[ppm/s]
upperFWC_lake <- 1e-2 #[ppm/s] #internally changed to zero for regions with below-atmospheric CO2 (HUC04 and HUC16)

#GA meta-parameters
myPopSize <- 25 #population size within each generation
mymaxIter <- 1000 # maximum generations before termination
myRun <- 300 #calibration will be terminated if no improvement over this many generations
myRunHARD <- 100 #calibration will be terminated if no improvement over this many generations (RELAXED VERSION FOR SLOW BASINS...)
mutationRate <- 0.25 #mutation probability to avoid getting trapped in local maxima
cores <- 25 #how many cores to run GA in parallel on (also need to set in the slurm.tmpl file FYI...)

#dummy parameter set to make sure all routing works before running calibration
#calibratedParameters <- list('Cbz_riv'=200,
#            'Cbz_lake'=2,
#            'Fwc_riv'=1e-4,
#            'Fwc_lake'=1e-7)

######################
#### SETUP STATIC BRANCHING OF BASINS PER PROCESSING LEVEL-----------------------------------------------------
#####################
#headwater terminal basins (can be run completely independently)
mapped_lvlTerminal <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 0 & is.na(lookUpTable$toBasin) == 1 & !(lookUpTable$HUC4 %in% c('1710', '1801', '1802')),]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, NA, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, NA)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

mapped_lvlTerminalHARD <- tar_map( #three terminal basins that are need less conservative calibration because they're too slow... Max at out 100 iterations of identical performance
  unlist=FALSE,
  values = tibble(
    method_function = rlang::syms("setupHydrography"),
    huc4 = lookUpTable[lookUpTable$level == 0 & is.na(lookUpTable$toBasin) == 1 & lookUpTable$HUC4 %in% c('1710', '1801', '1802'),]$HUC4),
  names = "huc4",
  tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
  tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, NA, #calibrate model to raymond CO2
                                                         lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                         upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                         myPopSize, mymaxIter, myRunHARD, mutationRate, cores),
             resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
  tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, NA)), #run final version of model
  tar_target(written, writeToFile(final, huc4)), #write final model to file
  tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#Headwater basins that export into the next level of basins
mapped_lvl0 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 0 & is.na(lookUpTable$toBasin) == 0 & lookUpTable$HUC4 != '1102',]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, NA, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, NA)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,C_atmosphere)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#Headwater basins that export into the next level of basins
mapped_lvl0HARD <- tar_map( #basins that are need less conservative calibration because they're too slow... Max at out 100 iterations of identical performance
  unlist=FALSE,
  values = tibble(
    method_function = rlang::syms("setupHydrography"),
    huc4 = lookUpTable[lookUpTable$level == 0 & is.na(lookUpTable$toBasin) == 0 & lookUpTable$HUC4 == '1102',]$HUC4),
  names = "huc4",
  tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
  tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, NA, #calibrate model to raymond CO2
                                                         lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                         upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                         myPopSize, mymaxIter, myRunHARD, mutationRate, cores),
             resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
  tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, NA)), #run final version of model
  tar_target(written, writeToFile(final, huc4)), #write final model to file
  tar_target(exportedCO2, getExported(final, huc4, lookUpTable,C_atmosphere)), #get exported CO2 and reach end node for routing to next downstream basins
  tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#level 1 downstream basins
mapped_lvl1 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 1,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl0)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#level 2 downstream basins
mapped_lvl2 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 2,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl1, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl1)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#level 3 downstream basins
mapped_lvl3 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 3,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl2, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl2)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#level 4 downstream basins
mapped_lvl4 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 4,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl3, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl3)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#level 5 downstream basins
mapped_lvl5 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 5,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl4, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl4)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#level 6 downstream basins
mapped_lvl6 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 6,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl5, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl5)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#level 7 downstream basins
mapped_lvl7 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 7,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl6, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl6)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#level 8 downstream basins
mapped_lvl8 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 8,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl7, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl7)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)


#level 9 downstream basins
mapped_lvl9 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 9,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl8, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl8)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)


#level 10 downstream basins
mapped_lvl10 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 10,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl9, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl9)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)


#level 11 downstream basins
mapped_lvl11 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 11,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl10, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl10)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)


#level 12 downstream basins
mapped_lvl12 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 12,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl11, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl11)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)


#level 13 downstream basins
mapped_lvl13 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 13,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl12, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl12)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)


#level 14 downstream basins
mapped_lvl14 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 14,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl13, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl13)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)


#level 15 downstream basins
mapped_lvl15 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 15,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl14, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl14)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)

#level 16 downstream basins
mapped_lvl16 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 16,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl15, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl15)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)


#level 17 downstream basins
mapped_lvl17 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 17,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl16, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl16)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)), #get exported CO2 and reach end node for routing to next downstream basins
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)


#level 18 downstream basins
mapped_lvl18 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 18,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl17, #calibrate model to raymond CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, mutationRate, cores),
                                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #specify cores wildcard for calibration
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl17)), #run final version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)




######################
#### ACTUAL PIPELINE----------------------------------
######################
list(
  ### level 0 but terminal (can be run independently)
  mapped_lvlTerminal,
  mapped_lvlTerminalHARD, #HARD BASINS THAT NEED LESS RIGOROUS CALIBRATION without rerunning anything else
  tar_combine(combined_emissions_lvlTerminal, c(mapped_lvlTerminal$emissions,
                                                mapped_lvlTerminalHARD$emissions), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 0
  mapped_lvl0,
  mapped_lvl0HARD,  #HARD BASINS THAT NEED LESS RIGOROUS CALIBRATION without rerunning anything else
  tar_combine(combined_emissions_lvl0, list(mapped_lvl0$emissions,
                                            mapped_lvl0HARD$emissions), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl0, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0HARD$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 1
  mapped_lvl1,
  tar_combine(combined_emissions_lvl1, mapped_lvl1$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl1, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 2
  mapped_lvl2,
  tar_combine(combined_emissions_lvl2, mapped_lvl2$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl2, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 3
  mapped_lvl3,
  tar_combine(combined_emissions_lvl3, mapped_lvl3$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl3, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 4
  mapped_lvl4,
  tar_combine(combined_emissions_lvl4, mapped_lvl4$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl4, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 5
  mapped_lvl5,
  tar_combine(combined_emissions_lvl5, mapped_lvl5$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl5, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 6
  mapped_lvl6,
  tar_combine(combined_emissions_lvl6, mapped_lvl6$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl6, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 7
  mapped_lvl7,
  tar_combine(combined_emissions_lvl7, mapped_lvl7$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl7, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 8
  mapped_lvl8,
  tar_combine(combined_emissions_lvl8, mapped_lvl8$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl8, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 9
  mapped_lvl9,
  tar_combine(combined_emissions_lvl9, mapped_lvl9$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl9, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 10
  mapped_lvl10,
  tar_combine(combined_emissions_lvl10, mapped_lvl10$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl10, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 11
  mapped_lvl11,
  tar_combine(combined_emissions_lvl11, mapped_lvl11$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl11, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 12
  mapped_lvl12,
  tar_combine(combined_emissions_lvl12, mapped_lvl12$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl12, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2,
                                     mapped_lvl12$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 13
  mapped_lvl13,
  tar_combine(combined_emissions_lvl13, mapped_lvl13$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl13, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2,
                                     mapped_lvl12$exportedCO2,
                                     mapped_lvl13$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 14
  mapped_lvl14,
  tar_combine(combined_emissions_lvl14, mapped_lvl14$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl14, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2,
                                     mapped_lvl12$exportedCO2,
                                     mapped_lvl13$exportedCO2,
                                     mapped_lvl14$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 15
  mapped_lvl15,
  tar_combine(combined_emissions_lvl15, mapped_lvl15$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl15, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2,
                                     mapped_lvl12$exportedCO2,
                                     mapped_lvl13$exportedCO2,
                                     mapped_lvl14$exportedCO2,
                                     mapped_lvl15$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 16
  mapped_lvl16,
  tar_combine(combined_emissions_lvl16, mapped_lvl16$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl16, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2,
                                     mapped_lvl12$exportedCO2,
                                     mapped_lvl13$exportedCO2,
                                     mapped_lvl14$exportedCO2,
                                     mapped_lvl15$exportedCO2,
                                     mapped_lvl16$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 17
  mapped_lvl17,
  tar_combine(combined_emissions_lvl17, mapped_lvl17$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl17, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2,
                                     mapped_lvl12$exportedCO2,
                                     mapped_lvl13$exportedCO2,
                                     mapped_lvl14$exportedCO2,
                                     mapped_lvl15$exportedCO2,
                                     mapped_lvl16$exportedCO2,
                                     mapped_lvl17$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 18
  mapped_lvl18,
  tar_combine(combined_emissions_lvl18, mapped_lvl18$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### run raymond upscaling per HUC2 (must be hardcoded, unfortuantely, to get the HUC4 network objects)
  tar_target(raymond_01, runRaymondModel(path_to_dataRaymond, '01', raymond_coscat_lookup, list(final_0101, final_0102, final_0103, final_0104, final_0105, final_0106, final_0107, final_0108, final_0109, final_0110))),
  tar_target(raymond_02, runRaymondModel(path_to_dataRaymond, '02', raymond_coscat_lookup, list(final_0202, final_0203, final_0204, final_0205))),
  tar_target(raymond_03, runRaymondModel(path_to_dataRaymond, '03', raymond_coscat_lookup, list(final_0301, final_0302, final_0303, final_0304, final_0305, final_0306, final_0307, final_0308, final_0309, final_0310, final_0311, final_0312, final_0313, final_0314, final_0315, final_0316, final_0317, final_0318))),
  tar_target(raymond_04, runRaymondModel(path_to_dataRaymond, '04', raymond_coscat_lookup, list(final_0401,final_0402, final_0403, final_0404, final_0405, final_0406, final_0407, final_0408, final_0409, final_0410, final_0411, final_0412, final_0413, final_0414, final_0418, final_0419, final_0420, final_0424,
                                                                                            final_0426, final_0427, final_0428, final_0429, final_0430))),
  tar_target(raymond_05, runRaymondModel(path_to_dataRaymond, '05', raymond_coscat_lookup, list(final_0501, final_0502, final_0503, final_0504, final_0505, final_0506, final_0507, final_0508, final_0509, final_0510, final_0511, final_0512, final_0513, final_0514))),
  tar_target(raymond_06, runRaymondModel(path_to_dataRaymond, '06', raymond_coscat_lookup, list(final_0601, final_0602, final_0603, final_0604))),
  tar_target(raymond_07, runRaymondModel(path_to_dataRaymond, '07', raymond_coscat_lookup, list(final_0701, final_0702, final_0703, final_0704, final_0705, final_0706, final_0707, final_0708, final_0709, final_0710, final_0711, final_0712, final_0713, final_0714))),
  tar_target(raymond_08, runRaymondModel(path_to_dataRaymond, '08', raymond_coscat_lookup, list(final_0801, final_0802, final_0803, final_0804, final_0805, final_0806, final_0808, final_0809))),
  tar_target(raymond_09, runRaymondModel(path_to_dataRaymond, '09', raymond_coscat_lookup, list(final_0901, final_0902, final_0903, final_0904))),
  tar_target(raymond_10, runRaymondModel(path_to_dataRaymond, '10', raymond_coscat_lookup, list(final_1002, final_1003, final_1004, final_1005, final_1006, final_1007, final_1008, final_1009, final_1010, final_1011, final_1012, final_1013, final_1014, final_1015,
                                                                                            final_1016, final_1017, final_1018, final_1019, final_1020, final_1021, final_1022, final_1023, final_1024, final_1025, final_1026, final_1027, final_1028, final_1029, final_1030))),
  tar_target(raymond_11, runRaymondModel(path_to_dataRaymond, '11', raymond_coscat_lookup, list(final_1101, final_1103, final_1103, final_1104, final_1105, final_1106, final_1107, final_1108, final_1109, final_1110, final_1111, final_1112, final_1113, final_1114))),
  tar_target(raymond_12, runRaymondModel(path_to_dataRaymond, '12', raymond_coscat_lookup, list(final_1201, final_1202, final_1203, final_1204, final_1205, final_1206, final_1207, final_1208, final_1209, final_1210, final_1211))),
  tar_target(raymond_13, runRaymondModel(path_to_dataRaymond, '13', raymond_coscat_lookup, list(final_1301, final_1302, final_1303, final_1304, final_1305, final_1306, final_1307, final_1308, final_1309))),
  tar_target(raymond_14, runRaymondModel(path_to_dataRaymond, '14', raymond_coscat_lookup, list(final_1401, final_1402, final_1403, final_1404, final_1405, final_1406, final_1407, final_1408))),
  tar_target(raymond_15, runRaymondModel(path_to_dataRaymond, '15', raymond_coscat_lookup, list(final_1501, final_1502, final_1503, final_1504, final_1505, final_1506, final_1507, final_1508))),
  tar_target(raymond_16, runRaymondModel(path_to_dataRaymond, '16', raymond_coscat_lookup, list(final_1601, final_1602, final_1603, final_1604, final_1605, final_1606))),
  tar_target(raymond_17, runRaymondModel(path_to_dataRaymond, '17', raymond_coscat_lookup, list(final_1701, final_1702, final_1703, final_1704, final_1705, final_1706, final_1707, final_1708, final_1709, final_1710, final_1711, final_1712))),
  tar_target(raymond_18, runRaymondModel(path_to_dataRaymond, '18', raymond_coscat_lookup, list(final_1801, final_1802, final_1803, final_1804, final_1805, final_1806, final_1807, final_1808, final_1809, final_1810))),

  #### bring together all levels of results via utility functions (can't combine a combined for some reason...)
  tar_target(allModelResults, aggregateAllLevels(combined_emissions_lvlTerminal, combined_emissions_lvl0, combined_emissions_lvl1, combined_emissions_lvl2, combined_emissions_lvl3, combined_emissions_lvl4,
                                                 combined_emissions_lvl5, combined_emissions_lvl6, combined_emissions_lvl7, combined_emissions_lvl8, combined_emissions_lvl9,
                                                 combined_emissions_lvl10, combined_emissions_lvl11, combined_emissions_lvl12, combined_emissions_lvl13, combined_emissions_lvl14,
                                                 combined_emissions_lvl15, combined_emissions_lvl16, combined_emissions_lvl17, combined_emissions_lvl18), deployment = "main"),

  #### join raymond model and aggregate to HUC2 level
  tar_target(allResults, abstractAllResults(allModelResults, list(raymond_01, raymond_02, raymond_03, raymond_04, raymond_05, raymond_06, raymond_07, raymond_08, raymond_09, raymond_10,
                                                                  raymond_11, raymond_12, raymond_13, raymond_14, raymond_15, raymond_16, raymond_17, raymond_18)), deployment = "main")

  # validate discharge model

)
