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

######################
#### PIPELINE PARAMETERS------------------------------------------------------------------
######################
#meta parameters
path_to_data <- '/nas/cee-water/cjgleason/craig/CONUS_ephemeral_data' #path to data repo (separate from code repo)
path_to_dataRaymond <- '/nas/cee-water/cjgleason/craig/CONUS_CO2_data' #path to raymond data (seperate from code repo)
glorich_data <- readr::read_csv('data/HUC4_calibration.csv') #faster than rds file formats
raymond_coscat_lookup <- readr::read_csv('data/raymond_coscat.csv') #raymond coscat lookup table
lookUpTable <- readr::read_csv('data/HUC4_lookup.csv') #basin routing lookup table

#calibration parameters
C_atmosphere <- 400 #[ppm]
emergenceQ <- 0.000105 #[m3/s]: Allen etal 2018 width of 30cm plugged into our HG equation for width --> smallest headwater Q
C_groundwater <- 16000  #[ppm]: held constant and not calibrated

#these are all set to be 2 orders of magntiude wide
lowerCBZ_riv <- 10 #[ppm]
lowerCBZ_lake <- 1 #[ppm]
lowerFWC_riv <- 1e-6 #[ppm/s]
lowerFWC_lake <- 1e-8 #[ppm/s] #internally changed to negative for regions with below-atmospheric CO2

upperCBZ_riv <- 2000 #[ppm]
upperCBZ_lake <- 100 #[ppm]
upperFWC_riv <- 1e-3 #[ppm/s]
upperFWC_lake <- 1e-6 #[ppm/s] #internally changed to zero for regions with below-atmospheric CO2

myPopSize <- 60 #population size within each generation
mymaxIter <- 1000 # maximum generations before termination
myRun <- 300 #calibration will be terminated if no improvement over this many generations
cores <- 30 #how many cores to run GA in parallel on (also need to set in the slurm.tmpl file FYI...)

#dummy parameter set to make sure all routing works before running calibration
calibratedParameters <- list('Cbz_riv'=200,
            'Cbz_lake'=2,
            'Fwc_riv'=1e-4,
            'Fwc_lake'=1e-7)

######################
#### SETUP STATIC BRANCHING OF BASINS PER PROCESSING LEVEL-----------------------------------------------------
#####################
#Headwater basins (no upstream basins, these terminate into other basins or the ocean)
mapped_lvl0 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 0,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4)), #prep hydrography for routing
    #   tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, NA,
    #                                                          lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
    #                                                          upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
    #                                                          myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, NA)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl0)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl1)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl2)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl3)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl4)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl5)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl6)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl7)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl8)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl9)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl10)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl11)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl12)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl13)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl14)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl15)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl16)), #run final version of model
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
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, cores)), #calibrate model to raymond CO2
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl17)), #run final version of model
       tar_target(emissions, calcEmissions(final, huc4)) #calc carbon emissions from final calibrated model
)




######################
#### ACTUAL PIPELINE----------------------------------
######################
list(
  #### level 0
  mapped_lvl0,
  tar_combine(combined_emissions_lvl0, mapped_lvl0$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl0, mapped_lvl0$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 1
  mapped_lvl1,
  tar_combine(combined_emissions_lvl1, mapped_lvl1$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl1, mapped_lvl1$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 2
  mapped_lvl2,
  tar_combine(combined_emissions_lvl2, mapped_lvl2$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl2, mapped_lvl2$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 3
  mapped_lvl3,
  tar_combine(combined_emissions_lvl3, mapped_lvl3$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl3, mapped_lvl3$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 4
  mapped_lvl4,
  tar_combine(combined_emissions_lvl4, mapped_lvl4$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl4, mapped_lvl4$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 5
  mapped_lvl5,
  tar_combine(combined_emissions_lvl5, mapped_lvl5$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl5, mapped_lvl5$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 6
  mapped_lvl6,
  tar_combine(combined_emissions_lvl6, mapped_lvl6$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl6, mapped_lvl6$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 7
  mapped_lvl7,
  tar_combine(combined_emissions_lvl7, mapped_lvl7$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl7, mapped_lvl7$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 8
  mapped_lvl8,
  tar_combine(combined_emissions_lvl8, mapped_lvl8$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl8, mapped_lvl8$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 9
  mapped_lvl9,
  tar_combine(combined_emissions_lvl9, mapped_lvl9$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl9, mapped_lvl9$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 10
  mapped_lvl10,
  tar_combine(combined_emissions_lvl10, mapped_lvl10$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl10, mapped_lvl10$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 11
  mapped_lvl11,
  tar_combine(combined_emissions_lvl11, mapped_lvl11$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl11, mapped_lvl11$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 12
  mapped_lvl12,
  tar_combine(combined_emissions_lvl12, mapped_lvl12$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl12, mapped_lvl12$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 13
  mapped_lvl13,
  tar_combine(combined_emissions_lvl13, mapped_lvl13$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl13, mapped_lvl13$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 14
  mapped_lvl14,
  tar_combine(combined_emissions_lvl14, mapped_lvl14$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl14, mapped_lvl14$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 15
  mapped_lvl15,
  tar_combine(combined_emissions_lvl15, mapped_lvl15$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl15, mapped_lvl15$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 16
  mapped_lvl16,
  tar_combine(combined_emissions_lvl16, mapped_lvl16$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl16, mapped_lvl16$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 17
  mapped_lvl17,
  tar_combine(combined_emissions_lvl17, mapped_lvl17$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  tar_combine(exportedCO2_lvl17, mapped_lvl17$exportedCO2, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### LEVEL 18
  mapped_lvl18,
  tar_combine(combined_emissions_lvl18, mapped_lvl18$emissions, command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### run raymond upscaling per HUC2 (must be hardcoded, unfortuantely, to get the HUC4 network objects)
  tar_target(raymond_01, runRaymondModel(path_to_dataRaymond, '01', raymond_coscat_lookup, final_0101, final_0102, final_0103, final_0104, final_0105, final_0106, final_0107, final_0108, final_0109, final_0110), deployment = "main"),

  #### bring together all levels of results via utility functions (can't combine a combined for some reason...)
  tar_target(allModelResults, aggregateAllLevels(combined_emissions_lvl0, combined_emissions_lvl1, combined_emissions_lvl2, combined_emissions_lvl3, combined_emissions_lvl4,
                                                 combined_emissions_lvl5, combined_emissions_lvl6, combined_emissions_lvl7, combined_emissions_lvl8, combined_emissions_lvl9,
                                                 combined_emissions_lvl10, combined_emissions_lvl11, combined_emissions_lvl12, combined_emissions_lvl13, combined_emissions_lvl14,
                                                 combined_emissions_lvl15, combined_emissions_lvl16, combined_emissions_lvl17, combined_emissions_lvl18), deployment = "main"),

  #### join raymond model and aggregate to HUC2 level
  tar_target(allResults, abstractAllResults(allModelResults, raymond_01), deployment = "main")

  # validate discharge model

)
