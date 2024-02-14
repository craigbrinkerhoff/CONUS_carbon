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
source('src/run_lumped.R')
source('src/validateQ.R')
source('src/figures.R')
source('src/run_sources.R')

#parallelization and pipeline settings
plan(batchtools_slurm, template = "slurm_future.tmpl")
options(future.wait.interval = 5.0)
tar_option_set(packages = c('tidyr', 'plyr', 'dplyr', 'readr', 'cowplot', 'colorspace', 'ggplot2', 'GA', 'lubridate', 'Metrics', 'roxygen2', 'doParallel', 'terra', 'sf')) #required packages
tar_option_set(format = "qs") #qs compressed/serialized r objects
tar_option_set(resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = 1))))) #default num_cores setting (is changed for calibration later)
cores_req <- 25 #cores requested for calibration (overrides the default set right above this)



#### PIPELINE PARAMETERS------------------------------------------------------------------
#meta parameters
path_to_data <- '/nas/cee-water/cjgleason/craig/CONUS_ephemeral_data' #path to data repo
glorich_data <- readr::read_csv('data/HUC4_calibration.csv') #regional CO2 values for calibrating each HUC4 basin
raymond_coscat_lookup <- readr::read_csv('data/raymond_coscat.csv') #raymond co2 data by HUC2 region
lookUpTable <- readr::read_csv('data/HUC4_lookup.csv') #basin routing lookup table

#constant parameters
C_atmosphere <- 400 #[ppm]
emergenceQ <- 0.000105 #[m3/s]
C_groundwater <- 16000  #[ppm]

#calibrated parameters
lowerCBZ_riv <- 0 #[ppm]
lowerCBZ_lake <- 0 #[ppm]
lowerFWC_riv <- 0 #[ppm/s]
lowerFWC_lake <- 0 #[ppm/s]

upperCBZ_riv <- 2000 #[ppm]
upperCBZ_lake <- 2000 #[ppm]
upperFWC_riv <- 1e-2 #[ppm/s]
upperFWC_lake <- 1e-2 #[ppm/s]

#GA meta-parameters
myPopSize <- 25 #population size within each generation
mymaxIter <- 500 # maximum number generations before termination
myRun <- 50 #calibration will be terminated if no improvement over this many generations
maxFitness <- 1/10 #upper bound on fitness function before terminating calibration [1/ppm]
mutationRate <- 0.10 # mutation probability to avoid getting trapped in local maxima
cores <- 25 #how many cores to run GA in parallel on (also need to set in the slurm.tmpl file FYI...)




#### SETUP STATIC BRANCHING OF BASINS PER PROCESSING LEVEL-----------------------------------------------------
#headwater terminal basins
mapped_lvlTerminal <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 0 & is.na(lookUpTable$toBasin) == 1 & !(lookUpTable$HUC4 %in% c('1710', '0601', '1709', '1701', '1801', '1711', '1019', '1012', '1802', '0305')),]$HUC4), #ignore basins that need to be restarted
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))), #prep hydrography for routing
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, NA, #calibrate model to CO2
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              cue = tar_cue(mode = "never"),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, NA),cue = tar_cue(mode = "never")), #run calibrated version of model
       tar_target(written, writeToFile(final, huc4)), #write final model to file
       tar_target(emissions, calcEmissions(final, huc4)), #calc carbon emissions from model
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)), #calc emissions uncertainty due to calibration
       tar_target(map, indvRiverMaps(final, huc4)), #build river network map
       tar_target(basinProperties, calcBasinProperties(final, huc4)), #gather basin properties of interest
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)), #gather basin lake properties of interest
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)), #grab random sample for figure 2
       tar_target(glorich, getGlorichUS(path_to_data,huc4)), #snap in situ data to hydrography for figure 2
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters,C_groundwater, emissions, huc4, path_to_data, NA)), #calculate basin-scale CO2 sources
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)), #calculate reach-scale CO2 sources
       tar_target(sources_by_order, getResultsByOrder(sources)) #summarise reach-scale CO2 sources by stream order
)

#terminal basins that need to be restarted due to HPC issue
mapped_lvlTerminal_bugFix <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$HUC4 == '1802',]$HUC4), 
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(huc4)),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, NA),cue = tar_cue(mode = "never")),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, NA)),
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


# all other headwater basins
mapped_lvl0 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 0 & is.na(lookUpTable$toBasin) == 0 & !(lookUpTable$HUC4 %in% c('0601', '1709', '1701', '1801', '1711', '1019', '1012', '1802', '0305')),]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, NA,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              cue = tar_cue(mode = "never"),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, NA),cue = tar_cue(mode = "never")),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,C_atmosphere),cue = tar_cue(mode = "never")),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, NA)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)

# all other headwater basins that need to be restarted due to HPC issue
mapped_lvl0_bugFix <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$HUC4 %in% c('0601', '1709', '1701', '1801', '1711', '1019', '1012', '0305'),]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(huc4)),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, NA),cue = tar_cue(mode = "never")),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm),cue = tar_cue(mode = "never")),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, NA)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)

#level 1 basins
mapped_lvl1 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 1 & lookUpTable$HUC4 != '1804',]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl0,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              cue = tar_cue(mode = "never"),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl0),cue = tar_cue(mode = "never")),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm),cue = tar_cue(mode = "never")),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl0)),
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)

#level 1 basins that need to be restarted due to HPC issue
mapped_lvl1_bugFix <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 1 & lookUpTable$HUC4 == '1804',]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(huc4)),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl0),cue = tar_cue(mode = "never")),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm),cue = tar_cue(mode = "never")),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl0)),
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)

#level 2 basins
mapped_lvl2 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 2 & !(lookUpTable$HUC4 %in% c('1114', '1706')),]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl1,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              cue = tar_cue(mode = "never"),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl1),cue = tar_cue(mode = "never")),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm),cue = tar_cue(mode = "never")),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters,  C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl1)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)

#level 2 basins that need to be restarted due to HPC issue
mapped_lvl2_bugFix <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 2 & lookUpTable$HUC4 %in% c('1114', '1706'),]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(huc4)),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl1),cue = tar_cue(mode = "never")),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm),cue = tar_cue(mode = "never")),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl1)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 3 basins
mapped_lvl3 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 3 & !(lookUpTable$HUC4 %in% c('1702', '1304')),]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl2,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              cue = tar_cue(mode = "never"),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl2),cue = tar_cue(mode = "never")),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl2)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)

#level 3 basins that need to be restarted due to HPC issue
mapped_lvl3_bugFix <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 3 & lookUpTable$HUC4 %in% c('1304', '1702'),]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(huc4)),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl2)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl2)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 4 basins
mapped_lvl4 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 4,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl3,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl3)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl3)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)



#level 5 basins
mapped_lvl5 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 5 & lookUpTable$HUC4 != '1013',]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl4,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl4)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl4)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)




#level 5 basins that need to be restarted due to HPC issue
mapped_lvl5_bugFix <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 5 & lookUpTable$HUC4 == '1013',]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(huc4)),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl4)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl4)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)





#level 6 basins
mapped_lvl6 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 6 & !(lookUpTable$HUC4 %in% c('1014', '0427')),]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl5,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl5)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl5)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)



#level 6 basins that need to be restarted due to HPC issue
mapped_lvl6_bugFix <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 6 & lookUpTable$HUC4 %in% c('1014', '0427'),]$HUC4), 
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(huc4)),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl5)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl5)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)





#level 7 basins
mapped_lvl7 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 7,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl6,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl6)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl6)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)

#level 8 basins
mapped_lvl8 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 8,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl7,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl7)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl7)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 9 basins
mapped_lvl9 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 9,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl8,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl8)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl8)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 10 basins
mapped_lvl10 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 10,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl9,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl9)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl9)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 11 basins
mapped_lvl11 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 11,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl10,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl10)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl10)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 12 basins
mapped_lvl12 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 12,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
      # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl11,
      #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
      #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
      #                                                        myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
      #                                        resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(huc4)), #SINGLE BASIN: USE CALL THIS VERSION OF THE TARGET TO GRAB THE CACHED CALIBRATED PARAMETERS IF NECESSARY
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl11)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl11)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 13 basins
mapped_lvl13 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 13,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl12,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl12)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl12)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 14 basins
mapped_lvl14 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 14,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl13,
       #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
       #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
       #                                                        myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
       #                                        resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(huc4)), #SINGLE BASIN: USE CALL THIS VERSION OF THE TARGET TO GRAB THE CACHED CALIBRATED PARAMETERS IF NECESSARY   
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl13)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl13)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 15 basins
mapped_lvl15 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 15,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       # tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl14,
       #                                                        lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
       #                                                        upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
       #                                                        myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
       #                                        resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(huc4)), #SINGLE BASIN: USE CALL THIS VERSION OF THE TARGET TO GRAB THE CACHED CALIBRATED PARAMETERS IF NECESSARY
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl14)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl14)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)

#level 16 basins
mapped_lvl16 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 16,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl15,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl15)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl15)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 17 basins
mapped_lvl17 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 17,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl16,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl16)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(exportedCO2, getExported(final, huc4, lookUpTable,Catm)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl16)),              
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


#level 18 basins
mapped_lvl18 <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("setupHydrography"),
         huc4 = lookUpTable[lookUpTable$level == 18,]$HUC4),
       names = "huc4",
       tar_target(hydrography, method_function(path_to_data, huc4),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, huc4, glorich_data, C_groundwater, C_atmosphere, emergenceQ, exportedCO2_lvl17,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, huc4, emergenceQ, exportedCO2_lvl17)),
       tar_target(written, writeToFile(final, huc4)),
       tar_target(emissions, calcEmissions(final, huc4)),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,huc4)),
       tar_target(map, indvRiverMaps(final, huc4)),
       tar_target(basinProperties, calcBasinProperties(final, huc4)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, huc4)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, huc4)),
       tar_target(glorich, getGlorichUS(path_to_data,huc4)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, huc4, path_to_data, exportedCO2_lvl17)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,huc4)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)

#Special 1710 basin splitting
mapped_1710a <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("split1710"),
         sub = c('1710a')),
       names = "sub",
       tar_target(hydrography, method_function(hydrography_1710, sub)),
       tar_target(calibratedParameters, calibrateModelWrapper(hydrography, substr(sub,1,4), glorich_data, C_groundwater, C_atmosphere, emergenceQ, NA,
                                                              lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake,
                                                              upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake,
                                                              myPopSize, mymaxIter, myRun, maxFitness, mutationRate, cores),
                                              cue = tar_cue(mode = "never"),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, substr(sub,1,4), emergenceQ, NA),cue = tar_cue(mode = "never")),
       tar_target(written, writeToFile(final, substr(sub,1,4))),
       tar_target(emissions, calcEmissions(final, substr(sub,1,4))),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,sub)),
       tar_target(map, indvRiverMaps(final, sub)),
       tar_target(basinProperties, calcBasinProperties(final, sub)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, sub)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, sub)),
       tar_target(glorich, getGlorichUS(path_to_data,sub)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, sub, path_to_data, NA)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,sub)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)


mapped_1710b <- tar_map(
       unlist=FALSE,
       values = tibble(
         method_function = rlang::syms("split1710"),
         sub = c('1710b')),
       names = "sub",
       tar_target(hydrography, method_function(hydrography_1710, sub)),
       tar_target(calibratedParameters, grabCalibratedParameters_from_logs(sub)),
       tar_target(final, runModel(hydrography, calibratedParameters, C_groundwater, C_atmosphere, substr(sub,1,4), emergenceQ, NA)),
       tar_target(written, writeToFile(final, substr(sub,1,4))),
       tar_target(emissions, calcEmissions(final, substr(sub,1,4))),
       tar_target(cal_uncertainty, emissions_uncertainty(calibratedParameters, final,sub)),
       tar_target(map, indvRiverMaps(final, sub)),
       tar_target(basinProperties, calcBasinProperties(final, sub)),
       tar_target(basinLakeProperties, calcBasinLakeProperties(final, sub)),
       tar_target(randomSample, getRandomSample(final, raymond_coscat_lookup, sub)),
       tar_target(glorich, getGlorichUS(path_to_data,sub)),
       tar_target(sources_basin, getSourcesByBasin(hydrography, final, calibratedParameters, C_groundwater, emissions, sub, path_to_data, NA)),       
       tar_target(sources, getSourcesByRiver(hydrography, final, C_groundwater, calibratedParameters, emergenceQ,sub)),
       tar_target(sources_by_order, getResultsByOrder(sources))
)








#### ACTUAL PIPELINE----------------------------------
list(
  #### level terminal restart basins
  mapped_lvlTerminal_bugFix,

  #### level 0 terminal basins
  mapped_lvlTerminal,

  #### level 0 restart basins
  mapped_lvl0_bugFix,

  #### level 0
  mapped_lvl0,
  tar_combine(exportedCO2_lvl0, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches
  
  #### level 1 restart basins
  mapped_lvl1_bugFix,

  #### level 1 basins
  mapped_lvl1,
  tar_combine(exportedCO2_lvl1, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 2 restart basins
  mapped_lvl2_bugFix,

  #### level 2 basins
  mapped_lvl2,
  tar_combine(exportedCO2_lvl2, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 3 restart basins
  mapped_lvl3_bugFix,

  #### level 3 basins
  mapped_lvl3,
  tar_combine(exportedCO2_lvl3, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 4 basins
  mapped_lvl4,
  tar_combine(exportedCO2_lvl4, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 5 restart basins
  mapped_lvl5_bugFix,

  #### level 5 basins
  mapped_lvl5,
  tar_combine(exportedCO2_lvl5, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,                                     
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 6 restart basins
  mapped_lvl6_bugFix,

  #### level 6 basins
  mapped_lvl6,
  tar_combine(exportedCO2_lvl6, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 7 basins
  mapped_lvl7,
  tar_combine(exportedCO2_lvl7, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
                                     mapped_lvl7$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 8 basins
  mapped_lvl8,
  tar_combine(exportedCO2_lvl8, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,                                     
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 9 basins
  mapped_lvl9,
  tar_combine(exportedCO2_lvl9, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 10 basins
  mapped_lvl10,
  tar_combine(exportedCO2_lvl10, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,                                     
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 11 basins
  mapped_lvl11,
  tar_combine(exportedCO2_lvl11, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 12 basins
  mapped_lvl12,
  tar_combine(exportedCO2_lvl12, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2,
                                     mapped_lvl12$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 13 basins
  mapped_lvl13,
  tar_combine(exportedCO2_lvl13, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2,
                                     mapped_lvl12$exportedCO2,
                                     mapped_lvl13$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 14 basins
  mapped_lvl14,
  tar_combine(exportedCO2_lvl14, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2,
                                     mapped_lvl12$exportedCO2,
                                     mapped_lvl13$exportedCO2,
                                     mapped_lvl14$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 15 basins
  mapped_lvl15,
  tar_combine(exportedCO2_lvl15, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
                                     mapped_lvl7$exportedCO2,
                                     mapped_lvl8$exportedCO2,
                                     mapped_lvl9$exportedCO2,
                                     mapped_lvl10$exportedCO2,
                                     mapped_lvl11$exportedCO2,
                                     mapped_lvl12$exportedCO2,
                                     mapped_lvl13$exportedCO2,
                                     mapped_lvl14$exportedCO2,
                                     mapped_lvl15$exportedCO2), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment = "main"),  #aggregate model results across branches

  #### level 16 basins
  mapped_lvl16,
  tar_combine(exportedCO2_lvl16, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
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

  #### level 17 basins
  mapped_lvl17,
  tar_combine(exportedCO2_lvl17, list(mapped_lvl0$exportedCO2,
                                     mapped_lvl0_bugFix$exportedCO2,
                                     mapped_lvl1$exportedCO2,
                                     mapped_lvl1_bugFix$exportedCO2,
                                     mapped_lvl2$exportedCO2,
                                     mapped_lvl2_bugFix$exportedCO2,
                                     mapped_lvl3$exportedCO2,
                                     mapped_lvl3_bugFix$exportedCO2,
                                     mapped_lvl4$exportedCO2,
                                     mapped_lvl5$exportedCO2,
                                     mapped_lvl5_bugFix$exportedCO2,
                                     mapped_lvl6$exportedCO2,
                                     mapped_lvl6_bugFix$exportedCO2,
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

  #### level 18 basins
  mapped_lvl18,


  #### 1710 basin (split in two)
  tar_target(hydrography_1710, setupHydrography(path_to_data, '1710'),
                                              resources = tar_resources(future = tar_resources_future(plan = tweak(batchtools_slurm,template = "slurm_future.tmpl",resources = list(num_cores = cores_req))))),
  mapped_1710a,
  mapped_1710b,
 
  #### RUN LUMPED MODELS AND COMPARE AGAINST OUR MODEL (AND ESTIMATE UNCERTAINTY)----------------------------------------------------
  tar_target(lumped_01, runLumpedModels('01', raymond_coscat_lookup, list(hydrography_0101, hydrography_0102, hydrography_0103, hydrography_0104, hydrography_0105, hydrography_0106, hydrography_0107, hydrography_0108, hydrography_0109, hydrography_0110),
                                                                                          list(final_0101, final_0102, final_0103, final_0104, final_0105, final_0106, final_0107, final_0108, final_0109, final_0110),
                                                                                          list(emissions_0101, emissions_0102, emissions_0103, emissions_0104, emissions_0105, emissions_0106, emissions_0107, emissions_0108, emissions_0109, emissions_0110),
                                                                                          list(cal_uncertainty_0101, cal_uncertainty_0102, cal_uncertainty_0103, cal_uncertainty_0104, cal_uncertainty_0105, cal_uncertainty_0106, cal_uncertainty_0107, cal_uncertainty_0108, cal_uncertainty_0109, cal_uncertainty_0110))),
  tar_target(lumped_02, runLumpedModels('02', raymond_coscat_lookup, list(hydrography_0202, hydrography_0203, hydrography_0204, hydrography_0205),
                                                                                          list(final_0202, final_0203, final_0204, final_0205),
                                                                                          list(emissions_0202, emissions_0203, emissions_0204, emissions_0205),
                                                                                          list(cal_uncertainty_0202, cal_uncertainty_0203, cal_uncertainty_0204, cal_uncertainty_0205))),
  tar_target(lumped_03, runLumpedModels('03', raymond_coscat_lookup, list(hydrography_0301, hydrography_0302, hydrography_0303, hydrography_0304, hydrography_0305, hydrography_0306, hydrography_0307, hydrography_0308, hydrography_0309, hydrography_0310, hydrography_0311, hydrography_0312, hydrography_0313, hydrography_0314, hydrography_0315, hydrography_0316, hydrography_0317, hydrography_0318),
                                                                                          list(final_0301, final_0302, final_0303, final_0304, final_0305, final_0306, final_0307, final_0308, final_0309, final_0310, final_0311, final_0312, final_0313, final_0314, final_0315, final_0316, final_0317, final_0318),
                                                                                          list(emissions_0301, emissions_0302, emissions_0303, emissions_0304, emissions_0305, emissions_0306, emissions_0307, emissions_0308, emissions_0309, emissions_0310, emissions_0311, emissions_0312, emissions_0313, emissions_0314, emissions_0315, emissions_0316, emissions_0317, emissions_0318),
                                                                                          list(cal_uncertainty_0301, cal_uncertainty_0302, cal_uncertainty_0303, cal_uncertainty_0304, cal_uncertainty_0305, cal_uncertainty_0306, cal_uncertainty_0307, cal_uncertainty_0308, cal_uncertainty_0309, cal_uncertainty_0310, cal_uncertainty_0311, cal_uncertainty_0312, cal_uncertainty_0313, cal_uncertainty_0314, cal_uncertainty_0315, cal_uncertainty_0316, cal_uncertainty_0317, cal_uncertainty_0318))),
  tar_target(lumped_04, runLumpedModels('04', raymond_coscat_lookup, list(hydrography_0401,hydrography_0402, hydrography_0403, hydrography_0404, hydrography_0405, hydrography_0406, hydrography_0407, hydrography_0408, hydrography_0409, hydrography_0410, hydrography_0411, hydrography_0412, hydrography_0413, hydrography_0414, hydrography_0418, hydrography_0419, hydrography_0420, hydrography_0424,
                                                                                            hydrography_0426, hydrography_0427, hydrography_0428, hydrography_0429, hydrography_0430),
                                                                                          list(final_0401,final_0402, final_0403, final_0404, final_0405, final_0406, final_0407, final_0408, final_0409, final_0410, final_0411, final_0412, final_0413, final_0414, final_0418, final_0419, final_0420, final_0424,
                                                                                            final_0426, final_0427, final_0428, final_0429, final_0430),
                                                                                          list(emissions_0401,emissions_0402, emissions_0403, emissions_0404, emissions_0405, emissions_0406, emissions_0407, emissions_0408, emissions_0409, emissions_0410, emissions_0411, emissions_0412, emissions_0413, emissions_0414, emissions_0418, emissions_0419, emissions_0420, emissions_0424,
                                                                                            emissions_0426, emissions_0427, emissions_0428, emissions_0429, emissions_0430),
                                                                                          list(cal_uncertainty_0401,cal_uncertainty_0402, cal_uncertainty_0403, cal_uncertainty_0404, cal_uncertainty_0405, cal_uncertainty_0406, cal_uncertainty_0407, cal_uncertainty_0408, cal_uncertainty_0409, cal_uncertainty_0410, cal_uncertainty_0411, cal_uncertainty_0412, cal_uncertainty_0413, cal_uncertainty_0414, cal_uncertainty_0418, cal_uncertainty_0419, cal_uncertainty_0420, cal_uncertainty_0424,
                                                                                            cal_uncertainty_0426, cal_uncertainty_0427, cal_uncertainty_0428, cal_uncertainty_0429, cal_uncertainty_0430))),
  tar_target(lumped_05, runLumpedModels('05', raymond_coscat_lookup, list(hydrography_0501, hydrography_0502, hydrography_0503, hydrography_0504, hydrography_0505, hydrography_0506, hydrography_0507, hydrography_0508, hydrography_0509, hydrography_0510, hydrography_0511, hydrography_0512, hydrography_0513, hydrography_0514),
                                                                                          list(final_0501, final_0502, final_0503, final_0504, final_0505, final_0506, final_0507, final_0508, final_0509, final_0510, final_0511, final_0512, final_0513, final_0514),
                                                                                          list(emissions_0501, emissions_0502, emissions_0503, emissions_0504, emissions_0505, emissions_0506, emissions_0507, emissions_0508, emissions_0509, emissions_0510, emissions_0511, emissions_0512, emissions_0513, emissions_0514),
                                                                                          list(cal_uncertainty_0501, cal_uncertainty_0502, cal_uncertainty_0503, cal_uncertainty_0504, cal_uncertainty_0505, cal_uncertainty_0506, cal_uncertainty_0507, cal_uncertainty_0508, cal_uncertainty_0509, cal_uncertainty_0510, cal_uncertainty_0511, cal_uncertainty_0512, cal_uncertainty_0513, cal_uncertainty_0514))),
  tar_target(lumped_06, runLumpedModels('06', raymond_coscat_lookup, list(hydrography_0601, hydrography_0602, hydrography_0603, hydrography_0604),
                                                                                          list(final_0601, final_0602, final_0603, final_0604),
                                                                                          list(emissions_0601, emissions_0602, emissions_0603, emissions_0604),
                                                                                          list(cal_uncertainty_0601, cal_uncertainty_0602, cal_uncertainty_0603, cal_uncertainty_0604))),
  tar_target(lumped_07, runLumpedModels('07', raymond_coscat_lookup, list(hydrography_0701, hydrography_0702, hydrography_0703, hydrography_0704, hydrography_0705, hydrography_0706, hydrography_0707, hydrography_0708, hydrography_0709, hydrography_0710, hydrography_0711, hydrography_0712, hydrography_0713, hydrography_0714),
                                                                                          list(final_0701, final_0702, final_0703, final_0704, final_0705, final_0706, final_0707, final_0708, final_0709, final_0710, final_0711, final_0712, final_0713, final_0714),
                                                                                          list(emissions_0701, emissions_0702, emissions_0703, emissions_0704, emissions_0705, emissions_0706, emissions_0707, emissions_0708, emissions_0709, emissions_0710, emissions_0711, emissions_0712, emissions_0713, emissions_0714),
                                                                                          list(cal_uncertainty_0701, cal_uncertainty_0702, cal_uncertainty_0703, cal_uncertainty_0704, cal_uncertainty_0705, cal_uncertainty_0706, cal_uncertainty_0707, cal_uncertainty_0708, cal_uncertainty_0709, cal_uncertainty_0710, cal_uncertainty_0711, cal_uncertainty_0712, cal_uncertainty_0713, cal_uncertainty_0714))),
  tar_target(lumped_08, runLumpedModels('08', raymond_coscat_lookup, list(hydrography_0801, hydrography_0802, hydrography_0803, hydrography_0804, hydrography_0805, hydrography_0806, hydrography_0808, hydrography_0809),
                                                                                          list(final_0801, final_0802, final_0803, final_0804, final_0805, final_0806, final_0808, final_0809),
                                                                                          list(emissions_0801, emissions_0802, emissions_0803, emissions_0804, emissions_0805, emissions_0806, emissions_0808, emissions_0809),
                                                                                          list(cal_uncertainty_0801, cal_uncertainty_0802, cal_uncertainty_0803, cal_uncertainty_0804, cal_uncertainty_0805, cal_uncertainty_0806, cal_uncertainty_0808, cal_uncertainty_0809))),
  tar_target(lumped_09, runLumpedModels('09', raymond_coscat_lookup, list(hydrography_0901, hydrography_0902, hydrography_0903, hydrography_0904),
                                                                                          list(final_0901, final_0902, final_0903, final_0904),
                                                                                          list(emissions_0901, emissions_0902, emissions_0903, emissions_0904),
                                                                                          list(cal_uncertainty_0901, cal_uncertainty_0902, cal_uncertainty_0903, cal_uncertainty_0904))),
  tar_target(lumped_10, runLumpedModels('10', raymond_coscat_lookup, list(hydrography_1002, hydrography_1003, hydrography_1004, hydrography_1005, hydrography_1006, hydrography_1007, hydrography_1008, hydrography_1009, hydrography_1010, hydrography_1011, hydrography_1012, hydrography_1013, hydrography_1014, hydrography_1015,
                                                                                            hydrography_1016, hydrography_1017, hydrography_1018, hydrography_1019, hydrography_1020, hydrography_1021, hydrography_1022, hydrography_1023, hydrography_1024, hydrography_1025, hydrography_1026, hydrography_1027, hydrography_1028, hydrography_1029, hydrography_1030),
                                                                                          list(final_1002, final_1003, final_1004, final_1005, final_1006, final_1007, final_1008, final_1009, final_1010, final_1011, final_1012, final_1013, final_1014, final_1015,
                                                                                            final_1016, final_1017, final_1018, final_1019, final_1020, final_1021, final_1022, final_1023, final_1024, final_1025, final_1026, final_1027, final_1028, final_1029, final_1030),
                                                                                          list(emissions_1002, emissions_1003, emissions_1004, emissions_1005, emissions_1006, emissions_1007, emissions_1008, emissions_1009, emissions_1010, emissions_1011, emissions_1012, emissions_1013, emissions_1014, emissions_1015,
                                                                                            emissions_1016, emissions_1017, emissions_1018, emissions_1019, emissions_1020, emissions_1021, emissions_1022, emissions_1023, emissions_1024, emissions_1025, emissions_1026, emissions_1027, emissions_1028, emissions_1029, emissions_1030),
                                                                                          list(cal_uncertainty_1002, cal_uncertainty_1003, cal_uncertainty_1004, cal_uncertainty_1005, cal_uncertainty_1006, cal_uncertainty_1007, cal_uncertainty_1008, cal_uncertainty_1009, cal_uncertainty_1010, cal_uncertainty_1011, cal_uncertainty_1012, cal_uncertainty_1013, cal_uncertainty_1014, cal_uncertainty_1015,
                                                                                            cal_uncertainty_1016, cal_uncertainty_1017, cal_uncertainty_1018, cal_uncertainty_1019, cal_uncertainty_1020, cal_uncertainty_1021, cal_uncertainty_1022, cal_uncertainty_1023, cal_uncertainty_1024, cal_uncertainty_1025, cal_uncertainty_1026, cal_uncertainty_1027, cal_uncertainty_1028, cal_uncertainty_1029, cal_uncertainty_1030))),
  tar_target(lumped_11, runLumpedModels('11', raymond_coscat_lookup, list(hydrography_1101, hydrography_1103, hydrography_1103, hydrography_1104, hydrography_1105, hydrography_1106, hydrography_1107, hydrography_1108, hydrography_1109, hydrography_1110, hydrography_1111, hydrography_1112, hydrography_1113, hydrography_1114),
                                                                                          list(final_1101, final_1103, final_1103, final_1104, final_1105, final_1106, final_1107, final_1108, final_1109, final_1110, final_1111, final_1112, final_1113, final_1114),
                                                                                          list(emissions_1101, emissions_1103, emissions_1103, emissions_1104, emissions_1105, emissions_1106, emissions_1107, emissions_1108, emissions_1109, emissions_1110, emissions_1111, emissions_1112, emissions_1113, emissions_1114),
                                                                                          list(cal_uncertainty_1101, cal_uncertainty_1103, cal_uncertainty_1103, cal_uncertainty_1104, cal_uncertainty_1105, cal_uncertainty_1106, cal_uncertainty_1107, cal_uncertainty_1108, cal_uncertainty_1109, cal_uncertainty_1110, cal_uncertainty_1111, cal_uncertainty_1112, cal_uncertainty_1113, cal_uncertainty_1114))),
  tar_target(lumped_12, runLumpedModels('12', raymond_coscat_lookup, list(hydrography_1201, hydrography_1202, hydrography_1203, hydrography_1204, hydrography_1205, hydrography_1206, hydrography_1207, hydrography_1208, hydrography_1209, hydrography_1210, hydrography_1211),
                                                                                          list(final_1201, final_1202, final_1203, final_1204, final_1205, final_1206, final_1207, final_1208, final_1209, final_1210, final_1211),
                                                                                          list(emissions_1201, emissions_1202, emissions_1203, emissions_1204, emissions_1205, emissions_1206, emissions_1207, emissions_1208, emissions_1209, emissions_1210, emissions_1211),
                                                                                          list(cal_uncertainty_1201, cal_uncertainty_1202, cal_uncertainty_1203, cal_uncertainty_1204, cal_uncertainty_1205, cal_uncertainty_1206, cal_uncertainty_1207, cal_uncertainty_1208, cal_uncertainty_1209, cal_uncertainty_1210, cal_uncertainty_1211))),
  tar_target(lumped_13, runLumpedModels('13', raymond_coscat_lookup, list(hydrography_1301, hydrography_1302, hydrography_1303, hydrography_1304, hydrography_1305, hydrography_1306, hydrography_1307, hydrography_1308, hydrography_1309),
                                                                                          list(final_1301, final_1302, final_1303, final_1304, final_1305, final_1306, final_1307, final_1308, final_1309),
                                                                                          list(emissions_1301, emissions_1302, emissions_1303, emissions_1304, emissions_1305, emissions_1306, emissions_1307, emissions_1308, emissions_1309),
                                                                                          list(cal_uncertainty_1301, cal_uncertainty_1302, cal_uncertainty_1303, cal_uncertainty_1304, cal_uncertainty_1305, cal_uncertainty_1306, cal_uncertainty_1307, cal_uncertainty_1308, cal_uncertainty_1309))),
  tar_target(lumped_14, runLumpedModels('14', raymond_coscat_lookup, list(hydrography_1401, hydrography_1402, hydrography_1403, hydrography_1404, hydrography_1405, hydrography_1406, hydrography_1407, hydrography_1408),
                                                                                          list(final_1401, final_1402, final_1403, final_1404, final_1405, final_1406, final_1407, final_1408),
                                                                                          list(emissions_1401, emissions_1402, emissions_1403, emissions_1404, emissions_1405, emissions_1406, emissions_1407, emissions_1408),
                                                                                          list(cal_uncertainty_1401, cal_uncertainty_1402, cal_uncertainty_1403, cal_uncertainty_1404, cal_uncertainty_1405, cal_uncertainty_1406, cal_uncertainty_1407, cal_uncertainty_1408))),
  tar_target(lumped_15, runLumpedModels('15', raymond_coscat_lookup, list(hydrography_1501, hydrography_1502, hydrography_1503, hydrography_1504, hydrography_1505, hydrography_1506, hydrography_1507, hydrography_1508),
                                                                                          list(final_1501, final_1502, final_1503, final_1504, final_1505, final_1506, final_1507, final_1508),
                                                                                          list(emissions_1501, emissions_1502, emissions_1503, emissions_1504, emissions_1505, emissions_1506, emissions_1507, emissions_1508),
                                                                                          list(cal_uncertainty_1501, cal_uncertainty_1502, cal_uncertainty_1503, cal_uncertainty_1504, cal_uncertainty_1505, cal_uncertainty_1506, cal_uncertainty_1507, cal_uncertainty_1508))),
  tar_target(lumped_16, runLumpedModels('16', raymond_coscat_lookup, list(hydrography_1601, hydrography_1602, hydrography_1603, hydrography_1604, hydrography_1605, hydrography_1606),
                                                                                          list(final_1601, final_1602, final_1603, final_1604, final_1605, final_1606),
                                                                                          list(emissions_1601, emissions_1602, emissions_1603, emissions_1604, emissions_1605, emissions_1606),
                                                                                          list(cal_uncertainty_1601, cal_uncertainty_1602, cal_uncertainty_1603, cal_uncertainty_1604, cal_uncertainty_1605, cal_uncertainty_1606))),
  tar_target(lumped_17, runLumpedModels('17', raymond_coscat_lookup, list(hydrography_1701, hydrography_1702, hydrography_1703, hydrography_1704, hydrography_1705, hydrography_1706, hydrography_1707, hydrography_1708, hydrography_1709, hydrography_1710a, hydrography_1710b, hydrography_1711, hydrography_1712),
                                                                                          list(final_1701, final_1702, final_1703, final_1704, final_1705, final_1706, final_1707, final_1708, final_1709, final_1710a, final_1710b, final_1711, final_1712),
                                                                                          list(emissions_1701, emissions_1702, emissions_1703, emissions_1704, emissions_1705, emissions_1706, emissions_1707, emissions_1708, emissions_1709, emissions_1710a, emissions_1710b, emissions_1711, emissions_1712),
                                                                                          list(cal_uncertainty_1701, cal_uncertainty_1702, cal_uncertainty_1703, cal_uncertainty_1704, cal_uncertainty_1705, cal_uncertainty_1706, cal_uncertainty_1707, cal_uncertainty_1708, cal_uncertainty_1709, cal_uncertainty_1710a, cal_uncertainty_1710b, cal_uncertainty_1711, cal_uncertainty_1712))),
  tar_target(lumped_18, runLumpedModels('18', raymond_coscat_lookup, list(hydrography_1801, hydrography_1802, hydrography_1803, hydrography_1804, hydrography_1805, hydrography_1806, hydrography_1807, hydrography_1808, hydrography_1809, hydrography_1810),
                                                                                          list(final_1801, final_1802, final_1803, final_1804, final_1805, final_1806, final_1807, final_1808, final_1809, final_1810),
                                                                                          list(emissions_1801, emissions_1802, emissions_1803, emissions_1804, emissions_1805, emissions_1806, emissions_1807, emissions_1808, emissions_1809, emissions_1810),
                                                                                          list(cal_uncertainty_1801, cal_uncertainty_1802, cal_uncertainty_1803, cal_uncertainty_1804, cal_uncertainty_1805, cal_uncertainty_1806, cal_uncertainty_1807, cal_uncertainty_1808, cal_uncertainty_1809, cal_uncertainty_1810))),

  #AGGREGATE ALL PER-BASIN TARGETS----------------------------------------------------------
  #combine all calibration uncertainty targets
  tar_combine(combined_uncertainty_init, list(mapped_lvlTerminal$cal_uncertainty,
                                       mapped_lvlTerminal_bugFix$cal_uncertainty,
                                       mapped_lvl0$cal_uncertainty,
                                       mapped_lvl0_bugFix$cal_uncertainty,
                                       mapped_lvl1$cal_uncertainty,
                                       mapped_lvl1_bugFix$cal_uncertainty,
                                       mapped_lvl2$cal_uncertainty,
                                       mapped_lvl2_bugFix$cal_uncertainty,
                                       mapped_lvl3$cal_uncertainty,
                                       mapped_lvl3_bugFix$cal_uncertainty,
                                       mapped_lvl4$cal_uncertainty,
                                       mapped_lvl5$cal_uncertainty,
                                       mapped_lvl5_bugFix$cal_uncertainty,
                                       mapped_lvl6$cal_uncertainty,
                                       mapped_lvl6_bugFix$cal_uncertainty,
                                       mapped_lvl7$cal_uncertainty,
                                       mapped_lvl8$cal_uncertainty,
                                       mapped_lvl9$cal_uncertainty,
                                       mapped_lvl10$cal_uncertainty,
                                       mapped_lvl11$cal_uncertainty,
                                       mapped_lvl12$cal_uncertainty,
                                       mapped_lvl13$cal_uncertainty,
                                       mapped_lvl14$cal_uncertainty,
                                       mapped_lvl15$cal_uncertainty,
                                       mapped_lvl16$cal_uncertainty,
                                       mapped_lvl17$cal_uncertainty,
                                       mapped_lvl18$cal_uncertainty,
                                       mapped_1710a$cal_uncertainty,
                                       mapped_1710b$cal_uncertainty), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment='main'),
  tar_target(combined_uncertainty, fixCombo0427(combined_uncertainty_init)),

  #combine all basin-scale CO2 source targets
  tar_combine(combined_sources_basin_init, list(mapped_lvlTerminal$sources_basin,
                                       mapped_lvlTerminal_bugFix$sources_basin,
                                       mapped_lvl0$sources_basin,
                                       mapped_lvl0_bugFix$sources_basin,
                                       mapped_lvl1$sources_basin,
                                       mapped_lvl1_bugFix$sources_basin,
                                       mapped_lvl2$sources_basin,
                                       mapped_lvl2_bugFix$sources_basin,
                                       mapped_lvl3$sources_basin,
                                       mapped_lvl3_bugFix$sources_basin,
                                       mapped_lvl4$sources_basin,
                                       mapped_lvl5$sources_basin,
                                       mapped_lvl5_bugFix$sources_basin,
                                       mapped_lvl6$sources_basin,
                                       mapped_lvl6_bugFix$sources_basin,
                                       mapped_lvl7$sources_basin,
                                       mapped_lvl8$sources_basin,
                                       mapped_lvl9$sources_basin,
                                       mapped_lvl10$sources_basin,
                                       mapped_lvl11$sources_basin,
                                       mapped_lvl12$sources_basin,
                                       mapped_lvl13$sources_basin,
                                       mapped_lvl14$sources_basin,
                                       mapped_lvl15$sources_basin,
                                       mapped_lvl16$sources_basin,
                                       mapped_lvl17$sources_basin,
                                       mapped_lvl18$sources_basin,
                                       mapped_1710a$sources_basin,
                                       mapped_1710b$sources_basin), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment='main'),
  tar_target(combined_sources_basin, fixCombo0427(combined_sources_basin_init)),

  #combine all model emission targets
  tar_combine(combined_emissions_init, list(mapped_lvlTerminal$emissions,
                                       mapped_lvlTerminal_bugFix$emissions,
                                       mapped_lvl0$emissions,
                                       mapped_lvl0_bugFix$emissions,
                                       mapped_lvl1$emissions,
                                       mapped_lvl1_bugFix$emissions,
                                       mapped_lvl2$emissions,
                                       mapped_lvl2_bugFix$emissions,
                                       mapped_lvl3$emissions,
                                       mapped_lvl3_bugFix$emissions,
                                       mapped_lvl4$emissions,
                                       mapped_lvl5$emissions,
                                       mapped_lvl5_bugFix$emissions,
                                       mapped_lvl6$emissions,
                                       mapped_lvl6_bugFix$emissions,
                                       mapped_lvl7$emissions,
                                       mapped_lvl8$emissions,
                                       mapped_lvl9$emissions,
                                       mapped_lvl10$emissions,
                                       mapped_lvl11$emissions,
                                       mapped_lvl12$emissions,
                                       mapped_lvl13$emissions,
                                       mapped_lvl14$emissions,
                                       mapped_lvl15$emissions,
                                       mapped_lvl16$emissions,
                                       mapped_lvl17$emissions,
                                       mapped_lvl18$emissions,                                       
                                       mapped_1710a$emissions,
                                       mapped_1710b$emissions), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment='main'),
  tar_target(combined_emissions, fixCombo0427(combined_emissions_init)),

  #combine all model snapped in situ data targets
  tar_combine(combined_glorich_init, list(mapped_lvlTerminal$glorich,
                                       mapped_lvlTerminal_bugFix$glorich,
                                       mapped_lvl0$glorich,
                                       mapped_lvl0_bugFix$glorich,
                                       mapped_lvl1$glorich,
                                       mapped_lvl1_bugFix$glorich,
                                       mapped_lvl2$glorich,
                                       mapped_lvl2_bugFix$glorich,
                                       mapped_lvl3$glorich,
                                       mapped_lvl3_bugFix$glorich,
                                       mapped_lvl4$glorich,
                                       mapped_lvl5$glorich,
                                       mapped_lvl5_bugFix$glorich,
                                       mapped_lvl6$glorich,
                                       mapped_lvl6_bugFix$glorich,
                                       mapped_lvl7$glorich,
                                       mapped_lvl8$glorich,
                                       mapped_lvl9$glorich,
                                       mapped_lvl10$glorich,
                                       mapped_lvl11$glorich,
                                       mapped_lvl12$glorich,
                                       mapped_lvl13$glorich,
                                       mapped_lvl14$glorich,
                                       mapped_lvl15$glorich,
                                       mapped_lvl16$glorich,
                                       mapped_lvl17$glorich,
                                       mapped_lvl18$glorich,                                       
                                       mapped_1710a$glorich,
                                       mapped_1710b$glorich), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment='main'),
  tar_target(combined_glorich, fixCombo0427(combined_glorich_init)),

  #combine all by-basin property targets
  tar_combine(combined_basinProperties_init, list(mapped_lvlTerminal$basinProperties,
                                       mapped_lvlTerminal_bugFix$basinProperties,
                                       mapped_lvl0$basinProperties,
                                       mapped_lvl0_bugFix$basinProperties,
                                       mapped_lvl1$basinProperties,
                                       mapped_lvl1_bugFix$basinProperties,
                                       mapped_lvl2$basinProperties,
                                       mapped_lvl2_bugFix$basinProperties,
                                       mapped_lvl3$basinProperties,
                                       mapped_lvl3_bugFix$basinProperties,
                                       mapped_lvl4$basinProperties,
                                       mapped_lvl5$basinProperties,
                                       mapped_lvl5_bugFix$basinProperties,
                                       mapped_lvl6$basinProperties,
                                       mapped_lvl6_bugFix$basinProperties,
                                       mapped_lvl7$basinProperties,
                                       mapped_lvl8$basinProperties,
                                       mapped_lvl9$basinProperties,
                                       mapped_lvl10$basinProperties,
                                       mapped_lvl11$basinProperties,
                                       mapped_lvl12$basinProperties,
                                       mapped_lvl13$basinProperties,
                                       mapped_lvl14$basinProperties,
                                       mapped_lvl15$basinProperties,
                                       mapped_lvl16$basinProperties,
                                       mapped_lvl17$basinProperties,
                                       mapped_lvl18$basinProperties,                                       
                                       mapped_1710a$basinProperties,
                                       mapped_1710b$basinProperties), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment='main'),
  tar_target(combined_basinProperties, fixCombo0427(combined_basinProperties_init)),



  #combine all by-basin lake property targets
  tar_combine(combined_basinLakeProperties_init, list(mapped_lvlTerminal$basinLakeProperties,
                                       mapped_lvlTerminal_bugFix$basinLakeProperties,
                                       mapped_lvl0$basinLakeProperties,
                                       mapped_lvl0_bugFix$basinLakeProperties,
                                       mapped_lvl1$basinLakeProperties,
                                       mapped_lvl1_bugFix$basinLakeProperties,
                                       mapped_lvl2$basinLakeProperties,
                                       mapped_lvl2_bugFix$basinLakeProperties,
                                       mapped_lvl3$basinLakeProperties,
                                       mapped_lvl3_bugFix$basinLakeProperties,
                                       mapped_lvl4$basinLakeProperties,
                                       mapped_lvl5$basinLakeProperties,
                                       mapped_lvl5_bugFix$basinLakeProperties,
                                       mapped_lvl6$basinLakeProperties,
                                       mapped_lvl6_bugFix$basinLakeProperties,
                                       mapped_lvl7$basinLakeProperties,
                                       mapped_lvl8$basinLakeProperties,
                                       mapped_lvl9$basinLakeProperties,
                                       mapped_lvl10$basinLakeProperties,
                                       mapped_lvl11$basinLakeProperties,
                                       mapped_lvl12$basinLakeProperties,
                                       mapped_lvl13$basinLakeProperties,
                                       mapped_lvl14$basinLakeProperties,
                                       mapped_lvl15$basinLakeProperties,
                                       mapped_lvl16$basinLakeProperties,
                                       mapped_lvl17$basinLakeProperties,
                                       mapped_lvl18$basinLakeProperties,                                       
                                       mapped_1710a$basinLakeProperties,
                                       mapped_1710b$basinLakeProperties), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment='main'),
  tar_target(combined_basinLakeProperties, fixCombo0427(combined_basinLakeProperties_init)),

  #combine all basin maps
  tar_combine(combined_rivermaps, list(mapped_lvlTerminal$map,
                                       mapped_lvlTerminal_bugFix$map,
                                       mapped_lvl0$map,
                                       mapped_lvl0_bugFix$map,
                                       mapped_lvl1$map,
                                       mapped_lvl1_bugFix$map,
                                       mapped_lvl2$map,
                                       mapped_lvl2_bugFix$map,
                                       mapped_lvl3$map,
                                       mapped_lvl3_bugFix$map,
                                       mapped_lvl4$map,
                                       mapped_lvl5$map,
                                       mapped_lvl5_bugFix$map,
                                       mapped_lvl6$map,
                                       mapped_lvl6_bugFix$map,
                                       mapped_lvl7$map,
                                       mapped_lvl8$map,
                                       mapped_lvl9$map,
                                       mapped_lvl10$map,
                                       mapped_lvl11$map,
                                       mapped_lvl12$map,
                                       mapped_lvl13$map,
                                       mapped_lvl14$map,
                                       mapped_lvl15$map,
                                       mapped_lvl16$map,
                                       mapped_lvl17$map,
                                       mapped_lvl18$map,                                       
                                       mapped_1710a$map,
                                       mapped_1710b$map), deployment='main', command = list(!!!.x)),

  #combine all random hydrography samples
  tar_combine(combined_randomSample_init, list(mapped_lvlTerminal$randomSample,
                                       mapped_lvlTerminal_bugFix$randomSample,
                                       mapped_lvl0$randomSample,
                                       mapped_lvl0_bugFix$randomSample,
                                       mapped_lvl1$randomSample,
                                       mapped_lvl1_bugFix$randomSample,
                                       mapped_lvl2$randomSample,
                                       mapped_lvl2_bugFix$randomSample,
                                       mapped_lvl3$randomSample,
                                       mapped_lvl3_bugFix$randomSample,
                                       mapped_lvl4$randomSample,
                                       mapped_lvl5$randomSample,
                                       mapped_lvl5_bugFix$randomSample,
                                       mapped_lvl6$randomSample,
                                       mapped_lvl6_bugFix$randomSample,
                                       mapped_lvl7$randomSample,
                                       mapped_lvl8$randomSample,
                                       mapped_lvl9$randomSample,
                                       mapped_lvl10$randomSample,
                                       mapped_lvl11$randomSample,
                                       mapped_lvl12$randomSample,
                                       mapped_lvl13$randomSample,
                                       mapped_lvl14$randomSample,
                                       mapped_lvl15$randomSample,
                                       mapped_lvl16$randomSample,
                                       mapped_lvl17$randomSample,
                                       mapped_lvl18$randomSample,                                       
                                       mapped_1710a$randomSample,
                                       mapped_1710b$randomSample), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment='main'),
  tar_target(combined_randomSample, fixCombo0427(combined_randomSample_init)),

  #combine all model calibration outputs
  tar_combine(combined_calibrationOutput, list(mapped_lvlTerminal$calibratedParameters,
                                       mapped_lvlTerminal_bugFix$calibratedParameters,
                                       mapped_lvl0$calibratedParameters,
                                       mapped_lvl0_bugFix$calibratedParameters,
                                       mapped_lvl1$calibratedParameters,
                                       mapped_lvl1_bugFix$calibratedParameters,
                                       mapped_lvl2$calibratedParameters,
                                       mapped_lvl2_bugFix$calibratedParameters,
                                       mapped_lvl3$calibratedParameters,
                                       mapped_lvl3_bugFix$calibratedParameters,
                                       mapped_lvl4$calibratedParameters,
                                       mapped_lvl5$calibratedParameters,
                                       mapped_lvl5_bugFix$calibratedParameters,
                                       mapped_lvl6$calibratedParameters,
                                       mapped_lvl6_bugFix$calibratedParameters,
                                       mapped_lvl7$calibratedParameters,
                                       mapped_lvl8$calibratedParameters,
                                       mapped_lvl9$calibratedParameters,
                                       mapped_lvl10$calibratedParameters,
                                       mapped_lvl11$calibratedParameters,
                                       mapped_lvl12$calibratedParameters,
                                       mapped_lvl13$calibratedParameters,
                                       mapped_lvl14$calibratedParameters,
                                       mapped_lvl15$calibratedParameters,
                                       mapped_lvl16$calibratedParameters,
                                       mapped_lvl17$calibratedParameters,
                                       mapped_lvl18$calibratedParameters,                                       
                                       mapped_1710a$calibratedParameters,
                                       mapped_1710b$calibratedParameters), deployment='main', command = list(!!!.x)),

  #combine all reach-scale CO2 sources, summarized by stream order
  tar_combine(combined_sources_by_order_init, list(mapped_lvlTerminal$sources_by_order,
                                       mapped_lvlTerminal_bugFix$sources_by_order,
                                       mapped_lvl0$sources_by_order,
                                       mapped_lvl0_bugFix$sources_by_order,
                                       mapped_lvl1$sources_by_order,
                                       mapped_lvl1_bugFix$sources_by_order,
                                       mapped_lvl2$sources_by_order,
                                       mapped_lvl2_bugFix$sources_by_order,
                                       mapped_lvl3$sources_by_order,
                                       mapped_lvl3_bugFix$sources_by_order,
                                       mapped_lvl4$sources_by_order,
                                       mapped_lvl5$sources_by_order,
                                       mapped_lvl5_bugFix$sources_by_order,
                                       mapped_lvl6$sources_by_order,
                                       mapped_lvl6_bugFix$sources_by_order,
                                       mapped_lvl7$sources_by_order,
                                       mapped_lvl8$sources_by_order,
                                       mapped_lvl9$sources_by_order,
                                       mapped_lvl10$sources_by_order,
                                       mapped_lvl11$sources_by_order,
                                       mapped_lvl12$sources_by_order,
                                       mapped_lvl13$sources_by_order,
                                       mapped_lvl14$sources_by_order,
                                       mapped_lvl15$sources_by_order,
                                       mapped_lvl16$sources_by_order,
                                       mapped_lvl17$sources_by_order,
                                       mapped_lvl18$sources_by_order,                                       
                                       mapped_1710a$sources_by_order,
                                       mapped_1710b$sources_by_order), command = dplyr::bind_rows(!!!.x, .id = "method"), deployment='main'),
  tar_target(combined_sources_by_order, fixCombo0427(combined_sources_by_order_init)),

  #####BUILD SHAPEFILES FOR MAPPING
  tar_target(shapefile_huc2, saveShapefile_huc2(path_to_data,
                                                c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18'),
                                                list(lumped_01, lumped_02, lumped_03, lumped_04, lumped_05, lumped_06, lumped_07, lumped_08, lumped_09, lumped_10, lumped_11, lumped_12, lumped_13, lumped_14, lumped_15, lumped_16, lumped_17, lumped_18))),
  tar_target(shapefile_huc4, saveShapefile_huc4(path_to_data, combined_sources_basin, combined_emissions)),

  #### BUILD SUMMARY OBJECT AND WRITE IT TO RDS
  tar_target(summaryObject, gatherResults(combined_emissions, combined_sources_basin, figModelCompare, combined_basinProperties, combined_basinLakeProperties, combined_sources_by_order,
                                          list(lumped_01, lumped_02, lumped_03, lumped_04, lumped_05, lumped_06, lumped_07, lumped_08, lumped_09, lumped_10, lumped_11, lumped_12, lumped_13, lumped_14, lumped_15, lumped_16, lumped_17, lumped_18))),

  ###### GENERATE PAPER FIGURES
  tar_target(figMainMap1_jt, mainMapFunction1(combined_rivermaps)),
  tar_target(figMainMap2_jt, mainMapFunction2(map_0205, map_0206,map_0207,map_0208,map_0502, map_0503, map_0501, map_0505)),
  tar_target(figMainMap1, mainMapFunction1(combined_rivermaps)),
  tar_target(figMainMap2, mainMapFunction2(map_0205, map_0206,map_0207,map_0208,map_0502, map_0503, map_0501, map_0505)),
  tar_target(figSources, sourcesMap(path_to_data, shapefile_huc4, combined_sources_by_order)),
  tar_target(figLakes, lakesMap(path_to_data, shapefile_huc4)),
  tar_target(figCompareLumped, compareAgainstLumped(path_to_data, shapefile_huc2)),
  tar_target(figModelCompare, compareModels(path_to_data, combined_randomSample, list(lumped_01, lumped_02, lumped_03, lumped_04, lumped_05, lumped_06, lumped_07, lumped_08, lumped_09, lumped_10, lumped_11, lumped_12, lumped_13, lumped_14, lumped_15, lumped_16, lumped_17, lumped_18), combined_glorich)),

  ###### VALIDATE RIVER DISCHARGE MODEL
  tar_target(nhdGages, getNHDGages(path_to_data, c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18'))), #gages joined to NHD a priori, used for erom verification
  tar_target(USGS_data, getGageData(path_to_data, nhdGages, c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18'))), #calculates mean observed flow 1970-2018 to verify erom model

  ###### GENERATE SUPPLEMENTARY FIGURES
  tar_target(eromValidation, eromValidationFig(USGS_data, nhdGages)),
  tar_target(modelsConceptual_0102, conceptualPlot(final_0102, raymond_coscat_lookup, '0102')),
  tar_target(modelsConceptual_0202, conceptualPlot(final_0202, raymond_coscat_lookup, '0202')),
  tar_target(modelsConceptual_1702, conceptualPlot(final_1702, raymond_coscat_lookup, '1702')),
  tar_target(modelsConceptual_1302, conceptualPlot(final_1302, raymond_coscat_lookup, '1302')),
  tar_target(modelsConceptual_1601, conceptualPlot(final_1601, raymond_coscat_lookup, '1601')),
  tar_target(modelsConceptual_0701, conceptualPlot(final_0701, raymond_coscat_lookup, '0701')),
  tar_target(sources_by_order_regional_e_w, sources_by_order_regional(combined_sources_by_order)),

  ##### GENERATE CALIBRATION PERFORMANCE FIGURES
  tar_target(calibFigures, calibrationFigures(combined_calibrationOutput))
)
