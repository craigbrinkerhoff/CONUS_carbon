##Calibration functions for CO2 transport model
##Craig Brinkerhoff
##Fall 2022


#' Wrapper to perform calibration via a genetic algorithm
#'
#' @name calibrateModelWrapper
#'
#' @param hydrography: prepped and cleaned NHD basin hydrography
#' @param huc4: basin id
#' @param glorich_data: obsered CO2 concentrations per region used to upscale
#' @param Cgw: groundwater CO2 parameter [ppm]
#' @param lowerCBZ_riv: lower river benthic CO2 parameter bound (for calibration) [ppm]
#' @param lowerCBZ_lake: lower lake benthic CO2 parameter bound (for calibration) [ppm]
#' @param lowerFWC_riv: lower river water-column respiration CO2 parameter bound (for calibration) [ppm/s]
#' @param lowerFWC_lake: lower lake water-column respiration CO2 parameter bound (for calibration) [ppm/s]
#' @param upperCBZ_riv: upper river benthic CO2 parameter bound (for calibration) [ppm]
#' @param upperCBZ_lake: upper lake benthic CO2 parameter bound (for calibration) [ppm]
#' @param upperFWC_riv: upper river water-column respiration CO2 parameter bound (for calibration) [ppm/s]
#' @param upperFWC_lake: upper lake water-column respiration CO2 parameter bound (for calibration) [ppm/s]
#' @param myPopSize: size of each population in the genetic algorithm [# models per generation]
#' @param mymaxIter: stopping criterion for max number of generations to evolve
#' @param myRun: stopping criterion for number of generations with identical performance before stopping
#' @param mymaxFitness: stopping criterion for upper bound on fitness function [1/ppm]
#' @param mutationRate: probably of mutation in GA parent chromosome
#' @param cores: number of cores to run each generation in parallel (i.e. number of population members that are run in parrallel)
#'
#' @import ga
#' @import tidyr
#' @import ggplot2
#'
#' @return list of calibrated parameters, calibration figure, and calibration performance
calibrateModelWrapper <- function(hydrography, huc4, glorich_data, Cgw, Catm, emergenceQ, upstreamDF, lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake, upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake, myPopSize, mymaxIter, myRun, mymaxFitness, mutationRate, cores) {
  theme_set(theme_classic())

  #skip great lakes
  if(huc4 %in% c('0418', '0419', '0424', '0426', '0428')) {
    out <- list('Cbz_riv'=NA,
                'Cbz_lake'=NA,
                'Fwc_riv'=NA,
                'Fwc_lake'=NA,
                'fitness'=NA,
                'plot'=NA)
  }

  #normal basins
  else{
    #handle negative respiration in Great Lakes and Great basin
    lowerFWC_lake <- ifelse(substr(huc4,1,2) %in% c('04','16'), -0.00001, lowerFWC_lake)#NEGATIVE IS FOR GREAT BASIN/GREAT LAKES, WHERE WE LET PHOTOSYNTHESIS OCCUR)
    upperFWC_lake <- ifelse(substr(huc4,1,2) %in% c('04','16'), 0, upperFWC_lake)#NEGATIVE IS FOR GREAT BASIN/GREAT LAKES, WHERE WE LET PHOTOSYNTHESIS OCCUR)
  
    start <- Sys.time()
    
    #run genetic algorithm
    calibrateParams <- GA::ga(type = "real-valued",
                            fitness =  function(x) calibrateModel(x, hydrography, huc4, glorich_data, Cgw, Catm, emergenceQ, upstreamDF),
                            lower = c(lowerCBZ_riv, lowerCBZ_lake, lowerFWC_riv, lowerFWC_lake),
                            upper = c(upperCBZ_riv, upperCBZ_lake, upperFWC_riv, upperFWC_lake),
                            popSize = myPopSize,
                            maxiter = mymaxIter,
                            maxFitness = mymaxFitness,
                            run=myRun,
                            names=c(huc4,NA,NA,NA),
                            parallel=cores,
                            pmutation = mutationRate,
                            optim=TRUE, #L-BFGS-B optimization
                            keepBest=TRUE,
                            postFitness = saveIntermediateResults, #for saving intermediate solutions in case of catastrophe
                            seed = 12) #reproducibility
    
    end <- Sys.time()
    
    #plot and save to file
    forPlot <- data.frame(calibrateParams@summary)
    forPlot$generation <- 1:nrow(forPlot)
    forPlot <- tidyr::gather(forPlot, key=key, value=value, 'mean', 'max')

    calibrationPlot <- ggplot(forPlot, aes(x=generation, y=1/value, color=key, group=key)) +
      geom_point(size=4) +
      geom_line(size=1) +
      scale_y_log10()+
      scale_color_brewer(palette='Dark2', name='Current generations \n fitness') +
      ylab('Cost Function [ppm]') +
      xlab('Species generation')

      #save calibrated parameter values, summary plot (and the entire GA object if that's your jam)
      out <- list('Cbz_riv'=calibrateParams@solution[1],
                  'Cbz_lake'=calibrateParams@solution[2],
                  'Fwc_riv'=calibrateParams@solution[3],
                  'Fwc_lake'=calibrateParams@solution[4],
                  'fitness'=calibrateParams@fitnessValue,
                  'plot'=calibrationPlot,
                  'iter'=calibrateParams@iter,
                  'calibrationTime'=end - start,
                  'ga'=calibrateParams)
  }
 return(out)
}






#' Runs the actual model calibration, called by the geneti algorithm
#'
#' @name calibrateModel
#'
#' @param par: vector string of terms to calibrate (see GA function call below and GA package documentation)
#' @param hydrography: prepped and cleaned NHD basin hydrography
#' @param huc4: basin id
#' @param Cgw: groundwater CO2 parameter [ppm]
#' @param Catm: atmospheric CO2 constant [ppm]
#' @param emergenceQ: emergent Q constant [m3/s]
#' @param upstreamDF: data frame of 'exporting CO2 reaches' for basins from previous level
#'
#' @import dplyr
#'
#' @return cost function value, evaluate given a set of parameter values
calibrateModel <- function(par, hydrography, huc4, glorich_data, Cgw, Catm, emergenceQ, upstreamDF) {
  #for parallel runs
  source('src/utils.R')
  source('src/model.R')

  #HUC2 <- substr(network_name, 5,nchar(network_name)) #grab HUC2 code onlY
  riverCO2 <- glorich_data[glorich_data$HUC4 == huc4,]$River #[ppm]
  lakeCO2 <- glorich_data[glorich_data$HUC4 == huc4,]$Lake #[ppm]

  #sort rivers from upstream to downstream
  hydrography <- dplyr::filter(hydrography, HydroSeq != 0)
  hydrography <- hydrography[order(-hydrography$HydroSeq),] #sort descending

  #vectorize hydrography to help with speed
  StartFlag_vec <- as.vector(hydrography$StartFlag)
  Divergence_vec <- as.vector(hydrography$Divergence)
  fromNode_vec <- as.vector(hydrography$FromNode)
  toNode_vec <- as.vector(hydrography$ToNode)
  waterbody_vec <- as.vector(hydrography$waterbody)
  hrt_vec <- as.vector(hydrography$HRT) #[s]
  slope_vec <- as.vector(hydrography$Slope)
  width_vec <- as.vector(hydrography$W) #[m]
  vel_vec <- as.vector(hydrography$V) #[m/s]
  kco2_vec <- as.vector(hydrography$k_co2) #[1/s]
  kbz_vec <- as.vector(hydrography$k_bz) #[1/s]
  Q_vec <- as.vector(hydrography$Q_cms) #[m3/s]
  lakeArea_vec <- as.vector(hydrography$frac_lakeSurfaceArea_m2) #[m2]
  temp_vec <- as.vector(hydrography$temp_c) #[C]

  #results vector
  CO2_vec <- rep(NA, length(Q_vec)) #[ppm]

  #Append required upstream IDs and CO2s from previous HUC4s to this dataset. Because the indexing is relative, we can just add them to the end of the vectors and then remove later when routing is done
  if(is.na(upstreamDF) == 0){
    upstreamDF <- dplyr::filter(upstreamDF, downstreamBasin == huc4)

    toNode_vec <- c(toNode_vec, upstreamDF$exported_ToNode)
    CO2_vec <- c(CO2_vec, upstreamDF$exported_CO2_ppm)
    Q_vec <- c(Q_vec, upstreamDF$exported_Q_cms)
  }

  #Assumed known groundwater parameter
  Cgroundwater_vec <- rep(Cgw, length(Q_vec))

  #vectorized parameters to be calibrated
  bz_vec <- as.vector(ifelse(waterbody_vec == 'River', par[1], par[2])) #[ppm]
  Fwc_vec <- as.vector(ifelse(waterbody_vec == 'River', par[3], par[4])) #[ppm/s]

  #run vectorized model
  for (i in 1:nrow(hydrography)) {
    CO2_vec[i] <- CO2_MODEL(hrt_vec[i], Q_vec[i], Cgroundwater_vec[i], Catm, bz_vec[i], kco2_vec[i], kbz_vec[i], Fwc_vec[i], fromNode_vec[i], Divergence_vec[i], StartFlag_vec[i], waterbody_vec[i], toNode_vec, CO2_vec, Q_vec, emergenceQ)
  }

  #remove upstream CO2s added to the vector temporarily
  if(is.na(upstreamDF) == 0){
    CO2_vec <- CO2_vec[1:nrow(hydrography)]
  }

  #add results to river network
  hydrography$CO2_ppm <- CO2_vec #[ppm]

  ###########EVALUATE COST FUNCTION
  lakesVSrivers <- dplyr::group_by(hydrography, waterbody) %>%
      dplyr::summarise(medianPCO2 = median(CO2_ppm, na.rm=T))

  cost <- sum(c(abs(lakesVSrivers[lakesVSrivers$waterbody == 'Lake/Reservoir',]$medianPCO2 - lakeCO2), abs(lakesVSrivers[lakesVSrivers$waterbody == 'River',]$medianPCO2 - riverCO2)))
  out <- 1/cost #take reciprocal of function to convert to a maximization problem

  return(out) #because the GA package maximizes the cost function
}
