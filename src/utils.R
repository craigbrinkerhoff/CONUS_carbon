#Utility Functions for CO2 transport model
#Craig Brinkerhoff
#Summer 2022


#' temperature dependent Henry's law constant
#'
#' @name henry_func
#'
#' @param temp: water temperature [c]
#'
#' @return a henry's law constant as a function of water temperature [mol/L*atm]
henry_func <- function(temp) {
  A <- 108.3865 #constant
  B <- 0.01985076 #constant
  C <- -6919.53 #constant
  D <- -40.4515 #constant
  E <- 669365 #constant

  temp_k <- temp + 273.15 #[kelvin]
  output <- 10^(A + B*temp_k + C/temp_k + D*log10(temp_k) + E/temp_k^2)

  return(output)
}





#' water residence time for rivers or lakes/reservoirs
#'
#' @name restimeWater
#'
#' @param Vol: lake volume [m3]
#' @param lengthKM: reach length [km]
#' @param v: reach-averaged flow velocity [m/s]
#' @param Q: discharge [m3/s]
#' @param waterbody: flag for whether reach is a river or lake/reservoir [1/0]
#'
#' @return residence time [s]
restimeWater <- function(Vol, lengthKm, v, Q, waterbody){
  if (Q == 0){
    return(NA)
  }
  else if (waterbody == 'Lake/Reservoir') {
    return(((Vol)/(Q)))
  }
  else if (waterbody == 'River'){
    Lc <- lengthKm*1000

   return(((Lc)/(v)))
  }
  else{
    return(NA)
  }
}





#' width for rivers
#'
#' @name width_func
#'
#' @param waterbody: flag for whether reach is a river or lake/reservoir #[1/0]
#' @param Q: discharge [m3/s]
#' @param a: width~Q AHG model intercept
#' @param b: width~Q AHG model coefficient
#'
#' @return river width via hydraulic geometry [m]
width_func <- function(waterbody, Q, a, b){
  if (waterbody == 'River') {
    output <- exp(a)*Q^(b) #river width [m]
  }
  else {
    output <- NA #river width makes no sense in lakes so don't do it!
  }
return(output)
}





#' depth for rivers or lakes/reservoirs
#'
#' @name depth_func
#'
#' @param waterbody: flag for whether reach is a river or lake/reservoir [1/0]
#' @param Q: discharge [m3/s]
#' @param lakeVol: lake/reservoir volume, using fraction assigned to this flowline [m3]
#' @param lakeArea: lake/reservoir surface area, using fraction assigned to this flowline [m2]
#' @param c: depth~Q AHG model intercept
#' @param f: depth~Q AHG model coefficient
#'
#' @return channel depth via hydraulic geometry [m]
depth_func <- function(waterbody, Q, lakeVol, lakeArea, c, f) {
  if (waterbody == 'River') {
    output <- exp(c)*Q^(f) #river depth [m]
  }
  else {
    output <- lakeVol/lakeArea #mean lake depth [m]
  }
  return(output)
}





#' velocity for rivers
#'
#' @name velocity_func
#'
#' @param waterbody: flag for whether reach is a river or lake/reservoir #[1/0]
#' @param Q: discharge [m3/s]
#' @param W: width [m]
#' @param D: depth [m]
#'
#' @return river velocity via continuity assumptions from hydraulic geometry
velocity_func <- function(waterbody, Q, W, D){
  if (waterbody == 'River') {
    output <- Q/(W*D) #continuity equation
  }
  else {
    output <- NA
  }
  return(output)
}







#' kco2 for rivers or lakes/reservoirs via Ulseth etal 2019 and Read etal 2012/Raymond etal 2013
#'
#' @name kco2_func
#'
#' @param velocity: reach-averaged flow velocity [m/s]
#' @param slope: reach bed slope [unitless]
#' @param depth: reach-averaged channel depth [m]
#' @param temp: water temperature [C]
#' @param lakeArea: lake/reservoir surface area [km2]
#' @param waterbody: flag for whether reach is a river or lake/reservoir [0/1]
#'
#' @return CO2 gas exchange rate constant [1/s]
kco2_func <- function(velocity, slope, depth, temp, lakeArea, waterbody){
  if (waterbody == 'River') {
    ##CALCULATE k_600 FOR RIVERS FOLLOWING ULSETH ETAL 2019
    eD <- 9.8*velocity*slope #[m2/s3]

    if(eD <= 0.02) k_600 <- exp(3.10+0.35*log(eD)) #Ulseth etal 2019
    else k_600 <- exp(6.43+1.18*log(eD)) #m/day

  }

  else {
    ##CALCULATE K600 FOR LAKES FOLLOWING RAYMOND ETAL 2013 + READ ETAL 2012
    classes <- c(0.1, 1, 10) #[km2]
    k_600 <- ifelse(lakeArea <= classes[1], 0.54,
                     ifelse(lakeArea <= classes[2], 1.16,
                            ifelse(lakeArea <= classes[3], 1.32, 1.90))) #[m/day]
  }

  ### CONVERT K600 TO K_CO2 BASED ON SCHMIDT NUMBER AND CONVERT TO SECONDS
  sc <- 1911-118.11*temp+3.453*temp^2-0.0413*temp^3   #Raymond2012/Wanninkof 1991
  k_co2 <- k_600/(600/sc)^-0.5
  k_co2 <- (k_co2/depth)/(24*60*60) #[1/s]

  return(k_co2)
}






#' kbz for rivers or lakes/reservoirs via Grant etal 2018 and Lorke & Peeters 2006
#'
#' @name kbz_func
#'
#' @param slope: reach bed slope [unitless]
#' @param depth: reach-averaged channel depth [m]
#' @param waterbody: flag for whether reach is a river or lake/reservoir [0/1]
#'
#' @return mass transfer rate constant from benthic/sediment zones for rivers [1/s]
kbz_func <- function(slope, depth, waterbody, temp){
  sc <- 1911-118.11*temp+3.453*temp^2-0.0413*temp^3   #Raymond2012/Wanninkof 1991
  Ustar <- sqrt(9.8*depth*slope) #shear velocity [m/s]

  if(waterbody == 'River'){
    #RIVER BENTHIC EXCHANGE RATE VIA GRANT ETAL 2018 [m/s]
    kbz <- 0.3*Ustar*sc^(-2/3)
  }
  else {
    #LAKE BENTHIC EXCHANGE RATE VIA LORKE & PEETERS 2006 [m/s]
    kbz <- (1/9)*sc^(-1/2)*Ustar
  }
  kbz <- kbz/depth #[1/s]

  return(kbz)
}



#' calculate CO2 flux to atmosphere for a river reach
#'
#' @name calcEmissions
#'
#' @param model: river network data frame
#'
#' @return river network data frame with CO2 fluxes added [gC/m2/yr]
calcEmissions <- function(model, huc4){
  #calculate fluxes per reach
  model$FCO2_gC_yr <- ifelse(model$waterbody == 'River',
                                   model$FCO2_gC_m2_yr * model$W_m * model$LengthKM * 1000, #river g-C/yr
                                   model$FCO2_gC_m2_yr * model$lakeSA_m2) #lake/reservoir g-C/yr

  #aggregate over watershed
  networkOutput <- dplyr::group_by(model, waterbody) %>%
                       dplyr::summarise(sumFCO2_TgC_yr = sum(FCO2_gC_yr, na.rm=T)*1e-12, #basin Tg-C/yr
                                        sumFCO2_conus_TgC_yr = sum(FCO2_gC_yr*conus, na.rm=T)*1e-12, #basin Tg-C/yr with no international streams
                                        n = n())

  out <- networkOutput
  return(out)
}


emissions_uncertainty <- function(calibratedParameters, model){
  combined_fitness <- calibratedParameters$fitness
  cost <- (1/combined_fitness)/2 #eq 16 in paper

  model$sa <- ifelse(model$waterbody == 'River', model$W_m*model$LengthKM*1000, model$lakeSA_m2) #m2
  SA <- sum(model$sa, na.rm=T) #m2

  sigma <- median(model$k_co2_m_s, na.rm=T) * ((cost*median(model$henry, na.rm=T))/1000000)*(1/0.001)*12.01*(60*60*24*365) #g-C/m2/yr
  sigma <- sigma * SA * 1e-12 #Tg-C/yr

  return(sigma) #Tg-C/yr
}



saveShapefile_huc2 <- function(path_to_data, codes_huc02, raymond_01, raymond_02, raymond_03, raymond_06, raymond_09, raymond_11, raymond_12, raymond_14, raymond_16, raymond_18){
  combined_results <- rbind(raymond_01, raymond_02, raymond_03, raymond_06, raymond_09, raymond_11, raymond_12, raymond_14, raymond_16, raymond_18)

    #read in all HUC4 basins
    basins_overall <- sf::st_read(paste0(path_to_data, '/HUC2_', codes_huc02[1], '/WBD_', codes_huc02[1], '_HU2_Shape/Shape/WBDHU2.shp')) %>% dplyr::select(c('huc2', 'name'))
    for(i in codes_huc02[-1]){
      basins <- sf::st_read(paste0(path_to_data, '/HUC2_', i, '/WBD_', i, '_HU2_Shape/Shape/WBDHU2.shp')) %>%
          dplyr::select(c('huc2', 'name')) #basin polygons
      basins_overall <- rbind(basins_overall, basins)
    }

    #join model results
    basins_overall <- dplyr::left_join(basins_overall, combined_results, by='huc2') %>%
      dplyr::group_by(huc2) %>% #sum rivers and lakes/reservoirs
      dplyr::summarise(sumFCO2_lumped_TgC_yr = sum(sumFCO2_lumped_TgC_yr),
               sumFCO2_TgC_yr = sum(sumFCO2_TgC_yr),
               sumFCO2_conus_TgC_yr = sum(sumFCO2_conus_TgC_yr),
               sumFCO2_semiDist_TgC_yr = sum(sumFCO2_semiDist_TgC_yr),
               cal_uncertainty = mean(cal_uncertainty)) #take mean of identical numbers to pass the value through the group_by

    #round for mapping
    basins_overall <- dplyr::select(basins_overall, c('huc2', 'sumFCO2_lumped_TgC_yr', 'sumFCO2_semiDist_TgC_yr', 'sumFCO2_TgC_yr', 'sumFCO2_conus_TgC_yr', 'cal_uncertainty'))
  
    #return shapefile
    return(basins_overall)
}




#' Finds model properties for reaches that connect to basins downstream (i.e. the exported values from the basin)
#'
#' @name getExported
#'
#' @param model: river network data frame
#' @param huc4: river network basin code
#' @param lookUpTable: table indicating the downstream basins (when applicable) for all OCNUS basins
#' @param Catm: atmopsheric CO2 constant [ppm]
#'
#'
#' @return list with 'exported properties' from reaches that connect to basins downstream. List includes rech fromnode (for routing), reach exported CO2, and reach discharge
getExported <- function(model, huc4, lookUpTable,Catm) {
  lookUpTable <- dplyr::filter(lookUpTable, HUC4 == huc4)
  downstreamBasins <- lookUpTable$toBasin #downstream basin ID
  if(is.na(downstreamBasins)) {
    out <- data.frame('downstreamBasin'=NA,
                      'exported_CO2_ppm'=NA,
                      'exported_ToNode'=NA,
                      'exported_Q_cms'=NA)
    return(out)
  }

  indiana_hucs <- c('0508', '0509', '0514', '0512', '0712', '0404', '0405', '0410') #Indiana-effected basins

  out <- data.frame()
  for(downstreamBasin in downstreamBasins){
      #grab and prep downstream river network (to then grab the right routing ID)
      huc2 <- substr(downstreamBasin, 1, 2)
      dsnPath <- paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', downstreamBasin, '_HU4_GDB/NHDPLUS_H_', downstreamBasin, '_HU4_GDB.gdb')
      if(downstreamBasin %in% indiana_hucs) {
        nhd_d <- sf::st_read(paste0(path_to_data, '/HUC2_', huc2, '/indiana/indiana_fixed_', downstreamBasin, '.shp'))
        nhd_d <- sf::st_zm(nhd_d)
        colnames(nhd_d)[10] <- 'WBArea_Permanent_Identifier'
        nhd_d$NHDPlusID <- round(nhd_d$NHDPlusID, 0) #some of these have digits for some reason......
      }
      else{
        nhd_d <- sf::st_read(dsn=dsnPath, layer='NHDFlowline', quiet=TRUE)
        nhd_d <- sf::st_zm(nhd_d)
        nhd_d <- fixGeometries(nhd_d)
      }

      NHD_HR_VAA <- sf::st_read(dsn = dsnPath, layer = "NHDPlusFlowlineVAA", quiet=TRUE) #additional 'value-added' attributes
      NHD_HR_EROM <- sf::st_read(dsn = dsnPath, layer = "NHDPlusEROMMA", quiet=TRUE) #mean annual flow table

      nhd_d <- dplyr::left_join(nhd_d, NHD_HR_VAA)
      nhd_d <- dplyr::left_join(nhd_d, NHD_HR_EROM)

      nhd_d$StreamOrde <- nhd_d$StreamCalc #stream calc handles divergent streams correctly: https://pubs.usgs.gov/of/2019/1096/ofr20191096.pdf
      nhd_d$Q_cms <- nhd_d$QEMA * 0.0283 #cfs to cms
      nhd_d <- dplyr::filter(nhd_d, Q_cms > 0) #remove streams with no flow
      nhd_d <- dplyr::filter(nhd_d, StreamOrde > 0 & is.na(HydroSeq)==0 & FlowDir == 1)

      #filter for correct reach
      model_filt <- dplyr::filter(model, ToNode %in% nhd_d$FromNode)

      #handle the basins that flow into great lakes but aren't exports from agreat lake, i.e. no topological connection to lake & thus NA export reach
      if(nrow(model_filt) == 0){
        out <- data.frame('downstreamBasin'=downstreamBasin,
                          'exported_CO2_ppm'=model[which.max(model$Q_m3_s),]$CO2_ppm, #since we can't confirm, using topology, what the actual export is for this special subset of basins, we just use the CO2 from the greatest Q reach. AGai, it doesn't really matter since it's just going into a great lake and hitting atmospheric anyway
                          'exported_ToNode'=NA,
                          'exported_Q_cms'=NA)
      }

      else{
        exported_CO2_ppm <- model_filt$CO2_ppm
        exported_ToNode <- model_filt$ToNode
        exported_Q <- model_filt$Q_m3_s

        temp <- data.frame('downstreamBasin'=downstreamBasin,
                          'exported_CO2_ppm'=exported_CO2_ppm,
                          'exported_ToNode'=exported_ToNode,
                          'exported_Q_cms'=exported_Q)
        out <- rbind(out, temp)
      }
    }

  return(out)
}


#' Fixes geometries that are saved as multicurves rather than multilines
#'
#' @name fixGeometries
#'
#' @param rivnet: river network hydrography object
#'
#' @import dplyr
#' @import sf
#'
#' @return updated (if necessary) river network object
fixGeometries <- function(rivnet){
  curveLines <- dplyr::filter(rivnet, sf::st_geometry_type(rivnet) == 'MULTICURVE')
  if(nrow(curveLines) > 0){ #if saved as a curve, recast geometry as a line
    rivnet <- sf::st_cast(rivnet, 'MULTILINESTRING')
  }

  return(rivnet)
}



#' Fixes flat/missing/erronous slopes by using the average slope of the immedately upstream/downstream reaches
#'
#' @name fixBadSlopes
#'
#' @param slope
#' @param FromNode: reach from node for finding upstream reaches
#' @param toNode_vec: vector of all tonodes in network for finding upstream reaches
#' @param slope_vec: vector of all reach slopes in network for finding upstream reach slopes
#'
#'
#' @return updated (if necessary) river network slopes
fixBadSlopes <- function(slope, slope_vec, fromNode, fromNode_vec, toNode, toNode_vec) {
  upstreamIndexes <- which(toNode_vec == fromNode) #get directly upstream reaches
  downstreamIndexes <- which(fromNode_vec == toNode)

  upstream_slopes <- slope_vec[upstreamIndexes]
  upstream_slopes <- upstream_slopes[upstream_slopes > 0] #don't accidently include other erronous slopes in here...

  downstream_slopes <- slope_vec[downstreamIndexes]
  downstream_slopes <- downstream_slopes[downstream_slopes > 0] #don't accidently include other erronous slopes in here...

  all_slopes <- c(upstream_slopes, downstream_slopes)

  if(all(is.na(all_slopes))==1){ #if everything upstream is a problem, then just set it to the minimum value 1e-5 (i.e.e 1005 basin)
    out <- 1e-5
  }
  else{ #otherwise, take mean of all good upstream values
    out <- mean(all_slopes, na.rm=T)
  }
  return(out)
}



#' Fixes missing temperatures by using the average temperture of the immedately upstream/downstream reaches
#'
#' @name fixBadTemps
#'
#' @param temp: reach temperature (could be water or air) [celsius]
#' @param FromNode: reach from node for finding upstream reaches
#' @param toNode_vec: vector of all tonodes in network for finding upstream reaches
#' @param temp_vec: vector of all temperatures in network for finding upstream reach temperatures
#'
#'
#' @return updated (if necessary) river network temperatures (could be air or water)
fixBadTemps <- function(temp, temp_vec, fromNode, fromNode_vec, toNode, toNode_vec) {
  upstreamIndexes <- which(toNode_vec == fromNode) #get directly upstream reaches
  downstreamIndexes <- which(fromNode_vec == toNode)

  upstream_temps <- temp_vec[upstreamIndexes]
  downstream_temps <- temp_vec[downstreamIndexes]

  all_temps <- c(upstream_temps, downstream_temps)
  out <- mean(all_temps, na.rm=T)
  return(out)
}






#' Aggregates combined targets at each processing level into a single dataset of basin flux results
#'
#' @name aggregateAllLevels
#'
#' @param combined_emissions_lvlx: combined targets for each processing level
#'
#'
#' @return data frame of all combined targets at each processing level into a single dataset of basin flux results
aggregateAllLevels <- function(combined_emissions_lvlTerminal, combined_emissions_lvl0, combined_emissions_lvl1, combined_emissions_lvl2, combined_emissions_lvl3, combined_emissions_lvl4,
                                               combined_emissions_lvl5, combined_emissions_lvl6, combined_emissions_lvl7, combined_emissions_lvl8, combined_emissions_lvl9,
                                               combined_emissions_lvl10, combined_emissions_lvl11, combined_emissions_lvl12, combined_emissions_lvl13, combined_emissions_lvl14,
                                               combined_emissions_lvl15, combined_emissions_lvl16, combined_emissions_lvl17, combined_emissions_lvl18){
  #aggregate our model results at huc4
  out <- rbind(combined_emissions_lvlTerminal, combined_emissions_lvl0, combined_emissions_lvl1, combined_emissions_lvl2, combined_emissions_lvl3, combined_emissions_lvl4,
                                                 combined_emissions_lvl5, combined_emissions_lvl6, combined_emissions_lvl7, combined_emissions_lvl8, combined_emissions_lvl9,
                                                 combined_emissions_lvl10, combined_emissions_lvl11, combined_emissions_lvl12, combined_emissions_lvl13, combined_emissions_lvl14,
                                                 combined_emissions_lvl15, combined_emissions_lvl16, combined_emissions_lvl17, combined_emissions_lvl18)

  out$huc4 <- substr(out$method, 11, 16)
  out$huc2 <- substr(out$huc4, 1, 2)

  return(out)
}



#' Aggregates model results to HUC2 regional level to compare against raymond model
#'
#' @name abstractAllResults
#'
#' @param combined_emissions_lvlx: combined targets for each processing level
#'
#'
#' @return data frame of all model results per HUC2 region AND all raymond upscaling results per HUC2 region
abstractAllResults <- function(allResults, raymondList){
  raymond <- do.call("rbind", raymondList) #make raymond model object

  huc2Results <- dplyr::group_by(allResults, huc2) %>%
        dplyr::summarise(sumFCO2_conus_TgC_yr = sum(sumFCO2_conus_TgC_yr),
                         sumFCO2_TgC_yr = sum(sumFCO2_TgC_yr),
                         n = sum(n))

  huc2Raymond <- dplyr::group_by(raymond, huc2) %>%
        dplyr::summarise(sumFCO2_raymond_TgC_yr = sum(sumFCO2_lumped_TgC_yr))

  out <- left_join(huc2Results, huc2Raymond, by='huc2')
  return(out)
}


#' Write selected river networks to csv so that brian can map them and play with them. Uses `fwrite` from the data.table package to be as efficient as possible
#'
#' @name writeToFile
#'
#' @param rivnet: target object for final routed river network
#' @param huc4: huc4 basin ID code
#'
#' @return print statement but write to file
writeToFile <- function(rivnet, huc4){
  data.table::fwrite(rivnet, paste0('cache/final_', huc4, '.csv'))
  return('Written to file')
}




#' Exploits the postFitness option to amend the GA object at each iteration. Allows us to write the best solution from each population to file, and just return the untouched GA object
#'
#' @name saveIntermediateResults
#'
#' @param object: GA object from current iteration
#'
#' @return untouched GA object. Also writes every 10th population best parameter set to file
saveIntermediateResults <- function(object, ...){
  library(readr, lib.loc = "/nas/cee-water/cjgleason/r-lib/")

  #only save every 5th population's best parameter set
  if(object@iter %in% seq(1, object@iter, 5)){
    parameters <- data.frame('Cbz_riv'=object@bestSol[[object@iter]][1],
                    'Cbz_lake'=object@bestSol[[object@iter]][2],
                    'Fwc_riv'=object@bestSol[[object@iter]][3],
                    'Fwc_lake'=object@bestSol[[object@iter]][4])

    #plot and save to file
    forPlot <- data.frame(object@summary)
    forPlot$generation <- 1:nrow(forPlot)
    forPlot <- tidyr::gather(forPlot, key=key, value=value, 'mean', 'max')

    calibrationPlot <- ggplot(forPlot, aes(x=generation, y=1/value, color=key, group=key)) +
      geom_point(size=4) +
      geom_line(size=1) +
      scale_y_log10()+
      scale_color_brewer(palette='Dark2', name='Current generations \n fitness') +
      ylab('Cost Function [ppm]') +
      xlab('Species generation')

    out <- list('parameters'=parameters,
                'fitness'=1/max(object@fitness, na.rm=T), #multiply by -1 to convert back to real ppm error
                'iter'=object@iter,
                'plot'=calibrationPlot)
    
    #keep overwriting intermediate results in case a crash or something happens
    write_rds(out, paste0('cache/intermediateResults/', object@names[[1]],'.rds')) 
  }
  
  #return object as is, DO NOT UPDATE don't want to mess up the calibration!!
  return(object)
}
