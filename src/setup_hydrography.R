##########################
#preps hydrography for routing
#Craig Brinkerhoff
#Fall 2022
##########################


#' Preps, builds, and cleans nhd into a usable routing file
#'
#' @name calibrateModelWrapper
#'
#' @param path_to_data: path to NHD geodatabases
#' @param huc4: basin id
#'
#' @import sf
#' @import dplyr
#' @import terra
#' @import tidyr
#' @import ggplot2
#' @import readr
#'
#' @return usable routing file for NHD
setupHydrography <- function(path_to_data, huc4){
  indiana_hucs <- c('0508', '0509', '0514', '0512', '0712', '0404', '0405', '0410') #indiana-effected basins

  sf::sf_use_s2(FALSE)

  huc2 <- substr(huc4, 1, 2)

  #USGS NHD
  dsnPath <- paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4, '_HU4_GDB/NHDPLUS_H_', huc4, '_HU4_GDB.gdb')

  #load river network, depending on indiana-effect or not
  if(huc4 %in% indiana_hucs) {
    nhd <- sf::st_read(paste0(path_to_data, '/HUC2_', huc2, '/indiana/indiana_fixed_', huc4, '.shp'))
    nhd <- sf::st_zm(nhd)
    colnames(nhd)[10] <- 'WBArea_Permanent_Identifier'
    nhd$NHDPlusID <- round(nhd$NHDPlusID, 0) #some of these have digits for some reason......
  }
  else{
    nhd <- sf::st_read(dsn=dsnPath, layer='NHDFlowline', quiet=TRUE)
    nhd <- sf::st_zm(nhd)
    nhd <- fixGeometries(nhd)
  }

  #load lakes
  lakes <- sf::st_read(dsn=dsnPath, layer='NHDWaterbody', quiet=TRUE)
  lakes <- sf::st_zm(lakes)

  lakes <- as.data.frame(lakes) %>%
    dplyr::filter(FType %in% c(390, 436)) #lakes/reservoirs only
  colnames(lakes)[6] <- 'LakeAreaSqKm'
  NHD_HR_EROM <- sf::st_read(dsn = dsnPath, layer = "NHDPlusEROMMA", quiet=TRUE) #mean annual flow table
  NHD_HR_VAA <- sf::st_read(dsn = dsnPath, layer = "NHDPlusFlowlineVAA", quiet=TRUE) #additional 'value-added' attributes

  #get mean annual T (direct column indexing because of typos in USGS tables...)
  NHD_HR_temp_01 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM01", quiet=TRUE) #additional 'value-added' attributes
  temp <- data.frame('NHDPlusID'=NHD_HR_temp_01[,1],
                     'temp_c_01' = NHD_HR_temp_01[,3] / 100)

  NHD_HR_temp_02 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM02", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_02) > 0){
    NHD_HR_temp <- NHD_HR_temp_02[,3]
    temp$temp_c_02 <- NHD_HR_temp / 100
  }

  NHD_HR_temp_03 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM03", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_03) > 0){
    NHD_HR_temp <- NHD_HR_temp_03[,3]
    temp$temp_c_03 <- NHD_HR_temp / 100
  }

  NHD_HR_temp_04 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM04", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_04) > 4){
    NHD_HR_temp <- NHD_HR_temp_04[,3]
    temp$temp_c_04 <- NHD_HR_temp / 100
  }

  NHD_HR_temp_05 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM05", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_05) > 0){
    NHD_HR_temp <- NHD_HR_temp_05[,3]
    temp$temp_c_05 <- NHD_HR_temp / 100
  }

  NHD_HR_temp_06 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM06", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_06) > 0){
    NHD_HR_temp <- NHD_HR_temp_06[,3]
    temp$temp_c_06 <- NHD_HR_temp / 100
  }

  NHD_HR_temp_07 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM07", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_07) > 0){
    NHD_HR_temp <- NHD_HR_temp_07[,3]
    temp$temp_c_07 <- NHD_HR_temp / 100
  }

  NHD_HR_temp_08 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM08", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_08) > 0){
    NHD_HR_temp <- NHD_HR_temp_08[,3]
    temp$temp_c_08 <- NHD_HR_temp / 100
  }

  NHD_HR_temp_09 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM09", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_09) > 0){
    NHD_HR_temp <- NHD_HR_temp_09[,3]
    temp$temp_c_09 <- NHD_HR_temp / 100
  }

  NHD_HR_temp_10 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM10", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_10) > 0){
  NHD_HR_temp <- NHD_HR_temp_10[,3]
  temp$temp_c_10 <- NHD_HR_temp / 100
  }

  NHD_HR_temp_11 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM11", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_11) > 0){
    NHD_HR_temp <- NHD_HR_temp_11[,3]
    temp$temp_c_11 <- NHD_HR_temp / 100
  }

  NHD_HR_temp_12 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM12", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_12) > 0){
    NHD_HR_temp <- NHD_HR_temp_12[,3]
    temp$temp_c_12 <- NHD_HR_temp / 100
  }

  temp$airTemp_mean_c <- rowMeans(temp[,2:ncol(temp)], na.rm=T)
  temp <- dplyr::select(temp, c('NHDPlusID', 'airTemp_mean_c'))

  nhd <- left_join(nhd, lakes, by=c('WBArea_Permanent_Identifier'='Permanent_Identifier'))

  if(huc4 %in% indiana_hucs){
    colnames(nhd)[17] <- 'NHDPlusID' #some manual rewriting b/c this columns get doubled from previous joins where data was needed for specific GIS tasks...
    nhd$NHDPlusID <- round(nhd$NHDPlusID, 0) #some of these have digits for some reason......
    colnames(nhd)[12] <- 'FCode_riv'
    colnames(nhd)[31] <- 'FCode_waterbody'
  }
  else{
    colnames(nhd)[16] <- 'NHDPlusID' #some manual rewriting b/c this columns get doubled from previous joins where data was needed for specific GIS tasks...
    colnames(nhd)[11] <- 'FCode_riv'
    colnames(nhd)[27] <- 'FCode_waterbody'
  }

  nhd <- dplyr::left_join(nhd, NHD_HR_EROM, by='NHDPlusID')
  nhd <- dplyr::left_join(nhd, NHD_HR_VAA, by='NHDPlusID')
  nhd <- dplyr::left_join(nhd, temp, by='NHDPlusID')

  #add flag for non-US rivers that are needed for routing, but shouldn't be in the final FCO2 estimates
  #determine if stream is international or not
  conus <- sf::st_read('data/cb_2018_us_nation_20m.shp')
  conus <- dplyr::select(conus, 'GEOID', 'geometry')
  nhd_conus <- sf::st_intersection(nhd, conus)
  nhd$conus <- ifelse(nhd$NHDPlusID %in% nhd_conus$NHDPlusID, 1,0)

  nhd$StreamOrde <- nhd$StreamCalc #stream calc handles divergent streams correctly: https://pubs.usgs.gov/of/2019/1096/ofr20191096.pdf
  nhd$Q_cms <- nhd$QEMA * 0.0283 #cfs to cms

  #handle indiana-effect basin stream orders
  if(huc4 %in% indiana_hucs){
    thresh <- c(2,2,2,2,3,2,3,2) #see README file
    thresh <- thresh[which(indiana_hucs == huc4)]
    nhd$StreamOrde <- ifelse(nhd$indiana_fl == 1, nhd$StreamOrde - thresh, nhd$StreamOrde)
  }

  #assign waterbody type for depth modeling
  nhd$waterbody <- ifelse(is.na(nhd$WBArea_Permanent_Identifier)==0 & is.na(nhd$LakeAreaSqKm) == 0 & nhd$LakeAreaSqKm > 0, 'Lake/Reservoir', 'River')

  #only keep non-divergent, physically possible reaches
  nhd <- dplyr::filter(nhd, StreamOrde > 0 & is.na(HydroSeq)==0 & FlowDir == 1)
  #setup data (all other hydrography setup is done in the munge/generateHydrography script)
  nhd <- dplyr::filter(nhd, Q_cms > 0) #remove streams with no flow
  nhd$waterbody <- as.character(nhd$waterbody)

  #calculate depths and widths via hydraulic geomtery and lake volume modeling
  nhd$lakeVol_m3 <- 0.533 * (nhd$LakeAreaSqKm*1e6)^1.204 #Cael et al. 2016 function

  #fix NA or 0 or missing slopes (when appropriate, use average slope of directly upstream reaches)
  slope_vec <- as.vector(nhd$Slope)
  toNode_vec <- as.vector(nhd$ToNode)
  badSlopes <- which(slope_vec <= 0)
  for(k in badSlopes){
    nhd[k,]$Slope <- fixBadSlopes(nhd[k,]$Slope, nhd[k,]$FromNode, toNode_vec, slope_vec)
  }

  #fix missing air temps by grabbing the one upstream)
  temp_vec <- as.vector(nhd$airTemp_mean_c)
  badTemps <- which(is.na(temp_vec))
  for(k in badTemps){
    nhd[k,]$airTemp_mean_c <- fixBadTemps(nhd[k,]$airTemp_mean_c, nhd[k,]$FromNode, toNode_vec, temp_vec)
  }

  #no impossible reaches
  nhd <- dplyr::filter(nhd, Slope > 0 & Q_cms > 0)

  #Calculate and assign lake percents to each throughflow line so that we have fracVols and fracSAs for each throughflow line
  sumThroughFlow <- dplyr::filter(as.data.frame(nhd), is.na(WBArea_Permanent_Identifier)==0) %>% #This is based on reachLength/total throughflow line reach length
    dplyr::group_by(WBArea_Permanent_Identifier) %>%
    dplyr::summarise(sumThroughFlow = sum(LengthKM))
  nhd <- dplyr::left_join(nhd, sumThroughFlow, by='WBArea_Permanent_Identifier')
  nhd$lakePercent <- nhd$LengthKM / nhd$sumThroughFlow
  nhd$frac_lakeVol_m3 <- nhd$lakeVol_m3 * nhd$lakePercent
  nhd$frac_lakeSurfaceArea_m2 <- nhd$LakeAreaSqKm * nhd$lakePercent * 1e6

  #hydraulic geometry parameters
  depAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/depAHG.rds') #depth AHG model
  widAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/widAHG.rds') #depth AHG model
  nhd$a <- widAHG$coefficients[1]
  nhd$b <- widAHG$coefficients[2]
  nhd$c <- depAHG$coefficients[1]
  nhd$f <- depAHG$coefficients[2]

  #get w, d, vol differentially by river vs lake/reservoir
  nhd$D <- mapply(depth_func, nhd$waterbody, nhd$Q_cms, nhd$frac_lakeVol_m3, nhd$frac_lakeSurfaceArea_m2, nhd$c, nhd$f)
  nhd$W <- mapply(width_func, nhd$waterbody, nhd$Q_cms, nhd$a, nhd$b)
  nhd$V <- mapply(velocity_func, nhd$waterbody, nhd$Q_cms, nhd$W, nhd$D)

  #temperature
  nhd$temp_c <- 3.941 + 0.818*nhd$airTemp_mean_c #convert air temp to water temp following Lauerwald etal 2015: r2 0.88

  #Calculate residence time [seconds]
  nhd$HRT <- mapply(restimeWater, nhd$frac_lakeVol_m3, nhd$LengthKM, nhd$V, nhd$Q_cms, nhd$waterbody)

  #temperature dependent Henry's constant. Used to calculate ppm for Wc respiration rates
  nhd$henry <- mapply(henry_func, nhd$temp_c)

  #get mass transfer or respiration rate parameters for reaches
  nhd$k_co2 <- mapply(kco2_func, nhd$V, nhd$Slope, nhd$D, nhd$temp_c, nhd$frac_lakeSurfaceArea_m2, nhd$waterbody)
  nhd$k_bz <- mapply(kbz_func, nhd$Slope, nhd$D, nhd$waterbody, nhd$temp_c)

  nhd <- as.data.frame(nhd)

  colnames(nhd)[87] <- 'CatchmentAreaSqKm'

  nhd <- dplyr::select(nhd, c('NHDPlusID', 'NHDPlusID', 'WBArea_Permanent_Identifier', 'conus', 'FromNode', 'ToNode', 'HydroSeq', 'StartFlag', 'Divergence', 'waterbody', 'StreamOrde', 'LengthKM', 'Slope', 'Q_cms', 'frac_lakeSurfaceArea_m2',
                            'frac_lakeVol_m3', 'CatchmentAreaSqKm', 'temp_c', 'HRT', 'henry', 'k_co2', 'k_bz', 'W', 'D', 'V'))

  return(nhd)
}
