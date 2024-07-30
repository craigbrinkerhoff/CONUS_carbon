######################################
## Prep hydrography for routing
## Craig Brinkerhoff
## Fall 2022
####################################


#' Prep, build, and clean the NHD-HR hydrography into a usable routing file
#'
#' @name setupHydrography
#'
#' @param path_to_data: path to data repo
#' @param huc4: basin id
#'
#' @import sf
#' @import dplyr
#' @import readr
#'
#' @return NHD-HR routing file for CO2 model
setupHydrography <- function(path_to_data, huc4){
  indiana_hucs <- c('0508', '0509', '0514', '0512', '0712', '0404', '0405', '0410') #indiana-effected basins

  sf::sf_use_s2(FALSE)

  huc2 <- substr(huc4, 1, 2)

  #SETUP PATH TO BASIN FOLDER WITHIN THE DATA REPO-------------------------------
  dsnPath <- paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4, '_HU4_GDB/NHDPLUS_H_', huc4, '_HU4_GDB.gdb')

  #READ IN HYDROGRAPHY------------------------------
  #Indiana
  if(huc4 %in% indiana_hucs) {
    nhd <- sf::st_read(paste0(path_to_data, '/HUC2_', huc2, '/indiana/indiana_fixed_', huc4, '.shp'))
    nhd <- sf::st_zm(nhd)
    colnames(nhd)[10] <- 'WBArea_Permanent_Identifier'
    nhd$NHDPlusID <- round(nhd$NHDPlusID, 0) #some of these have digits for some reason......
  }
  #all other basins
  else{
    nhd <- sf::st_read(dsn=dsnPath, layer='NHDFlowline', quiet=TRUE)
    nhd <- sf::st_zm(nhd)
    nhd <- fixGeometries(nhd)
  }

  #lakes
  lakes <- sf::st_read(dsn=dsnPath, layer='NHDWaterbody', quiet=TRUE)
  lakes <- sf::st_zm(lakes)

  #WRANGLING OF THE VARIOUS ATTRIBUTE DATA FOR HYDROGRAPHY-------------------------------------------------
  lakes <- as.data.frame(lakes) %>%
    dplyr::filter(FType %in% c(390, 436)) #lakes/reservoirs only
  colnames(lakes)[6] <- 'LakeAreaSqKm'
  NHD_HR_EROM <- sf::st_read(dsn = dsnPath, layer = "NHDPlusEROMMA", quiet=TRUE) #mean annual flow table
  NHD_HR_VAA <- sf::st_read(dsn = dsnPath, layer = "NHDPlusFlowlineVAA", quiet=TRUE) #additional 'value-added' attributes

  #WRANGLE MEAN MONTHLY AIR TEMPERATURE (direct column indexing because of typos in USGS tables...)---------------------------------------------------------
  #jan
  NHD_HR_temp_01 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM01", quiet=TRUE) #additional 'value-added' attributes
  temp <- data.frame('NHDPlusID'=NHD_HR_temp_01[,1],
                     'temp_c_01' = NHD_HR_temp_01[,3] / 100)

  #Feb
  NHD_HR_temp_02 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM02", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_02) > 0){
    NHD_HR_temp <- NHD_HR_temp_02[,3]
    temp$temp_c_02 <- NHD_HR_temp / 100
  }

  #Mar
  NHD_HR_temp_03 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM03", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_03) > 0){
    NHD_HR_temp <- NHD_HR_temp_03[,3]
    temp$temp_c_03 <- NHD_HR_temp / 100
  }

  #Apr
  NHD_HR_temp_04 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM04", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_04) > 4){
    NHD_HR_temp <- NHD_HR_temp_04[,3]
    temp$temp_c_04 <- NHD_HR_temp / 100
  }

  #May
  NHD_HR_temp_05 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM05", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_05) > 0){
    NHD_HR_temp <- NHD_HR_temp_05[,3]
    temp$temp_c_05 <- NHD_HR_temp / 100
  }

  #Jun
  NHD_HR_temp_06 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM06", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_06) > 0){
    NHD_HR_temp <- NHD_HR_temp_06[,3]
    temp$temp_c_06 <- NHD_HR_temp / 100
  }

  #Jul
  NHD_HR_temp_07 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM07", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_07) > 0){
    NHD_HR_temp <- NHD_HR_temp_07[,3]
    temp$temp_c_07 <- NHD_HR_temp / 100
  }

  #Aug
  NHD_HR_temp_08 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM08", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_08) > 0){
    NHD_HR_temp <- NHD_HR_temp_08[,3]
    temp$temp_c_08 <- NHD_HR_temp / 100
  }

  #Sep
  NHD_HR_temp_09 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM09", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_09) > 0){
    NHD_HR_temp <- NHD_HR_temp_09[,3]
    temp$temp_c_09 <- NHD_HR_temp / 100
  }

  #Oct
  NHD_HR_temp_10 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM10", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_10) > 0){
  NHD_HR_temp <- NHD_HR_temp_10[,3]
  temp$temp_c_10 <- NHD_HR_temp / 100
  }

  #Nov
  NHD_HR_temp_11 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM11", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_11) > 0){
    NHD_HR_temp <- NHD_HR_temp_11[,3]
    temp$temp_c_11 <- NHD_HR_temp / 100
  }

  #Dec
  NHD_HR_temp_12 <- sf::st_read(dsn = dsnPath, layer = "NHDPlusIncrTempMM12", quiet=TRUE) #additional 'value-added' attributes
  if(nrow(NHD_HR_temp_12) > 0){
    NHD_HR_temp <- NHD_HR_temp_12[,3]
    temp$temp_c_12 <- NHD_HR_temp / 100
  }

  #get mean annual air temps
  temp$airTemp_mean_c <- rowMeans(temp[,2:ncol(temp)], na.rm=T)
  temp <- dplyr::select(temp, c('NHDPlusID', 'airTemp_mean_c'))

  nhd <- left_join(nhd, lakes, by=c('WBArea_Permanent_Identifier'='Permanent_Identifier'))

  #some manual rewriting b/c column names are duplicated across attribute tables and river vs lake hydrography
  if(huc4 %in% indiana_hucs){
    colnames(nhd)[17] <- 'NHDPlusID'
    nhd$NHDPlusID <- round(nhd$NHDPlusID, 0) #some of these have digits for some reason......
    colnames(nhd)[12] <- 'FCode_riv'
    colnames(nhd)[31] <- 'FCode_waterbody'
  }
  else{
    colnames(nhd)[16] <- 'NHDPlusID'
    colnames(nhd)[11] <- 'FCode_riv'
    colnames(nhd)[27] <- 'FCode_waterbody'
  }

  #JOIN ALL THE DESIRED ATTRIBUTES TO RIVER NETWORK-------------------------------------------
  nhd <- dplyr::left_join(nhd, NHD_HR_EROM, by='NHDPlusID')
  nhd <- dplyr::left_join(nhd, NHD_HR_VAA, by='NHDPlusID')
  nhd <- dplyr::left_join(nhd, temp, by='NHDPlusID')

  #ADD FLAG FOR NON-US RIVERS----------------------------------------------------------------
  conus <- sf::st_read('data/cb_2018_us_nation_20m.shp')
  conus <- dplyr::select(conus, 'GEOID', 'geometry')
  nhd_conus <- sf::st_intersection(nhd, conus)
  nhd$conus <- ifelse(nhd$NHDPlusID %in% nhd_conus$NHDPlusID, 1,0)

  #UNIT CONVERSIONS----------------------------------------------------------------------------
  nhd$StreamOrde <- nhd$StreamCalc #stream calc handles divergent streams correctly: https://pubs.usgs.gov/of/2019/1096/ofr20191096.pdf
  nhd$Q_cms <- nhd$QBMA * 0.0283 #cfs to cms

  #HANDLE INDIANA EFFECTED STREAM ORDERING (remap using the augmented shapefiles)------------------------------------------------------
  if(huc4 %in% indiana_hucs){
    thresh <- c(2,2,2,2,3,2,3,2) #see README file for notes and description of this process
    thresh <- thresh[which(indiana_hucs == huc4)]
    nhd$StreamOrde <- ifelse(nhd$indiana_fl == 1, nhd$StreamOrde - thresh, nhd$StreamOrde)
  }

  #ASSIGN WATERBODY TYPE--------------------------------------------------------
  nhd$waterbody <- ifelse(is.na(nhd$WBArea_Permanent_Identifier)==0 & is.na(nhd$LakeAreaSqKm) == 0 & nhd$LakeAreaSqKm > 0, 'Lake/Reservoir', 'River')

  #FIX THREE IDENTIFIED ROUTING ID ERRORS---------------------------------------------
  if(huc4 == '0514'){nhd[nhd$NHDPlusID == 24000100384878,]$StreamOrde <- 8}   #fix erronous 'divergent' reach in the Ohio mainstem (mathing Indiana file upstream)
  if(huc4 == '0514'){nhd[nhd$NHDPlusID == 24000100569580,]$ToNode <- 22000100085737} #from/to node ID typo (from Ohio River to Missouri River) so I manually fix it
  if(huc4 == '0706'){nhd[nhd$NHDPlusID == 22000400022387,]$StreamOrde <- 7} #error in stream order calcualtion because reach is miss-assigned as stream order 0 (on divergent path) which isn't true. Easiest to just skip over the reach because it's just a connector into the Misssissippi River (from Wisconsin river)

  #FINAL HYDROGRAPHY SETUP------------------------------------------------
  nhd <- dplyr::filter(nhd, StreamOrde > 0 & is.na(HydroSeq)==0 & FlowDir == 1)
  nhd <- dplyr::filter(nhd, Q_cms > 0) #remove streams with no mean annual flow
  nhd$waterbody <- as.character(nhd$waterbody)
  nhd$lakeVol_m3 <- 0.533 * (nhd$LakeAreaSqKm*1e6)^1.204 #calculate lake volume (Cael et al. 2016)

  #FIX NA OR 0 SLOPES (when appropriate, use average slope of directly upstream and downstream reaches)-----------------------------------------------
  slope_vec <- as.vector(nhd$Slope)
  toNode_vec <- as.vector(nhd$ToNode)
  fromNode_vec <- as.vector(nhd$FromNode)
  badSlopes <- which(slope_vec <= 0)
  for(k in badSlopes){
    nhd[k,]$Slope <- fixBadSlopes(nhd[k,]$Slope, slope_vec, nhd[k,]$FromNode, fromNode_vec, nhd[k,]$ToNode, toNode_vec)
  }

  #FIX MISSING AIR TEMPS (when appropriate, use average temp of the directly upstream reaches)--------------------------------------------------------
  temp_vec <- as.vector(nhd$airTemp_mean_c)
  badTemps <- which(is.na(temp_vec))
  for(k in badTemps){
    nhd[k,]$airTemp_mean_c <- fixBadTemps(nhd[k,]$airTemp_mean_c, temp_vec, nhd[k,]$FromNode, fromNode_vec, nhd[k,]$ToNode, toNode_vec) #temp, temp_vec, fromNode, fromNode_vec, toNode, toNode_vec)
  }

  #filter reaches with no flow and no slope
  nhd <- dplyr::filter(nhd, Slope > 0 & Q_cms > 0)

  #CALCULATE FRACTIONAL LAKE AREA AND VOLUME FOR EACH THROUGHFLOW LINE (Brinkerhoff et al. 2021)---------------------------------------------------------------
  sumThroughFlow <- dplyr::filter(as.data.frame(nhd), is.na(WBArea_Permanent_Identifier)==0) %>%
    dplyr::group_by(WBArea_Permanent_Identifier) %>%
    dplyr::summarise(sumThroughFlow = sum(LengthKM))
  nhd <- dplyr::left_join(nhd, sumThroughFlow, by='WBArea_Permanent_Identifier')
  nhd$lakePercent <- nhd$LengthKM / nhd$sumThroughFlow #fraction of lake assigned to each throughflow line is used to split the lake up
  nhd$frac_lakeVol_m3 <- nhd$lakeVol_m3 * nhd$lakePercent #[m3]
  nhd$frac_lakeSurfaceArea_m2 <- nhd$LakeAreaSqKm * nhd$lakePercent * 1e6 #[m2]

  #CALCULATE NECESSARY VARIABLES FOR THE MODEL-------------------------------------------------------------
  #hydraulic geometry
  depAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/depAHG.rds') #these are hard coded here. the models are available at the ref in the paper
  widAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/widAHG.rds')
  nhd$a <- widAHG$coefficients[1]
  nhd$b <- widAHG$coefficients[2]
  nhd$c <- depAHG$coefficients[1]
  nhd$f <- depAHG$coefficients[2]

  #calculate w, d, v differently for river vs lake/reservoir (see ~src/utils.R)
  nhd$D <- mapply(depth_func, nhd$waterbody, nhd$Q_cms, nhd$frac_lakeVol_m3, nhd$frac_lakeSurfaceArea_m2, nhd$c, nhd$f) #[m]
  nhd$W <- mapply(width_func, nhd$waterbody, nhd$Q_cms, nhd$a, nhd$b) #[m]
  nhd$V <- mapply(velocity_func, nhd$waterbody, nhd$Q_cms, nhd$W, nhd$D) #[m/s]

  #convert temperature to celsius
  nhd$temp_c <- 3.941 + 0.818*nhd$airTemp_mean_c #[c] #convert air temp to water temp following Lauerwald etal 2015: r2 0.88

  #calculate hydraulic residence time [seconds]
  nhd$HRT <- mapply(restimeWater, nhd$frac_lakeVol_m3, nhd$LengthKM, nhd$V, nhd$Q_cms, nhd$waterbody) #[s]

  #calculate Henry's constant.
  nhd$henry <- mapply(henry_func, nhd$temp_c) #[mol/atm*L]

  #calculate gas exchange rate constants differently for river vs lake/reservoir
  nhd$k_co2 <- mapply(kco2_func, nhd$V, nhd$Slope, nhd$D, nhd$temp_c, nhd$frac_lakeSurfaceArea_m2, nhd$waterbody) #[1/s]
  nhd$k_bz <- mapply(kbz_func, nhd$Slope, nhd$D, nhd$waterbody, nhd$temp_c) #[1/s]

  nhd <- as.data.frame(nhd)

  #duplicate column names, just manually rename the column we care about
  colnames(nhd)[87] <- 'CatchmentAreaSqKm'

  #PREP OUTPUT----------------------------------------------------------------------------------------
  nhd <- dplyr::select(nhd, c('NHDPlusID', 'NHDPlusID', 'WBArea_Permanent_Identifier', 'conus', 'FromNode', 'ToNode', 'HydroSeq', 'StartFlag', 'Divergence', 'waterbody', 'StreamOrde', 'LengthKM', 'Slope', 'Q_cms', 'frac_lakeSurfaceArea_m2',
                            'frac_lakeVol_m3', 'CatchmentAreaSqKm', 'temp_c', 'HRT', 'henry', 'k_co2', 'k_bz', 'W', 'D', 'V'))

  #RETURN ROUTING TABLE---------------------------------------------------
  return(nhd)
}







#' Split 1710 hydrography into two for treating as seperate basins. These are coastal watersheds N and S of Colombia River mouth, so together there are millions of streams and it makes compute very difficult.
#'    So, we split the basin along the Colombia to make compute easier, but also give the basins more physical meaning
#'
#' @name split1710
#'
#' @param hydrography_1710: 1710 basin routing table
#' @param sub: '1710a' or '1710b' for 1710 subbasin
#'
#' @import dplyr
#' @import readr
#'
#' @return usable routing file for NHD
split1710 <- function(hydrography_1710, sub){
  IDs_1710sub <- readr::read_csv(paste0('data/',sub,'IDs.csv')) %>%
    dplyr::select(c('NHDPlusID'))

  hydrography_1710sub <- dplyr::filter(hydrography_1710, NHDPlusID %in% IDs_1710sub$NHDPlusID)

  return(hydrography_1710sub)
}