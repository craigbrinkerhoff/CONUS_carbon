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
#' @param temp: water temperature (Celsius)
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
#' @param huc4: basin ID
#'
#' @import dplyr
#'
#' @return river network data frame with CO2 fluxes added [gC/m2/yr]
calcEmissions <- function(model, huc4){
  #fix HUC4s with fake shoreline rivers
  if(huc4 %in% c('0402', '0405', '0406', '0407', '0408', '0411', '0412','0401','0410','0414','0403','0404')) {
    fix <- readr::read_csv(paste0('data/fix_',huc4,'.csv'))
    model <- dplyr::left_join(model, fix, by='NHDPlusID')

    model[model$GL_pass == '0',]$FCO2_gC_m2_yr <- 0
    model[model$GL_pass == '0',]$W_m <- 0
    model[model$GL_pass == '0',]$LengthKM <- 0
  }

  #calculate fluxes per reach
  model$FCO2_gC_yr <- ifelse(model$waterbody == 'River',
                                   model$FCO2_gC_m2_yr * model$W_m * model$LengthKM * 1000, #river g-C/yr
                                   model$FCO2_gC_m2_yr * model$lakeSA_m2) #lake/reservoir g-C/yr

  #aggregate over watershed
  networkOutput <- model %>%
      dplyr::mutate(SA_m2 = ifelse(waterbody=='River', W_m*LengthKM*1000, lakeSA_m2)) %>%
      dplyr::group_by(waterbody) %>%
      dplyr::summarise(huc4=huc4,
          sumFCO2_TgC_yr = sum(FCO2_gC_yr, na.rm=T)*1e-12, #basin Tg-C/yr
          sumFCO2_conus_TgC_yr = sum(FCO2_gC_yr*conus, na.rm=T)*1e-12, #basin Tg-C/yr with no international streams
          sumSurfaceArea_skm = sum(SA_m2, na.rm=T)*1e-6,
          sumSurfaceArea_conus_skm = sum(SA_m2*conus, na.rm=T)*1e-6,
          n = n())

  out <- networkOutput
  return(out)
}






#' calculate basin stats
#'
#' @name calcBasinProperties
#'
#' @param model: river network data frame
#' @param huc4: basin ID
#'
#' @return river network data frame with CO2 fluxes added [gC/m2/yr]
calcBasinProperties <- function(model, huc4){
  if(huc4 %in% c('0418', '0419', '0424', '0426', '0428')){
    out <- data.frame('huc4'=huc4,
                    'mean_k600_m_s'=NA,
                    'median_k600_m_s'=NA,
                    'mean_kco2_m_s'=NA,
                    'median_kco2_m_s'=NA,
                    'mean_slope'=NA,
                    'median_slope'=NA)
  }
  else{
    model$SA_m2 <- ifelse(model$waterbody == 'River', model$W_m*model$LengthKM*1000, model$lakeSA_m2)
    #summary stats
    out <- data.frame('huc4'=huc4,
                    'mean_k600_m_s'=mean(model$k600_m_s, na.rm=T),
                    'median_k600_m_s'=median(model$k600_m_s,na.rm=T),
                    'mean_kco2_m_s'=mean(model$k_co2_m_s,na.rm=T),
                    'median_kco2_m_s'=median(model$k_co2_m_s,na.rm=T),
                    'mean_slope'=mean(model$Slope, na.rm=T),
                    'median_slope'=median(model$Slope, na.rm=T))
  }
  return(out)
}





#' calculate basin lake stats
#'
#' @name calcBasinLakeProperties
#'
#' @param model: river network data frame
#' @param huc4: basin ID
#'
#' @import dplyr
#'
#' @return river network data frame with CO2 fluxes added [gC/m2/yr]
calcBasinLakeProperties <- function(model, huc4){
  if(huc4 %in% c('0418', '0419', '0424', '0426', '0428')){
    out <- data.frame('huc4'=huc4,
                      'lakeBin'=NA,
                      'n_reaches' = NA,
                      'binArea_skm' = NA,
                      'binFlux_gC_m2_yr' = NA,
                      'binFlux_TgC_yr'=NA)
  }
  else{
    model$lakeBin <- ifelse(model$lakeSA_m2*1e-6 < 0.001, '0.001',
                        ifelse(model$lakeSA_m2*1e-6 < 0.01, '0.01',
                            ifelse(model$lakeSA_m2*1e-6 < 0.1, '0.1',
                                ifelse(model$lakeSA_m2*1e-6 < 1, '1',
                                    ifelse(model$lakeSA_m2*1e-6 < 10,'10',
                                        ifelse(model$lakeSA_m2*1e-6 < 100, '100', '100+'))))))

    out <- model %>%
      dplyr::filter(lakeSA_m2 > 0) %>%
      dplyr::group_by(lakeBin) %>%
      dplyr::summarise(binArea_skm = sum(lakeSA_m2*1e-6, na.rm=T),
                       binFlux_gC_m2_yr = sum(FCO2_gC_m2_yr, na.rm=T),
                       n_reaches = n()) %>%
      dplyr::mutate(binFlux_TgC_yr = binFlux_gC_m2_yr*binArea_skm*1e6*1e-12,
                    huc4=huc4) %>%
      dplyr::select(c('huc4', 'lakeBin', 'n_reaches', 'binArea_skm', 'binFlux_gC_m2_yr', 'binFlux_TgC_yr'))
  }
  return(out)
}





#' Calculate calibration uncertainty per basin
#'
#' @name emissions_uncertainty
#'
#' @param calibratedParameters: list of calibration results
#' @param model: river network data frame
#' @param huc4: model huc basin code
#'
#' @return uncertainty in carbon emissions due to model calibration [Tg-C/yr]
emissions_uncertainty <- function(calibratedParameters, model, huc4){
  combined_fitness <- calibratedParameters$fitness
  cost <- (1/combined_fitness)/2 #eq 16 in paper

  model$sa <- ifelse(model$waterbody == 'River', model$W_m*model$LengthKM*1000, model$lakeSA_m2) #m2
  SA <- sum(model$sa, na.rm=T) #m2

  sigma <- median(model$k_co2_m_s, na.rm=T) * ((cost*median(model$henry, na.rm=T))/1000000)*(1/0.001)*12.01*(60*60*24*365) #g-C/m2/yr
  sigma <- sigma * SA * 1e-12 #Tg-C/yr

  out <- data.frame('huc4'=huc4,
                    'sigma'=sigma)

  return(out) #Tg-C/yr
}



#' Build the results shapefile that compares the four carbon models
#'
#' @name saveShapefile_huc2
#'
#' @param path_to_data: path to NHD geodatabases
#' @param codes_huc02: list of HUC2 ids
#' @param lumpedList: list of model results per hUC2 region
#'
#' @import dplyr
#' @import sf
#'
#' @return print statement, writes shapefile to file
saveShapefile_huc2 <- function(path_to_data, codes_huc02, lumpedList){
    combined_results <- do.call("rbind", lumpedList) #make lumped model object

    #read in all HUC4 basins
    basins_overall <- sf::st_read(paste0(path_to_data, '/HUC2_', codes_huc02[1], '/WBD_', codes_huc02[1], '_HU2_Shape/Shape/WBDHU2.shp')) %>% dplyr::select(c('huc2'))
    for(i in codes_huc02[-1]){
      if (i %in% c('13','04')){
        basins <- sf::st_read(paste0(path_to_data, '/HUC2_',i,'_clipped.shp')) %>%
          sf::st_make_valid() %>%
          dplyr::mutate('huc2'=i) %>%
          dplyr::select(c('huc2')) #basin polygons
      }
      else{
        basins <- sf::st_read(paste0(path_to_data, '/HUC2_', i, '/WBD_', i, '_HU2_Shape/Shape/WBDHU2.shp')) %>%
          dplyr::select(c('huc2')) #basin polygons        
      }

      basins_overall <- rbind(basins_overall, basins)
    }

    #join model results
    basins_overall <- dplyr::left_join(basins_overall, combined_results, by='huc2') %>%
      dplyr::group_by(huc2) %>% #sum rivers and lakes/reservoirs
      dplyr::summarise(sumFCO2_lumped_TgC_yr = sum(sumFCO2_lumped_TgC_yr),
               sumFCO2_TgC_yr = sum(sumFCO2_TgC_yr),
               sumFCO2_conus_TgC_yr = sum(sumFCO2_conus_TgC_yr),
               sumFCO2_lumped_k_TgC_yr = sum(sumFCO2_lumped_k_TgC_yr),
               sumFCO2_lumped_co2_TgC_yr = sum(sumFCO2_lumped_co2_TgC_yr),
               cal_uncertainty = sum(cal_uncertainty, na.rm=T)) #take sum number and NA to pass the value through the group_by

    #round for mapping
    basins_overall <- dplyr::select(basins_overall, c('huc2', 'sumFCO2_lumped_TgC_yr', 'sumFCO2_lumped_k_TgC_yr', 'sumFCO2_lumped_co2_TgC_yr', 'sumFCO2_TgC_yr', 'sumFCO2_conus_TgC_yr', 'cal_uncertainty'))
  
    #return shapefile
    return(basins_overall)
}




#' Build the results shapefile that looks at GW and lake influences
#'
#' @name saveShapefile_huc4
#'
#' @param path_to_data: path to NHD geodatabases
#' @param combined_contribSources: df of various source and sink terms for basin CO2
#' @param combined_emissions: df of just emissions results parsed by river vs lake
#'
#' @import dplyr
#' @import sf
#'
#' @return print statement, writes shapefile to file
saveShapefile_huc4 <- function(path_to_data, combined_contribSources, combined_emissions){
    combined_contribSources <- dplyr::select(combined_contribSources, !c('emissions_TgC_yr', 'contribBZ_TgC_yr_massBal'))

    #combine lake and rivers for total
    combined_emissions_total <- combined_emissions %>%
      dplyr::group_by(huc4) %>%
      dplyr::summarise(sumFCO2_TgC_yr = sum(sumFCO2_TgC_yr, na.rm=T),
                       sumFCO2_conus_TgC_yr = sum(sumFCO2_conus_TgC_yr, na.rm=T),
                       sumSurfaceArea_skm = sum(sumSurfaceArea_skm, na.rm=T)) #for GW comparison, we compare it against total GW lost (whether through emissions or photosynthesis)

    #add percent lakes
    lakeEmissions = combined_emissions %>%
      dplyr::filter(waterbody == 'Lake/Reservoir') %>%
      dplyr::group_by(huc4) %>% #account for 1710
      dplyr::summarise(lakeFCO2_TgC_yr = sum(sumFCO2_TgC_yr, na.rm=T))

    combined_emissions_total <- combined_emissions_total %>%
      dplyr::left_join(lakeEmissions, by='huc4') %>%
      dplyr::mutate(lakeFCO2_TgC_yr = ifelse(sumFCO2_TgC_yr == 0, NA, lakeFCO2_TgC_yr)) #handle great lakes

    #combine 1710a and 1710b into single values for mapping
    gw_1710 <- combined_contribSources[combined_contribSources$huc4 == '1710a',]$contribGW_TgC_yr + combined_contribSources[combined_contribSources$huc4 == '1710b',]$contribGW_TgC_yr #Tg-C/yr
    up_1710 <- combined_contribSources[combined_contribSources$huc4 == '1710a',]$contribUP_TgC_yr + combined_contribSources[combined_contribSources$huc4 == '1710b',]$contribUP_TgC_yr #Tg-C/yr
    wc_1710 <- combined_contribSources[combined_contribSources$huc4 == '1710a',]$contribWC_TgC_yr + combined_contribSources[combined_contribSources$huc4 == '1710b',]$contribWC_TgC_yr #Tg-C/yr
    bz_1710 <- combined_contribSources[combined_contribSources$huc4 == '1710a',]$contribBZ_TgC_yr + combined_contribSources[combined_contribSources$huc4 == '1710b',]$contribBZ_TgC_yr #Tg-C/yr
    exp_1710 <- combined_contribSources[combined_contribSources$huc4 == '1710a',]$exported_TgC_yr + combined_contribSources[combined_contribSources$huc4 == '1710b',]$exported_TgC_yr #Tg-C/yr
    combined_contribSources <- rbind(combined_contribSources, data.frame('method'=NA, 'huc4'='1710', 'contribGW_TgC_yr'=gw_1710, 'contribUP_TgC_yr'=up_1710, 'contribWC_TgC_yr'=wc_1710, 'contribBZ_TgC_yr'=bz_1710,'exported_TgC_yr'=exp_1710))
    combined_contribSources <- dplyr::filter(combined_contribSources, !(huc4 %in% c('1710a', '1710b')))

    basins <- combined_contribSources$huc4

    #read in all HUC4 basins
    huc2 <- substr(basins[1], 1, 2)
    basins_overall <- sf::st_read(paste0(path_to_data, '/HUC2_', huc2, '/WBD_', huc2, '_HU2_Shape/Shape/WBDHU4.shp')) %>% dplyr::filter(huc4 == basins[1]) %>% dplyr::select(c('huc4', 'name'))
    for(i in basins[-1]){
      huc2 <- substr(i,1,2)
      basin <- sf::st_read(paste0(path_to_data, '/HUC2_', huc2, '/WBD_', huc2, '_HU2_Shape/Shape/WBDHU4.shp')) %>%
          dplyr::filter(huc4 == i) %>%
          dplyr::select(c('huc4', 'name')) #basin polygons
      basins_overall <- rbind(basins_overall, basin)
    }

    #join model results
    basins_overall <- basins_overall %>%
      dplyr::left_join(combined_contribSources, by='huc4') %>%
      dplyr::left_join(combined_emissions_total, by='huc4')

    basins_overall <- dplyr::select(basins_overall, c('huc4', 'contribGW_TgC_yr', 'contribUP_TgC_yr', 'contribWC_TgC_yr', 'contribBZ_TgC_yr','exported_TgC_yr','sumFCO2_TgC_yr', 'sumFCO2_conus_TgC_yr','lakeFCO2_TgC_yr', 'sumSurfaceArea_skm'))
  
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
#' @import sf
#' @import dplyr
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







#' Write selected river networks to csv so that brian can map them and play with them. Uses `fwrite` from the data.table package to be as efficient as possible
#'
#' @name writeToFile
#'
#' @param rivnet: target object for final routed river network
#' @param huc4: huc4 basin ID code
#'
#' @import data.table
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
#' @import readr
#' @import tidyr
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





#' removes the duplicate 0427 obejct (two inputs, but it is a great lake that goes to atmospheric so everything is identical)
#'
#' @name fixCombo0427
#'
#' @param in_df: any combined target df (misleading name...)
#'
#' @import dplyr
#'
#' @return combined df with only one 0427
fixCombo0427 <- function(in_df){
  
  out <- dplyr::filter(in_df, substr(method, nchar(method)-1, nchar(method)) != '_1')
  
  return(out)
}






#' create summary object to base manuscript numbers off of
#'
#' @name gatherResults
#'
#' @param combined_emissions: all model CO2 emissions by basin
#' @param combined_contribSources: all model CO2 emission sources by basin
#' @param combined_uncertainty: all model CO2 emissions uncertainties by basin
#' @param combined_basinProperties: all river network stats by basin
#' @param combined_basinLakeProperties: all lake network stats by basin
#' @param combined_sources_by_order: all model CO2 emission sources by reach by order by basin
#' @param lumpedList: all regional upscaling model results
#'
#' @import readr
#'
#' @return list of all of these results!! written to file also
gatherResults <- function(combined_emissions, combined_contribSources,combined_uncertainty, combined_basinProperties, combined_basinLakeProperties, combined_sources_by_order, lumpedList){
  combined_lumped <- do.call("rbind", lumpedList) #make lumped model object

  out <- list('totalFlux_TgC_yr'=sum(combined_lumped$sumFCO2_TgC_yr,na.rm=T),
              'totalFlux_conus_TgC_yr'=sum(combined_lumped$sumFCO2_conus_TgC_yr,na.rm=T),
              'totalFluxLumped_TgC_yr'=sum(combined_lumped$sumFCO2_lumped_TgC_yr, na.rm=T),
              'totalFluxLumped_k_TgC_yr'=sum(combined_lumped$sumFCO2_lumped_co2_TgC_yr, na.rm=T),
              'totalFluxLumped_co2_TgC_yr'=sum(combined_lumped$sumFCO2_lumped_k_TgC_yr, na.rm=T),
              'conus_uncertainty_TgC_yr'=sum(combined_uncertainty$sigma, na.rm=T), #great lakes are NA                     
              'huc4_emissions'=combined_emissions,
              'huc4_Sources'=combined_contribSources,
              'huc4_basinProperties'=combined_basinProperties,
              'huc4_basinLakeProperties'=combined_basinLakeProperties,
              'sources_by_order'=combined_sources_by_order,
              'huc2_results'=combined_lumped)

  readr::write_rds(out, 'cache/summaryResults.rds')
  return(out)
}






#' randomly sample 1000 reaches from a basin
#'
#' @name getRandomSample
#'
#' @param network: model results df
#' @param glorich_data: regional river and lake CO2
#' @param huc4: basin ID
#'
#' @import dplyr
#'
#' @return random sample of 1000 reaches (with both models) for x basin
getRandomSample <- function(network, glorich_data, huc4) {
  HUC2 <- substr(huc4, 1, 2)

  #first get lumped model------------------------------------------------
  rivers_by_order <- network %>%
    dplyr::group_by(StreamOrde) %>%
    dplyr::summarise(k600_m_s = mean(k600_m_s, na.rm=T), #m/s
                     SA_m2 = sum(LengthKM*W_m*1000, na.rm=T))

  rivers_k600_m_s_lumped <- weighted.mean(rivers_by_order$k600_m_s, rivers_by_order$SA_m2, na.rm=T) #normalize by surface area for each order

  #get glorich co2
  riverCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$river #[ppm]
  lakeCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$lake #[ppm]

  network$lumped_k600_m_dy <- ifelse(network$waterbody=='River',rivers_k600_m_s_lumped*86400,ifelse(network$lakeSA_m2*1e6 < 0.1, 0.54, #[m/dy] Read etal 2012
                                                                                          ifelse(network$lakeSA_m2*1e6 < 1, 1.16,
                                                                                            ifelse(network$lakeSA_m2*1e6 < 10, 1.32, 1.9))))
  network$lumped_CO2 <- ifelse(network$waterbody == 'River', riverCO2, lakeCO2)

  #then randomly sample 1000-----------------------------------
  set.seed(345)
  out <- network %>%
    dplyr::slice_sample(n=1000)

  return(out)
}







#' get appropriate UTM zone from longitude
#'
#' @name long2UTM
#'
#' @param long: Longitude
#'
#' @return UTM zone (N) as numeric
long2UTM <- function(long) {
    out <- (floor((long + 180)/6) %% 60) + 1
    return(out)
}








#' join Glorich to our hydrography (to get slopes)
#'
#' @name getGlorichUS
#'
#' @param path_to_data: path to data repo
#' @param huc4: basin ID
#'
#' @import dplyr
#' @import sf
#'
#' @return random sample of 1000 reaches (with both models) for x basin
getGlorichUS <- function(path_to_data, huc4){
  huc2 <- substr(huc4,1,2)
  huc4n <- ifelse(nchar(huc4) > 4, substr(huc4, 1, 4), huc4) #handle 1710a and 1710b (left joiin below takes care of this)

  #get shapfile
  dsnPath <- paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4n, '_HU4_GDB/NHDPLUS_H_', huc4n, '_HU4_GDB.gdb')
  nhd <- sf::st_read(dsn=dsnPath, layer='NHDFlowline', quiet=TRUE)
  basin <- sf::st_read(paste0(path_to_data, '/HUC2_', huc2, '/WBD_', huc2, '_HU2_Shape/Shape/WBDHU4.shp'), quiet=TRUE) %>%
    dplyr::filter(huc4 == huc4n)

  NHD_HR_VAA <- sf::st_read(dsn = dsnPath, layer = "NHDPlusFlowlineVAA", quiet=TRUE) #additional 'value-added' attributes
  NHD_HR_VAA <- dplyr::select(NHD_HR_VAA, c('NHDPlusID', 'Slope'))

  NHD_HR_EROM <- sf::st_read(dsn = dsnPath, layer = "NHDPlusEROMMA", quiet=TRUE) #additional 'value-added' attributes
  NHD_HR_EROM$nhdQ_cms <- NHD_HR_EROM$QBMA * 0.0283 #cfs to cms
  NHD_HR_EROM <- dplyr::select(NHD_HR_EROM, c('NHDPlusID', 'nhdQ_cms'))

  nhd <- dplyr::left_join(nhd, NHD_HR_VAA, by='NHDPlusID')
  nhd <- dplyr::left_join(nhd, NHD_HR_EROM, by='NHDPlusID')
  nhd <- sf::st_zm(nhd)
  nhd <- fixGeometries(nhd)


  #extract coords from river network
  coords <- sf::st_coordinates(sf::st_centroid(nhd$Shape)) #get each line centroid
  utm_zone <- long2UTM(mean(coords[,1]))#get appropriate UTM zone using mean network longitude (~/src/utils.R)

  #project to given UTM zone for distance calcs
  epsg <- as.numeric(paste0('326', as.character(utm_zone)))
  nhd <- sf::st_transform(nhd, epsg)
  basin <- sf::st_transform(basin, epsg)

  #read glorich and filter for basin
  glorich <- readr::read_csv('data/glorich_rocher_ros_2019.csv')
  glorich_shp <- sf::st_as_sf(x=glorich,
                   coords = c("Longitude", "Latitude"),
                  crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  glorich_shp <- sf::st_transform(glorich_shp, epsg)

  glorich_shp <- sf::st_filter(glorich_shp, basin)

   if(nrow(glorich_shp)==0){
     return(data.frame('NHDPlusID'=NA, 'snap_distance_m'=NA, 'nhd_slope'=NA, 'nhdQ_cms'=NA,'STAT_ID'=NA, 'pco2'=NA, 'avg_temp_air'=NA))
   }

  #snap each point to nearest river
  nearestIndex <- sf::st_nearest_feature(glorich_shp, nhd)

  #Get the actual snapping distance
  distance <- sf::st_distance(glorich_shp, nhd[nearestIndex,], by_element = TRUE)

  out <- data.frame('NHDPlusID'=nhd[nearestIndex,]$NHDPlusID,
                    'snap_distance_m'=distance,
                    'nhd_slope'=nhd[nearestIndex,]$Slope,
                    'nhdQ_cms'=nhd[nearestIndex,]$nhdQ_cms)

  out <- cbind(out, glorich_shp)
  out <- dplyr::select(out, c('NHDPlusID', 'snap_distance_m', 'nhd_slope', 'nhdQ_cms','STAT_ID', 'pco2', 'avg_temp_air'))

  return(out)
}