##########################
## Calculates upscaled CO2 flux
## Spring 2023
## Craig Brinkerhoff/Brian Saccardi
##########################





#' Runs upscaling model per huc2 region (and estimates uncertainty)
#'
#' @name runLumpedModels
#'
#' @param HUC2: HUC2 region id
#' @param glorich_data: observed CO2 concentrations per region (HUC2) used to upscale
#' @param hydrographyList: list of regional (HUC2) river networks to do upscaling on
#' @param co2List: list of calibrated model runs for all basins in the region
#' @param emissions_uncertainty: list of transport model uncertanties for the region
#'
#' @import dplyr
#'
#' @return final upscaled CO2 flux estimates for a given HUC2 region
runLumpedModels <- function(HUC2, glorich_data,hydrographyList,co2List,emissions_list,emissions_uncertainty_list) {
  ###########SETUP-------------------------------------
  #get glorich co2
  riverCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$river #[ppm]
  lakeCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$lake #[ppm]

  #setup hydrography
  network <- do.call("rbind", hydrographyList) #make HUC2 river network
  model_co2 <- do.call("rbind", co2List) #make HUC2 results network
  model_emissions <- do.call("rbind", emissions_list) #make HUC2 emissions
  cal_uncertainty <- do.call("rbind", emissions_uncertainty_list) #make HUC2 uncertainty estimates
  cal_uncertainty <- sum(cal_uncertainty$sigma, na.rm=T) #great lakes are NA

  network <- dplyr::filter(network, HydroSeq != 0)

  #handle HUC4s with fake shoreline rivers
  if(HUC2 == '04'){
    network_new <- data.frame()
    for(i in c('0402', '0405', '0406', '0407', '0408', '0411', '0412','0401','0410','0414','0403','0404')){
      fix <- readr::read_csv(paste0('data/fix_',i,'.csv')) %>%
        dplyr::filter(GL_pass == '0')

      network_new <- rbind(network_new, fix)
    }
      network <- dplyr::filter(network, !(NHDPlusID %in% network_new$NHDPlusID))
  }

  #regional average water temperature following raymond 2013
  temp_c <- mean(network$temp_c, na.rm=T)
  henry <- henry_func(temp_c)
  sc <- 1911-118.11*temp_c+3.453*temp_c^2-0.0413*temp_c^3 #Raymond2012/Wanninkof 1991
  

  #ACTUAL LUMPED/UPSCALING APPROACH CALCULATION-------------------------------------
  #RIVERS---------------------------------------
  depAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/depAHG.rds') #these are hard coded here. the models are available at the ref in the paper
  widAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/widAHG.rds') #these are hard coded here. the models are available at the ref in the paper
  rivers_by_order <- network %>%
    dplyr::group_by(StreamOrde) %>%
    dplyr::summarise(kco2_m_s = mean(k_co2*D, na.rm=T), #m/s
                     SA_m2 = sum(LengthKM*W*1000, na.rm=T))

  #get regional k co2
  rivers_k_co2_lumped_m_s <- weighted.mean(rivers_by_order$kco2_m_s, rivers_by_order$SA_m2, na.rm=T) #normalize by surface area for each order

  #calculate a regional fco2
  rivers_FCO2_lumped <- ((riverCO2-400)*henry*1e-6)*rivers_k_co2_lumped_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]

  #calculate regional carbon flux
  rivers_FCO2_lumped_total <- rivers_FCO2_lumped*sum(rivers_by_order$SA_m2, na.rm=T)*1e-12 #[Tg-C/yr]

  #LAKES/RESERVOIRS---------------------------------------
  lakes_by_order <- network %>%
    dplyr::filter(waterbody == 'Lake/Reservoir') %>% #build combined lakes dataset for lake scaling
    dplyr::group_by(WBArea_Permanent_Identifier) %>%
    dplyr::summarise(area_skm = sum(frac_lakeSurfaceArea_m2, na.rm=T)*1e-6) %>%
        dplyr::mutate(k600_m_dy = ifelse(area_skm < 0.1, 0.54, #[m/dy] Read etal 2012
                       ifelse(area_skm < 1, 1.16,
                           ifelse(area_skm < 10, 1.32, 1.9)))) %>%
    dplyr::mutate(kco2_m_dy = k600_m_dy/(600/sc)^-0.5) %>% #m/dy
    dplyr::mutate(kco2_m_s = (kco2_m_dy)/(24*60*60))  #m/s

  #calculate a regional fco2
  lakes_by_order$lakes_FCO2_lumped <- ((lakeCO2-400)*henry*1e-6)*lakes_by_order$kco2_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]

  #calculate regional carbon emissions flux
  lakes_FCO2_lumped_total <- sum(lakes_by_order$lakes_FCO2_lumped*lakes_by_order$area_skm*1e6, na.rm=T)*1e-12 #[Tg-C/yr]



  #ESTIMATE UPSCALING UNCERTAINTY-------------------------------------------------------------------------
  network2 <- network %>%
    dplyr::filter(waterbody == 'River') %>%
    dplyr::mutate(kco2_m_s = k_co2*D,
                  SA_m2 = LengthKM*W*1000)

  rivers_k_co2_not_lumped <- median(network2$kco2_m_s,  na.rm=T)
  cost <- abs(rivers_k_co2_lumped_m_s - rivers_k_co2_not_lumped) #m/s

  lumpedFluxFin_sigma <- cost * (((riverCO2-400)*median(network2$henry, na.rm=T))/1000000)*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]
  lumpedFluxFin_sigma <- lumpedFluxFin_sigma * sum(network2$SA_m2, na.rm=T) * 1e-12 #[Tg-C/yr]


  #SEMI-LUMPED MODELS. NOT ACTUALLY USED IN PAPER RESULTS BUT INCLUDED IN SOME OF THE EXTENDED DATA FIGURES-------------------------------------------
  #LUMPED CO2 MODEL
  network$lumpedCO2_FCO2_gC_m2_yr <- ifelse(network$waterbody == 'River', ((riverCO2-400)*network$henry*1e-6)*(network$k_co2*network$D)*(1/0.001)*12.01*(60*60*24*365), ((lakeCO2-400)*network$henry*1e-6)*(network$k_co2*network$D)*(1/0.001)*12.01*(60*60*24*365)) #gC_m2_yr
  network$lumpedCO2_FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$lumpedCO2_FCO2_gC_m2_yr*network$W*network$LengthKM*1000, network$lumpedCO2_FCO2_gC_m2_yr*network$frac_lakeSurfaceArea_m2)

  #LUMPED k MODEL
  model_co2$lumpedK_FCO2_gC_m2_yr <- ifelse(model_co2$waterbody == 'River', ((model_co2$CO2_ppm-400)*henry*1e-6)*(rivers_k_co2_lumped_m_s)*(1/0.001)*12.01*(60*60*24*365), ((model_co2$CO2_ppm-400)*henry*1e-6)*(model_co2$k_co2_m_s)*(1/0.001)*12.01*(60*60*24*365)) #gC_m2_yr
  model_co2$lumpedK_FCO2_gC_yr <- ifelse(model_co2$waterbody == 'River', model_co2$lumpedK_FCO2_gC_m2_yr*model_co2$W_m*model_co2$LengthKM*1000, model_co2$lumpedK_FCO2_gC_m2_yr*model_co2$lakeSA_m2)


  #OUR MODEL--------------------------------------
  networkOutput <- dplyr::group_by(model_emissions, waterbody) %>%
                       dplyr::summarise(sumFCO2_TgC_yr = sum(sumFCO2_TgC_yr, na.rm=T), #basin Tg-C/yr
                                        sumFCO2_conus_TgC_yr = sum(sumFCO2_conus_TgC_yr, na.rm=T), #basin Tg-C/yr with no international streams
                                        n = n())

  #save results to file-----------------------------------
  network_fluxes_lumped <- data.frame('huc2'=HUC2,
                                  'sumFCO2_lumped_TgC_yr' = c(rivers_FCO2_lumped_total, lakes_FCO2_lumped_total),
                                  'sumFCO2_lumped_co2_TgC_yr'=c(sum(network[network$waterbody == 'River',]$lumpedCO2_FCO2_gC_yr, na.rm=T)*1e-12, sum(network[network$waterbody == 'Lake/Reservoir',]$lumpedCO2_FCO2_gC_yr, na.rm=T)*1e-12),
                                  'sumFCO2_lumped_k_TgC_yr'=c(sum(model_co2[model_co2$waterbody == 'River',]$lumpedK_FCO2_gC_yr, na.rm=T)*1e-12, sum(model_co2[model_co2$waterbody == 'Lake/Reservoir',]$lumpedK_FCO2_gC_yr, na.rm=T)*1e-12),                                  
                                  'sumFCO2_TgC_yr'=c(networkOutput[networkOutput$waterbody == 'River',]$sumFCO2_TgC_yr, networkOutput[networkOutput$waterbody == 'Lake/Reservoir',]$sumFCO2_TgC_yr),
                                  'sumFCO2_conus_TgC_yr'=c(networkOutput[networkOutput$waterbody == 'River',]$sumFCO2_conus_TgC_yr, networkOutput[networkOutput$waterbody == 'Lake/Reservoir',]$sumFCO2_conus_TgC_yr),
                                  'waterbody'=c('River', 'Lake/Reservoir'),
                                  'cal_uncertainty'=c(cal_uncertainty,NA), #Just set to NA on the lakes half because this is for the entire basin
                                  'lumped_uncertainty'=c(lumpedFluxFin_sigma, NA)) #Just set to NA on the lakes half because this is for the entire basin

 return(network_fluxes_lumped)

}