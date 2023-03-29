## Calculates Raymond CO2 flux using their CO2 and their hydrography
## Winter 2022
## Craig Brinkerhoff/Brian Saccardi











#' Runs rayond upscaling model per huc2 region
#'
#' @name runRaymondModel
#'
#' @param path_to_data: path to NHD geodatabases
#' @param HUC2: HUC2 region id
#' @param glorich_data: obsered CO2 concentrations per region used to upscale
#'
#' @import dplyr
#' @import terra
#' @import dplyr
#'
#' @return final upscaled CO2 flux estimates for a given HUC2 region
runLumpedModels <- function(path_to_data, HUC2, glorich_data,hydrographyList,emissions_list,emissions_uncertainty_list) {
  ###########SETUP-------------------------------------
  #get glorich co2
  riverCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$river #[ppm]
  lakeCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$lake #[ppm]

  #setup hydrography
  network <- do.call("rbind", hydrographyList) #make HUC2 river network
  model_emissions <- do.call("rbind", emissions_list) #make HUC2 uncertainty estimates
  cal_uncertainty <- sum(do.call("rbind", emissions_uncertainty_list)) #make HUC2 uncertainty estimates

  #filter to match model run hydrography
  network <- dplyr::filter(network, HydroSeq != 0)

  #regional average water temperature following raymond 2013
  temp_c <- mean(network$temp_c, na.rm=T)
  henry <- henry_func(temp_c)
  sc <- 1911-118.11*temp_c+3.453*temp_c^2-0.0413*temp_c^3 #Raymond2012/Wanninkof 1991

  ##########RIVERS---------------------------------------
  rivers_by_order <- dplyr::group_by(network[network$waterbody == 'River',], StreamOrde) %>%
    dplyr::summarise(kco2_m_s = mean(k_co2*D),
                     SA_m2 = sum(LengthKM*W*1000))

  #get regional k co2
  rivers_k_co2_lumped_m_s <- weighted.mean(rivers_by_order$kco2_m_s, rivers_by_order$SA_m2, na.rm=T) #normalize by surface area for each order

  #calculate a regional fco2
  rivers_FCO2_lumped <- ((riverCO2-390)*henry*1e-6)*rivers_k_co2_lumped_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]

  #calculate regional carbon flux
  rivers_FCO2_lumped_total <- rivers_FCO2_lumped*sum(rivers_by_order$SA_m2, na.rm=T) #[g-C/yr]




  ##########LAKES/RESERVOIRS---------------------------------------
  lakes_by_order <- dplyr::filter(network, waterbody == 'Lake/Reservoir') %>% #build combined lakes dataset for lake scaling
    dplyr::group_by(WBArea_Permanent_Identifier) %>%
    dplyr::summarise(area_skm = sum(frac_lakeSurfaceArea_m2, na.rm=T)*1e-6) %>%
    dplyr::mutate(k600_m_dy = ifelse(area_skm < 0.1, 0.54, #[m/dy] Read etal 2012
                        ifelse(area_skm < 1, 1.16,
                          ifelse(area_skm < 3.16, 1.32,
                            ifelse(area_skm < 10, 1.9, 1.9))))) %>%
    dplyr::mutate(kco2_m_dy = k600_m_dy/(600/sc)^-0.5) %>% #m/dy
    dplyr::mutate(kco2_m_s = (kco2_m_dy)/(24*60*60))  #m/s

  #calculate a regional fco2
  lakes_by_order$lakes_FCO2_lumped <- ((lakeCO2-390)*henry*1e-6)*lakes_by_order$kco2_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]

  #calculate regional carbon emissions flux
  lakes_FCO2_lumped_total <- sum(lakes_by_order$lakes_FCO2_lumped*lakes_by_order$area_skm*1e6, na.rm=T) #[g-C/yr]

 
  #SEMI-DISTRIBUTED MODEL------------------------
  network$semi_FCO2_gC_m2_yr <- ifelse(network$waterbody == 'River', ((riverCO2-390)*henry*1e-6)*(network$k_co2*network$D)*(1/0.001)*12.01*(60*60*24*365), ((lakeCO2-390)*henry*1e-6)*(network$k_co2*network$D)*(1/0.001)*12.01*(60*60*24*365)) #gC_m2_yr
  network$semi_FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$semi_FCO2_gC_m2_yr*network$W*network$LengthKM*1000, network$semi_FCO2_gC_m2_yr*network$frac_lakeSurfaceArea_m2)

  #OUR MODEL EMISSIONS--------------------------------------
  networkOutput <- dplyr::group_by(model_emissions, waterbody) %>%
                       dplyr::summarise(sumFCO2_TgC_yr = sum(sumFCO2_TgC_yr, na.rm=T), #basin Tg-C/yr
                                        sumFCO2_conus_TgC_yr = sum(sumFCO2_conus_TgC_yr, na.rm=T), #basin Tg-C/yr with no international streams
                                        n = n())

  #save results to file-----------------------------------
  network_fluxes_lumped <- data.frame('huc2'=HUC2,
                                  'sumFCO2_lumped_TgC_yr' = c(rivers_FCO2_lumped_total*1e-12, lakes_FCO2_lumped_total*1e-12),
                                  'sumFCO2_semiDist_TgC_yr'=c(sum(network[network$waterbody == 'River',]$semi_FCO2_gC_yr, na.rm=T)*1e-12, sum(network[network$waterbody == 'Lake/Reservoir',]$semi_FCO2_gC_yr, na.rm=T)*1e-12),
                                  'sumFCO2_TgC_yr'=c(networkOutput[networkOutput$waterbody == 'River',]$sumFCO2_TgC_yr, networkOutput[networkOutput$waterbody == 'Lake/Reservoir',]$sumFCO2_TgC_yr),
                                  'sumFCO2_conus_TgC_yr'=c(networkOutput[networkOutput$waterbody == 'River',]$sumFCO2_conus_TgC_yr, networkOutput[networkOutput$waterbody == 'Lake/Reservoir',]$sumFCO2_conus_TgC_yr),
                                  'waterbody'=c('River', 'Lake/Reservoir'),
                                  'cal_uncertainty'=c(cal_uncertainty,cal_uncertainty)) #duplicated for the river row and the lake row
 return(network_fluxes_lumped)

}
