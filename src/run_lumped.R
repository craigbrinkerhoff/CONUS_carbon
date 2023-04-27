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
runLumpedModels <- function(path_to_data, HUC2, glorich_data,hydrographyList,co2List,emissions_list,emissions_uncertainty_list) {
  ###########SETUP-------------------------------------
  #get glorich co2
  riverCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$river #[ppm]
  lakeCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$lake #[ppm]

  #setup hydrography
  network <- do.call("rbind", hydrographyList) #make HUC2 river network
  model_co2 <- do.call("rbind", co2List) #make HUC2 results network
  model_emissions <- do.call("rbind", emissions_list) #make HUC2 uncertainty estimates
  cal_uncertainty <- do.call("rbind", emissions_uncertainty_list) #make HUC2 uncertainty estimates
  cal_uncertainty <- sum(cal_uncertainty$sigma, na.rm=T) #great lakes are NA

  #filter to match model run hydrography
  network <- dplyr::filter(network, HydroSeq != 0)

  #fix HUC4s with fake shoreline rivers
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


  ##########RIVERS---------------------------------------
  #upscaling models don't distinguish lakes from rivers in hydrography, so we need to treat all lakes like rivers too or this exercise...
  depAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/depAHG.rds') #depth AHG model
  widAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/widAHG.rds') #depth AHG model
  rivers_by_order <- network %>%
    dplyr::group_by(StreamOrde) %>%
    dplyr::summarise(kco2_m_s = mean(k_co2*D, na.rm=T), #m/s
                     SA_m2 = sum(LengthKM*W*1000, na.rm=T))

  #get regional k co2
  rivers_k_co2_lumped_m_s <- weighted.mean(rivers_by_order$kco2_m_s, rivers_by_order$SA_m2, na.rm=T) #normalize by surface area for each order

  #calculate a regional fco2
  rivers_FCO2_lumped <- ((riverCO2-400)*henry*1e-6)*rivers_k_co2_lumped_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]

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
  lakes_by_order$lakes_FCO2_lumped <- ((lakeCO2-400)*henry*1e-6)*lakes_by_order$kco2_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]

  #calculate regional carbon emissions flux
  lakes_FCO2_lumped_total <- sum(lakes_by_order$lakes_FCO2_lumped*lakes_by_order$area_skm*1e6, na.rm=T) #[g-C/yr]

 
  #LUMPED CO2 MODEL---------------------------------
  network$lumpedCO2_FCO2_gC_m2_yr <- ifelse(network$waterbody == 'River', ((riverCO2-400)*network$henry*1e-6)*(network$k_co2*network$D)*(1/0.001)*12.01*(60*60*24*365), ((lakeCO2-400)*network$henry*1e-6)*(network$k_co2*network$D)*(1/0.001)*12.01*(60*60*24*365)) #gC_m2_yr
  network$lumpedCO2_FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$lumpedCO2_FCO2_gC_m2_yr*network$W*network$LengthKM*1000, network$lumpedCO2_FCO2_gC_m2_yr*network$frac_lakeSurfaceArea_m2)

  #LUMPED k MODEL---------------------------------
  model_co2$lumpedK_FCO2_gC_m2_yr <- ifelse(model_co2$waterbody == 'River', ((model_co2$CO2_ppm-400)*henry*1e-6)*(rivers_k_co2_lumped_m_s)*(1/0.001)*12.01*(60*60*24*365), ((model_co2$CO2_ppm-400)*henry*1e-6)*(model_co2$k_co2_m_s)*(1/0.001)*12.01*(60*60*24*365)) #gC_m2_yr
  model_co2$lumpedK_FCO2_gC_yr <- ifelse(model_co2$waterbody == 'River', model_co2$lumpedK_FCO2_gC_m2_yr*model_co2$W_m*model_co2$LengthKM*1000, model_co2$lumpedK_FCO2_gC_m2_yr*model_co2$lakeSA_m2)


  #OUR MODEL EMISSIONS--------------------------------------
  networkOutput <- dplyr::group_by(model_emissions, waterbody) %>%
                       dplyr::summarise(sumFCO2_TgC_yr = sum(sumFCO2_TgC_yr, na.rm=T), #basin Tg-C/yr
                                        sumFCO2_conus_TgC_yr = sum(sumFCO2_conus_TgC_yr, na.rm=T), #basin Tg-C/yr with no international streams
                                        n = n())

  #save results to file-----------------------------------
  network_fluxes_lumped <- data.frame('huc2'=HUC2,
                                  'sumFCO2_lumped_TgC_yr' = c(rivers_FCO2_lumped_total*1e-12, lakes_FCO2_lumped_total*1e-12),
                                  'sumFCO2_lumped_co2_TgC_yr'=c(sum(network[network$waterbody == 'River',]$lumpedCO2_FCO2_gC_yr, na.rm=T)*1e-12, sum(network[network$waterbody == 'Lake/Reservoir',]$lumpedCO2_FCO2_gC_yr, na.rm=T)*1e-12),
                                  'sumFCO2_lumped_k_TgC_yr'=c(sum(model_co2[model_co2$waterbody == 'River',]$lumpedK_FCO2_gC_yr, na.rm=T)*1e-12, sum(model_co2[model_co2$waterbody == 'Lake/Reservoir',]$lumpedK_FCO2_gC_yr, na.rm=T)*1e-12),                                  
                                  'sumFCO2_TgC_yr'=c(networkOutput[networkOutput$waterbody == 'River',]$sumFCO2_TgC_yr, networkOutput[networkOutput$waterbody == 'Lake/Reservoir',]$sumFCO2_TgC_yr),
                                  'sumFCO2_conus_TgC_yr'=c(networkOutput[networkOutput$waterbody == 'River',]$sumFCO2_conus_TgC_yr, networkOutput[networkOutput$waterbody == 'Lake/Reservoir',]$sumFCO2_conus_TgC_yr),
                                  'waterbody'=c('River', 'Lake/Reservoir'),
                                  'cal_uncertainty'=c(cal_uncertainty,NA)) #Just set to NA on the lakes half because this is for the entire basin


  theme_set(theme_classic())
    forPlot <- dplyr::left_join(network, model_co2, by='NHDPlusID') %>%
    dplyr::select(c('NHDPlusID', 'lumpedCO2_FCO2_gC_m2_yr', 'lumpedK_FCO2_gC_m2_yr', 'FCO2_gC_m2_yr')) %>%
    tidyr::gather(key=key, value=value, c('lumpedCO2_FCO2_gC_m2_yr', 'lumpedK_FCO2_gC_m2_yr', 'FCO2_gC_m2_yr'))

  plotFlux <- ggplot(forPlot, aes(x=value, fill=key))+
    geom_histogram(linewidth=1, color='black') +
    scale_x_log10()+
    scale_fill_brewer(palette='Dark2', name='', labels=c('Distributed','Lumped CO2', 'Lumped kco2'))+
    xlab('Flux [gC/m2/yr]')+
    ylab('') +
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"),
          legend.text = element_text(size=20),
          legend.position=c(0.25,0.8))    


  plotk <- ggplot(model_co2, aes(x=k_co2_m_s*86400))+
    geom_histogram(linewidth=1, color='black', fill='darkgrey') +
    scale_x_log10()+
    xlab('kco2 [m/dy]')+
    ylab('') +
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"),
          legend.text = element_text(size=20),
          legend.position=c(0.8,0.2))


  plotco2 <- ggplot(model_co2, aes(x=CO2_ppm))+
    geom_histogram(linewidth=1, color='black', fill='darkgrey') +
    scale_x_log10()+
    xlab('pCO2 [ppm]')+
    ylab('Count') +
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"),
          legend.text = element_text(size=20),
          legend.position=c(0.8,0.2))               

    design <- "
    ABC
    "
    comboPlot <- patchwork::wrap_plots(A=plotco2, B=plotk, C=plotFlux, design=design)

  ggsave(filename=paste0('cache/figures/fluxSupp_',HUC2, '.jpg'),plot=comboPlot,width=20,height=8)

  # forPlot <- dplyr::left_join(network, model_co2, by='NHDPlusID') %>%
  #   dplyr::select(c('NHDPlusID', 'lumpedCO2_FCO2_gC_m2_yr', 'lumpedK_FCO2_gC_m2_yr', 'FCO2_gC_m2_yr','k_co2_m_s', 'CO2_ppm')) %>%
  #   dplyr::mutate(lumpedCO2_FCO2_gC_m2_yr = lumpedCO2_FCO2_gC_m2_yr / mean(lumpedCO2_FCO2_gC_m2_yr,na.rm=T),
  #                 lumpedK_FCO2_gC_m2_yr = lumpedK_FCO2_gC_m2_yr / mean(lumpedK_FCO2_gC_m2_yr, na.rm=T),
  #                 FCO2_gC_m2_yr = FCO2_gC_m2_yr / mean(FCO2_gC_m2_yr,na.rm=T),
  #                 k_co2_m_s = k_co2_m_s / mean(k_co2_m_s,na.rm=T),
  #                 CO2_ppm = CO2_ppm / mean(CO2_ppm,na.rm=T)) %>%
  #   tidyr::gather(key=key, value=value, c('lumpedCO2_FCO2_gC_m2_yr', 'lumpedK_FCO2_gC_m2_yr', 'FCO2_gC_m2_yr','k_co2_m_s', 'CO2_ppm'))

  # plotFlux <- ggplot(forPlot, aes(x=value, color=key))+
  #   stat_ecdf(linewidth=3) +
  #   scale_x_log10()+
  #   scale_color_manual(name='', labels=c('CO2','Distributed','kco2 [m/s]','Lumped CO2', 'Lumped kco2'), values=c('#BFCC94', '#060B0F', '#344966', '#B4CDED', '#BBCDB6'))+
  #   xlab('Mean-normalized value')+
  #   ylab('Probability') +
  #   theme(axis.text=element_text(size=20),
  #         axis.title=element_text(size=24,face="bold"),
  #         legend.text = element_text(size=20),
  #         legend.position=c(0.8,0.2))    

  # ggsave(filename=paste0('cache/figures/fluxSupp_',HUC2, '.jpg'),plot=plotFlux,width=20,height=15)


 return(network_fluxes_lumped)

}