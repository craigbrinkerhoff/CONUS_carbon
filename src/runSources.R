#################
## Craig Brinkerhoff
## Summer 2023
## functions to get CO2 source %s per river reach
################



getSourcesByRiver <- function(hydrography, model, Cgw_ppm, calibratedParameters, emergenceQ){
  hydrography <- dplyr::filter(hydrography, HydroSeq != 0)

  model <- dplyr::select(model, c('NHDPlusID', 'FCO2_gC_m2_yr', 'CO2_ppm'))
  hydrography <- dplyr::left_join(hydrography, model, by='NHDPlusID')

  hydrography$GWcontrib_TgC_yr <- NA
  hydrography$Fwccontrib_TgC_yr <- NA
  hydrography$CO2imported_TgC_yr <- NA
  hydrography$CO2exported_TgC_yr <- NA
  hydrography$CO2emitted_TgC_yr <- NA
  hydrography$Fbenthic_TgC_yr <- NA
  for (i in 1:nrow(hydrography)) {
    Fwc_ppm_s <-  ifelse(hydrography[i,]$waterbody == 'River', calibratedParameters$Fwc_riv, calibratedParameters$Fwc_lake) #ppm/s
    Cbz_ppm <-   ifelse(hydrography[i,]$waterbody == 'River', calibratedParameters$Cbz_riv, calibratedParameters$Cbz_lake) #ppm/s

    #if negative, i.e. mineralization sink, just set to zero for these realtive calcs
    Fwc_ppm_s <- ifelse(Fwc_ppm_s < 0,0,Fwc_ppm_s)

    #get imported CO2 from upstream
    ### HEADWATER/DIVERGENT REACH FLAG
    if (hydrography[i,]$StartFlag == 1 || hydrography[i,]$Divergence == 2){ #if headwater or divergent stream, just set to groundwater value and emergent streamflow
      CO2_upstream <- 0
      upstreamQ_cms <- ifelse(emergenceQ >= hydrography[i,]$Q_cms, 0, emergenceQ) #only use emergenceQ if headwater is 'big enough'

      lateralQ <- hydrography[i,]$Q_cms -  sum(upstreamQ_cms,na.rm=T)
    }
    else{
      upstreamCO2_ppm <- hydrography[which(hydrography$ToNode == hydrography[i,]$FromNode),]$CO2_ppm
      upstreamQ_cms <- hydrography[which(hydrography$ToNode == hydrography[i,]$FromNode),]$Q_cms
      CO2_upstream <- weighted.mean(upstreamCO2_ppm, upstreamQ_cms, na.rm=T) #ppm

      lateralQ <- hydrography[i,]$Q_cms - sum(upstreamQ_cms,na.rm=T)
      lateralQ <- ifelse(lateralQ < 0, 0, lateralQ) #if losing reach, we don't even have groundwater co2 loading so just make it zero 
    }
    
    Vol_m3 <- ifelse(hydrography[i,]$waterbody == 'River', hydrography[i,]$W*hydrography[i,]$D*hydrography[i,]$LengthKM*1000, hydrography[i,]$frac_lakeVol_m3)
    SA_m2 <- ifelse(hydrography[i,]$waterbody == 'River', hydrography[i,]$W*hydrography[i,]$LengthKM*1000, hydrography[i,]$frac_lakeSurfaceArea_m2)

    hydrography[i,]$GWcontrib_TgC_yr <- ((Cgw_ppm)*hydrography[i,]$henry*1e-6)*(1/0.001)*12.01*lateralQ*(60*60*24*365) * 1e-12#Tg-C/yr
    hydrography[i,]$CO2emitted_TgC_yr <- hydrography[i,]$FCO2_gC_m2_yr*SA_m2*1e-12#Tg-C/yr
    hydrography[i,]$CO2exported_TgC_yr <- (hydrography[i,]$CO2_ppm*hydrography[i,]$henry*1e-6)*(1/0.001)*12.01*hydrography[i,]$Q_cms*(60*60*24*365) * 1e-12#Tg-C/yr
    hydrography[i,]$CO2imported_TgC_yr <- (CO2_upstream*hydrography[i,]$henry*1e-6)*(1/0.001)*12.01*hydrography[i,]$Q_cms*(60*60*24*365) * 1e-12#Tg-C/yr
    hydrography[i,]$Fwccontrib_TgC_yr <- (Fwc_ppm_s*hydrography[i,]$henry*1e-6)*(1/0.001)*12.01*Vol_m3*(60*60*24*365) * 1e-12#Tg-C/yr

    hydrography[i,]$Fbenthic_TgC_yr <-((hydrography[i,]$k_bz*hydrography[i,]$D*(Cbz_ppm))*hydrography[i,]$henry*1e-6)*(1/0.001)*12.01*SA_m2*(60*60*24*365) * 1e-12#Tg-C/yr
    #hydrography[i,]$Fbenthic_TgC_yr <- hydrography[i,]$CO2emitted_TgC_yr - (hydrography[i,]$Fwccontrib_TgC_yr + hydrography[i,]$GWcontrib_TgC_yr) #solved by mass balance Tg-C/yr
  }


  out <- dplyr::select(hydrography, c('NHDPlusID', 'StreamOrde','Q_cms','GWcontrib_TgC_yr', 'CO2emitted_TgC_yr', 'CO2exported_TgC_yr', 'CO2imported_TgC_yr', 'Fwccontrib_TgC_yr', 'Fbenthic_TgC_yr'))
  return(out)
}











#' Calculates ephemeral contributions per stream order per basin (Q and drainage area)
#'
#' @name getResultsByOrder
#'
#' @param nhd_df: basin routing table + results
#' 
#' @import dplyr
#'
#' @return ephemeral fraction summary stats by order
getResultsByOrder <- function(nhd_df) {
  #percents by order----------------------
  results_by_order <- nhd_df %>%
    dplyr::group_by(StreamOrde) %>%
    dplyr::summarise(percGW_reach_median = median(GWcontrib_TgC_yr/(CO2emitted_TgC_yr)),
                     percBZ_reach_median = median(Fbenthic_TgC_yr/(CO2emitted_TgC_yr)),
                     percWC_reach_median = median(Fwccontrib_TgC_yr/(CO2emitted_TgC_yr)))

  return(results_by_order)
}







#' Calculates ephemeral contributions per stream order per basin (Q and drainage area)
#'
#' @name getResultsByOrder
#'
#' @param nhd_df: basin routing table + results
#' 
#' @import dplyr
#'
#' @return ephemeral fraction summary stats by order
getResultsBySpecials <- function(nhd_df){
  results_order_under_5 <- nhd_df %>%
    dplyr::filter(StreamOrde <=5) %>%
    dplyr::summarise(percGW_reach_median = median(GWcontrib_TgC_yr/(CO2emitted_TgC_yr)),
                     percBZ_reach_median = median(Fbenthic_TgC_yr/(CO2emitted_TgC_yr)),
                     percWC_reach_median = median(Fwccontrib_TgC_yr/(CO2emitted_TgC_yr))) %>%
    dplyr::mutate(type='maxorder5')


  results_order_under_001 <- nhd_df %>%
    dplyr::filter(Q_cms <=0.001) %>%
    dplyr::summarise(percGW_reach_median = median(GWcontrib_TgC_yr/(CO2emitted_TgC_yr)),
                     percBZ_reach_median = median(Fbenthic_TgC_yr/(CO2emitted_TgC_yr)),
                     percWC_reach_median = median(Fwccontrib_TgC_yr/(CO2emitted_TgC_yr))) %>%
    dplyr::mutate(type='maxQ001')


  results_order_under_100 <- nhd_df %>%
    dplyr::filter(Q_cms <=100) %>%
    dplyr::summarise(percGW_reach_median = median(GWcontrib_TgC_yr/(CO2emitted_TgC_yr)),
                     percBZ_reach_median = median(Fbenthic_TgC_yr/(CO2emitted_TgC_yr)),
                     percWC_reach_median = median(Fwccontrib_TgC_yr/(CO2emitted_TgC_yr))) %>%
    dplyr::mutate(type='maxQ100')


  out <- rbind(results_order_under_5, results_order_under_001, results_order_under_100)

  return(out)
}