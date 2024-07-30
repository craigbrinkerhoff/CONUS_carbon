##############################
## functions to get CO2 source contributions
## Craig Brinkerhoff/Brian Saccardi
## Summer 2023
##############################





#' estimate CO2 emission sources by reach (reach-scale)
#'
#' @name getSourcesByRiver
#'
#' @param hydrography: basin river network
#' @param model: basin model results df
#' @param Cgw_ppm: groundwater CO2 parameter
#' @param calibratedParameters: calibrated parameters for basin
#' @param emergenceQ: emergent Q parameter
#' @param huc4: basin ID
#'
#' @import dplyr
#' @import readr
#'
#' @return df of sources per reach
getSourcesByRiver <- function(hydrography, model, Cgw_ppm, calibratedParameters, emergenceQ,huc4){
  #SETUP----------------------------------------------------------------------
  #handle HUC4s with fake shoreline rivers
  if(huc4 %in% c('0402', '0405', '0406', '0407', '0408', '0411', '0412','0401','0410','0414','0403','0404')) {
    fix <- readr::read_csv(paste0('data/fix_',huc4,'.csv'))

    hydrography <- dplyr::left_join(hydrography, fix, by='NHDPlusID')

    hydrography <- hydrography[hydrography$GL_pass == '1',]
  }

  #match the hydrography used in model runs
  hydrography <- dplyr::filter(hydrography, HydroSeq != 0)

  #wrangle into a usable form
  model <- dplyr::select(model, c('NHDPlusID', 'FCO2_gC_m2_yr', 'CO2_ppm'))
  hydrography <- dplyr::left_join(hydrography, model, by='NHDPlusID')

  #calculate sources in vector to speed it way up
  GWcontrib_TgC_yr <- NA
  Fwccontrib_TgC_yr <- NA
  CO2imported_TgC_yr <- NA
  CO2exported_TgC_yr <- NA
  CO2emitted_TgC_yr <- NA
  Fbenthic_TgC_yr <- NA

  #populate vectors
  #hydrography/hydrology vectors
  Q_vec <- hydrography$Q_cms
  CO2_vec <- hydrography$CO2_ppm
  FCO2_gC_m2_yr_vec <- hydrography$FCO2_gC_m2_yr
  Vol_m3_vec <- ifelse(hydrography$waterbody == 'River', hydrography$W*hydrography$D*hydrography$LengthKM*1000, hydrography$frac_lakeVol_m3)
  SA_m2_vec <- ifelse(hydrography$waterbody == 'River', hydrography$W*hydrography$LengthKM*1000, hydrography$frac_lakeSurfaceArea_m2)
  henry_vec <- hydrography$henry
  k_bz_vec <- hydrography$k_bz
  D_vec <- hydrography$D

  #calibrated parameter vectors
  Fwc_ppm_s <-  ifelse(hydrography$waterbody == 'River', calibratedParameters$Fwc_riv, calibratedParameters$Fwc_lake) #ppm/s
  Cbz_ppm <-   ifelse(hydrography$waterbody == 'River', calibratedParameters$Cbz_riv, calibratedParameters$Cbz_lake) #ppm/s
  Fwc_ppm_s <- ifelse(Fwc_ppm_s < 0,0,Fwc_ppm_s)   #if negative, i.e. mineralization sink, just set to zero for these relative calculations

  #network topology vectors
  StartFlag_vec <- hydrography$StartFlag
  Divergence_vec <- hydrography$Divergence
  ToNode_vec <- hydrography$ToNode
  FromNode_vec <- hydrography$FromNode

  #CALCULATE REACH-SCALE CO2 SOURCE CONTRIBUTIONS: loop through reaches -> find upstream reaches -> calculate CO2 sources
  for (i in 1:nrow(hydrography)) {
    ### headwater/divergent reach flag
    if (StartFlag_vec[i] == 1 || Divergence_vec[i] == 2){ #if headwater or divergent stream, just set to groundwater value and emergent streamflow
      CO2_upstream <- 0
      upstreamQ_cms <- ifelse(emergenceQ >= Q_vec[i], 0, emergenceQ) #see model.R
      lateralQ <- Q_vec[i] -  sum(upstreamQ_cms,na.rm=T)
      Cgw_ppm_here <- Cgw_ppm*2 #see model.R
    }

    #all other rivers
    else{
      upstreamCO2_ppm <- CO2_vec[which(ToNode_vec == FromNode_vec[i])]
      upstreamQ_cms <- Q_vec[which(ToNode_vec == FromNode_vec[i])]
      CO2_upstream <- weighted.mean(upstreamCO2_ppm, upstreamQ_cms, na.rm=T) #ppm

      lateralQ <- Q_vec[i] - sum(upstreamQ_cms,na.rm=T)
      lateralQ <- ifelse(lateralQ < 0, 0, lateralQ) #if losing reach, we don't even have groundwater co2 loading so just make it zero
      Cgw_ppm_here <- Cgw_ppm
    }

    #calculate sources. see Methods for equations
    GWcontrib_TgC_yr[i] <- (Cgw_ppm_here*henry_vec[i]*1e-6)*(1/0.001)*12.01*lateralQ*(60*60*24*365) * 1e-12 #[Tg-C/yr]
    CO2emitted_TgC_yr[i] <- FCO2_gC_m2_yr_vec[i]*SA_m2_vec[i]*1e-12 #[Tg-C/yr]
    CO2exported_TgC_yr[i] <- (CO2_vec[i]*henry_vec[i]*1e-6)*(1/0.001)*12.01*Q_vec[i]*(60*60*24*365) * 1e-12 #[Tg-C/yr]
    CO2imported_TgC_yr[i] <- (CO2_upstream*henry_vec[i]*1e-6)*(1/0.001)*12.01*Q_vec[i]*(60*60*24*365) * 1e-12 #[Tg-C/yr]
    Fwccontrib_TgC_yr[i] <- (Fwc_ppm_s[i]*henry_vec[i]*1e-6)*(1/0.001)*12.01*Vol_m3_vec[i]*(60*60*24*365) * 1e-12 #[Tg-C/yr]
    Fbenthic_TgC_yr[i] <-(k_bz_vec[i]*D_vec[i]*Cbz_ppm[i]*henry_vec[i]*1e-6)*(1/0.001)*12.01*SA_m2_vec[i]*(60*60*24*365) * 1e-12 #[Tg-C/yr]
  }

  #APPEND SOURCE CONTRIBUTIONS BACK ONTO DF-------------------------------------------------
  hydrography$GWcontrib_TgC_yr <- GWcontrib_TgC_yr
  hydrography$Fwccontrib_TgC_yr <- Fwccontrib_TgC_yr
  hydrography$CO2imported_TgC_yr <- CO2imported_TgC_yr
  hydrography$CO2exported_TgC_yr <- CO2exported_TgC_yr
  hydrography$CO2emitted_TgC_yr <- CO2emitted_TgC_yr
  hydrography$Fbenthic_TgC_yr <- Fbenthic_TgC_yr

  #PREP DF-----------------------------------
  out <- dplyr::select(hydrography, c('NHDPlusID', 'StreamOrde','Q_cms','GWcontrib_TgC_yr', 'CO2emitted_TgC_yr', 'CO2exported_TgC_yr', 'CO2imported_TgC_yr', 'Fwccontrib_TgC_yr', 'Fbenthic_TgC_yr'))

  #RETURN DF--------------------------------------------------
  return(out)
}










#' Calculate CO2 emission sources by basin (basin-scale)
#'
#' @name getSourcesByBasin
#'
#' @param model: river network data frame
#' @param results: model results df
#' @param calibratedParameters: calibration results object
#' @param Cgw_ppm: groundwater CO2 parameter [ppm]
#' @param emissions: CO2 emissions for basin
#' @param huc4: basin ID
#' @param path_to_data: path to data repo
#' @param upstreamDF: df of upstream basins and their exported discharges
#'
#' @param import dplyr
#'
#' @return Groundwater Co2 contributions to CO2 evasion [Tg-C/yr]
getSourcesByBasin <- function(model, results, calibratedParameters, Cgw_ppm, emissions, huc4, path_to_data, upstreamDF){
  #HANDLE GREAT LAKES-------------------------------------------------------
  if(huc4 %in% c('0418', '0419', '0424', '0426', '0428')){
    out <- data.frame('huc4'=huc4,
                    'contribGW_TgC_yr'=NA)
  }

  #ALL OTHER RIVERS--------------------------------------------
  else{
    #handle HUC4s with fake shoreline rivers
    remove_Q <- 0 #for those great lakes basins
    if(huc4 %in% c('0402', '0405', '0406', '0407', '0408', '0411', '0412','0401','0410','0414','0403','0404')) {
        fix <- readr::read_csv(paste0('data/fix_',huc4,'.csv'))

        model <- dplyr::left_join(model, fix, by='NHDPlusID')

        model <- model[model$GL_pass == '1',]
      }

    
    #GET UPSTREAM BASIN Q AND CO2-----------------------------------
    upstreamQ <- 0
    upstreamCarbon <- 0
    if(is.na(upstreamDF) == 0){ #skip this in level 0
      upstreamDF <- dplyr::filter(upstreamDF, downstreamBasin == huc4)
      
      upstreamQ <- sum(upstreamDF$exported_Q_cms, na.rm=T) # upstream Q

      henry <- model %>% #get henry coefficients for specific importing CO2 reaches
        dplyr::select(c('FromNode', 'henry'))

      upstreamDF <- dplyr::left_join(upstreamDF, henry, by = c('exported_ToNode'='FromNode'))
 
      #calculate basin-scale CO2 imported and exported
      upstreamDF$exported_CO2_gC_m3 <- ((upstreamDF$exported_CO2_ppm*upstreamDF$henry)/1000000)*(1/0.001)*12.01 #[g-C/m3]
      upstreamDF$exported_CO2_TgC_yr <- upstreamDF$exported_CO2_gC_m3 * 1e-12 *  upstreamDF$exported_Q_cms * (60*60*24*365) #[Tg-C/yr]
      upstreamCarbon <- sum(upstreamDF$exported_CO2_TgC_yr,na.rm=T) #[Tg-C/yr]
    }

    #SETUP BASIN IDs-------------------------------------------------------------
    huc2 <- substr(huc4, 1, 2)
    huc4n <- ifelse(nchar(huc4) > 4, substr(huc4, 1, 4), huc4) #handle 1710a and 1710b

    #CALCULATE BASIN GW CO2 CONTRIBUTION--------------------------------------------
    #add TerminalPa to network to find network outlets
    dsnPath <- paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4n, '_HU4_GDB/NHDPLUS_H_', huc4n, '_HU4_GDB.gdb')
    NHD_HR_VAA <- sf::st_read(dsn = dsnPath, layer = "NHDPlusFlowlineVAA", quiet=TRUE) #additional 'value-added' attributes
    NHD_HR_VAA <- dplyr::select(NHD_HR_VAA, c('NHDPlusID', 'TerminalPa'))

    #get total exported Q
    results <- dplyr::select(results, c('NHDPlusID', 'CO2_ppm'))
    exportedQ <- model %>%
      dplyr::left_join(NHD_HR_VAA, by='NHDPlusID') %>%
      dplyr::left_join(results, by='NHDPlusID') %>%
      dplyr::group_by(TerminalPa) %>%
      dplyr::slice_max(Q_cms) %>%
      dplyr::select(c('Q_cms', 'CO2_ppm', 'henry')) %>%
      dplyr::mutate(exported_CO2_gC_m3 = ((CO2_ppm*henry)/1000000)*(1/0.001)*12.01) %>%
      dplyr::mutate(exported_CO2_TgC_yr = exported_CO2_gC_m3 * 1e-12 *  Q_cms * (60*60*24*365))

    #Get total Cgw contribution to basin using median water temp
    henry <- median(model$henry, na.rm=T)
    Q_fin <- sum(exportedQ$Q_cms) - upstreamQ
    Q_fin <- ifelse(Q_fin < 0, 0, Q_fin) #if it's a losing basin, no net gw introduced. set to 0 water entering network
    GWcontrib <- sum(((Cgw_ppm)*henry*1e-6)*(1/0.001)*12.01*Q_fin*(60*60*24*365)) * 1e-12 #[Tg-C/yr]

    exportedCO2 <- sum(exportedQ$exported_CO2_TgC_yr, na.rm=T)
    
    #CALCULATE BASIN WC RESPIRATION CONTRIBUTION--------------------------------------------------------------------
    model$respiration_gC_m3_s <-  ((ifelse(model$waterbody == 'River', calibratedParameters$Fwc_riv, calibratedParameters$Fwc_lake)*model$henry)/1000000)*(1/0.001)*12.01 #[gC/m3/s]
    model$respiration_TgC_yr <- model$respiration_gC_m3_s * ifelse(model$waterbody == 'River', model$W*model$D*model$LengthKM*1000, model$frac_lakeVol_m3) * (60*60*24*365) * 1e-12 #[TgC/yr]

    model$benthic_gC_m2_s <- (model$k_bz*model$D*(ifelse(model$waterbody == 'River', calibratedParameters$Cbz_riv, calibratedParameters$Cbz_lake)*model$henry)/1000000)*(1/0.001)*12.01 #[gC/m2/s]
    model$benthic_TgC_yr <- model$benthic_gC_m2_s * ifelse(model$waterbody == 'River', model$W*model$LengthKM*1000, model$frac_lakeSurfaceArea_m2) * (60*60*24*365) * 1e-12 #[TgC/yr]


    #BUILD OUTPUT---------------------------------------------------------------------------
    out <- data.frame('huc4'=huc4,
                      'contribGW_TgC_yr'=GWcontrib,
                      'contribUP_TgC_yr'=upstreamCarbon,
                      'exported_TgC_yr'=exportedCO2,
                      'emissions_TgC_yr'=sum(emissions$sumFCO2_TgC_yr, na.rm=T),
                      'contribWC_TgC_yr'=sum(model$respiration_TgC_yr, na.rm=T),
                      'contribBZ_TgC_yr'=sum(model$benthic_TgC_yr, na.rm=T))

    #if negative, i.e. CO2 sink through photosynthesis, just set to zero for these relative calcs
    out$contribWC_TgC_yr <- ifelse(out$contribWC_TgC_yr < 0,0,out$contribWC_TgC_yr)
  }

  #RETURN BASIN-SCALE SOURCE CONTRIBUTIONS-----------------------------------------------
  return(out)
}










#' Summarise reach-scale source contributions by stream order
#'
#' @name getResultsByOrder
#'
#' @param nhd_df: basin routing table + results
#' 
#' @import dplyr
#'
#' @return df of reach-scale source contributions by stream order (summary stats)
getResultsByOrder <- function(nhd_df) {

  #SUMMARISE BY ORDER----------------------
  results_by_order <- nhd_df %>%
    dplyr::mutate(StreamOrde = ifelse(StreamOrde >= 7, '7+', as.character(StreamOrde))) %>% #so few basins populate these stream orders, we just lump them all together as representative of 'big rivers'
    dplyr::group_by(StreamOrde) %>%
    dplyr::summarise(percGW_reach_median = median(GWcontrib_TgC_yr/(GWcontrib_TgC_yr+Fbenthic_TgC_yr+Fwccontrib_TgC_yr), na.rm=T),
                     percBZ_reach_median = median(Fbenthic_TgC_yr/(GWcontrib_TgC_yr+Fbenthic_TgC_yr+Fwccontrib_TgC_yr), na.rm=T),
                     percWC_reach_median = median(Fwccontrib_TgC_yr/(GWcontrib_TgC_yr+Fbenthic_TgC_yr+Fwccontrib_TgC_yr), na.rm=T))

  return(results_by_order)
}