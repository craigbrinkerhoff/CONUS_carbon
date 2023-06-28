#################
## Craig Brinkerhoff
## Summer 2023
## functions to get CO2 source %s per river reach
################





#' parse CO2 emission sources per reach (using network topology)
#'
#' @name getSourcesByRiver
#'
#' @param hydrography: basin river network
#' @param model: basin model results df
#' @param Cgw_ppm: groundwater CO2 parameter
#' @param calibratedParameters: calibration results object for x basin
#' @param emergenceQ: emergent Q parameter
#'
#' @import dplyr
#' @import sf
#'
#' @return df of sources per reach
getSourcesByRiver <- function(hydrography, model, Cgw_ppm, calibratedParameters, emergenceQ,huc4){
  #great lakes fix
  if(huc4 %in% c('0402', '0405', '0406', '0407', '0408', '0411', '0412','0401','0410','0414','0403','0404')) {
    fix <- readr::read_csv(paste0('data/fix_',huc4,'.csv'))

    hydrography <- dplyr::left_join(hydrography, fix, by='NHDPlusID')

    hydrography <- hydrography[hydrography$GL_pass == '1',]
  }

  hydrography <- dplyr::filter(hydrography, HydroSeq != 0)

  model <- dplyr::select(model, c('NHDPlusID', 'FCO2_gC_m2_yr', 'CO2_ppm'))
  hydrography <- dplyr::left_join(hydrography, model, by='NHDPlusID')

  GWcontrib_TgC_yr <- NA
  Fwccontrib_TgC_yr <- NA
  CO2imported_TgC_yr <- NA
  CO2exported_TgC_yr <- NA
  CO2emitted_TgC_yr <- NA
  Fbenthic_TgC_yr <- NA

  #hydrography/hydrology
  Q_vec <- hydrography$Q_cms
  CO2_vec <- hydrography$CO2_ppm
  FCO2_gC_m2_yr_vec <- hydrography$FCO2_gC_m2_yr
  Vol_m3_vec <- ifelse(hydrography$waterbody == 'River', hydrography$W*hydrography$D*hydrography$LengthKM*1000, hydrography$frac_lakeVol_m3)
  SA_m2_vec <- ifelse(hydrography$waterbody == 'River', hydrography$W*hydrography$LengthKM*1000, hydrography$frac_lakeSurfaceArea_m2)
  henry_vec <- hydrography$henry
  k_bz_vec <- hydrography$k_bz
  D_vec <- hydrography$D

  Fwc_ppm_s <-  ifelse(hydrography$waterbody == 'River', calibratedParameters$Fwc_riv, calibratedParameters$Fwc_lake) #ppm/s
  Cbz_ppm <-   ifelse(hydrography$waterbody == 'River', calibratedParameters$Cbz_riv, calibratedParameters$Cbz_lake) #ppm/s

  #if negative, i.e. mineralization sink, just set to zero for these realtive calcs
  Fwc_ppm_s <- ifelse(Fwc_ppm_s < 0,0,Fwc_ppm_s)

  StartFlag_vec <- hydrography$StartFlag
  Divergence_vec <- hydrography$Divergence
  ToNode_vec <- hydrography$ToNode
  FromNode_vec <- hydrography$FromNode

  for (i in 1:nrow(hydrography)) {
    #get imported CO2 from upstream
    ### HEADWATER/DIVERGENT REACH FLAG
    if (StartFlag_vec[i] == 1 || Divergence_vec[i] == 2){ #if headwater or divergent stream, just set to groundwater value and emergent streamflow
      CO2_upstream <- 0
      upstreamQ_cms <- ifelse(emergenceQ >= Q_vec[i], 0, emergenceQ) #see model.R
      lateralQ <- Q_vec[i] -  sum(upstreamQ_cms,na.rm=T)
      Cgw_ppm_here <- Cgw_ppm*2 #see model.R
    }
    else{
      upstreamCO2_ppm <- CO2_vec[which(ToNode_vec == FromNode_vec[i])]
      upstreamQ_cms <- Q_vec[which(ToNode_vec == FromNode_vec[i])]
      CO2_upstream <- weighted.mean(upstreamCO2_ppm, upstreamQ_cms, na.rm=T) #ppm

      lateralQ <- Q_vec[i] - sum(upstreamQ_cms,na.rm=T)
      lateralQ <- ifelse(lateralQ < 0, 0, lateralQ) #if losing reach, we don't even have groundwater co2 loading so just make it zero
      Cgw_ppm_here <- Cgw_ppm
    }

    GWcontrib_TgC_yr[i] <- (Cgw_ppm_here*henry_vec[i]*1e-6)*(1/0.001)*12.01*lateralQ*(60*60*24*365) * 1e-12#Tg-C/yr
    CO2emitted_TgC_yr[i] <- FCO2_gC_m2_yr_vec[i]*SA_m2_vec[i]*1e-12#Tg-C/yr
    CO2exported_TgC_yr[i] <- (CO2_vec[i]*henry_vec[i]*1e-6)*(1/0.001)*12.01*Q_vec[i]*(60*60*24*365) * 1e-12#Tg-C/yr
    CO2imported_TgC_yr[i] <- (CO2_upstream*henry_vec[i]*1e-6)*(1/0.001)*12.01*Q_vec[i]*(60*60*24*365) * 1e-12#Tg-C/yr
    Fwccontrib_TgC_yr[i] <- (Fwc_ppm_s[i]*henry_vec[i]*1e-6)*(1/0.001)*12.01*Vol_m3_vec[i]*(60*60*24*365) * 1e-12#Tg-C/yr
    Fbenthic_TgC_yr[i] <-(k_bz_vec[i]*D_vec[i]*Cbz_ppm[i]*henry_vec[i]*1e-6)*(1/0.001)*12.01*SA_m2_vec[i]*(60*60*24*365) * 1e-12#Tg-C/yr
  }


  hydrography$GWcontrib_TgC_yr <- GWcontrib_TgC_yr
  hydrography$Fwccontrib_TgC_yr <- Fwccontrib_TgC_yr
  hydrography$CO2imported_TgC_yr <- CO2imported_TgC_yr
  hydrography$CO2exported_TgC_yr <- CO2exported_TgC_yr
  hydrography$CO2emitted_TgC_yr <- CO2emitted_TgC_yr
  hydrography$Fbenthic_TgC_yr <- Fbenthic_TgC_yr


  out <- dplyr::select(hydrography, c('NHDPlusID', 'StreamOrde','Q_cms','GWcontrib_TgC_yr', 'CO2emitted_TgC_yr', 'CO2exported_TgC_yr', 'CO2imported_TgC_yr', 'Fwccontrib_TgC_yr', 'Fbenthic_TgC_yr'))
  return(out)
}










#' Calculate CO2 emission sources by basin (basin scale)
#'
#' @name getSources
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

  else{

  #HANDLE HUC4s WITH FAKE SHORELINES-----------------------------------------
    remove_Q <- 0 #for those great lakes basins
  if(huc4 %in% c('0402', '0405', '0406', '0407', '0408', '0411', '0412','0401','0410','0414','0403','0404')) {
      fix <- readr::read_csv(paste0('data/fix_',huc4,'.csv'))

      model <- dplyr::left_join(model, fix, by='NHDPlusID')

      model <- model[model$GL_pass == '1',]
      
     # remove <- model[model$GL_pass == '0',]
     # remove_Q <- max(remove$Q_cms, na.rm=T)
    }

    
    #GET UPSTREAM BASIN Q (AND LIKEWISE GET IMPORTED CO2)-----------------------------------
    upstreamQ <- 0
    upstreamCarbon <- 0
    if(is.na(upstreamDF) == 0){ #to skip doing this in level 0
      # get discharge contributed by basins upstream
      upstreamDF <- dplyr::filter(upstreamDF, downstreamBasin == huc4)
      upstreamQ <- sum(upstreamDF$exported_Q_cms, na.rm=T)

      #get henry coefficients for specific importing CO2 reaches
      henry <- model %>%
        dplyr::select(c('FromNode', 'henry'))

      upstreamDF <- dplyr::left_join(upstreamDF, henry, by = c('exported_ToNode'='FromNode'))
 
      upstreamDF$exported_CO2_gC_m3 <- ((upstreamDF$exported_CO2_ppm*upstreamDF$henry)/1000000)*(1/0.001)*12.01 #g-C/m3
      upstreamDF$exported_CO2_TgC_yr <- upstreamDF$exported_CO2_gC_m3 * 1e-12 *  upstreamDF$exported_Q_cms * (60*60*24*365) #Tg-C/yr
      upstreamCarbon <- sum(upstreamDF$exported_CO2_TgC_yr,na.rm=T)
    }

    #Get % GW and %CO2 exported downstream---------------------------------------------------------------------------
    #read in TerminalPa b/c i forgot to add it to hydrography table...
    huc2 <- substr(huc4, 1, 2)
    huc4n <- ifelse(nchar(huc4) > 4, substr(huc4, 1, 4), huc4) #handle 1710a and 1710b (left joiin below takes care of this)

    dsnPath <- paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4n, '_HU4_GDB/NHDPLUS_H_', huc4n, '_HU4_GDB.gdb')
    NHD_HR_VAA <- sf::st_read(dsn = dsnPath, layer = "NHDPlusFlowlineVAA", quiet=TRUE) #additional 'value-added' attributes
    NHD_HR_VAA <- dplyr::select(NHD_HR_VAA, c('NHDPlusID', 'TerminalPa'))

    #get total exported Q to apply Cgw to
    results <- dplyr::select(results, c('NHDPlusID', 'CO2_ppm'))
    exportedQ <- model %>%
      dplyr::left_join(NHD_HR_VAA, by='NHDPlusID') %>%
      dplyr::left_join(results, by='NHDPlusID') %>%
      dplyr::group_by(TerminalPa) %>%
      dplyr::slice_max(Q_cms) %>%
      dplyr::select(c('Q_cms', 'CO2_ppm', 'henry')) %>%
      dplyr::mutate(exported_CO2_gC_m3 = ((CO2_ppm*henry)/1000000)*(1/0.001)*12.01) %>%
      dplyr::mutate(exported_CO2_TgC_yr = exported_CO2_gC_m3 * 1e-12 *  Q_cms * (60*60*24*365))

    #Get total Cgw contribution to basin
    henry <- median(model$henry, na.rm=T)
    Q_fin <- sum(exportedQ$Q_cms) - upstreamQ
    Q_fin <- ifelse(Q_fin < 0, 0, Q_fin) #if losing basins/t no net gw introduced, just set to 0 water entering network
    GWcontrib <- sum(((Cgw_ppm)*henry*1e-6)*(1/0.001)*12.01*Q_fin*(60*60*24*365)) * 1e-12#Tg-C/yr

    exportedCO2 <- sum(exportedQ$exported_CO2_TgC_yr, na.rm=T)
    
    #GET WC RESPIRATION--------------------------------------------------------------------
    model$respiration_gC_m3_s <-  ((ifelse(model$waterbody == 'River', calibratedParameters$Fwc_riv, calibratedParameters$Fwc_lake)*model$henry)/1000000)*(1/0.001)*12.01 #gC/m3/s
    model$respiration_TgC_yr <- model$respiration_gC_m3_s * ifelse(model$waterbody == 'River', model$W*model$D*model$LengthKM*1000, model$frac_lakeVol_m3) * (60*60*24*365) * 1e-12 #TgC/yr

    model$benthic_gC_m2_s <- (model$k_bz*model$D*(ifelse(model$waterbody == 'River', calibratedParameters$Cbz_riv, calibratedParameters$Cbz_lake)*model$henry)/1000000)*(1/0.001)*12.01 #gC/m2/s
    model$benthic_TgC_yr <- model$benthic_gC_m2_s * ifelse(model$waterbody == 'River', model$W*model$LengthKM*1000, model$frac_lakeSurfaceArea_m2) * (60*60*24*365) * 1e-12 #TgC/yr


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

    #get benthic contribution via mass balance (can't actually solve at basin scale because every river is parameterized differently)
    out$contribBZ_TgC_yr_massBal <- (out$emissions_TgC_yr + out$exported_TgC_yr) - (out$contribGW_TgC_yr + out$contribWC_TgC_yr + out$contribUP_TgC_yr)
  }
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
    dplyr::summarise(percGW_reach_median = median(GWcontrib_TgC_yr/(GWcontrib_TgC_yr+Fbenthic_TgC_yr+Fwccontrib_TgC_yr), na.rm=T),
                     percBZ_reach_median = median(Fbenthic_TgC_yr/(GWcontrib_TgC_yr+Fbenthic_TgC_yr+Fwccontrib_TgC_yr), na.rm=T),
                     percWC_reach_median = median(Fwccontrib_TgC_yr/(GWcontrib_TgC_yr+Fbenthic_TgC_yr+Fwccontrib_TgC_yr), na.rm=T))

  return(results_by_order)
}