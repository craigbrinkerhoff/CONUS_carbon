##########################
#Runs calibrated CO2 transport model
#Craig Brinkerhoff
#Fall 2022
##########################



#' Runs final calibrated version of model
#'
#' @name runModel
#'
#' @param hydrography: river network to run model on
#' @param calibrationResults: list of calibrated parameters
#' @param Cgw: groundwater CO2 parameter [ppm]
#' @param Catm: atmospheric CO2 constant [ppm]
#' @param huc4: basin id
#' @param emergenceQ: emergent Q constant [m3/s]
#' @param upstreamDF: upstream reaches for basin to basin routing
#'
#' @import dplyr
#'
#' @return final routed CO2 results for basin
runModel <- function(hydrography, calibrationResults, Cgw, Catm, huc4, emergenceQ, upstreamDF) {
  #set to atmospheric for great lakes so that exported CO2 is atmosheric
  if(huc4 %in% c('0418', '0419', '0424', '0426', '0428')){
    hydrography$CO2_ppm <- Catm

    hydrography$HRT_s <- hydrography$HRT #[s]
    hydrography$k_co2_m_s <- hydrography$k_co2 * hydrography$D #[m/s]
    hydrography$k600_m_s <- hydrography$k_co2_m_s * (600/(1911-118.11*hydrography$temp_c+3.453*hydrography$temp_c^2-0.0413*hydrography$temp_c^3))^-0.5 #[m/s] convert back to a k600 just to have
    hydrography$k_bz_m_s <- hydrography$k_bz * hydrography$D #[m/s]
    hydrography$W_m <- hydrography$W #[m]
    hydrography$V_m_s <- hydrography$V #[m/s]
    hydrography$Q_m3_s <- hydrography$Q_cms #[m3/s]
    hydrography$lakeSA_m2 <- hydrography$frac_lakeSurfaceArea_m2 #[m2]
    hydrography$Water_temp_c <- hydrography$temp_c #[c]
    hydrography$FCO2_gC_m2_yr <- hydrography$k_co2_m_s*(((hydrography$CO2_ppm - Catm)*hydrography$henry)/1000000)*(1/0.001)*12.01*(60*60*24*365)  #[g-C/m2yr]

    out <- dplyr::select(hydrography, c('NHDPlusID', 'WBArea_Permanent_Identifier', 'ToNode', 'FromNode','conus', 'CO2_ppm', 'StreamOrde', 'k_co2_m_s', 'k600_m_s', 'LengthKM', 'Q_m3_s', 'W_m', 'lakeSA_m2', 'waterbody', 'V_m_s', 'Slope', 'Water_temp_c', 'henry', 'FCO2_gC_m2_yr'))  #write to file
  }

  #all other basins
  else{
    #sort rivers from upstream to downstream
    hydrography <- dplyr::filter(hydrography, HydroSeq != 0)
    hydrography <- hydrography[order(-hydrography$HydroSeq),] #sort descending

    #vectorize hydrography to help with speed
    StartFlag_vec <- as.vector(hydrography$StartFlag)
    Divergence_vec <- as.vector(hydrography$Divergence)
    fromNode_vec <- as.vector(hydrography$FromNode)
    toNode_vec <- as.vector(hydrography$ToNode)
    waterbody_vec <- as.vector(hydrography$waterbody)
    hrt_vec <- as.vector(hydrography$HRT) #[s]
    slope_vec <- as.vector(hydrography$Slope)
    width_vec <- as.vector(hydrography$W) #[m]
    vel_vec <- as.vector(hydrography$V) #[m/s]
    kco2_vec <- as.vector(hydrography$k_co2) #[1/s]
    kbz_vec <- as.vector(hydrography$k_bz) #[1/s]
    Q_vec <- as.vector(hydrography$Q_cms) #[m3/s]
    lakeArea_vec <- as.vector(hydrography$frac_lakeSurfaceArea_m2) #[m2]
    temp_vec <- as.vector(hydrography$temp_c) #[C]

    #results vector
    CO2_vec <- rep(NA, length(Q_vec)) #[ppm]

    #Append required upstream IDs and CO2s from previous HUC4s to this dataset. Because the indexing is relative, we can just add them to the end of the vectors and then remove later when routing is done
    if(is.na(upstreamDF) == 0){
      upstreamDF <- dplyr::filter(upstreamDF, downstreamBasin == huc4)

      toNode_vec <- c(toNode_vec, upstreamDF$exported_ToNode)
      CO2_vec <- c(CO2_vec, upstreamDF$exported_CO2_ppm)
      Q_vec <- c(Q_vec, upstreamDF$exported_Q_cms)
    }

    #set up calibrated model parameters
    Cgroundwater_vec <- rep(Cgw, length(Q_vec))
    bz_vec <- as.vector(ifelse(waterbody_vec == 'Lake/Reservoir', calibrationResults$Cbz_lake, calibrationResults$Cbz_riv)) #[ppm]
    Fwc_vec <- as.vector(ifelse(waterbody_vec == 'River', calibrationResults$Fwc_riv, calibrationResults$Fwc_lake)) #[ppm/s]

    #run vectorized model
    for (i in 1:nrow(hydrography)) {
      CO2_vec[i] <- CO2_MODEL(hrt_vec[i], Q_vec[i], Cgroundwater_vec[i], Catm, bz_vec[i], kco2_vec[i], kbz_vec[i], Fwc_vec[i], fromNode_vec[i], Divergence_vec[i], StartFlag_vec[i], waterbody_vec[i], toNode_vec, CO2_vec, Q_vec, emergenceQ)
    }

    #remove upstream CO2s added to the vector temporarily
    if(is.na(upstreamDF) == 0){
      CO2_vec <- CO2_vec[1:nrow(hydrography)]
    }

    #add results to river network
    hydrography$CO2_ppm <- CO2_vec #[ppm]

    #prep output
    hydrography$HRT_s <- hydrography$HRT #[s]
    hydrography$k_co2_m_s <- hydrography$k_co2 * hydrography$D #[m/s]
    hydrography$k600_m_s <- hydrography$k_co2_m_s * (600/(1911-118.11*hydrography$temp_c+3.453*hydrography$temp_c^2-0.0413*hydrography$temp_c^3))^-0.5 #[m/s] convert back to a k600 just to have
    hydrography$k_bz_m_s <- hydrography$k_bz * hydrography$D #[m/s]
    hydrography$W_m <- hydrography$W #[m]
    hydrography$V_m_s <- hydrography$V #[m/s]
    hydrography$Q_m3_s <- hydrography$Q_cms #[m3/s]
    hydrography$lakeSA_m2 <- hydrography$frac_lakeSurfaceArea_m2 #[m2]
    hydrography$Water_temp_c <- hydrography$temp_c #[c]
    hydrography$FCO2_gC_m2_yr <- hydrography$k_co2_m_s*(((hydrography$CO2_ppm - Catm)*hydrography$henry)/1000000)*(1/0.001)*12.01*(60*60*24*365)  #[g-C/m2yr]

    out <- dplyr::select(hydrography, c('NHDPlusID', 'WBArea_Permanent_Identifier', 'ToNode', 'FromNode','conus', 'CO2_ppm', 'StreamOrde', 'k_co2_m_s', 'k600_m_s', 'LengthKM', 'Q_m3_s', 'W_m', 'lakeSA_m2', 'waterbody', 'V_m_s', 'Slope', 'Water_temp_c', 'henry', 'FCO2_gC_m2_yr'))  #write to file
  }
  return(out)
}
