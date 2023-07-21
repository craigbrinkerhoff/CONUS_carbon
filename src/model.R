########################
#CO2 reactive transport model
#Craig Brinkerhoff
#Fall 2022
#########################



#' Reach-scale CO2 Reactive Transport Model for rivers/lakes/reservoirs
#'
#' @name CO2_MODEL
#'
#' @param hrt: reach residence time [s]
#' @param Q: mass-conserved river discharge [m3/s]
#' @param C_gw: constant groundwater CO2 input [ppm]
#' @param C_atm: constant atmospheric CO2 value [ppm]
#' @param C_bz: Benthic zone CO2 value [ppm]
#' @param k_co2: gas exchange rate constant [1/s]
#' @param k_bz: benthic/sediment/hyporheic gas exchange rate constant [1/s]
#' @param F_wc: water column respiration rate [ppm/s]
#' @param fromNode: NHD fromNode ID for routing
#' @param divergence: flag for whether reach is divergent (minor flowpath) [0/1]
#' @param startflag: flag for whether reach is headwater/1st order [0/1]
#' @param waterbody: flag for whether reach is a river or lake/reservoir [0/1]
#' @param toNode_vec:  vector of to nodes for each reach (for routing)
#' @param CO2_vec:  vector of CO2 values for each reach (for routing)
#' @param Q_vec:  vector of Q values for each reach (for routing)
#' @param emergenceQ: emergent discharge for small streams
#'
#' @return returns CO2 concentration [ppm]
CO2_MODEL <- function(hrt, Q, C_gw, C_atm, C_bz, k_co2, k_bz, F_wc, fromNode, divergence, startflag, waterbody, toNode_vec, CO2_vec, Q_vec, emergenceQ) {
  ### HEADWATER/DIVERGENT REACHES
  if (startflag == 1 || divergence == 2){ #if headwater or divergent stream, just set to groundwater value and emergent streamflow
    CO2_last <- C_gw
    lastQ <- ifelse(emergenceQ >= Q, 0, emergenceQ) #only use emergenceQ if headwater is 'big enough'
  }

  ### ALL OTHER STREAMS
  else {
    upstreamIndexes <- which(toNode_vec == fromNode) #get directly upstream reaches
    CO2_last <- weighted.mean(CO2_vec[upstreamIndexes], Q_vec[upstreamIndexes], na.rm=TRUE) #weight inflowing CO2 by river discharge
    lastQ <- sum(Q_vec[upstreamIndexes], na.rm=TRUE) #sum all inflowing water because mass conservation

    #account for emerging streams not identified with startFlag. These are rivers whose upstream reach has Q=0 but isn't flagged by NHD
    if (all(is.na(Q_vec[upstreamIndexes]))==1) {
      CO2_last <- C_gw
      lastQ <- emergenceQ
      lastQ <- ifelse(emergenceQ >= Q, 0, emergenceQ)
    }
  }

  #FOR LOSING STREAMS, WE SET LASTQ TO ZERO SO THIS TERM CANCELS OUT.
  lastQ <- ifelse(Q < lastQ, Q, lastQ)

  ### CALCULATE CO2 CONCENTRATION BASED ON BACKWARDS DIFFERENCE METHOD OF TRANSPORT MODEL.
  if(waterbody == 'GreatLake'){ #skip Great Lakes, assume lake goes to atmospheric
    CO2 <- C_atm
    return(CO2)
  } else{ #normal rivers, lakes, reservoirs
      CO2 <- (CO2_last + ((Q-lastQ)/Q)*C_gw + hrt*k_co2*C_atm + hrt*k_bz*C_bz + hrt*F_wc)/(1 + hrt*k_co2 + (Q-lastQ)/Q)
      return(CO2)
  }
}
