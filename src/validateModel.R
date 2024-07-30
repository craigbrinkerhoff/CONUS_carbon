##########################
## Craig Brinkerhoff
## Functions to pseudo-validate CO2 results
## Summer 2024
##########################



#' build nhd discharge validation figure
#'
#' @name grabModelResults
#'
#' @param path_to_data: path to data repo
#' @param final: final model dataframe
#' @param huc4_id: huc4 basin ID code
#' @param bufferDist: buffer size for finding NHD reaches near the glorich sites
#'
#' @import dplyr
#' @import readr
#' @import sf
#'
#' @return glorich data joined to the nhd, with qaqc attributes for later filtering
grabModelResults <- function(final, huc4){
    huc2 <- substr(huc4, 1, 2)

    #skip great lakes
    if(huc4 %in% c('0418', '0419', '0424', '0426', '0428')) {
        out <- data.frame('huc4'=NA,
                    'waterbody'=NA,
                    'pCO2_calib_ppm'=NA,
                    'pCO2_model_ppm'=NA)
        return(out)
        }

    #get calibration numbers from glorich
    calibrationValues <- readr::read_csv('data/HUC4_calibration.csv') %>%
        dplyr::filter(HUC4 == huc4)

    #get model median pCO2s
    df <- final %>%
        dplyr::group_by(waterbody) %>%
        dplyr::summarise(median_pCO2_ppm = median(CO2_ppm, na.rm=T))

    #return results
    out <- data.frame('huc4'=huc4,
                    'waterbody'=c('river', 'lake/reservoir'),
                    'pCO2_calib_ppm'=c(calibrationValues[1,]$River,calibrationValues[1,]$Lake),
                    'pCO2_model_ppm'=c(df[df$waterbody == 'River',]$median_pCO2_ppm,df[df$waterbody == 'Lake/Reservoir',]$median_pCO2_ppm))
    return(out)
}