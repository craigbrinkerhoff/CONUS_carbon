##########################
## Validate NHD discharge model
## Craig Brinkerhoff
## Winter 2023
##########################


#' Return the set of USGS gages that are joined to the NHD-HR a priori (therefore these gaugs meet the USGS QA/QC requirements)
#'
#' @name getNHDGages
#'
#' @param path_to_data: data repo path
#' @param codes_huc02: HUC2 basins to get gages for
#'
#' @import dplyr
#' @import sf
#'
#' @return df of USGS gages on the NHD with flows converted to metric
getNHDGages <- function(path_to_data, codes_huc02){
  #GET USGS STATIONS JOINED A PRORI TO THE NHD-HR (i.e. IDs already matched to NHD-HR)--------------------------------------------------
  codes <- c(NA)
  for(code_huc2 in codes_huc02){
    code <- list.dirs(paste0(path_to_data, '/HUC2_', code_huc2), full.names = FALSE, recursive = FALSE)
    code <- code[grepl('NHDPLUS_H_', code)] #only keep geodatabase folders
    code <- substr(code, 11, nchar(code)-8)

    codes <- c(codes, code)
  }
  codes <- codes[-1]

  #LOOP THROUGH BASINS, GRABBING ALL OF THE GUAGE IDs---------------------------------------
  assessmentDF <- data.frame()
  for (i in codes){
    m <- substr(i, 1,2)
    dsnPath <- paste0(path_to_data, "/HUC2_", m, "/NHDPLUS_H_", i, "_HU4_GDB/NHDPLUS_H_", i, "_HU4_GDB.gdb")
    NHD_HR_EROM_gage <- st_read(dsn = dsnPath, layer = "NHDPlusEROMQAMA") #Quality controlled gauges joined to NHD-HR reaches a priori by USGS
    NHD_HR_EROM <- sf::st_read(dsn = dsnPath, layer = "NHDPlusEROMMA") #mean annual flow table
    NHD_HR_EROM <- dplyr::filter(NHD_HR_EROM, GageIDMA %in% NHD_HR_EROM_gage$GageID)
    temp <- NHD_HR_EROM %>%
      dplyr::select(c('NHDPlusID', 'QBMA', 'QEMA', 'GageQMA', 'GageIDMA'))

    assessmentDF <- rbind(assessmentDF, temp)
  }

  #CONVERT TO METRIC------------------------------------
  assessmentDF$QBMA <- assessmentDF$QBMA * 0.0283 #cfs to cms
  assessmentDF$QEMA <- assessmentDF$QEMA * 0.0283 #cfs to cms
  assessmentDF$GageQMA <- assessmentDF$GageQMA * 0.0283 #cfs to cms

  #RETURN GAUGE LOOKUP TABLE-------------------------------------
  return(assessmentDF)
}






#' Gather streamflow data at gauges and calculate mean annual flow from observed record
#'
#' @name getGageData
#'
#' @param path_to_data: path to data working directory
#' @param nhdGages: list of USGS streamgauges joined to NHD-HR
#' @param codes_huc02: HUC2 basins to get gage data for
#'
#' @import dataRetrieval
#' @import readr
#' @import dplyr
#'
#' @return df of USGS gauges + long term mean annual flow data
getGageData <- function(path_to_data, nhdGages, codes_huc02){
  #LOOP THROUGH HUC2s, QUERY GAUGE RECORDS, CALCULATE MEAN ANNUAL FLOW-----------------------------------------
  for(m in codes_huc02){
    #NOTE::::: will be longer than the final sites b/c some of them don't have 20 yrs of data  within the bounds.
        #This function only finds gages that intersect our time domain, but not necessarily 20 yrs of data within the domain. This is handeled later.
        #Further, some gages have errors in data or are missing data and we throw them out later.
    
    #If the site hasn't been run yet
    if(!file.exists(paste0('cache/gageDataTemp/siteNos_', m, '.rds'))){
      #get usgs gages by
      sites_full <- dataRetrieval::whatNWISdata(huc=m,
                                 parameterCd ='00060',
                                 service='dv',
                                 startDate = '1970-10-01', #water year
                                 endDate = '2018-09-30')

      write_rds(sites_full, paste0('cache/gageDataTemp/siteNos_', m, '.rds'))
      sites <- unique(sites_full$site_no)
      sites <- sites[which(sites %in% nhdGages$GageIDMA)] #filter for the gages joined to NHD-HR a priori
    }
    #if the API query has already been done, just load in the flow record
    else{
      sites_full <- read_rds(paste0('cache/gageDataTemp/siteNos_', m, '.rds'))
      sites <- unique(sites_full$site_no)
      sites <- sites[which(sites %in% nhdGages$GageIDMA)] #filter for only gages joined to NHD a priori
    }

    #some zones don't have gages joined to NHD-HR after QA/QC
    if(length(sites)==0){next}

    ##########CALCUALTE MEAN ANNUAL FLOW--------------------------------------------------
    results <- data.frame()
    k <- 1
    #only run if region has not been done yet
    if(!file.exists(paste0('cache/gageDataTemp/trainingData_', m, '.rds'))){
      #loop through gauges within region and query the API
      for(i in sites){
        #GRAB GAUGE DATA
        gageQ <- tryCatch(dataRetrieval::readNWISstat(siteNumbers = i, #check if site mets our date requirements
                                       parameterCd = '00060', #discharge
                                       startDate = '1970-10-01',
                                       endDate = '2018-09-30'),
                          error = function(m){ #if site doesn't have data from 1970-2018, skip
                            print('no site')
                            next})

        if(nrow(gageQ) == 0){next} #sometimes these are empty

        if(gageQ[1,]$count_nu <= 20){next}#minimum 20 years of measurements
        if(nrow(gageQ)!= 366){next} #need data for every day of the year

        gageQ$Q_cms <- gageQ$mean_va*0.0283 #cfs to cms
        gageQ$Q_cms <- round(gageQ$Q_cms, 3) #round to 1 decimal to account for low-flow errors

        #Actually calculate mean annual flow
        gageQ <- select(gageQ, c('site_no', 'Q_cms', 'month_nu')) %>%
          mutate(Q_MA = mean(gageQ$Q_cms, na.rm=T),
                 date=1:nrow(gageQ),
                 month=month_nu) #cfs to cms.

        if(gageQ$Q_MA == 0){next}

        #prep result
        temp <- data.frame('gageID'=gageQ[1,]$site_no,
                           'Q_MA'=gageQ[1,]$Q_MA)

        #append result to validation df
        results <- rbind(results, temp)
      }
    
    #PREP REGIONAL VALIDATION OUTPUT-------------------------------------------
    results <- select(results, c('gageID', 'Q_MA')) %>%
      distinct(.keep_all = TRUE)

    write_rds(results, paste0('cache/gageDataTemp/trainingData_', m, '.rds')) #temp files in case process gets interrupted or something

    Sys.sleep(60) #wait 1 minute to USGS doesn't get angry :)
   }
  }

  #CONCATENATE ALL REGIONAL VALIDATION DFs INTO A CONUS VALIDATION DF (janky but works)-----------------------------------------------------
  results_all <- data.frame()
  for(i in codes_huc02){
    temp_d <- tryCatch(read_rds(paste0('cache/gageDataTemp/trainingData_', i, '.rds')),error=function(k){'none'})
    if(temp_d == 'none'){
      next
    } else{
      temp_d$huc2 <- i
      results_all <- rbind(results_all, temp_d)
    }
  }

  out <- results_all

  #RETURN CONUS VALIDATION DF--------------------------------------------------
  return(out)
}