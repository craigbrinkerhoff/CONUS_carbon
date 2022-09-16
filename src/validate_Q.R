###################
## Quick script to verify the erom mean annual flows
## Craig Brinkerhoff
## Summer 2022
#######################


eromValidation <- function(){
  if(!file.exists('cache/eromAssessment.rds')){
    codes_huc02 <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18')
    path_to_data <- 'C:/Users/craig/OneDrive - University of Massachusetts/Ongoing Projects/CONUS_CO2_prep'

    #get USGS stations joined to NHD that meet their QA/QC requirements
    codes <- c(NA)
    for(code_huc2 in codes_huc02){
      code <- list.dirs(paste0(path_to_data, '/HUC2_', code_huc2), full.names = FALSE, recursive = FALSE)
      code <- code[grepl('NHDPLUS_H_', code)] #only keep geodatabase folders
      code <- substr(code, 11, nchar(code)-8)

      codes <- c(codes, code)
    }
    codes <- codes[-1]

    #gather usgs gages already joined to NHD and QA/QCed by USGS
    assessmentDF <- data.frame()
    for (i in codes){
      m <- substr(i, 1,2)
      dsnPath <- paste0(path_to_data, "/HUC2_", m, "/NHDPLUS_H_", i, "_HU4_GDB/NHDPLUS_H_", i, "_HU4_GDB.gdb")
      NHD_HR_EROM_gage <- st_read(dsn = dsnPath, layer = "NHDPlusEROMQAMA") #Quality controlled gauges joined to NHD reaches a priori by USGS
      NHD_HR_EROM <- st_read(dsn = dsnPath, layer = "NHDPlusEROMMA") #mean annual flow table
      NHD_HR_EROM <- filter(NHD_HR_EROM, GageIDMA %in% NHD_HR_EROM_gage$GageID)
      temp <- NHD_HR_EROM %>%
        dplyr::select(c('NHDPlusID', 'QDMA', 'QEMA', 'GageQMA', 'GageIDMA'))

      assessmentDF <- rbind(assessmentDF, temp)
    }

    #get long term mean annual flow, aside from the 1970-2000 requirement of values within the NHD...
    out <- rep(NA, nrow(assessmentDF))
    for(i in 1630:nrow(assessmentDF)){
      if(nchar(assessmentDF[i,]$GageIDMA) < 8){
        print('Canadian')
        out[i] <- -9999 #Canadian gauge. Maybe try to get???
        next
      }

      Qs <- dataRetrieval::readNWISstat(siteNumbers=assessmentDF[i,]$GageIDMA, parameterCd="00060",
                                  statReportType="annual", statType="mean")

      if(nrow(Qs)==0){
        print('nuthin')
        next
      }

      Qs <- filter(Qs, year_nu >= 1970 & year_nu <= 2018)

      #minimum 20 years of data between 1970-2018
      if(nrow(Qs)< 20){
          print('too few data')
          next
        }

      Q_MA_g <- mean(Qs$mean_va)
      out[i] <- Q_MA_g
    }
    assessmentDF$Q_MA_g <- out

    assessmentDF$QDMA <- assessmentDF$QDMA * 0.0283 #cfs to cms
    assessmentDF$QEMA <- assessmentDF$QEMA * 0.0283 #cfs to cms
    assessmentDF$GageQMA <- assessmentDF$GageQMA * 0.0283 #cfs to cms
    assessmentDF$Q_MA_g <- assessmentDF$Q_MA_g * 0.0283 #cfs to cms

    write_rds(assessmentDF, 'cache/eromAssessment.rds')
  }

  assessmentDF <- read_rds('cache/eromAssessment.rds')
  assessmentDF <- tidyr::drop_na(assessmentDF)

  #make figure
  theme_set(theme_classic())

  eromVerification_QDMA <- ggplot(assessmentDF, aes(x=GageQMA, y=QDMA)) +
    geom_abline(linetype='dashed', color='darkgrey', size=2)+
    geom_point(size=3.5, alpha=0.3, color='darkblue')+
    xlab('')+
    ylab('NHD Ungaged Flow')+
    geom_smooth(method='lm', size=1.5, color='black', se=F)+
    annotate('text', label=paste0('r2: ', round(summary(lm(log(QDMA)~log(GageQMA), data=assessmentDF))$r.squared,2)), x=0.01, y=175, size=9)+
    annotate('text', label=paste0('MAE: ', round(Metrics::mae(assessmentDF$QDMA, assessmentDF$GageQMA),1), ' m3/s'), x=0.01, y=950, size=9)+
    annotate('text', label=paste0(nrow(assessmentDF), ' gages'), x=100, y=0.001, size=7, color='darkblue')+
    scale_y_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100,1000, 10000),
                  labels=c('0.0001', '0.001', '0.01', '0.1', '1', '10', '100', '1000', '10000'))+
    scale_x_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000),
                  labels=c('0.0001','0.001', '0.01', '0.1', '1', '10', '100', '1000', '10000'))+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"),
          legend.text = element_text(size=17),
          plot.title = element_text(size = 30, face = "bold"))

  eromVerification_QEMA <- ggplot(assessmentDF, aes(x=GageQMA, y=QEMA)) +
    geom_abline(linetype='dashed', color='darkgrey', size=2)+
    geom_point(size=3.5, alpha=0.3, color='darkblue')+
    xlab('Observed Mean Annual Flow\n(1970-2018)')+
    ylab('NHD Gage Flow')+
    geom_smooth(method='lm', size=1.5, color='black', se=F)+
    annotate('text', label=paste0('r2: ', round(summary(lm(log(QEMA)~log(GageQMA), data=assessmentDF))$r.squared,2)), x=0.01, y=175, size=9)+
    annotate('text', label=paste0('MAE: ', round(Metrics::mae(assessmentDF$QEMA, assessmentDF$GageQMA),1), ' m3/s'), x=0.01, y=950, size=9)+
    annotate('text', label=paste0(nrow(assessmentDF), ' gages'), x=100, y=0.001, size=7, color='darkblue')+
    scale_y_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100,1000, 10000),
                  labels=c('0.0001', '0.001', '0.01', '0.1', '1', '10', '100', '1000', '10000'))+
    scale_x_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000),
                  labels=c('0.0001','0.001', '0.01', '0.1', '1', '10', '100', '1000', '10000'))+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"),
          legend.text = element_text(size=17),
          plot.title = element_text(size = 30, face = "bold"))

  plot_fin <- plot_grid(eromVerification_QDMA, eromVerification_QEMA, ncol=1)

  #write to file
  ggsave('cache/eromVerification.jpg', plot_fin, width=10, height=15)

  return(plot_fin)
}
