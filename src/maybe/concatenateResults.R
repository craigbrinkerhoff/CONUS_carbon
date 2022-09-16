##################
## Collect FCO2 model results
## Craig Brinkerhoff
## Summer 2022
##################

######################
#SET UP PROJECT
######################
.libPaths('/nas/cee-water/cjgleason/r-lib')
library(ProjectTemplate, lib.loc = "/nas/cee-water/cjgleason/r-lib/")
load.project()

theme_set(theme_classic())

############################
## LOAD IN RESULTS
###########################
files <- list.files(path='cache/results', pattern = "^totalNetworkFluxes.csv$", recursive = TRUE) #grab results files
totalResults <- data.frame()
for (i in files){
  df <- read_csv(paste0('cache/results/', i), col_types = cols())
  df$HUC2 <- substr(i,1,nchar(i)-23)
  totalResults <- rbind(totalResults, df)
}
totalResults <- totalResults[,-1]

#get number of total reaches for later
nReaches <- sum(totalResults[totalResults$waterbody=='Combined',]$n)

#save to file
write_csv(totalResults, 'cache/combined_FCO2_Results_seperated.csv')

######################################
## SAME THING FOR MODEL ERRORS
#####################################
files <- list.files(path='cache/calibration_upscaling',  pattern = "^calibration_output.csv$", recursive = TRUE)
totalCalResults <- data.frame()
for (i in files){
  df <- read_csv(paste0('cache/calibration_upscaling/', i), col_types = cols())
  df$HUC2 <- substr(i,1,nchar(i)-23)
  totalCalResults <- rbind(totalCalResults, df)
}
totalCalResults <- totalCalResults[,-1]

#add glorich for perc errors
glorich <- read_csv('data/glorich_data.csv')
totalCalResults <- left_join(totalCalResults, glorich, by='HUC2')
totalCalResults$combo <- totalCalResults$river + totalCalResults$lake
totalCalResults$perc_cal_error <- (1/totalCalResults$fitness) / totalCalResults$combo

write_csv(totalCalResults, 'cache/combined_calibration_results.csv')

#########################################
#CALC SOME CONUS-WIDE SUMMARY STATS (INCLUDING AVG CALIBRATION ERROR ACROSS ALL HUCS) (save later once we can add the CONUS fluxes)
###########################################
avg_cal_perc_err <- mean((1/totalCalResults$fitness) / totalCalResults$combo)

out <- data.frame('n_reaches'=nReaches,
                  'avg_glorich_L+R_ppm'=mean(totalCalResults$combo),
                  'avg_cal_error_L+R_ppm'=mean(1/totalCalResults$fitness),
                  'avg_cal_perc_err'=avg_cal_perc_err)

#GO BACK TO FLUX RESULTS AND ADD CALIBRATION UNCERTAINTY ESTIMATES: #get perc calibration uncertianty to then calc calibration upper and lower bounds (by HUC2)
#joining results dfs
totalCalResults <- select(totalCalResults, c('HUC2', 'fitness'))
fco2Results <- read_csv('cache/combined_FCO2_Results_seperated.csv')
fco2Results <- left_join(fco2Results, glorich, by='HUC2')
fco2Results <- left_join(fco2Results, totalCalResults, by='HUC2')

fco2Results$realHUC2 <- substr(fco2Results$HUC2, 1, 2)
fco2Results <- filter(fco2Results, waterbody=='Combined') %>%
      group_by(realHUC2) %>%
      summarise(sumFCO2_TgC_yr = sum(sumFCO2_TgC_yr),
                sumFCO2_RH_TgC_yr = sum(sumFCO2_RH_TgC_yr),
                sumFCO2_RG_TgC_yr = sum(sumFCO2_RG_TgC_yr),
                river=mean(river), #fake means of identicals to keep this attribute through the summarization
                lake=mean(lake),
                fitness=mean(fitness)) %>%
      select(c('sumFCO2_TgC_yr', 'sumFCO2_RH_TgC_yr', 'sumFCO2_RG_TgC_yr', 'realHUC2', 'river', 'lake', 'fitness'))
colnames(fco2Results) <- c('sumFCO2_TgC_yr', 'sumFCO2_RH_TgC_yr', 'sumFCO2_RG_TgC_yr', 'HUC2', 'river', 'lake', 'fitness')

#calc errors
fco2Results$combo <- fco2Results$river + fco2Results$lake
fco2Results$perc_cal_error <- (1/fco2Results$fitness) / fco2Results$combo
fco2Results$sumFCO2_TgC_yr_lower <- fco2Results$sumFCO2_TgC_yr - (fco2Results$sumFCO2_TgC_yr*fco2Results$perc_cal_error)
fco2Results$sumFCO2_TgC_yr_upper <- fco2Results$sumFCO2_TgC_yr + (fco2Results$sumFCO2_TgC_yr*fco2Results$perc_cal_error)

write_csv(fco2Results, 'cache/combined_FCO2_Results_uncertainty.csv')

#add CONUS fluxes to summary stats (then write to file)----------------------------------------------
out$totalModelFlux_TgC_yr_low <- sum(fco2Results$sumFCO2_TgC_yr_lower) #rivers + lakes
out$totalModelFlux_TgC_yr_high <- sum(fco2Results$sumFCO2_TgC_yr_upper) #rivers + lakes
out$totalModelFlux_TgC_yr <- sum(fco2Results$sumFCO2_TgC_yr) #rivers + lakes
out$Raymond_etal_2013_perHUC_TgC_yr <- sum(fco2Results$sumFCO2_RG_TgC_yr) #rivers + lakes
out$Raymond_etal_2013_perHUC_hydrography_TgC_yr <- sum(fco2Results$sumFCO2_RH_TgC_yr) #rivers + lakes
out$Butman_etal_2015_TgC_yr <- 85 #rivers + lakes
out$Liu_etal_2022_TgC_yr <- 57 #rivers only
out$Raymond_etal_2013_TgC_yr <- 91 #rivers and lakes
out$Butman_2011_TgC_yr <- 97 #rivers
out$Lauerwald_etal_2015 <- 19 #rivers only, missing small stream orders

write_csv(out, 'cache/summaryStats.csv')

###############################
## MAKE SOME SUMMARY FIGURES------------------------
###############################
#fco2 vs raymond 2013-------------------------------
fco2Results$region <- ifelse(fco2Results$HUC2 %in% c('01', '02', '03', '04', '05', '06', '07', '08', '09'), 'East', 'West')
t <- ggplot(fco2Results) +
    geom_pointrange(aes(x=sumFCO2_RG_TgC_yr, ymin=sumFCO2_TgC_yr_lower, ymax=sumFCO2_TgC_yr_upper, y=sumFCO2_TgC_yr, color=region), size=1.25, shape=22) +
    geom_abline(linetype='dashed', size=2, color='darkgrey') +
    scale_color_brewer(palette='Dark2')+
    ylim(0,30)+
    xlim(0,30)+
    xlab('Raymond 2013 FCO2 [Tg-C/yr]')+
    ylab('Our FCO2 [Tg-C/yr]') +
#    scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
#                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
#                  limits=c(10^-0.25,10^1.5))+
#    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
#                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
#                limits=c(10^-0.25,10^1.5))+
    theme(legend.position = "bottom",
          axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"),
          legend.text = element_text(size=17),
          legend.title = element_text(size=17, face='bold'))
ggsave('cache/comparisonPlot_raymond2013.jpg',t, width=8, height=8)

#fco2 vs butman 2015 (when possible)-------------------------------------
butman2015 <- read_csv('data/butman2015.csv')
butman2015$HUC2 <- substr(butman2015$HUC2, 2, 3)
fco2Results <- left_join(fco2Results, butman2015, by='HUC2')
t <- ggplot(fco2Results) +
    geom_pointrange(aes(x=co2_Tg_c_yr, ymin=sumFCO2_TgC_yr_lower, ymax=sumFCO2_TgC_yr_upper, y=sumFCO2_TgC_yr, color=region), size=1.25, shape=22) +
    geom_abline(linetype='dashed', size=2, color='darkgrey') +
    scale_color_brewer(palette='Dark2')+
    xlab('Butman 2015 FCO2 [Tg-C/yr]')+
    ylab('Our FCO2 [Tg-C/yr]') +
    xlim(0,10)+
    ylim(0,10)+
  #  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #                labels = scales::trans_format("log10", scales::math_format(10^.x)),
  #                limits=c(10^-0.25,10^1.5))+
  #  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #                labels = scales::trans_format("log10", scales::math_format(10^.x)),
  #              limits=c(10^-0.25,10^1.5))+
    theme(legend.position = "bottom",
          axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"),
          legend.text = element_text(size=17),
          legend.title = element_text(size=17, face='bold'))
ggsave('cache/comparisonPlot_butman2015.jpg',t, width=8, height=8)

#barplot of conus fluxes-------------------------------------
forPlot <- tidyr::gather(out, key=key, value=value, c('totalModelFlux_TgC_yr',
          'Raymond_etal_2013_perHUC_TgC_yr',
          'Butman_etal_2015_TgC_yr', 'Liu_etal_2022_TgC_yr', 'Raymond_etal_2013_TgC_yr',
          'Butman_2011_TgC_yr', 'Lauerwald_etal_2015'))

#hardcode uncertainties in here------------------------------------------
  #FOR BUTMAN & RAYMOND 2011: CONUS uncertainty reported
  #FOR LAUERWALD 2015: apply global uncertainty to the US number (~ 20% less and 24% more)
  #FOR RAYMOND 2013: apply global uncertainty to the US number (~ 24% less and 38% more)
  #FOR BUTMAN 2015: CONUS uncertainty reported
  #FOR LIU 2022: apply global uncertainty to the US number (~ 10%)
  #FOR OUR RAYMOND 2013 REPRODUCTIONS: just use raymond 2013 reported uncertainty
forPlot$high <- c(forPlot[1,]$totalModelFlux_TgC_yr_high,
                  out$Raymond_etal_2013_TgC_yr + (out$Raymond_etal_2013_TgC_yr * 0.38),
              #    out$Raymond_etal_2013_TgC_yr + (out$Raymond_etal_2013_TgC_yr * 0.38),
                  18.7+109.6,
                  out$Liu_etal_2022_TgC_yr + (out$Liu_etal_2022_TgC_yr*0.10),
                  out$Raymond_etal_2013_TgC_yr + (out$Raymond_etal_2013_TgC_yr * 0.38),
                  out$Butman_2011_TgC_yr + 32,
                  out$Lauerwald_etal_2015 + (out$Lauerwald_etal_2015*0.24))
forPlot$low <- c(forPlot[1,]$totalModelFlux_TgC_yr_low,
                out$Raymond_etal_2013_TgC_yr - (out$Raymond_etal_2013_TgC_yr * 0.24),
              #  out$Raymond_etal_2013_TgC_yr - (out$Raymond_etal_2013_TgC_yr * 0.24),
                14.3+36,
                out$Liu_etal_2022_TgC_yr - (out$Liu_etal_2022_TgC_yr * 0.10),
                out$Raymond_etal_2013_TgC_yr - (out$Raymond_etal_2013_TgC_yr * 0.24),
                out$Butman_2011_TgC_yr - 32,
                out$Lauerwald_etal_2015 - (out$Lauerwald_etal_2015*0.20))

#add river only flag
forPlot$riverOnlyFlag <- c('Rivers+Lakes', 'Rivers+Lakes', 'Rivers+Lakes', 'Rivers', 'Rivers+Lakes', 'Rivers', 'Rivers')

barPlot <- ggplot(forPlot, aes(x=key, y=value, fill=key, color=riverOnlyFlag)) +
    geom_bar(stat='identity', position=position_dodge(), size=2) +
    geom_errorbar(aes(ymin=low, ymax=high), size=1, width=0.6, color='black')+
    scale_color_manual(values=c('darkblue', 'darkred'), name='Model Type', labels=c('Rivers', 'Rivers+Lakes+Reservoirs'))+
    scale_fill_brewer(palette='Set2', name='Study', labels=c('Butman & Raymond 2011',
                                                         'Butman et al. 2015',
                                                         'Lauerwald et al 2015',
                                                         'Liu et al. 2022',
                                                         'Raymond et al. 2013\n(reported)',
                                                         'Raymond et al. 2013\n(calc per HUC)',
                                                         'This Study')) +
    ylab('CONUS CO2 Emissions [Tg-C/yr]')+
    theme(legend.position = "right",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=20),
          axis.title.y=element_text(size=24,face="bold"),
          legend.text = element_text(size=17),
          legend.title = element_text(size=17, face='bold'))
ggsave('cache/CONUS_barplot.jpg', barPlot, width=12, height=8)
