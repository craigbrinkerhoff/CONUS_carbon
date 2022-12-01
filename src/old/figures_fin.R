##########################
#Creator: Craig Brinkerhoff
#Date: Winter 2022
#Description: Generate figures from calibrated CO2 model output for a given model domain
##########################

print('Building figures...')

#READ IN RESULTS (and glorich data)
results <- read.csv(paste0('cache/results/', HUC2, '/results_', HUC2, '.csv'))
if(HUC2 == '04'){
  results <- dplyr::filter(results, waterbody != 'GreatLake') #remove from flux calculations
}
riverCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$river #[ppm]
lakeCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$lake #[ppm]

#GET FCO2 FOR OUR MODEL AND RAYMOND+HYDROGRAPHY MODEL
results$FCO2 <- results$k_co2_m_s*(((results$CO2_ppm - C_atmosphere)*results$henry)/1000000)*(1/0.001)*12.01*(60*60*24*365)  #[g-C/m2yr]
results$FCO2_RH <- mapply(FCO2_MODEL_RAYMOND, results$Q_m3_s, results$Slope, results$Water_temp_c, results$lakeSA_m2, results$waterbody, results$henry, riverCO2, lakeCO2) #[g-C/m2/yr]

#GET SUMMARY STATS
results$Q_bin <- ifelse(results$Q_m3_s < 0.001, '< 0.001',
                    ifelse(results$Q_m3_s < 0.01, '< 0.01',
                          ifelse(results$Q_m3_s < 0.1, '< 0.1',
                              ifelse(results$Q_m3_s < 1, '< 1',
                                  ifelse(results$Q_m3_s < 10, '< 10',
                                      ifelse(results$Q_m3_s < 100, '< 100', '100+'))))))
results$Q_bin <- factor(results$Q_bin, levels=c('< 0.001', '< 0.01', '< 0.1', '< 1', '< 10', '< 100', '100+'), ordered=TRUE)

statsPCO2 <- dplyr::group_by(results, Q_bin) %>%
  dplyr::summarise(meanCO2 = mean(CO2_ppm, na.rm=T),
            medianCO2 = median(CO2_ppm, na.rm=T),
            maxCO2 = max(CO2_ppm, na.rm=T),
            minCO2 = min(CO2_ppm, na.rm=T))
write.csv(statsPCO2, paste0('cache/results/', HUC2, '/statsPCO2.csv'))

statsFCO2 <- dplyr::group_by(results, Q_bin) %>%
  dplyr::summarise(meanFCO2 = mean(FCO2, na.rm=T),
            medianFCO2 = median(FCO2, na.rm=T),
            maxFCO2 = max(FCO2, na.rm=T),
            minFCO2 = min(FCO2, na.rm=T))
write.csv(statsFCO2, paste0('cache/results/', HUC2, '/statsFCO2.csv'))

resultsPCO2 <- left_join(results, statsPCO2, by='Q_bin')
resultsFCO2 <- left_join(results, statsFCO2, by='Q_bin')

###############
## GET NETWORK FLUXES (ACCOUNTING FOR SURFACE AREA)--------------------
################
#US
resultsFCO2$FCO2_total <- ifelse(resultsFCO2$waterbody == 'River',
                                 resultsFCO2$FCO2 * resultsFCO2$W_m * resultsFCO2$LengthKM * 1000, #river g-C/yr
                                 resultsFCO2$FCO2 * resultsFCO2$lakeSA_m2) #lake/reservoir g-C/yr

#RAYMOND+HYDROGRAPHY
resultsFCO2$W_m_RH <- ifelse(resultsFCO2$waterbody == 'River', mean(c(exp(2.56+0.423*log(resultsFCO2$Q_m3_s)), exp(1.86+0.51*log(resultsFCO2$Q_m3_s)))), NA)
resultsFCO2$FCO2_RH_total <- ifelse(resultsFCO2$waterbody == 'River',
                                   resultsFCO2$FCO2_RH*resultsFCO2$W_m_RH*resultsFCO2$LengthKM*1000,
                                   resultsFCO2$FCO2_RH*resultsFCO2$lakeSA_m2)

#SUMMARIZE NETWORK RESULTS
networkOutput <- dplyr::group_by(resultsFCO2, waterbody) %>%
  dplyr::summarise(sumFCO2_TgC_yr = sum(FCO2_total, na.rm=T)*1e-12,
                   sumFCO2_RH_TgC_yr = sum(FCO2_RH_total, na.rm=T)*1e-12,
                   n = n())
RG <- read.csv(paste0('cache/results/', HUC2, '/totalNetworkFluxes_RG.csv'))
networkOutput <- left_join(networkOutput, RG, by='waterbody') %>% select('waterbody', 'sumFCO2_TgC_yr', 'sumFCO2_RH_TgC_yr', 'sumFCO2_RG_TgC_yr', 'n')
networkOutput <- rbind(networkOutput, c(NA, sum(resultsFCO2$FCO2_total, na.rm=T)*1e-12, sum(resultsFCO2$FCO2_RH_total, na.rm=T)*1e-12, sum(RG$sumFCO2_RG_TgC_yr, na.rm=T), nrow(resultsFCO2)))#, sum(c(0.53181, 2.5118))))
networkOutput$waterbody <- as.character(networkOutput$waterbody)
networkOutput[3,]$waterbody <- 'Combined'
write.csv(networkOutput, paste0('cache/results/', HUC2, '/totalNetworkFluxes.csv'))

######################
##PLOTS---------------------------
######################
#BARPLOT OF TOTAL NETWORK FLUXES
forPlot <- gather(networkOutput, key=key, value=value, c('sumFCO2_TgC_yr', 'sumFCO2_RH_TgC_yr', 'sumFCO2_RG_TgC_yr'))
barPlot <- ggplot(forPlot, aes(x=waterbody, y=value, fill=key)) +
  geom_bar(stat='identity', position=position_dodge(),color='black', size=1.2) +
  xlab('')+
  ylab('CO2 Flux [tG-C/yr]') +
  scale_fill_brewer(palette = 'Dark2', name='', labels=c('Raymond 2013', 'Raymond 2013 + \nOur hydrography', 'Our Calibrated\nModel')) + #'Raymond 2013\n+ Transport',
  theme(legend.position = "bottom",
        axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        legend.text = element_text(size=17),
        legend.title = element_text(size=17, face='bold'))
ggsave(paste0('cache/results/', HUC2, '/barPlot.jpg'), barPlot, width=8, height=8)

#PCO2 PLOT
boxes_pCO2 <- ggplot(resultsPCO2) +
  geom_boxplot(aes(x=factor(Q_bin), y=CO2_ppm, fill=factor(Q_bin), group=factor(Q_bin)), size=2, color='black') +
  geom_hline(yintercept=riverCO2, linetype='dashed', size=2, color='darkblue')+
  geom_hline(yintercept=400, linetype='dashed', size=2, color='black')+
  #geom_point(aes(x=factor(Q_bin), y=meanCO2), size=12, color='darkred')+
  #geom_line(aes(x=Q_bin, y=meanCO2), size=3, color='darkred')+
  scale_fill_discrete_sequential(palette = 'Emrld', name='Stream Order')+
  xlab('') +
  ylab('Model CO2 [ppm]')+
  theme(legend.position = 'none') +
 theme(axis.text=element_text(size=23),
    axis.title=element_text(size=28,face="bold"),
    legend.text = element_text(size=17),
    legend.title = element_text(size=17, face='bold'),
    legend.position = 'none')

ggsave(paste0('cache/results/', HUC2, '/pCO2.jpg'), boxes_pCO2, width=15, height=15)

#LAKES VS RIVERS
cdfs_FCO2 <- ggplot(resultsFCO2, aes(x=FCO2_total, color=waterbody)) +
  stat_ecdf(size=3) +
  scale_color_brewer(palette='Dark2') +
  ylab('Probability of Occurence')+
  xlab('FCO2 [g-C/yr]')+
  scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(axis.text.x=element_text(size=19),
      axis.title.x=element_text(size=24,face="bold"),
      legend.text = element_text(size=17),
      legend.title = element_text(size=17, face='bold'),
      legend.position = 'bottom')
ggsave(paste0('cache/results/', HUC2, '/cdfs.jpg'), cdfs_FCO2, width=8, height=8)

#ROCHER-ROS ETAL 2019 PLOT
co2_k600_plot <- ggplot(resultsPCO2, aes(x=k600_m_s, y=CO2_ppm)) +
  geom_point() +
  xlab('k600 [m/s]') +
  ylab('Calibrated pCO2 [ppm]') +
  theme(axis.text=element_text(size=19),
      axis.title=element_text(size=24,face="bold"),
      legend.text = element_text(size=17),
      legend.title = element_text(size=17, face='bold'),
      legend.position = 'bottom')
ggsave(paste0('cache/results/', HUC2, '/co2_k600.jpg'), co2_k600_plot, width=8, height=8)
