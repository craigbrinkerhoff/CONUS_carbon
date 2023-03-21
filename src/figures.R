## Make some paper figures
## Craig Brinkerhoff
## Winter 2023



eromValidationFig <- function(USGS_data, nhdGages){
	theme_set(theme_classic())

	qma <- USGS_data
  	qma <- dplyr::select(qma, c('gageID','Q_MA'))
  	assessmentDF <- dplyr::left_join(nhdGages, qma, by=c('GageIDMA' = 'gageID')) %>%
  		drop_na()

  	eromVerification_QBMA <- ggplot(assessmentDF, aes(x=Q_MA, y=QBMA)) +
    	geom_abline(linetype='dashed', color='darkgrey', size=2)+
    	geom_point(size=4, color='#4281A4', alpha=0.25)+
    	xlab('Observed Mean Annual Flow')+
    	ylab('USGS Discharge Model')+
    	geom_smooth(method='lm', size=1.5, color='black', se=F)+
    	annotate('text', label=expr(r^2: ~ !!round(summary(lm(log(QBMA)~log(Q_MA), data=assessmentDF))$r.squared,2)), x=0.01, y=175, size=9)+
    	annotate('text', label=expr(MAE: ~ !!round(Metrics::mae(assessmentDF$QBMA, assessmentDF$Q_MA),1) ~ frac(m^3, s)), x=0.01, y=1000, size=9)+
    	annotate('text', label=paste0('n = ', nrow(assessmentDF), ' streams'), x=100, y=0.001, size=7, color='black')+
    	scale_y_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100,1000, 10000),
        	          labels=c('0.0001', '0.001', '0.01', '0.1', '1', '10', '100', '1000', '10000'))+
    	scale_x_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000),
        	          labels=c('0.0001','0.001', '0.01', '0.1', '1', '10', '100', '1000', '10000'))+
    	theme(axis.text=element_text(size=20),
        	  axis.title=element_text(size=24,face="bold"),
          	legend.text = element_text(size=20),
          	legend.position='bottom',
          	plot.title = element_text(size = 30, face = "bold"))

  ggsave('cache/figures/eromValidation.jpg', eromVerification_QBMA, width=10, height=10)
  return(paste('see cache/figures/eromValidation.jpg'))
}





calibrationFigures <- function(combined_calib){
	theme_set(theme_classic())

	combined_fitness <- sapply(combined_calib, function(x){return(x$fitness)})
	combined_fitness <- (1/combined_fitness) #convert to ppm per 'median river/lake/reservoir' reach

	forPlot <- data.frame('huc4'=substr(names(combined_fitness), 22,26),
						  'fitness'=combined_fitness)
	plot <- ggplot(forPlot, aes(x=fitness)) +
		stat_ecdf(size=2, color='black') +
		geom_vline(xintercept = median(forPlot$fitness, na.rm=T), color='darkgrey', linetype='dotted', size=1.5) + 
    	geom_hline(yintercept = 0.50, color='darkgrey', linetype='dotted', size=1.5) +
    	annotate('text', label=paste0('median error across\nbasins: ', round(median(forPlot$fitness, na.rm=T),0), ' ppm'), x=0.01, y=0.85, size=8)+
		scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
   					  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
		annotation_logticks(sides='b')+
		xlab('Calibration error per basin [ppm]') +
		ylab('Probability') +
		theme(axis.title = element_text(size=20),
          axis.text = element_text(family="Futura-Medium", size=18))+ #axis text settings
    	theme(legend.position = 'none') #legend position settings

	ggsave('cache/figures/calib/calibrationSummary.jpg', plot, width=8, height=8)


	ticker <- 1
	a <- 1
	b <- 20
	while(ticker <= 9){
		c <- 1
		plot <- list()
	  	while(a <= b){
			huc4 <- substr(names(combined_calib[a]),22,26)
			basin <- combined_calib[[a]]

			if(is.na(basin$plot)){
				a <- a + 1 #skip NAs (great lakes)
				b <- b + 1
				next
			}

			plot[[c]] <- basin$plot +
				ggtitle(huc4)+
				scale_y_log10()+
				ylab('')+
				xlab('') +
				theme(plot.title = element_text(size=30),
          	  		axis.text = element_text(family="Futura-Medium", size=18)) #axis text settings
			
			a <- a + 1
			c <- c + 1
		}

		#legend
		legend <- cowplot::get_legend(
    	plot[[1]] +
      		labs(tag = '')+
      		theme(legend.position = "bottom",
            	legend.text = element_text(size=24),
            	legend.title = element_text(size=26,
                                        face='bold'),
            	legend.box="vertical",
            	legend.margin=margin(),
            	legend.spacing.x = unit(2, 'cm')) +
      		guides(color = guide_legend(override.aes = list(size=10))))

		#x-axis
		x_axis <- cowplot::get_plot_component(ggplot() +
  	  		labs(x = "Generation") +
  	  		theme(axis.title = element_text(size=30)), "xlab-b")

		#y-axis
		y_axis <- cowplot::get_plot_component(ggplot() +
  	  		labs(y = "Calibration Error [ppm]") +
  	  		theme(axis.title = element_text(size=30)), "ylab-l")


    	if(ticker != 9){
    		#plot design
  			design <- "
   	 		WAABBCCDD
   	 		WAABBCCDD
   	 		WEEFFGGHH
   	 		WEEFFGGHH
   	 		WIIJJKKLL
   	 		WIIJJKKLL
   	 		WMMNNOOPP
   	 		WMMNNOOPP
   	 		WQQRRSSTT
   	 		WQQRRSSTT
   		 	WUUUUUUUU
	   	 	WVVVVVVVV
    		"

  			comboPlot <- patchwork::wrap_plots(A=plot[[1]] + theme(legend.position='none'), B=plot[[2]] + theme(legend.position='none'), C=plot[[3]] + theme(legend.position='none'), D=plot[[4]] + theme(legend.position='none'),
        	                	             E=plot[[5]] + theme(legend.position='none'), F=plot[[6]] + theme(legend.position='none'), G=plot[[7]] + theme(legend.position='none'), H=plot[[8]] + theme(legend.position='none'),
            	        	                 I=plot[[9]] + theme(legend.position='none'), J=plot[[10]] + theme(legend.position='none'), K=plot[[11]] + theme(legend.position='none'), L=plot[[12]] + theme(legend.position='none'),
                		                     M=plot[[13]] + theme(legend.position='none'), N=plot[[14]] + theme(legend.position='none'), O=plot[[15]] + theme(legend.position='none'), P=plot[[16]] + theme(legend.position='none'),
                	    	                 Q=plot[[17]] + theme(legend.position='none'), R=plot[[18]] + theme(legend.position='none'), S=plot[[19]] + theme(legend.position='none'), T=plot[[20]] + theme(legend.position='none'),
            	            	             U=x_axis, V=legend, W=y_axis, design=design)

  			ggsave(paste0('cache/figures/calib/comboPlot_', ticker, '.jpg'), comboPlot, width=20, height=20)

  			ticker <- ticker + 1
  			b <- b + 20
    	}
    	else {
    		#plot design
  			design <- "
   	 		WAABBCCDD
   	 		WAABBCCDD
   	 		WEEFFGGHH
   	 		WEEFFGGHH
   		 	WUUUUUUUU
	   	 	WVVVVVVVV
    		"

  			comboPlot <- patchwork::wrap_plots(A=plot[[1]] + theme(legend.position='none'), B=plot[[2]] + theme(legend.position='none'), C=plot[[3]] + theme(legend.position='none'), D=plot[[4]] + theme(legend.position='none'),
        	                	             E=plot[[5]] + theme(legend.position='none'), F=plot[[6]] + theme(legend.position='none'), G=plot[[7]] + theme(legend.position='none'), H=plot[[8]] + theme(legend.position='none'),
            	            	             U=x_axis, V=legend, W=y_axis, design=design)

  			ggsave(paste0('cache/figures/calib/comboPlot_', ticker, '.jpg'), comboPlot, width=20, height=20)

  			ticker <- ticker + 1
  			b <- b + 20
  		}
	}
  
  return('all calibration results written to file at ~/cache/figures/calib')
}