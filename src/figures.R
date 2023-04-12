## Make some paper figures
## Craig Brinkerhoff
## Spring 2023



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
	while(ticker <= 9){ #really janky way to iterate through the correct basins....
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






compareLumpedFig <- function(path_to_data, results) {
  	theme_set(theme_classic())
  
  	# CONUS boundary
  	states <- sf::st_read(paste0(path_to_data, '/other_shapefiles/cb_2018_us_state_5m.shp'))
  	states <- dplyr::filter(states, !(NAME %in% c('Alaska',
                                                'American Samoa',
                                                'Commonwealth of the Northern Mariana Islands',
                                                'Guam',
                                                'District of Columbia',
                                                'Puerto Rico',
                                                'United States Virgin Islands',
                                                'Hawaii'))) #remove non CONUS states/territories
  	states <- sf::st_union(states)
  
  	#round results
  	results$perc_diff <- round(((results$sumFCO2_TgC_yr - results$sumFCO2_lumped_TgC_yr)/results$sumFCO2_lumped_TgC_yr )*100,0) #setup percent

  	#PERC DIFF MAP-------------------------------------------------
  	map <- ggplot(results) +
    	geom_sf(aes(fill=perc_diff), #actual map
        	    color='black',
            	size=0.5) +
    	geom_sf(data=states, #conus boundary
        	    color='black',
            	size=1.25,
            	alpha=0)+
    	labs(tag='A')+
    	scale_fill_gradientn(name='% difference in carbon\nemissions from lumped',
        	                 colors=c('#e63946', 'white', '#1d3557'),
            	             limits=c(-400,400),
                	         guide = guide_colorbar(direction = "horizontal",
                    	                            title.position = "bottom"))+
    	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
    	theme(legend.position = c(0.15, 0.1),
        	  legend.key.size = unit(2, 'cm'))+ #legend position settings
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
        	  legend.title = element_text(face = "bold", size = 18),
          	  legend.text = element_text(family = "Futura-Medium", size = 18),
          	  plot.tag = element_text(size=26,
            	                      face='bold'))+
    	xlab('')+
    	ylab('')


  	#BARPLOTS TOTAL--------------------------------------------
    #sum across the U.S.
    forPlot <- data.frame('Distributed'=sum(results$sumFCO2_TgC_yr, na.rm=T),
    					  'SemiDistributed'=sum(results$sumFCO2_semiDist_TgC_yr, na.rm=T),
    					  'SemiDistributed2'=sum(results$sumFCO2_semiDist2_TgC_yr, na.rm=T),    					  
    					  'Lumped'=sum(results$sumFCO2_lumped_TgC_yr, na.rm=T),
    					  'Lumped_sigma'=sum(results$cal_uncertainty, na.rm=T))

  	forPlot <- tidyr::gather(forPlot, key=key, value=value, c('Distributed', 'Lumped', 'SemiDistributed', 'SemiDistributed2'))
  	forPlot[forPlot$key != 'Distributed',]$Lumped_sigma <- NA #don't apply to upscaling model
  	forPlot$key <- factor(forPlot$key, levels=c('Lumped', 'SemiDistributed', 'SemiDistributed2','Distributed'))

  	bars <- ggplot(forPlot, aes(x=key, y=value, fill=key)) +
  		geom_col(size=1.5, color='black', size=1.5) +
  		geom_errorbar(aes(ymin=value-Lumped_sigma, ymax=value+Lumped_sigma), width=.15, size=1.75) +
  		scale_fill_manual(values=c('#f4f1de', '#e07a5f', '#3d405b', '#81b29a'))+
  		#scale_fill_manual(values=c('#5f0f40','#9a031e', '#fb8b24')) +
  		labs(tag='B')+
  		xlab('')+
  		ylab('CO2 Flux [Tg-C/yr]') +
  		theme(legend.position='none') +
  		theme(axis.title = element_text(size=26, face='bold', color='black'), axis.text = element_text(size=20, color='black'))+ #axis text settings
    	theme(plot.tag = element_text(size=26,face='bold'))


  	#COMBO PLOT---------------------------------------------
  	design <- "
  		AAAAAAA
  		AAAAAAA
  		AAAAAAA
  		AAAAAAA
  		AAAAAAA
  		CBBBBBD
  		CBBBBBD
  	"

  	comboPlot <- patchwork::wrap_plots(A=map, B=bars, design=design)

  	ggsave('cache/figures/modelsCompare.jpg', comboPlot, width=20, height=20)
  	return('see cache/figures/modelsCompare.jpg')
}







conceptualPlot <- function(final_model_0102, final_model_0202, glorich_data){
	theme_set(theme_classic())

	#################################
	##BASIN 0102-------------------------------------------------------------------------------------
	##################################
	final_model <- dplyr::select(final_model_0102, c('NHDPlusID', 'StreamOrde', 'waterbody', 'Water_temp_c','Q_m3_s','W_m','lakeSA_m2', 'k600_m_s','k_co2_m_s', 'CO2_ppm', 'FCO2_gC_m2_yr'))
  	network <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_01/NHDPLUS_H_0102_HU4_GDB/NHDPLUS_H_0102_HU4_GDB.gdb'), layer='NHDFlowline')
  	network <- dplyr::left_join(network, final_model, 'NHDPlusID') %>%
  		sf::st_zm() %>%
  		dplyr::filter(is.na(waterbody)==0)
  	basin <- sf::st_read(paste0(path_to_data, '/HUC2_01/WBD_01_HU2_Shape/Shape/WBDHU4.shp')) %>%
  		dplyr::filter(huc4 == '0102')


  	##CALCULATE LUMPED MODEL FOR 0102----------------------------------------------------------------------------------------
  	riverCO2 <- glorich_data[glorich_data$HUC2 == '01',]$river #[ppm]
  	lakeCO2 <- glorich_data[glorich_data$HUC2 == '01',]$lake #[ppm]
  	temp_c <- mean(network$Water_temp_c, na.rm=T)
  	henry <- henry_func(temp_c)
  	sc <- 1911-118.11*temp_c+3.453*temp_c^2-0.0413*temp_c^3 #Raymond2012/Wanninkof 1991

  	rivers_by_order <- dplyr::group_by(network[network$waterbody == 'River',], StreamOrde) %>%
    	dplyr::summarise(kco2_m_s = mean(k_co2_m_s),
    					 k600_m_s = mean(k600_m_s),
        	             SA_m2 = sum(LengthKM*W_m*1000))

  	#get basin k co2
  	rivers_k_co2_lumped_m_s <- weighted.mean(rivers_by_order$kco2_m_s, rivers_by_order$SA_m2, na.rm=T) #normalize by surface area for each order
  	rivers_k_600_lumped_m_s <- weighted.mean(rivers_by_order$k600_m_s, rivers_by_order$SA_m2, na.rm=T) #normalize by surface area for each order

  	#calculate a basin fco2
  	rivers_FCO2_lumped <- ((riverCO2-400)*henry*1e-6)*rivers_k_co2_lumped_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]
  	rivers_FCO2_lumped_total <- rivers_FCO2_lumped*sum(rivers_by_order$SA_m2, na.rm=T) #[g-C/yr]

  	lakes_by_area <- dplyr::filter(network, waterbody == 'Lake/Reservoir') %>% #build combined lakes dataset for lake scaling
    	dplyr::group_by(WBArea_Permanent_Identifier) %>%
    	dplyr::summarise(area_skm = sum(lakeSA_m2, na.rm=T)*1e-6) %>%
    	dplyr::mutate(k600_m_dy = ifelse(area_skm < 0.1, 0.54, #[m/dy] Read etal 2012
        	                ifelse(area_skm < 1, 1.16,
            	              ifelse(area_skm < 3.16, 1.32,
                	            ifelse(area_skm < 10, 1.9, 1.9))))) %>%
    	dplyr::mutate(kco2_m_dy = k600_m_dy/(600/sc)^-0.5) %>% #m/dy
    	dplyr::mutate(kco2_m_s = (kco2_m_dy)/(24*60*60))  #m/s

  	#calculate a basin fco2
  	lakes_by_area$lakes_FCO2_lumped <- ((lakeCO2-400)*henry*1e-6)*lakes_by_area$kco2_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]
  	lakes_FCO2_lumped_total <- sum(lakes_by_area$lakes_FCO2_lumped*lakes_by_area$area_skm*1e6, na.rm=T) #[g-C/yr]

  	#calculate basin carbon emissions flux
  	lumpedFCO2_gC_C_m2_yr <- rivers_FCO2_lumped + sum(lakes_by_area$lakes_FCO2_lumped, na.rm=T)

  	basin$lumpedFCO2_gC_C_m2_yr <- lumpedFCO2_gC_C_m2_yr


  	##CALCULATE SEMI-DISTRIBUTED MODEL FOR 0102----------------------------------------------------------------------------------------
  	network$semi_FCO2_gC_m2_yr <- ifelse(network$waterbody == 'River', ((riverCO2-400)*henry*1e-6)*network$k_co2_m_s*(1/0.001)*12.01*(60*60*24*365), ((lakeCO2-400)*henry*1e-6)*network$k_co2_m_s*(1/0.001)*12.01*(60*60*24*365)) #gC_m2_yr
  	network$semi_FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$semi_FCO2_gC_m2_yr*network$W_m*network$LengthKM*1000, network$semi_FCO2_gC_m2_yr*network$lakeSA_m2)

	##CALCULATE OTHER SEMI-DISTRIBUTED MODEL FOR 0102----------------------------------------------------------------------------------------
  	network$semi2_FCO2_gC_m2_yr <- ifelse(network$waterbody == 'River', ((network$CO2_ppm-400)*henry*1e-6)*rivers_k_co2_lumped_m_s*(1/0.001)*12.01*(60*60*24*365), ((network$CO2_ppm-400)*henry*1e-6)*network$k_co2_m_s*(1/0.001)*12.01*(60*60*24*365)) #gC_m2_yr
  	network$semi2_FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$semi2_FCO2_gC_m2_yr*network$W_m*network$LengthKM*1000, network$semi2_FCO2_gC_m2_yr*network$lakeSA_m2)

  	#CALCULATE TOTAL DISTRIBUTED FLUX----------------------------------------------------------------------------------------------
	network$FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$FCO2_gC_m2_yr*network$W_m*network$LengthKM*1000, network$FCO2_gC_m2_yr*network$lakeSA_m2)  	

  	## BASIN FLUXES----------------------------------------------------------------------------------------------
  	lumpedFCO2_GgC_C_yr <- (rivers_FCO2_lumped_total + lakes_FCO2_lumped_total) * 1e-9
  	semiDistFCO2_GgC_C_yr <- sum(network$semi_FCO2_gC_yr, na.rm=T)*1e-9
  	semi2DistFCO2_GgC_C_yr <- sum(network$semi2_FCO2_gC_yr, na.rm=T)*1e-9
  	distFCO2_GgC_yr <- sum(network$FCO2_gC_yr, na.rm=T)*1e-9

  	plotA1 <- patchwork::wrap_elements(grid::textGrob(paste0('Lumped:\n',round(lumpedFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))

  	plotB1 <- patchwork::wrap_elements(grid::textGrob(paste0('Semi-Distributed:\n', round(semi2DistFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))

  	plotC1 <- patchwork::wrap_elements(grid::textGrob(paste0('Semi-Distributed:\n', round(semiDistFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))  	

  	plotD1 <- patchwork::wrap_elements(grid::textGrob(paste0('Distributed:\n',round(distFCO2_GgC_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))


  	## INDIVIDUAL k vs co2----------------------------------------------------------------------------------------
  	network$lumpedCO2_ppm <- ifelse(network$waterbody == 'River', riverCO2, lakeCO2)

  	network$lumped_k600_m_s <- ifelse(network$waterbody == 'River', rivers_k_600_lumped_m_s, network$k600_m_s)
  	plotE1 <- ggplot(network, aes(x=lumped_k600_m_s*86400, y=lumpedCO2_ppm, color=waterbody))+
  		geom_point(size=3) +
  		scale_color_manual(values=c('#6b9080', '#e26d5c'), name = '')+
  		xlim(0,300)+
  		ylim(0,16000)+
  		labs(tag='A')+
  		theme(legend.text = element_text(size=22))+
  		guides(color = guide_legend(override.aes = list(size=15)))+
    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 22),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('k600 [m/dy]')+
    	ylab('CO2 [ppm]')



    plotF1 <- ggplot(network, aes(x=lumped_k600_m_s*86400, y=CO2_ppm, color=waterbody))+
  		geom_point(alpha=0.25, size=3) +
  		scale_color_manual(values=c('#6b9080', '#e26d5c'), name = '')+
  		xlim(0,300)+
  		ylim(0,16000)+
  		labs(tag='B')+
    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 22),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('k600 [m/dy]')+
    	ylab('')


    plotG1 <- ggplot(network, aes(x=k600_m_s*86400, y=lumpedCO2_ppm, color=waterbody))+
  		geom_point(alpha=0.25, size=3) +
  		scale_color_manual(values=c('#6b9080', '#e26d5c'), name = '')+
  		geom_segment(x = 250, y = 3200, xend = 300, yend = 3200,color='black',size=3,arrow = grid::arrow(length = unit(0.06, "npc"), ends = "last"))+
  		xlim(0,300)+
  		ylim(0,16000)+
  		labs(tag='C')+
    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 22),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('k600 [m/dy]')+
    	ylab('')


  	plotH1 <- ggplot(network, aes(x=k600_m_s*86400, y=CO2_ppm, color=waterbody))+
  		geom_point(alpha=0.25, size=3) +
  		scale_color_manual(values=c('#6b9080', '#e26d5c'), name = '')+
  		geom_segment(x = 250, y = 3200, xend = 300, yend = 3200,color='black', size=3, arrow = grid::arrow(length = unit(0.06, "npc"), ends = "last"))+
  		xlim(0,300)+
  		ylim(0,16000)+
  		labs(tag='D')+
    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 22),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('k600 [m/dy]')+
    	ylab('')


  	## INDIVIDUAL MAPS----------------------------------------------------------------------------------------
    basin$forPlot <- ifelse(basin$lumpedFCO2_gC_C_m2_yr*1e-3 < 1, '1',
							ifelse(basin$lumpedFCO2_gC_C_m2_yr*1e-3 < 2.5, '2.5',
								ifelse(basin$lumpedFCO2_gC_C_m2_yr*1e-3 < 5, '5',
									ifelse(basin$lumpedFCO2_gC_C_m2_yr*1e-3 < 10, '10', '5+'))))
    plotI1 <- ggplot(basin, aes(fill=forPlot)) +
    	geom_sf()+
    	coord_sf(datum = NA)+
    	scale_fill_manual(name='FCO2\n[Kg-C/m2/yr]',
						   values=c('#5c374c', '#985277', '#ce6a85', '#ff8c61','#faa275'),
                           breaks=c('0.5', '1', '2.5', '5', '5+'))+
    	labs(tag='E')+
    	theme(plot.title = element_text(face = "italic", size = 26),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    #	ggspatial::annotation_scale(location = "bl",
    #    	                        height = unit(0.5, "cm"),
    #        	                    text_cex = 1)+
    	xlab('')+
    	ylab('')


    network$forPlot <- ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 0.5, '0.5',
							ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 1, '1',
								ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 2.5, '2.5',
									ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 5, '5', '5+'))))
    plotJ1 <- ggplot(network, aes(color=forPlot, linewidth=Q_m3_s, alpha=Q_m3_s)) +
    	geom_sf()+
    	coord_sf(datum = NA)+
    	scale_color_manual(name='FCO2\n[Kg-C/m2/yr]',
						   values=c('#5c374c', '#985277', '#ce6a85', '#ff8c61','#faa275'),
                           breaks=c('0.5', '1', '2.5', '5', '5+'))+
    	scale_linewidth_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10,100),
            	          range=c(0.4,2),
                	      guide = "none")+
    	scale_alpha_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10),
            	          range=c(0.4,1),
                	      guide = "none")+   	
    	labs(tag='F')+
    	theme(plot.title = element_text(face = "italic", size = 26),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
   # 	ggspatial::annotation_scale(location = "bl",
   #     	                        height = unit(0.5, "cm"),
   #         	                    text_cex = 1)+
    	xlab('')+
    	ylab('')


    network$forPlot <- ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 0.5, '0.5',
							ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 1, '1',
								ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 2.5, '2.5',
									ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 5, '5', '5+'))))
    plotK1 <- ggplot(network, aes(color=forPlot, linewidth=Q_m3_s, alpha=Q_m3_s)) +
    	geom_sf()+
    	coord_sf(datum = NA)+
    	scale_color_manual(name='FCO2\n[Kg-C/m2/yr]',
						   values=c('#5c374c', '#985277', '#ce6a85', '#ff8c61','#faa275'),
                           breaks=c('0.5', '1', '2.5', '5', '5+'))+
    	scale_linewidth_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10,100),
            	          range=c(0.4,2),
                	      guide = "none")+
    	scale_alpha_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10),
            	          range=c(0.4,1),
                	      guide = "none")+   	
    	labs(tag='G')+
    	theme(plot.title = element_text(face = "italic", size = 26),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
   # 	ggspatial::annotation_scale(location = "bl",
   #     	                        height = unit(0.5, "cm"),
   #         	                    text_cex = 1)+
    	xlab('')+
    	ylab('')


    network$forPlot <- ifelse(network$FCO2_gC_m2_yr*1e-3 < 0.5, '0.5',
							ifelse(network$FCO2_gC_m2_yr*1e-3 < 1, '1',
								ifelse(network$FCO2_gC_m2_yr*1e-3 < 2.5, '2.5',
									ifelse(network$FCO2_gC_m2_yr*1e-3 < 5, '5', '5+'))))
    plotL1 <- ggplot(network, aes(color=forPlot, linewidth=Q_m3_s, alpha=Q_m3_s)) +
    	geom_sf()+
    	coord_sf(datum = NA)+
    	scale_color_manual(name='FCO2\n[Kg-C/m2/yr]',
						   values=c('#5c374c', '#985277', '#ce6a85', '#ff8c61','#faa275'),
                           breaks=c('0.5', '1', '2.5', '5', '5+'))+
    	scale_linewidth_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10,100),
            	          range=c(0.4,2),
                	      guide = "none")+
    	scale_alpha_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10),
            	          range=c(0.4,1),
                	      guide = "none")+    	
    	labs(tag='H')+
    	theme(plot.title = element_text(face = "italic", size = 26),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    #	ggspatial::annotation_scale(location = "bl",
    #    	                        height = unit(0.5, "cm"),
    #        	                    text_cex = 1)+
    	xlab('')+
    	ylab('')


    ##EXTRACT SHARED LEGEND-----------------
    legend1 <- cowplot::get_legend(plotL1 +
                            labs(tag = '')+
                            theme(legend.position = "bottom",
                                  legend.text = element_text(size=22),
                                  legend.title = element_text(size=24, face='bold'),
                                  legend.box="vertical",
                                  legend.margin=margin(),
                                  legend.spacing.x = unit(0.3, 'cm')) +
                            guides(color = guide_legend(override.aes = list(linewidth=20)))) #fill legend size settings

    ##RIVER NAME----------------------------
    plotN1 <- patchwork::wrap_elements(grid::textGrob('Penobscot River, Maine', y=0.6, gp=grid::gpar(col="black", fontsize=34)))






    ##################################
	##READ IN BASIN 0202-------------------------------------------------------------------------------------
	##################################
	final_model <- dplyr::select(final_model_0202, c('NHDPlusID', 'StreamOrde', 'waterbody', 'Water_temp_c','Q_m3_s','W_m','lakeSA_m2', 'k600_m_s','k_co2_m_s', 'CO2_ppm', 'FCO2_gC_m2_yr'))
  	network <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_02/NHDPLUS_H_0202_HU4_GDB/NHDPLUS_H_0202_HU4_GDB.gdb'), layer='NHDFlowline')
  	network <- dplyr::left_join(network, final_model, 'NHDPlusID') %>%
  		sf::st_zm() %>%
  		dplyr::filter(is.na(waterbody)==0)
  	basin <- sf::st_read(paste0(path_to_data, '/HUC2_02/WBD_02_HU2_Shape/Shape/WBDHU4.shp')) %>%
  		dplyr::filter(huc4 == '0202')


  	##CALCULATE LUMPED MODEL FOR 0102----------------------------------------------------------------------------------------
  	riverCO2 <- glorich_data[glorich_data$HUC2 == '01',]$river #[ppm]
  	lakeCO2 <- glorich_data[glorich_data$HUC2 == '01',]$lake #[ppm]
  	temp_c <- mean(network$Water_temp_c, na.rm=T)
  	henry <- henry_func(temp_c)
  	sc <- 1911-118.11*temp_c+3.453*temp_c^2-0.0413*temp_c^3 #Raymond2012/Wanninkof 1991

  	rivers_by_order <- dplyr::group_by(network[network$waterbody == 'River',], StreamOrde) %>%
    	dplyr::summarise(kco2_m_s = mean(k_co2_m_s),
    					 k600_m_s = mean(k600_m_s),
        	             SA_m2 = sum(LengthKM*W_m*1000))

  	#get basin k co2
  	rivers_k_co2_lumped_m_s <- weighted.mean(rivers_by_order$kco2_m_s, rivers_by_order$SA_m2, na.rm=T) #normalize by surface area for each order
  	rivers_k_600_lumped_m_s <- weighted.mean(rivers_by_order$k600_m_s, rivers_by_order$SA_m2, na.rm=T) #normalize by surface area for each order

  	#calculate a basin fco2
  	rivers_FCO2_lumped <- ((riverCO2-400)*henry*1e-6)*rivers_k_co2_lumped_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]
  	rivers_FCO2_lumped_total <- rivers_FCO2_lumped*sum(rivers_by_order$SA_m2, na.rm=T) #[g-C/yr]

  	lakes_by_area <- dplyr::filter(network, waterbody == 'Lake/Reservoir') %>% #build combined lakes dataset for lake scaling
    	dplyr::group_by(WBArea_Permanent_Identifier) %>%
    	dplyr::summarise(area_skm = sum(lakeSA_m2, na.rm=T)*1e-6) %>%
    	dplyr::mutate(k600_m_dy = ifelse(area_skm < 0.1, 0.54, #[m/dy] Read etal 2012
        	                ifelse(area_skm < 1, 1.16,
            	              ifelse(area_skm < 3.16, 1.32,
                	            ifelse(area_skm < 10, 1.9, 1.9))))) %>%
    	dplyr::mutate(kco2_m_dy = k600_m_dy/(600/sc)^-0.5) %>% #m/dy
    	dplyr::mutate(kco2_m_s = (kco2_m_dy)/(24*60*60))  #m/s

  	#calculate a basin fco2
  	lakes_by_area$lakes_FCO2_lumped <- ((lakeCO2-400)*henry*1e-6)*lakes_by_area$kco2_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]
  	lakes_FCO2_lumped_total <- sum(lakes_by_area$lakes_FCO2_lumped*lakes_by_area$area_skm*1e6, na.rm=T) #[g-C/yr]

  	#calculate basin carbon emissions flux
  	lumpedFCO2_gC_C_m2_yr <- rivers_FCO2_lumped + sum(lakes_by_area$lakes_FCO2_lumped, na.rm=T)

  	basin$lumpedFCO2_gC_C_m2_yr <- lumpedFCO2_gC_C_m2_yr


  	##CALCULATE SEMI-DISTRIBUTED MODEL FOR 0102----------------------------------------------------------------------------------------
  	network$semi_FCO2_gC_m2_yr <- ifelse(network$waterbody == 'River', ((riverCO2-400)*henry*1e-6)*network$k_co2_m_s*(1/0.001)*12.01*(60*60*24*365), ((lakeCO2-400)*henry*1e-6)*network$k_co2_m_s*(1/0.001)*12.01*(60*60*24*365)) #gC_m2_yr
  	network$semi_FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$semi_FCO2_gC_m2_yr*network$W_m*network$LengthKM*1000, network$semi_FCO2_gC_m2_yr*network$lakeSA_m2)

	##CALCULATE OTHER SEMI-DISTRIBUTED MODEL FOR 0102----------------------------------------------------------------------------------------
  	network$semi2_FCO2_gC_m2_yr <- ifelse(network$waterbody == 'River', ((network$CO2_ppm-400)*henry*1e-6)*rivers_k_co2_lumped_m_s*(1/0.001)*12.01*(60*60*24*365), ((network$CO2_ppm-400)*henry*1e-6)*network$k_co2_m_s*(1/0.001)*12.01*(60*60*24*365)) #gC_m2_yr
  	network$semi2_FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$semi2_FCO2_gC_m2_yr*network$W_m*network$LengthKM*1000, network$semi2_FCO2_gC_m2_yr*network$lakeSA_m2)

  	#CALCULATE TOTAL DISTRIBUTED FLUX----------------------------------------------------------------------------------------------
	network$FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$FCO2_gC_m2_yr*network$W_m*network$LengthKM*1000, network$FCO2_gC_m2_yr*network$lakeSA_m2)  	

  	## BASIN FLUXES----------------------------------------------------------------------------------------------
  	lumpedFCO2_GgC_C_yr <- (rivers_FCO2_lumped_total + lakes_FCO2_lumped_total) * 1e-9
  	semiDistFCO2_GgC_C_yr <- sum(network$semi_FCO2_gC_yr, na.rm=T)*1e-9
  	semi2DistFCO2_GgC_C_yr <- sum(network$semi2_FCO2_gC_yr, na.rm=T)*1e-9
  	distFCO2_GgC_yr <- sum(network$FCO2_gC_yr, na.rm=T)*1e-9

  	plotA2 <- patchwork::wrap_elements(grid::textGrob(paste0('Lumped:\n',round(lumpedFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))

  	plotB2 <- patchwork::wrap_elements(grid::textGrob(paste0('Semi-Distributed:\n', round(semi2DistFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))

  	plotC2 <- patchwork::wrap_elements(grid::textGrob(paste0('Semi-Distributed:\n', round(semiDistFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))  	

  	plotD2 <- patchwork::wrap_elements(grid::textGrob(paste0('Distributed:\n',round(distFCO2_GgC_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))


  	## INDIVIDUAL k vs co2----------------------------------------------------------------------------------------
  	network$lumpedCO2_ppm <- ifelse(network$waterbody == 'River', riverCO2, lakeCO2)

  	network$lumped_k600_m_s <- ifelse(network$waterbody == 'River', rivers_k_600_lumped_m_s, network$k600_m_s)
  	plotE2 <- ggplot(network, aes(x=lumped_k600_m_s*86400, y=lumpedCO2_ppm, color=waterbody))+
  		geom_point(size=3) +
  		scale_color_manual(values=c('#6b9080', '#e26d5c'), name = '')+
  		xlim(0,300)+
  		ylim(0,16000)+
  		labs(tag='I')+
  		theme(legend.text = element_text(size=22))+
  		guides(color = guide_legend(override.aes = list(size=15)))+
    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 22),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('k600 [m/dy]')+
    	ylab('CO2 [ppm]')



    plotF2 <- ggplot(network, aes(x=lumped_k600_m_s*86400, y=CO2_ppm, color=waterbody))+
  		geom_point(alpha=0.25, size=3) +
  		scale_color_manual(values=c('#6b9080', '#e26d5c'), name = '')+
  		xlim(0,300)+
  		ylim(0,16000)+
  		labs(tag='J')+
    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 22),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('k600 [m/dy]')+
    	ylab('')


    plotG2 <- ggplot(network, aes(x=k600_m_s*86400, y=lumpedCO2_ppm, color=waterbody))+
  		geom_point(alpha=0.25, size=3) +
  		scale_color_manual(values=c('#6b9080', '#e26d5c'), name = '')+
  		geom_segment(x = 250, y = 3200, xend = 300, yend = 3200,color='black',size=3,arrow = grid::arrow(length = unit(0.06, "npc"), ends = "last"))+
  		xlim(0,300)+
  		ylim(0,16000)+
  		labs(tag='K')+
    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 22),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('k600 [m/dy]')+
    	ylab('')


  	plotH2 <- ggplot(network, aes(x=k600_m_s*86400, y=CO2_ppm, color=waterbody))+
  		geom_point(alpha=0.25, size=3) +
  		scale_color_manual(values=c('#6b9080', '#e26d5c'), name = '')+
  		geom_segment(x = 250, y = 3200, xend = 300, yend = 3200,color='black', size=3, arrow = grid::arrow(length = unit(0.06, "npc"), ends = "last"))+
  		xlim(0,300)+
  		ylim(0,16000)+
  		labs(tag='L')+
    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 22),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('k600 [m/dy]')+
    	ylab('')


  	## INDIVIDUAL MAPS----------------------------------------------------------------------------------------
    basin$forPlot <- ifelse(basin$lumpedFCO2_gC_C_m2_yr*1e-3 < 1, '1',
							ifelse(basin$lumpedFCO2_gC_C_m2_yr*1e-3 < 2.5, '2.5',
								ifelse(basin$lumpedFCO2_gC_C_m2_yr*1e-3 < 5, '5',
									ifelse(basin$lumpedFCO2_gC_C_m2_yr*1e-3 < 10, '10', '5+'))))
    plotI2 <- ggplot(basin, aes(fill=forPlot)) +
    	geom_sf()+
    	coord_sf(datum = NA)+
    	scale_fill_manual(name='FCO2\n[Kg-C/m2/yr]',
						   values=c('#5c374c', '#985277', '#ce6a85', '#ff8c61','#faa275'),
                           breaks=c('0.5', '1', '2.5', '5', '5+'))+
    	labs(tag='M')+
    	theme(plot.title = element_text(face = "italic", size = 26),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    #	ggspatial::annotation_scale(location = "bl",
    #    	                        height = unit(0.5, "cm"),
    #        	                    text_cex = 1)+
    	xlab('')+
    	ylab('')


    network$forPlot <- ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 0.5, '0.5',
							ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 1, '1',
								ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 2.5, '2.5',
									ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 5, '5', '5+'))))
    plotJ2 <- ggplot(network, aes(color=forPlot, linewidth=Q_m3_s, alpha=Q_m3_s)) +
    	geom_sf()+
    	coord_sf(datum = NA)+
    	scale_color_manual(name='FCO2\n[Kg-C/m2/yr]',
						   values=c('#5c374c', '#985277', '#ce6a85', '#ff8c61','#faa275'),
                           breaks=c('0.5', '1', '2.5', '5', '5+'))+
    	scale_linewidth_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10,100),
            	          range=c(0.4,2),
                	      guide = "none")+
    	scale_alpha_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10),
            	          range=c(0.4,1),
                	      guide = "none")+   	
    	labs(tag='N')+
    	theme(plot.title = element_text(face = "italic", size = 26),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
   # 	ggspatial::annotation_scale(location = "bl",
   #     	                        height = unit(0.5, "cm"),
   #         	                    text_cex = 1)+
    	xlab('')+
    	ylab('')


    network$forPlot <- ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 0.5, '0.5',
							ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 1, '1',
								ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 2.5, '2.5',
									ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 5, '5', '5+'))))
    plotK2 <- ggplot(network, aes(color=forPlot, linewidth=Q_m3_s, alpha=Q_m3_s)) +
    	geom_sf()+
    	coord_sf(datum = NA)+
    	scale_color_manual(name='FCO2\n[Kg-C/m2/yr]',
						   values=c('#5c374c', '#985277', '#ce6a85', '#ff8c61','#faa275'),
                           breaks=c('0.5', '1', '2.5', '5', '5+'))+
    	scale_linewidth_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10,100),
            	          range=c(0.4,2),
                	      guide = "none")+
    	scale_alpha_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10),
            	          range=c(0.4,1),
                	      guide = "none")+   	
    	labs(tag='O')+
    	theme(plot.title = element_text(face = "italic", size = 26),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
   # 	ggspatial::annotation_scale(location = "bl",
   #     	                        height = unit(0.5, "cm"),
   #         	                    text_cex = 1)+
    	xlab('')+
    	ylab('')


    network$forPlot <- ifelse(network$FCO2_gC_m2_yr*1e-3 < 0.5, '0.5',
							ifelse(network$FCO2_gC_m2_yr*1e-3 < 1, '1',
								ifelse(network$FCO2_gC_m2_yr*1e-3 < 2.5, '2.5',
									ifelse(network$FCO2_gC_m2_yr*1e-3 < 5, '5', '5+'))))
    plotL2 <- ggplot(network, aes(color=forPlot, linewidth=Q_m3_s, alpha=Q_m3_s)) +
    	geom_sf()+
    	coord_sf(datum = NA)+
    	scale_color_manual(name='FCO2\n[Kg-C/m2/yr]',
						   values=c('#5c374c', '#985277', '#ce6a85', '#ff8c61','#faa275'),
                           breaks=c('0.5', '1', '2.5', '5', '5+'))+
    	scale_linewidth_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10,100),
            	          range=c(0.4,2),
                	      guide = "none")+
    	scale_alpha_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10),
            	          range=c(0.4,1),
                	      guide = "none")+    	
    	labs(tag='P')+
    	theme(plot.title = element_text(face = "italic", size = 26),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    #	ggspatial::annotation_scale(location = "bl",
    #    	                        height = unit(0.5, "cm"),
    #        	                    text_cex = 1)+
    	xlab('')+
    	ylab('')


    ##EXTRACT SHARED LEGEND-----------------
    legend2 <- cowplot::get_legend(plotL2 +
                            labs(tag = '')+
                            theme(legend.position = "bottom",
                                  legend.text = element_text(size=22),
                                  legend.title = element_text(size=24, face='bold'),
                                  legend.box="vertical",
                                  legend.margin=margin(),
                                  legend.spacing.x = unit(0.3, 'cm')) +
                            guides(color = guide_legend(override.aes = list(linewidth=20)))) #fill legend size settings

    ##RIVER NAME----------------------------
    plotN2 <- patchwork::wrap_elements(grid::textGrob('Upper Hudson River, New York', y=0.6, gp=grid::gpar(col="black", fontsize=34)))






  	#COMBO PLOT---------------------------------------------
  	design <- "
  		ABCD
  		EFGH
  		EFGH
  		EFGH
  		IJKL
  		IJKL
  		IJKL
  		MMNN
  		OPQR
  		OPQR
  		OPQR
  		STUV
  		STUV
  		STUV
  		WWXX
  	"

  	comboPlot <- patchwork::wrap_plots(A=plotA1, B=plotB1, C=plotC1, D=plotD1,
  									   E=plotE1+theme(legend.position=c(0.5, 0.8)), F=plotF1+theme(legend.position='none'), G=plotG1+theme(legend.position='none'), H=plotH1+theme(legend.position='none'),
  									   I=plotI1+theme(legend.position='none'), J=plotJ1+theme(legend.position='none'), K=plotK1+theme(legend.position='none'), L=plotL1+theme(legend.position='none'),
  									   M=legend1, N=plotN1,

  									   O=plotE2+theme(legend.position=c(0.5, 0.8)), P=plotF2+theme(legend.position='none'), Q=plotG2+theme(legend.position='none'), R=plotH2+theme(legend.position='none'),
  									   S=plotI2+theme(legend.position='none'), T=plotJ2+theme(legend.position='none'), U=plotK2+theme(legend.position='none'), V=plotL2+theme(legend.position='none'),
  									   W=legend2, X=plotN2,

  									   design=design)

  	ggsave('cache/figures/conceptualCompare.jpg', comboPlot, width=25, height=30)
  	return('see cache/figures/conceptualCompare.jpg')
}










lakes_GW_Plot <- function(path_to_data, results){
	theme_set(theme_classic())
  
  	# CONUS boundary
  	states <- sf::st_read(paste0(path_to_data, '/other_shapefiles/cb_2018_us_state_5m.shp'))
  	states <- dplyr::filter(states, !(NAME %in% c('Alaska',
                                                'American Samoa',
                                                'Commonwealth of the Northern Mariana Islands',
                                                'Guam',
                                                'District of Columbia',
                                                'Puerto Rico',
                                                'United States Virgin Islands',
                                                'Hawaii'))) #remove non CONUS states/territories
  	states <- sf::st_union(states)
  
  	#setup results to map
  	results$perc_GW <- round((results$contribGW_TgC_yr/results$sumFCO2_TgC_yr )*100,0) #setup percent
  	results$perc_Lakes <- round((results$lakeFCO2_TgC_yr/results$sumFCO2_TgC_yr )*100,0) #setup percent
  	results[!is.na(results$lakeFCO2_TgC_yr) & results$lakeFCO2_TgC_yr < 0,]$perc_Lakes <- 0 #if lakes are a sink, just set to 0

  	#PERC GW MAP-------------------------------------------------
  	cdf_inset <- ggplot(results, aes(perc_GW))+
    	stat_ecdf(size=2, color='black') +
    	xlim(0,100)+
    	xlab('% emissions from\ngroundwater')+
    	ylab('Probability')+
    	theme(axis.title = element_text(size=20),
        	  axis.text = element_text(family="Futura-Medium", size=18))+ #axis text settings
    	theme(legend.position = 'none') #legend position settings

  	mapGW <- ggplot(results) +
    	geom_sf(aes(fill=perc_GW), #actual map
        	    color='black',
            	size=0.5) +
    	geom_sf(data=states, #conus boundary
        	    color='black',
            	size=1.25,
            	alpha=0)+
    	labs(tag='A')+
    	scale_fill_gradientn(name='% emissions from groundwater',
        	                 colors=c('white', '#081c15'),
            	             limits=c(0,100),
                	         guide = guide_colorbar(direction = "horizontal",
                    	                            title.position = "bottom"))+
    	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
    	theme(legend.position = c(0.2, 0.1),
        	  legend.key.size = unit(2, 'cm'))+ #legend position settings
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
        	  legend.title = element_text(face = "bold", size = 18),
          	  legend.text = element_text(family = "Futura-Medium", size = 18),
          	  plot.tag = element_text(size=26,
            	                      face='bold'))+
    	xlab('')+
    	ylab('')

    mapGW <- mapGW +
    	patchwork::inset_element(cdf_inset, right = 0.975, bottom = 0.001, left = 0.775, top = 0.35)

  	#PERC LAKES MAP-------------------------------------------------
  	cdf_inset <- ggplot(results, aes(perc_GW))+
    	stat_ecdf(size=2, color='black') +
    	xlim(0,100)+
    	xlab('% emissions from\nlakes/reservoirs')+
    	ylab('Probability')+
    	theme(axis.title = element_text(size=20),
        	  axis.text = element_text(family="Futura-Medium", size=18))+ #axis text settings
    	theme(legend.position = 'none') #legend position settings

  	mapLakes <- ggplot(results) +
    	geom_sf(aes(fill=perc_Lakes), #actual map
        	    color='black',
            	size=0.5) +
    	geom_sf(data=states, #conus boundary
        	    color='black',
            	size=1.25,
            	alpha=0)+
    	labs(tag='B')+
    	scale_fill_gradientn(name='% emissions from lakes/reservoirs',
        	                 colors=c('white', '#10002b'),
            	             limits=c(0,100),
                	         guide = guide_colorbar(direction = "horizontal",
                    	                            title.position = "bottom"))+
    	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
    	theme(legend.position = c(0.2, 0.1),
        	  legend.key.size = unit(2, 'cm'))+ #legend position settings
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
        	  legend.title = element_text(face = "bold", size = 18),
          	  legend.text = element_text(family = "Futura-Medium", size = 18),
          	  plot.tag = element_text(size=26,
            	                      face='bold'))+
    	xlab('')+
    	ylab('')    	

    mapLakes <- mapLakes +
    	patchwork::inset_element(cdf_inset, right = 0.975, bottom = 0.001, left = 0.775, top = 0.35)


  	#COMBO PLOT---------------------------------------------
  	design <- "
  		A
  		B
  	"

  	comboPlot <- patchwork::wrap_plots(A=mapGW, B=mapLakes, design=design)


  	ggsave('cache/figures/modelGWLakes.jpg', comboPlot, width=16, height=20)
  	return('see cache/figures/modelGWLakes.jpg')
}