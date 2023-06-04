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
    	annotate('text', label=paste0('median error across\nbasins: ', round(median(forPlot$fitness, na.rm=T),0), ' ppm'), x=1000, y=0.65, size=8)+
		xlab('Calibration error per basin [ppm]') +
		ylab('Probability') +
		theme(axis.title = element_text(size=20),
          axis.text = element_text(family="Futura-Medium", size=18))+ #axis text settings
    	theme(legend.position = 'none') #legend position settings

	ggsave('cache/figures/calib/calibrationSummary.jpg', plot, width=8, height=8)


	ticker <- 1
	a <- 1
	b <- 20
	while(ticker <= 11){ #pretty janky way to iterate through the basins but it gets the job done....
		c <- 1
		b <- ifelse(ticker == 11, 212,b)
		plot <- list()
	  	while(a <= b){
			huc4 <- substr(names(combined_calib[a]),22,26)
			basin <- combined_calib[[a]]

			if(is.na(basin$plot) || huc4 == '0427_'){
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


		#manually loop through groups of 16 basins and write them to file
    	if(ticker != 11){
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
    		print(length(plot))
    		#plot design
  			design <- "
   	 		WAABBCC
   	 		WAABBCC
   	 		WDDEEFF
   	 		WDDEEFF
   		 	WUUUUUU
	   	 	WVVVVVV
    		"

  			comboPlot <- patchwork::wrap_plots(A=plot[[1]] + theme(legend.position='none'), B=plot[[2]] + theme(legend.position='none'), C=plot[[3]] + theme(legend.position='none'), D=plot[[4]] + theme(legend.position='none'),
        	                	             E=plot[[5]] + theme(legend.position='none'), F=plot[[6]] + theme(legend.position='none'),
            	            	             U=x_axis, V=legend, W=y_axis, design=design)

  			ggsave(paste0('cache/figures/calib/comboPlot_', ticker, '.jpg'), comboPlot, width=20, height=15)

  			ticker <- ticker + 1
  		}
	}
  
  return('all calibration results written to file at ~/cache/figures/calib/')
}






compareModels <- function(path_to_data, ourModel, lumpedList, glorich) {
  	theme_set(theme_classic())

		lumped <- do.call("rbind", lumpedList) #make raymond model object


  	#BARPLOTS TOTAL--------------------------------------------
    #sum across the U.S.
    forPlot <- data.frame('Distributed'=sum(lumped$sumFCO2_TgC_yr, na.rm=T),    					  
    					  'Lumped_full'=sum(lumped$sumFCO2_lumped_TgC_yr, na.rm=T),
    					  'Lumped_sigma'=sum(lumped$cal_uncertainty, na.rm=T))

  	forPlot <- tidyr::gather(forPlot, key=key, value=value, c('Distributed', 'Lumped_full'))
  	forPlot[forPlot$key != 'Distributed',]$Lumped_sigma <- NA #don't apply to upscaling model
  	forPlot$key <- factor(forPlot$key, levels=c('Lumped_full', 'Distributed'))

  	bars <- ggplot(forPlot, aes(x=key, y=value, fill=key)) +
  		geom_col(size=1.5, color='black', size=1.5) +
  		geom_errorbar(aes(ymin=value-Lumped_sigma, ymax=value+Lumped_sigma), width=.15, size=1.75) +
  		scale_fill_manual(values=c('#20a39e', '#ffba49'))+
  		scale_x_discrete(labels=c('Upscaling', 'Transport'))+
  		labs(tag='B')+
  		xlab('')+
  		ylab(expression(bold(FCO[2]~(Tg-C/yr)))) +
  		theme(legend.position='none') +
  		theme(axis.title = element_text(size=26, face='bold', color='black'), axis.text = element_text(size=20, color='black'))+ #axis text settings
    	theme(plot.tag = element_text(size=26,face='bold'))


    #COMPARE TO GLORICH----------------------------------------------------------------
     #hydraulic geometry parameters
  	depAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/depAHG.rds') #depth AHG model
  	widAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/widAHG.rds') #depth AHG model
  	glorich$a <- widAHG$coefficients[1]
  	glorich$b <- widAHG$coefficients[2]
  	glorich$c <- depAHG$coefficients[1]
  	glorich$f <- depAHG$coefficients[2]

  	#get w, d, vol differentially by river vs lake/reservoir
  	glorich$D <- exp(glorich$c) * glorich$nhdQ_cms^glorich$f
  	glorich$W <- exp(glorich$a) * glorich$nhdQ_cms^glorich$b
  	glorich$V <- glorich$nhdQ_cms / (glorich$D*glorich$W)

  	glorich$eD <- 9.8*glorich$V*glorich$nhd_slope #[m2/s3]
  	glorich$k600_m_dy <- ifelse(glorich$eD <= 0.02, exp(3.10+0.35*log(glorich$eD)), exp(6.43+1.18*log(glorich$eD)))

  	glorich$snap_distance_m_num <- as.numeric(glorich$snap_distance_m)
  	glorich <- dplyr::filter(glorich, snap_distance_m_num <= 20)

		glorich_shp <- readr::read_csv('data/glorich_rocher_ros_2019.csv')
		glorich_shp <- sf::st_as_sf(x=glorich_shp,
                   coords = c("Longitude", "Latitude"),
               		crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
				dplyr::filter(STAT_ID %in% glorich$STAT_ID)

	#little map
	# library(rnaturalearth)
	# library(rnaturalearthdata)
	# world <- ne_countries(scale = "medium", returnclass = "sf", country='united states of america')

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

	map_world <- ggplot(glorich_shp, aes(color=STAT_ID)) +
		geom_sf(color='#102542')+
    	geom_sf(data=states, #conus boundary
        	    color='black',
            	linewidth=0.25,
            	alpha=0) +
    	coord_sf(expand = FALSE) +
    	    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 18),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))

    #upscaling model
    lumpedModel <- dplyr::select(ourModel, c('lumped_k600_m_dy', 'lumped_CO2')) %>%
    	dplyr::mutate(model='Upscaling') %>%
    	dplyr::select('model', 'lumped_k600_m_dy', 'lumped_CO2') %>%
    	dplyr::distinct(lumped_k600_m_dy, .keep_all = TRUE)
    colnames(lumpedModel) <- c('model', 'k600_m_dy', 'CO2_ppm')

    #glorich plot
    ourModel <- dplyr::select(ourModel, c('k600_m_s', 'CO2_ppm')) %>%
    	dplyr::mutate(k600_m_dy = k600_m_s*86400,
    								model='Transport') %>%
    	dplyr::select(c('model','k600_m_dy', 'CO2_ppm'))

    glorich <- dplyr::select(glorich, c('k600_m_dy', 'pco2'))
    glorich$model <- 'In situ data'
    colnames(glorich) <- c('k600_m_dy', 'CO2_ppm','model')
    glorich <- dplyr::select(glorich, c('model', 'k600_m_dy', 'CO2_ppm'))

    forPlot <- rbind(ourModel, glorich, lumpedModel)
  	
  	glorichPlot <- ggplot(forPlot, aes(x=k600_m_dy, y=CO2_ppm, color=model))+
  		geom_point(size=4, alpha=0.3) +
  		geom_segment(x = 200, y = 3200, xend = 250, yend = 3200,color='black',size=3,arrow = grid::arrow(length = unit(0.06, "npc"), ends = "last"))+
  		scale_color_manual(name='', values=c('#102542', '#ffba49', '#20a39e'))+
  		xlim(0,250)+
  		ylim(0,24000)+
  		labs(tag='A')+
  		theme(legend.position='bottom',
  					legend.text=element_text(size=22))+
    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 22),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('k600 (m/dy)')+
    	ylab(expression(bold(CO[2]~(ppm)))) +
    	guides(color = guide_legend(override.aes = list(alpha=1)))

     glorichPlot <- glorichPlot +
    	 	patchwork::inset_element(map_world, right = 0.98, bottom = 0.6, left = 0.15, top = 0.99)

    #model plot
  	# ourPlot <- ggplot(ourModel, aes(x=k600_m_s*86400, y=CO2_ppm))+
  	# 	geom_point(size=4, alpha=0.4, color='#ffba49') +
  	# 	xlim(0,250)+
  	# 	ylim(0,24000)+
  	# 	labs(tag='A')+
  	# 	geom_segment(x = 200, y = 3200, xend = 250, yend = 3200,color='black',size=3,arrow = grid::arrow(length = unit(0.06, "npc"), ends = "last"))+
  	# 	theme(legend.text = element_text(size=22))+
    # 	theme(axis.title = element_text(face = "bold", size = 24),
    # 		  axis.text = element_text(size = 22),
    #     	  plot.tag = element_text(size=26,
    #         	                  face='bold'))+
    # 	xlab('k600 [m/dy]')+
    # 	ylab('CO2 [ppm]')   



  	#BARPLOTS LAKES--------------------------------------------
    # results_huc2 <- lumped %>%
    # 	dplyr::group_by(waterbody) %>%
    # 	dplyr::summarise(sumFCO2_TgC_yr = sum(sumFCO2_TgC_yr, na.rm=T),
    # 					 sumFCO2_lumped_TgC_yr = sum(sumFCO2_lumped_TgC_yr, na.rm=T))

    # #sum across the U.S.
    # forPlot <- data.frame('Distributed'=round((sum(results_huc2[results_huc2$waterbody == 'Lake/Reservoir',]$sumFCO2_TgC_yr, na.rm=T) / sum(results_huc2$sumFCO2_TgC_yr, na.rm=T))*100, 0), 					  
    # 					  'Lumped_full'=round((sum(results_huc2[results_huc2$waterbody == 'Lake/Reservoir',]$sumFCO2_lumped_TgC_yr, na.rm=T) / sum(results_huc2$sumFCO2_lumped_TgC_yr, na.rm=T))*100, 0))

  	# forPlot <- tidyr::gather(forPlot, key=key, value=value, c('Distributed', 'Lumped_full'))
  	# forPlot[forPlot$key != 'Distributed',]$Lumped_sigma <- NA #don't apply to upscaling model
  	# forPlot$key <- factor(forPlot$key, levels=c('Lumped_full', 'Distributed'))

  	# lakeBars <- ggplot(forPlot, aes(x=key, y=value, fill=key)) +
  	# 	geom_col(size=1.5, color='black', size=1.5) +
  	# 	scale_fill_manual(values=c('#20a39e', '#ffba49'))+
  	# 	scale_x_discrete(labels=c('Upscaling', 'Transport'))+
  	# 	labs(tag='B')+
  	# 	xlab('')+
  	# 	ylab('% emissions from lakes/reservoirs') +
  	# 	theme(legend.position='none') +
  	# 	theme(axis.title = element_text(size=26, face='bold', color='black'), axis.text = element_text(size=20, color='black'))+ #axis text settings
    # 	theme(plot.tag = element_text(size=26,face='bold'))

  	#COMBO PLOT---------------------------------------------
  	design <- "
  		AB
  	"

  	comboPlot <- patchwork::wrap_plots(A=glorichPlot, B=bars ,design=design)

  	ggsave('cache/figures/modelsCompare.jpg', comboPlot, width=20, height=10)
  	return('see cache/figures/modelsCompare.jpg')
}







conceptualPlot <- function(final_model, glorich_data, huc4_id){
	theme_set(theme_classic())

	huc2 <- substr(huc4_id, 1, 2)

	##READ IN BASIN-------------------------------------------------------------------------------------
	final_model <- dplyr::select(final_model, c('NHDPlusID', 'StreamOrde', 'waterbody', 'Water_temp_c','Q_m3_s','W_m','lakeSA_m2', 'k600_m_s','k_co2_m_s', 'CO2_ppm', 'FCO2_gC_m2_yr'))
  	network <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4_id, '_HU4_GDB/NHDPLUS_H_', huc4_id, '_HU4_GDB.gdb'), layer='NHDFlowline')
  	network <- dplyr::left_join(network, final_model, 'NHDPlusID') %>%
  		sf::st_zm() %>%
  		dplyr::filter(is.na(waterbody)==0)
  	basin <- sf::st_read(paste0(path_to_data, '/HUC2_', huc2, '/WBD_', huc2, '_HU2_Shape/Shape/WBDHU4.shp')) %>%
  		dplyr::filter(huc4 == huc4_id)

  	basin_name <- basin$name


  	##CALCULATE LUMPED MODEL FOR 0102----------------------------------------------------------------------------------------
  	riverCO2 <- glorich_data[glorich_data$HUC2 == huc2,]$river #[ppm]
  	lakeCO2 <- glorich_data[glorich_data$HUC2 == huc2,]$lake #[ppm]
  	temp_c <- mean(network$Water_temp_c, na.rm=T)
  	henry <- henry_func(temp_c)
  	sc <- 1911-118.11*temp_c+3.453*temp_c^2-0.0413*temp_c^3 #Raymond2012/Wanninkof 1991

  	rivers_by_order <- dplyr::group_by(network[network$waterbody == 'River',], StreamOrde) %>%
    	dplyr::summarise(kco2_m_s = mean(k_co2_m_s, na.rm=T),
    					 k600_m_s = mean(k600_m_s, na.rm=T),
        	             SA_m2 = sum(LengthKM*W_m*1000, na.rm=T))

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

  	network$lumpedFCO2_gC_C_m2_yr <- lumpedFCO2_gC_C_m2_yr


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

  	plotA <- patchwork::wrap_elements(grid::textGrob(paste0('Fully Lumped:\n',round(lumpedFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))

  	plotB <- patchwork::wrap_elements(grid::textGrob(paste0('Lumped kco2:\n', round(semi2DistFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))

  	plotC <- patchwork::wrap_elements(grid::textGrob(paste0('Lumped CO2:\n', round(semiDistFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))  	

  	plotD <- patchwork::wrap_elements(grid::textGrob(paste0('Distributed:\n',round(distFCO2_GgC_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))


  	## INDIVIDUAL k vs co2----------------------------------------------------------------------------------------
  	network$lumpedCO2_ppm <- ifelse(network$waterbody == 'River', riverCO2, lakeCO2)

  	network$lumped_k600_m_s <- ifelse(network$waterbody == 'River', rivers_k_600_lumped_m_s, network$k600_m_s)
  	plotE <- ggplot(network, aes(x=lumped_k600_m_s*86400, y=lumpedCO2_ppm, color=waterbody))+
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



    plotF <- ggplot(network, aes(x=lumped_k600_m_s*86400, y=CO2_ppm, color=waterbody))+
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


    plotG <- ggplot(network, aes(x=k600_m_s*86400, y=lumpedCO2_ppm, color=waterbody))+
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


  	plotH <- ggplot(network, aes(x=k600_m_s*86400, y=CO2_ppm, color=waterbody))+
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
    network$forPlot <- ifelse(network$lumpedFCO2_gC_C_m2_yr*1e-3 < 0.5, '0.5',
							ifelse(network$lumpedFCO2_gC_C_m2_yr*1e-3 < 1, '1',
								ifelse(network$lumpedFCO2_gC_C_m2_yr*1e-3 < 2.5, '2.5',
									ifelse(network$lumpedFCO2_gC_C_m2_yr*1e-3 < 5, '5', '5+'))))
    plotI <- ggplot() +
    	geom_sf(data=network, aes(color=forPlot, linewidth=Q_m3_s, alpha=Q_m3_s))+
    	ggsn::scalebar(data=network,location='bottomleft', dist = 25, dist_unit = "km",transform = TRUE, model = "WGS84",st.dist=0.05) +  
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
    	labs(tag='E')+
    	coord_sf(datum = NA)+    	
    	theme(plot.title = element_text(face = "italic", size = 26),
    		  axis.text = element_text(size = 22),
    		  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('')+
    	ylab('')


    network$forPlot <- ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 0.5, '0.5',
							ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 1, '1',
								ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 2.5, '2.5',
									ifelse(network$semi2_FCO2_gC_m2_yr*1e-3 < 5, '5', '5+'))))
    plotJ <- ggplot() +
    	geom_sf(data=network, aes(color=forPlot, linewidth=Q_m3_s, alpha=Q_m3_s))+
    	ggsn::scalebar(data=network,location='bottomleft', dist = 25, dist_unit = "km",transform = TRUE, model = "WGS84",st.dist=0.05) +      	
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
    	coord_sf(datum = NA)+    	
    	theme(plot.title = element_text(face = "italic", size = 26),
    		  axis.text = element_text(size = 22),
    		  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('')+
    	ylab('')


    network$forPlot <- ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 0.5, '0.5',
							ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 1, '1',
								ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 2.5, '2.5',
									ifelse(network$semi_FCO2_gC_m2_yr*1e-3 < 5, '5', '5+'))))
    plotK <- ggplot() +
    	geom_sf(data=network, aes(color=forPlot, linewidth=Q_m3_s, alpha=Q_m3_s))+
    	ggsn::scalebar(data=network,location='bottomleft', dist = 25, dist_unit = "km",transform = TRUE, model = "WGS84",st.dist=0.05) +      	
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
    	coord_sf(datum = NA)+    	
    	theme(plot.title = element_text(face = "italic", size = 26),
    		  axis.text = element_text(size = 22),
    		  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('')+
    	ylab('')


    network$forPlot <- ifelse(network$FCO2_gC_m2_yr*1e-3 < 0.5, '0.5',
							ifelse(network$FCO2_gC_m2_yr*1e-3 < 1, '1',
								ifelse(network$FCO2_gC_m2_yr*1e-3 < 2.5, '2.5',
									ifelse(network$FCO2_gC_m2_yr*1e-3 < 5, '5', '5+'))))
    plotL <- ggplot() +
    	geom_sf(data=network, aes(color=forPlot, linewidth=Q_m3_s, alpha=Q_m3_s))+
    	ggsn::scalebar(data=network,location='bottomleft', dist = 25, dist_unit = "km",transform = TRUE, model = "WGS84",st.dist=0.05) +    	
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
    	coord_sf(datum = NA)+    	
    	theme(plot.title = element_text(face = "italic", size = 26),
    		  axis.text = element_text(size = 22),
    		  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('')+
    	ylab('')


    ##EXTRACT SHARED LEGEND-----------------
    legend <- cowplot::get_legend(plotL +
                            labs(tag = '')+
                            theme(legend.position = "bottom",
                                  legend.text = element_text(size=22),
                                  legend.title = element_text(size=24, face='bold'),
                                  legend.box="vertical",
                                  legend.margin=margin(),
                                  legend.spacing.x = unit(0.3, 'cm')) +
                            guides(color = guide_legend(override.aes = list(linewidth=20)))) #fill legend size settings

    ##RIVER NAME----------------------------
    basin_name <- stringr::str_wrap(paste0(basin_name, ' River'), 30) #wrap to twenty characters, seems to fit nicely
    plotN <- patchwork::wrap_elements(grid::textGrob(basin_name, y=0.6, gp=grid::gpar(col="black", fontsize=34)))


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
  	"

  	comboPlot <- patchwork::wrap_plots(A=plotA, B=plotB, C=plotC, D=plotD,
  									   E=plotE+theme(legend.position=c(0.5, 0.8)), F=plotF+theme(legend.position='none'), G=plotG+theme(legend.position='none'), H=plotH+theme(legend.position='none'),
  									   I=plotI+theme(legend.position='none'), J=plotJ+theme(legend.position='none'), K=plotK+theme(legend.position='none'), L=plotL+theme(legend.position='none'),
  									   M=legend, N=plotN,

  									   design=design)

  	ggsave(paste0('cache/figures/conceptualCompare_', huc4_id, '.jpg'), comboPlot, width=26, height=18)
  	return(paste0('see cache/figures/conceptualCompare_', huc4_id, '.jpg'))
}










sourcesMap <- function(path_to_data, results){
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

  	results <- dplyr::filter(results, is.na(contribGW_TgC_yr)==0 & is.na(lakeFCO2_TgC_yr)==0) #remove great lakes

  	#setup results to map
  	results$perc_GW <- round((results$contribGW_TgC_yr/results$lostCO2_TgC_yr )*100,0) #setup percent
  	results$perc_WC <- round((results$contribWC_TgC_yr/results$lostCO2_TgC_yr )*100,0) #setup percent
  	results$perc_BZ <- round((results$contribBZ_TgC_yr/results$lostCO2_TgC_yr )*100,0) #setup percent
  	results$perc_UP <- round((results$contribUP_TgC_yr/results$lostCO2_TgC_yr )*100,0) #setup percent
  	results$perc_EMIT <- round((results$sumFCO2_TgC_yr/results$lostCO2_TgC_yr )*100,0) #setup percent
  	results$perc_EXP <- round((results$exported_TgC_yr/results$lostCO2_TgC_yr )*100,0) #setup percent

  	mapGW <- ggplot(results) +
    	geom_sf(aes(fill=perc_GW), #actual map
        	    color='black',
            	size=0.5) +
    	geom_sf(data=states, #conus boundary
        	    color='black',
            	linewidth=0.75,
            	alpha=0)+
    	labs(tag='B')+
    	ggtitle('% Groundwater loading')+    	
    	scale_fill_gradient2(name='',
    						low='white',
    						mid='#7ca982',
    						high='#243e36',
    						midpoint=50,
    						limits=c(0,100),
               	         	guide = guide_colorbar(direction = "horizontal",
                   	                               title.position = "bottom"))+
    	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=18))+ #axis text settings
    	theme(legend.position = c(0.2, 0.1),
        	  legend.key.size = unit(2, 'cm'))+
        	#  panel.background = element_rect(fill = '#e8998d', color = 'black'))+ #legend position settings
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
        	  legend.title = element_text(face = "bold", size = 18),
          	  legend.text = element_text(family = "Futura-Medium", size = 18),
          	  plot.tag = element_text(size=26,
            	                      face='bold'),
          	plot.title = element_text(size=26,
            	                      face='bold'))+
    	xlab('')+
    	ylab('')

  	#PERC WC MAP-------------------------------------------------
  	mapWC <- ggplot(results) +
    	geom_sf(aes(fill=perc_WC), #actual map
        	    color='black',
            	size=0.5) +
    	geom_sf(data=states, #conus boundary
        	    color='black',
            	linewidth=0.75,
            	alpha=0)+
    	labs(tag='C')+
    	ggtitle('% Net water-column\nrespiration')+    	
    	scale_fill_gradient2(name='',
    						low='white',
    						mid='#7ca982',
    						high='#243e36',
    						midpoint=50,
    						limits=c(0,100),
               	         	guide = guide_colorbar(direction = "horizontal",
                   	                               title.position = "bottom"))+
    	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=18))+ #axis text settings
    	theme(legend.position = c(0.2, 0.1),
        	  legend.key.size = unit(2, 'cm'))+
        	#  panel.background = element_rect(fill = '#e8998d', color = 'black'))+ #legend position settings
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
        	  legend.title = element_text(face = "bold", size = 18),
          	  legend.text = element_text(family = "Futura-Medium", size = 18),
          	  plot.tag = element_text(size=26,
            	                      face='bold'),
          	plot.title = element_text(size=26,
            	                      face='bold'))+
    	xlab('')+
    	ylab('')

  	#PERC BZ MAP-------------------------------------------------
  	mapBZ <- ggplot(results) +
    	geom_sf(aes(fill=perc_BZ), #actual map
        	    color='black',
            	size=0.5) +
    	geom_sf(data=states, #conus boundary
        	    color='black',
            	linewidth=0.75,
            	alpha=0)+
    	labs(tag='A')+
    	ggtitle('% Hyporheic zone\nrespiration')+    	
    	scale_fill_gradient2(name='',
    						low='white',
    						mid='#7ca982',
    						high='#243e36',
    						midpoint=50,
    						limits=c(0,100),
               	         	guide = guide_colorbar(direction = "horizontal",
                   	                               title.position = "bottom"))+
    	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=18))+ #axis text settings
    	theme(legend.position = c(0.2, 0.1),
        	  legend.key.size = unit(2, 'cm'))+
        	#  panel.background = element_rect(fill = '#e8998d', color = 'black'))+ #legend position settings
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
        	  legend.title = element_text(face = "bold", size = 18),
          	legend.text = element_text(family = "Futura-Medium", size = 18),
          	plot.tag = element_text(size=26,
                                 face='bold'),
          	plot.title = element_text(size=26,
                                 face='bold'))+
    	xlab('')+
    	ylab('')

  	#PERC UP MAP-------------------------------------------------
  	# mapUP <- ggplot(results) +
    # 	geom_sf(aes(fill=perc_UP), #actual map
    #     	    color='black',
    #         	size=0.5) +
    # 	geom_sf(data=states, #conus boundary
    #     	    color='black',
    #         	linewidth=0.75,
    #         	alpha=0)+
    # 	labs(tag='D')+
    # 	ggtitle('% upstream CO2 contributions')+
    # 	scale_fill_gradient2(name='',
    # 						low='white',
    # 						mid='#7ca982',
    # 						high='#243e36',
    # 						midpoint=50,
    # 						limits=c(0,100),
    #            	         	guide = guide_colorbar(direction = "horizontal",
    #                	                               title.position = "bottom"))+
    # 	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
    # 	theme(legend.position = c(0.2, 0.1),
    #     	  legend.key.size = unit(2, 'cm'),
    #     	  panel.background = element_rect(fill = '#e8998d', color = 'black'))+ #legend position settings
    # 	theme(text = element_text(family = "Futura-Medium"), #legend text settings
    #     	  legend.title = element_text(face = "bold", size = 18),
    #       	  legend.text = element_text(family = "Futura-Medium", size = 18),
    #       	  plot.tag = element_text(size=26,
    #         	                      face='bold'),
    #       	plot.title = element_text(size=26,
    #         	                      face='bold'))+
    # 	xlab('')+
    # 	ylab('')

	#PERC EXP MAP-------------------------------------------------
  	# mapEXP <- ggplot(results) +
    # 	geom_sf(aes(fill=perc_EXP), #actual map
    #     	    color='black',
    #         	size=0.5) +
    # 	geom_sf(data=states, #conus boundary
    #     	    color='black',
    #         	linewidth=0.75,
    #         	alpha=0)+
    # 	labs(tag='E')+
    # 	ggtitle('% CO2 to downstream')+
    # 	scale_fill_gradient2(name='',
    # 						low='white',
    # 						mid='#7ca982',
    # 						high='#243e36',
    # 						midpoint=50,
    # 						limits=c(0,100),
    #            	         	guide = guide_colorbar(direction = "horizontal",
    #                	                               title.position = "bottom"))+
    # 	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
    # 	theme(legend.position = c(0.2, 0.1),
    #     	  legend.key.size = unit(2, 'cm'),
    #     	  panel.background = element_rect(fill = '#465775', color = 'black'))+ #legend position settings
    # 	theme(text = element_text(family = "Futura-Medium"), #legend text settings
    #     	  legend.title = element_text(face = "bold", size = 18),
    #       	  legend.text = element_text(family = "Futura-Medium", size = 18),
    #       	  plot.tag = element_text(size=26,
    #         	                      face='bold'),
    #       	plot.title = element_text(size=26,
    #         	                      face='bold'))+
    # 	xlab('')+
    # 	ylab('')    	


  	#PERC UP MAP-------------------------------------------------
  	# mapEMIT <- ggplot(results) +
    # 	geom_sf(aes(fill=perc_EMIT), #actual map
    #     	    color='black',
    #         	size=0.5) +
    # 	geom_sf(data=states, #conus boundary
    #     	    color='black',
    #         	linewidth=0.75,
    #         	alpha=0)+
    # 	labs(tag='F')+
    # 	ggtitle('% CO2 to atmosphere')+
    # 	scale_fill_gradient2(name='',
    # 						low='white',
    # 						mid='#7ca982',
    # 						high='#243e36',
    # 						midpoint=50,
    # 						limits=c(0,100),
    #            	         	guide = guide_colorbar(direction = "horizontal",
    #                	                               title.position = "bottom"))+
    # 	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
    # 	theme(legend.position = c(0.2, 0.1),
    #     	  legend.key.size = unit(2, 'cm'),
    #     	  panel.background = element_rect(fill = '#465775', color = 'black'))+ #legend position settings
    # 	theme(text = element_text(family = "Futura-Medium"), #legend text settings
    #     	  legend.title = element_text(face = "bold", size = 18),
    #       	  legend.text = element_text(family = "Futura-Medium", size = 18),
    #       	  plot.tag = element_text(size=26,
    #         	                      face='bold'),
    #       	  plot.title = element_text(size=26,
    #         	                      face='bold'))+
    # 	xlab('')+
    # 	ylab('')

	#legend
	legend <- cowplot::get_legend(
    	mapBZ +
    		labs(tag = '')+
    		theme(legend.position = "bottom",
           		legend.text = element_text(size=24),
           		legend.title = element_text(size=26,
                	                       face='bold'),
           		legend.box="vertical",
           		legend.margin=margin(),
           		legend.spacing.x = unit(2, 'cm')) +
    		guides(color = guide_legend(override.aes = list(size=10))))


  ####OURCES BY ORDER-------------------
  combined_results_by_order <- readr::read_csv('sources_by_order_cira_6_4_23.csv') %>%
   	tidyr::gather(key=key, value=value, c('percGW_reach_median', 'percBZ_reach_median', 'percWC_reach_median'))
  # 	combined_results_by_order$huc2 <- substr(combined_results_by_order$method, 18, 19)
  # 	combined_results_by_order$huc4 <- substr(combined_results_by_order$method, 18, 21)
  # 	east <- c('0101', '0102', '0103', '0104', '0105', '0106', '0107', '0108', '0109', '0110', #all basins east of the Mississippi River (determined visually)
   #  	        '0202', '0203', '0206', '0207', '0208', '0204', '0205',
   #    	      '0301', '0302', '0303', '0304', '0305', '0306', '0307', '0308', '0309', '0310', '0311', '0312', '0313', '0314', '0315', '0316', '0317', '0318',
   #      	    '0401', '0402', '0403', '0404', '0405', '0406', '0407', '0408', '0409', '0410', '0411', '0412', '0413', '0414', '0420', '0427', '0429', '0430',
   #   	      	'0501', '0502', '0503', '0504', '0505', '0506', '0507', '0508', '0509', '0510', '0511', '0512', '0513', '0514',
   #    	      '0601', '0602', '0603', '0604',
   #      	    '0701', '0703', '0704', '0705', '0707', '0709', '0712', '0713', '0714',
   #        	  '0801', '0803', '0806', '0807', '0809',
   #        	  '0901', '0902', '0903', '0904')
  # 	west <- combined_results[!(combined_results$huc4 %in% c(east)),]$huc4
  
  # 	combined_results_by_order$region <- ifelse(combined_results_by_order$huc4 %in% east, 'East of Mississippi River','West of Mississippi River')
  
 	 # keepHUCs <- combined_results[is.na(combined_results$num_flowing_dys)==0,]$huc4
   # combined_results_by_order <- dplyr::filter(combined_results_by_order, huc4 %in% keepHUCs)

   #PLOT
   plotSources_by_order <- ggplot(combined_results_by_order, aes(fill=key, x=factor(StreamOrde), y=value*100)) +
     geom_boxplot(color='black', size=1.2)+
     xlab('Stream Order') +
     ylab('Median % of emissions')+
     scale_fill_manual(name='',
     									 labels=c('Hyporheic zone respiration', 'Groundwater', 'Net water-column respiration'),
                       values=c('#edae49', '#d1495b', '#00798c'))+
     ylim(0,100)+
     labs(tag='D')+
     theme(axis.title = element_text(size=26, face='bold'),
           axis.text = element_text(size=24,face='bold'),
           plot.tag = element_text(size=26,
                                   face='bold'),
           legend.position='bottom',
           legend.text = element_text(size=24))	


  	#COMBO PLOT---------------------------------------------
  	design <- "
  		ABC
  		ABC
  		ABC
  		DDD
  		EEE
  		EEE
  		EEE
  		EEE
  		EEE
  		EEE
  		EEE
  		EEE
  	"

  	comboPlot <- patchwork::wrap_plots(A=mapBZ + theme(legend.position='none'), B=mapGW + theme(legend.position='none'), C=mapWC + theme(legend.position='none'), D=legend, E=plotSources_by_order, design=design)


  	ggsave('cache/figures/mapSources.jpg', comboPlot, width=20, height=20)
  	return('see cache/figures/mapSources.jpg')
}







lakesMap <- function(path_to_data, results){
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

  	results <- dplyr::filter(results, is.na(contribGW_TgC_yr)==0 & is.na(lakeFCO2_TgC_yr)==0) #remove great lakes

  	#setup results to map
  	results$perc_Lakes <- round((results$lakeFCO2_TgC_yr/results$sumFCO2_TgC_yr )*100,0) #setup percent
  	results[!is.na(results$lakeFCO2_TgC_yr) & results$lakeFCO2_TgC_yr < 0,]$perc_Lakes <- 0 #if lakes are a sink, just set to 0

	  	#PERC LAKES MAP-------------------------------------------------
  	# cdf_inset <- ggplot(results, aes(perc_Lakes))+
    # 	stat_ecdf(size=2, color='black') +
    # 	xlim(0,100)+
    # 	xlab('% emissions from\nlakes/reservoirs')+
    # 	ylab('Probability')+
    # 	theme(axis.title = element_text(size=20),
    #     	  axis.text = element_text(family="Futura-Medium", size=18))+ #axis text settings
    # 	theme(legend.position = 'none') #legend position settings

  	mapLakes <- ggplot(results) +
    	geom_sf(aes(fill=perc_Lakes), #actual map
        	    color='black',
            	size=0.5) +
    	geom_sf(data=states, #conus boundary
        	    color='black',
            	linewidth=0.75,
            	alpha=0)+
    	scale_fill_gradient2(name='% emissions from lakes/reservoirs',
    						low='white',
    						mid='#427aa1',
    						high='#064789',
    						midpoint=50,
    						limits=c(0,100),
               	         	guide = guide_colorbar(direction = "horizontal",
                   	                               title.position = "bottom"))+
#    	scale_fill_gradientn(name='% emissions from lakes/reservoirs',
#        	                 colors=c('white', '#10002b'),
#            	             limits=c(0,50),
#                	         guide = guide_colorbar(direction = "horizontal",
#                    	                            title.position = "bottom"))+
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

     # mapLakes <- mapLakes +
     # 	patchwork::inset_element(cdf_inset, right = 0.975, bottom = 0.001, left = 0.775, top = 0.35)

  	ggsave('cache/figures/mapLakes.jpg', mapLakes, width=20, height=15)
  	return('see cache/figures/mapLakes.jpg')
}




#' prepping sf object for easy in-memory mapping
indvRiverMaps <- function(results, huc4){
	 huc2 <- substr(huc4, 1, 2)
	 huc4 <- ifelse(nchar(huc4)==5,substr(huc4,1,4),huc4)

	 if(huc4 %in% c('0418', '0419', '0424', '0426', '0428')){
	 	return(NA)
	 }
	else{
    	# Import shapefile
  		shapefile <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4, '_HU4_GDB/NHDPLUS_H_', huc4, '_HU4_GDB.gdb'), layer='NHDFlowline') %>%
  			sf::st_zm() %>%
  			dplyr::left_join(results, by='NHDPlusID') %>%
  			dplyr::filter(!is.na(StreamOrde))

  		#fix multicurves (if necessary)
  		shapefile <- fixGeometries(shapefile)
  	
  		# Adding column based on other column:
  		fin<-shapefile %>%
    		dplyr::mutate(CO2_col = dplyr::case_when(
      			FCO2_gC_m2_yr <= 500 ~ '0-500'
      			,FCO2_gC_m2_yr <= 1000 ~ '500-1000'
      			,FCO2_gC_m2_yr <= 2500 ~ '1000-2500'
      			,FCO2_gC_m2_yr <= 5000 ~ '2500-5000'
      			,FCO2_gC_m2_yr <= 10000 ~ '5000-10000'
      			,TRUE ~ '10000+'
    			)) %>%
    		dplyr::select(c('CO2_col', 'Q_m3_s', ))

    	fin$CO2_col <- factor(fin$CO2_col, levels = c('0-500', '500-1000', '1000-2500', '2500-5000', '5000-10000', '10000+'))
  	return(fin)
	}
}






#' actual mapping of entire US river network
mainMapFunction <- function(mapList, map_0205, map_0206,map_0207,map_0208,map_0204, map_0413, map_0501, map_0505){
	#remove NAs (great lakes)
	mapList <- mapList[!is.na(mapList)]

	theme_set(theme_classic())

	# CONUS boundary--------------------------------------
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

  	#BIG MAIN MAP------------------------------------------
	bigMap <- ggplot()+
		geom_sf(data=mapList[[1]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[2]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[3]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[4]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[5]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[6]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[7]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[8]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[9]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[10]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[11]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[12]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[13]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[14]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[15]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[16]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[17]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[18]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[19]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[20]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[21]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[22]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[23]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[24]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[25]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[26]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[27]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[28]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[29]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[30]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[31]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[32]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[33]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[34]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[35]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[36]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[37]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[38]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[39]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[40]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[41]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[42]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[43]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[44]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[45]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[46]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[47]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[48]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[49]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[50]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[51]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[52]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[53]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[54]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[55]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[56]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[57]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[58]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[59]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[60]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[61]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[62]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[63]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[64]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[65]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[66]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[67]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[68]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[69]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[70]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[71]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[72]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[73]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[74]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[75]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[76]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[77]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[78]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[79]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[80]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[81]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[82]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[83]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[84]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[85]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[86]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[87]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[88]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[89]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[90]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[91]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[92]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[93]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[94]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[95]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[96]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[97]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[98]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[99]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[100]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[101]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[102]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[103]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[104]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[105]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[106]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[107]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[108]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[109]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[110]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[111]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[112]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[113]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[114]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[115]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[116]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[117]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[118]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[119]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[120]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[121]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[122]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[123]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[124]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[125]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[126]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[127]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[128]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[129]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[130]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[131]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[132]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[133]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[134]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[135]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[136]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[137]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[138]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[139]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[140]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[141]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[142]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[143]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[144]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[145]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[146]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[147]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[148]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[149]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[150]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[151]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[152]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[153]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[154]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[155]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[156]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[157]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[158]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[159]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[160]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[161]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[162]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[163]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[164]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[165]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[166]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[167]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[168]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[169]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[170]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[171]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[172]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[173]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[174]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[175]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[176]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[177]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[178]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[179]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[180]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[181]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[182]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[183]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[184]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[185]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[186]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[187]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[188]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[189]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[190]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[191]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[192]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[193]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[194]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[195]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[196]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[197]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[198]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[199]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[200]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[201]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[202]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[203]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[204]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[205]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[206]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data = mapList[[207]], aes(color = CO2_col),linewidth=0.1) +
		geom_sf(data=states, #conus boundary
        	    color='black',
            	linewidth=0.75,
            	alpha=0)+
		scale_color_brewer(palette='RdYlBu', direction=-1,name='FCO2 [gC/m2/yr]')+
 		theme(plot.title = element_text(face = "italic", size = 26),
    	  	axis.text = element_text(size = 22),
        	plot.tag = element_text(size=26,
             	                  face='bold'),
        	legend.position=c(0.9,0.15),
        #	legend.key.size = unit(2, 'cm'),
 			legend.text = element_text(size=18),
 			legend.title = element_text(size=22,face="bold"),
 			legend.spacing.y = unit(0.1, 'cm'))+
 		guides(color = guide_legend(override.aes = list(linewidth=8), byrow = TRUE))

  	#INSET CENTERING----------------------------------
   	zoom_to <- c(-77.87450,39.52966)# c(-87.87,32.89) # inset centers

   	#set up zoom bounds
  	zoom_level <- 3
 	lon_span <- 360 / 5^zoom_level
 	lat_span <- 360 / 5^zoom_level
 	lon_bounds_1 <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
 	lat_bounds_1 <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

 	#set up inset box
     df <- data.frame(lon_bounds_1, lat_bounds_1)
 	box_1 <- df %>% 
   		sf::st_as_sf(coords = c("lon_bounds_1", "lat_bounds_1"), 
            		crs = 4326) %>% 
   		sf::st_bbox() %>% 
   		sf::st_as_sfc()

  	#set up zoom bounds
 	zoom_level <- 4
	lon_span <- 360 / 5^zoom_level
	lat_span <- 360 / 5^zoom_level
	lon_bounds_2 <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
	lat_bounds_2 <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

	#set up inset box
    df <- data.frame(lon_bounds_2, lat_bounds_2)
	box_2 <- df %>% 
  		sf::st_as_sf(coords = c("lon_bounds_2", "lat_bounds_2"), 
           		crs = 4326) %>% 
  		sf::st_bbox() %>% 
  		sf::st_as_sfc()

  	#set up zoom bounds
 	zoom_level <- 5
	lon_span <- 360 / 5^zoom_level
	lat_span <- 360 / 5^zoom_level
	lon_bounds_3 <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
	lat_bounds_3 <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

	#set up inset box
    df <- data.frame(lon_bounds_3, lat_bounds_3)
	box_3 <- df %>% 
  		sf::st_as_sf(coords = c("lon_bounds_3", "lat_bounds_3"), 
           		crs = 4326) %>% 
  		sf::st_bbox() %>% 
  		sf::st_as_sfc()

  	#set up zoom bounds
 	zoom_level <- 6
	lon_span <- 360 / 5^zoom_level
	lat_span <- 360 / 5^zoom_level
	lon_bounds_4 <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
	lat_bounds_4 <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

	#set up inset box
    df <- data.frame(lon_bounds_4, lat_bounds_4)
	box_4 <- df %>% 
  		sf::st_as_sf(coords = c("lon_bounds_4", "lat_bounds_4"), 
           		crs = 4326) %>% 
  		sf::st_bbox() %>% 
  		sf::st_as_sfc()  		


  	#for insets
  	insetNet <- rbind(map_0205, map_0206,map_0207,map_0208,map_0204, map_0413, map_0501, map_0505)

  	#CUSTOM COLOR SCALE-------------------
	myColors <- rev(RColorBrewer::brewer.pal(6,"RdYlBu"))
	names(myColors) <- levels(insetNet$CO2_col)
	colScale <- scale_colour_manual(name = "CO2_col",values = myColors)

  	#INSET 1--------------------------------------------------
  	insetShp1 <- sf::st_crop(insetNet, xmin=lon_bounds_1[1], xmax=lon_bounds_1[2], ymin=lat_bounds_1[1], ymax=lat_bounds_1[2])
  	inset1 <- ggplot() +
  		geom_sf(data = insetShp1, aes(color = CO2_col,linewidth=Q_m3_s)) +
	  	scale_linewidth_binned(name='Discharge [cms]',
        	breaks=c(0.01,0.1, 1, 10,100,1000),
            range=c(0.3,2.5))+  		
  		geom_sf(data=box_2,
    		color='#fca311',
    		linewidth=3,
    		alpha=0) +		
  		ggsn::scalebar(data=insetShp1,location='bottomleft', dist = 50, dist_unit = "km",transform = TRUE, model = "WGS84", box.fill=c('white','red'),st.color='white',st.dist=0.05,border.size=0.05) +  
  		xlab('')+
  		ylab('')+
  		coord_sf(datum=NA) +
	#	scale_color_brewer(palette='RdYlBu', direction=-1,name='FCO2 [gC/m2/yr]')+  				
    	theme(legend.position='none',
    		  panel.background = element_rect(fill = "black"))

   	#INSET 2--------------------------------------------------
  	insetShp2 <- sf::st_crop(insetNet, xmin=lon_bounds_2[1], xmax=lon_bounds_2[2], ymin=lat_bounds_2[1], ymax=lat_bounds_2[2])    
  	inset2 <- ggplot() +
  		geom_sf(data = insetShp2, aes(color = CO2_col,linewidth=Q_m3_s)) +
	  	scale_linewidth_binned(name='Discharge [cms]',
        	breaks=c(0.01,0.1, 1, 10,100,1000),
            range=c(0.3,2.5))+  		
  		geom_sf(data=box_3,
    		color='#fca311',
    		linewidth=3,
    		alpha=0) +
  		ggsn::scalebar(data=insetShp2,location='bottomleft', dist = 10, dist_unit = "km",transform = TRUE, model = "WGS84", box.fill=c('white','red'),st.color='white',st.dist=0.05,border.size=0.05) +  
  		xlab('')+
  		ylab('')+  		
  		coord_sf(datum=NA) +
	#	scale_color_brewer(palette='RdYlBu', direction=-1,name='FCO2 [gC/m2/yr]')+  				
  		#viridis::scale_color_viridis(option='mako',discrete=TRUE, name='CO2 [ppm]') +
     	theme(legend.position='none',
     		  panel.background = element_rect(fill = "black"))

  	#INSET 3--------------------------------------------------
  	insetShp3 <- sf::st_crop(insetNet, xmin=lon_bounds_3[1], xmax=lon_bounds_3[2], ymin=lat_bounds_3[1], ymax=lat_bounds_3[2])
 # 	waterbodies <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_03/NHDPLUS_H_0316_HU4_GDB/NHDPLUS_H_0316_HU4_GDB.gdb'), layer='NHDWaterbody') %>%
 # 		dplyr::filter(FType %in% c(390, 436))
 #   waterbodies <- sf::st_crop(waterbodies, xmin=lon_bounds_3[1], xmax=lon_bounds_3[2], ymin=lat_bounds_3[1], ymax=lat_bounds_3[2])
    #waterbodies <- sf::st_join(waterbodies, insetShp3)

  	inset3 <- ggplot() +
  		geom_sf(data = insetShp3, aes(color = CO2_col,linewidth=Q_m3_s)) +
#  		geom_sf(data = waterbodies, alpha=0, color='grey',size=1) +  		
	  	scale_linewidth_binned(name='Discharge [cms]',
        	breaks=c(0.01,0.1, 1, 10,100,1000),
            range=c(0.3,2.5))+  		
  		geom_sf(data=box_4,
    		color='#fca311',
    		linewidth=3,
    		alpha=0) +
    	ggsn::scalebar(data=insetShp3,location='bottomleft', dist = 2, dist_unit = "km",transform = TRUE, model = "WGS84", box.fill=c('white','red'),st.color='white',st.dist=0.05,border.size=0.05) +
    	xlab('')+
  		ylab('')+
  		coord_sf(datum=NA) +
	#	scale_color_brewer(palette='RdYlBu', direction=-1,name='FCO2 [gC/m2/yr]')+  
		#scale_fill_brewer(palette='RdYlBu', direction=-1,name='CO2 [ppm]')+						
     	theme(legend.position='none',
     		  panel.background = element_rect(fill = "black"))

  	#INSET 4-------------------------------------------------
  	insetShp4 <- sf::st_crop(insetNet, xmin=lon_bounds_4[1], xmax=lon_bounds_4[2], ymin=lat_bounds_4[1], ymax=lat_bounds_4[2])
 # 	waterbodies <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_03/NHDPLUS_H_0316_HU4_GDB/NHDPLUS_H_0316_HU4_GDB.gdb'), layer='NHDWaterbody') %>%
  #		dplyr::filter(FType %in% c(390, 436))
   # waterbodies <- sf::st_crop(waterbodies, xmin=lon_bounds_4[1], xmax=lon_bounds_4[2], ymin=lat_bounds_4[1], ymax=lat_bounds_4[2])
  #  waterbodies <- sf::st_join(waterbodies, insetShp4,  join = st_contains)

  	inset4 <- ggplot() +
  		geom_sf(data = insetShp4, aes(color = CO2_col,linewidth=Q_m3_s)) +
	#	geom_sf(data = waterbodies, aes(fill = CO2_col)) +  		
	  	scale_linewidth_binned(name='Discharge [cms]',
        	breaks=c(0.01,0.1, 1, 10,100,1000),
            range=c(0.3,2.5))+  		
    	ggsn::scalebar(data=insetShp4,location='bottomleft', dist = 0.5, dist_unit = "km",transform = TRUE, model = "WGS84", box.fill=c('white','red'),st.color='white',st.dist=0.05,border.size=0.05) +  
  		xlab('')+
  		ylab('')+
  		coord_sf(datum=NA) +  
	#	scale_color_brewer(palette='RdYlBu', direction=-1,name='FCO2 [gC/m2/yr]')+  
	#	scale_fill_brewer(palette='RdYlBu', direction=-1,name='FCO2 [gC/m2/yr]')+  								
     	theme(legend.position='none',
     		  panel.background = element_rect(fill = "black"))

    #ADD INSET 1 BOUNDING BOXES---------------------------------
    bigMap <- bigMap +
    	geom_sf(data=box_1,
    			color='#fca311',
    			linewidth=3,
    			alpha=0)

  	#COMBO PLOT---------------------------------------------
  	 design <- "
  	 	BCDE
  	 "

  	 comboPlot <- patchwork::wrap_plots(B=inset4 + colScale, C=inset3 + colScale, D=inset2 + colScale, E=inset1 + colScale, design=design)

	ggsave(filename="cache/figures/mainMap_1.jpg",plot=bigMap,width=20,height=15)
	ggsave(filename="cache/figures/mainMap_2.jpg",plot=comboPlot,width=20,height=5)
	
	return('see cache/figures/')
}






compareAgainstLumped <- function(path_to_data, results){
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
            	linewidth=0.75,
            	alpha=0)+
    	scale_fill_gradientn(name='% difference in carbon\nemissions from lumped',
        	                 colors=c('#e63946', 'white', '#1d3557'),
            	             limits=c(-100,100),
                	         guide = guide_colorbar(direction = "horizontal",
                    	                            title.position = "bottom"))+
    	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
    	theme(legend.position = 'bottom',
        	  legend.key.size = unit(2, 'cm'))+ #legend position settings
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
        	  legend.title = element_text(face = "bold", size = 18),
          	legend.text = element_text(family = "Futura-Medium", size = 18),
          	plot.tag = element_text(size=26,
            	                      face='bold'))+
    	xlab('')+
    	ylab('')


    ggsave('cache/figures/lumpedRegionalCompare.jpg', map, width=10, height=10)
    return('see cache/figures/comparisonHUC2.jpg')
}










# glorichCompare <- function(ourModel){
# 	theme_set(theme_classic())

# #	ourModel <- do.call("rbind", modelList) #make raymond model object
# 	glorich <- readr::read_csv('data/glorich_rocher_ros_2019.csv')
# 	glorich_shp <- sf::st_as_sf(x=glorich,
#                    coords = c("Longitude", "Latitude"),
#                		crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# 	ourModel <- ourModel %>%
# 		dplyr::select(c('CO2_ppm', 'k600_m_s')) %>%
# 		dplyr::mutate(type='model')
# #		dplyr::filter(waterbody == 'River')

# 	glorich <- glorich %>%
# 		dplyr::mutate(k600_m_s = k600/86400,
# 					  type='glorich') %>%
# 		dplyr::select(c('pco2', 'k600_m_s', 'type'))
# 	colnames(glorich) <- c('CO2_ppm', 'k600_m_s', 'type')

# 	#little map
# 	library(rnaturalearth)
# 	library(rnaturalearthdata)
# 	world <- ne_countries(scale = "medium", returnclass = "sf")

# 	map_world <- ggplot(glorich_shp, aes(color=STAT_ID)) +
# 		geom_sf(color='#102542')+
#     	geom_sf(data=world, #conus boundary
#         	    color='black',
#             	linewidth=0.25,
#             	alpha=0) +
#     	coord_sf(expand = FALSE) +
#     	    	theme(axis.title = element_text(face = "bold", size = 24),
#     		  axis.text = element_text(size = 22),
#         	  plot.tag = element_text(size=26,
#             	                  face='bold'))
   	
#    	#CDFs
#    	forPlot <- rbind(glorich, ourModel)

#    	co2CDF <- ggplot(forPlot, aes(x=CO2_ppm, color=type)) +
#    		stat_ecdf(size=1.25) +
#    		labs(tag='A')+
#    		scale_x_log10()+
#   		theme(legend.text = element_text(size=22))+
#     	theme(axis.title = element_text(face = "bold", size = 24),
#     		  axis.text = element_text(size = 22),
#         	  plot.tag = element_text(size=26,
#             	                  face='bold'))+
#     	xlab('CO2 [ppm]')+
#     	ylab('Percentile') 

#    	kCDF <- ggplot(forPlot, aes(x=k600_m_s, color=type)) +
#    		stat_ecdf(size=1.25) +
#    		labs(tag='B')+
#    		scale_x_log10()+
#   		theme(legend.text = element_text(size=22))+
#     	theme(axis.title = element_text(face = "bold", size = 24),
#     		  axis.text = element_text(size = 22),
#         	  plot.tag = element_text(size=26,
#             	                  face='bold'))+
#     	xlab('k600 [m/s]')+
#     	ylab('Percentile')  

#     # #glorich plot
#   	# glorichPlot <- ggplot(glorich, aes(x=k600/86400, y=pco2))+
#   	# 	geom_point(size=4, alpha=0.4, color='#102542') +
#   	# #	xlim(0,100)+
#   	# 	ylim(0,24000)+
#   	# 	labs(tag='B')+
#   	# 	theme(legend.position='none')+
#     # 	theme(axis.title = element_text(face = "bold", size = 24),
#     # 		  axis.text = element_text(size = 22),
#     #     	  plot.tag = element_text(size=26,
#     #         	                  face='bold'))+
#     # 	xlab('k600 [m/s]')+
#     # 	ylab('')

#     # glorichPlot <- glorichPlot +
#     # 	patchwork::inset_element(map_world, right = 0.98, bottom = 0.6, left = 0.28, top = 1.1)

#     # #model plot
#   	# ourPlot <- ggplot(ourModel, aes(x=k600_m_s, y=CO2_ppm))+
#   	# 	geom_point(size=4, alpha=0.4, color='#ffba49') +
#   	# 	xlim(0,0.02)+
#   	# 	ylim(0,24000)+
#   	# 	labs(tag='A')+
#   	# 	theme(legend.text = element_text(size=22))+
#     # 	theme(axis.title = element_text(face = "bold", size = 24),
#     # 		  axis.text = element_text(size = 22),
#     #     	  plot.tag = element_text(size=26,
#     #         	                  face='bold'))+
#     # 	xlab('k600 [m/s]')+
#     # 	ylab('CO2 [ppm]')    	


#   	#COMBO PLOT---------------------------------------------
#   	design <- "
#   		AB
#   	"

#   	comboPlot <- patchwork::wrap_plots(A=co2CDF, B=kCDF, design=design)

# 	ggsave(filename="cache/figures/glorichCompare.jpg",plot=comboPlot,width=20,height=10)
# 	return('see cache/figures/glorichCompare.jpg')    	
# }