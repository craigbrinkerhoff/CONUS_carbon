##########################
## Make paper figures
## Craig Brinkerhoff/Brian Saccardi
## Summer 2023
##########################







#' build nhd discharge validation figure
#'
#' @name eromValidationFig
#'
#' @param USGS_data: df of gauge data at mean annual flow
#' @param nhdGages: df of NHD model discharges
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return discharge validation figure written to file
eromValidationFig <- function(USGS_data, nhdGages){
	#set ggplot theme
	theme_set(theme_classic())

	#WRANGLE VALIDATION DF-------------------------------------
	qma <- USGS_data
  	qma <- dplyr::select(qma, c('gageID','Q_MA'))
  	assessmentDF <- dplyr::left_join(nhdGages, qma, by=c('GageIDMA' = 'gageID')) %>%
  		drop_na()

	#BUILD PLOT------------------------------------------------
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

	#WRITE TO FILE-------------------------------------------------
  	ggsave('cache/figures/eromValidation.jpg', eromVerification_QBMA, width=10, height=10)
  	
	#RETURN FILE PATH--------------------------------------------
	return(paste('see cache/figures/eromValidation.jpg'))
}







#' build figures of calibration performance
#'
#' @name calibrationFigures
#'
#' @param combined_calib: list of calibration results
#'
#' @import patchwork
#' @import cowplot
#' @import gpplot2
#'
#' @return calibration figures written to file
calibrationFigures <- function(combined_calib){
	#set ggplot theme
	theme_set(theme_classic())

	#grab calibration fitness scores across basins
	combined_fitness <- sapply(combined_calib, function(x){return(x$fitness)})
	combined_fitness <- (1/combined_fitness) #convert to ppm per 'median river/lake/reservoir' reach

	#SUMMARY PLOT-----------------------------------------------------------------------------------------
	#wrangle fitness scores and basin names
	forPlot <- data.frame('huc4'=substr(names(combined_fitness), 22,26),
						  'fitness'=combined_fitness)

	#build calibration summary plot (the eCDFs)
	plot <- ggplot(forPlot, aes(x=fitness)) +
		stat_ecdf(size=2, color='black') +
		geom_vline(xintercept = median(forPlot$fitness, na.rm=T), color='darkgrey', linetype='dotted', size=1.5) + 
    	geom_hline(yintercept = 0.50, color='darkgrey', linetype='dotted', size=1.5) +
    	annotate('text', label=paste0('median error across\nbasins: ', round(median(forPlot$fitness, na.rm=T),0), ' ppm'), x=1000, y=0.65, size=8)+
		xlab('Calibration error per basin (ppm)') +
		ylab('Probability') +
		theme(axis.title = element_text(size=20),
          axis.text = element_text(family="Futura-Medium", size=18))+ #axis text settings
    	theme(legend.position = 'none') #legend position settings

	#write to file
	ggsave('cache/figures/calib/calibrationSummary.jpg', plot, width=8, height=8)

	#BUILD CLAIBRATION FITNESS MULTI-PANEL PLOTS (supp figures)-----------------------------------------------------------------------------
	ticker <- 1
	a <- 1
	b <- 20
	while(ticker <= 11){ #pretty janky way to iterate through the basins but it gets the job done. Handles combo plots of multiple number of subplots
		c <- 1
		b <- ifelse(ticker == 11, 212,b)
		plot <- list()
	  	while(a <= b){
			huc4 <- substr(names(combined_calib[a]),22,26)
			basin <- combined_calib[[a]]

			if(is.na(basin$plot) || huc4 == '0427_'){ #skip NAs (great lakes)
				a <- a + 1
				b <- b + 1
				next
			}

			#build plot
			plot[[c]] <- basin$plot +
				ggtitle(huc4)+
				scale_y_log10()+
				ylab('')+
				xlab('') +
				theme(plot.title = element_text(size=30),
          	  		axis.text = element_text(family="Futura-Medium", size=18))
			
			a <- a + 1
			c <- c + 1
		}

		#extract legend
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

		#extract x-axis
		x_axis <- cowplot::get_plot_component(
			ggplot() +
  	  			labs(x = "Generation") +
  	  			theme(axis.title = element_text(size=30)), "xlab-b")

		#extract y-axis
		y_axis <- cowplot::get_plot_component(
			ggplot() +
  	  			labs(y = "Calibration Error (ppm)") +
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

			#build combo plot by indexing list of ggplots
  			comboPlot <- patchwork::wrap_plots(A=plot[[1]] + theme(legend.position='none'), B=plot[[2]] + theme(legend.position='none'), C=plot[[3]] + theme(legend.position='none'), D=plot[[4]] + theme(legend.position='none'),
        	                	             E=plot[[5]] + theme(legend.position='none'), F=plot[[6]] + theme(legend.position='none'), G=plot[[7]] + theme(legend.position='none'), H=plot[[8]] + theme(legend.position='none'),
            	        	                 I=plot[[9]] + theme(legend.position='none'), J=plot[[10]] + theme(legend.position='none'), K=plot[[11]] + theme(legend.position='none'), L=plot[[12]] + theme(legend.position='none'),
                		                     M=plot[[13]] + theme(legend.position='none'), N=plot[[14]] + theme(legend.position='none'), O=plot[[15]] + theme(legend.position='none'), P=plot[[16]] + theme(legend.position='none'),
                	    	                 Q=plot[[17]] + theme(legend.position='none'), R=plot[[18]] + theme(legend.position='none'), S=plot[[19]] + theme(legend.position='none'), T=plot[[20]] + theme(legend.position='none'),
            	            	             U=x_axis, V=legend, W=y_axis, design=design)

			#write to file
  			ggsave(paste0('cache/figures/calib/comboPlot_', ticker, '.jpg'), comboPlot, width=20, height=20)

			#next figure
  			ticker <- ticker + 1
  			b <- b + 20
    	}

		#build 6 panel plot of calibration performance (last six basins)
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

			#build plot by indexing list of ggplots
  			comboPlot <- patchwork::wrap_plots(A=plot[[1]] + theme(legend.position='none'), B=plot[[2]] + theme(legend.position='none'), C=plot[[3]] + theme(legend.position='none'), D=plot[[4]] + theme(legend.position='none'),
        	                	             E=plot[[5]] + theme(legend.position='none'), F=plot[[6]] + theme(legend.position='none'),
            	            	             U=x_axis, V=legend, W=y_axis, design=design)

			#write to file
  			ggsave(paste0('cache/figures/calib/comboPlot_', ticker, '.jpg'), comboPlot, width=20, height=15)

			#next figure
  			ticker <- ticker + 1
  		}
	}
  
  return('all calibration results written to file at ~/cache/figures/calib/')
}









#' build main text figure comparing transport and upscaling models
#'
#' @name compareModels
#'
#' @param path_to_data: character path to data repo
#' @param ourModel: df of random sample of model rivers (n per basin for all basins)
#' @param lumpedList: upscaling model results
#' @param glorich: in situ data df (glorich database)
#'
#' @import patchwork
#' @import sf
#' @import tidyr
#' @import readr
#' @import gpplot2
#' @import dplyr
#' @import reshape2
#'
#' @return print statement (figure written to file)
compareModels <- function(path_to_data, ourModel, lumpedList, glorich) {
	#set ggplot theme
  	theme_set(theme_classic())

	#make lumped model object
	lumped <- do.call("rbind", lumpedList)

  	#BUILD BARPLOTS FIGURE--------------------------------------------
    #sum emissions across all basins, by model
    numbers <- data.frame('Distributed'=sum(lumped$sumFCO2_TgC_yr, na.rm=T),    					  
    					  'Lumped_full'=sum(lumped$sumFCO2_lumped_TgC_yr, na.rm=T),
    					  'cal_sigma'=sum(lumped$cal_uncertainty, na.rm=T),
						  'lumped_sigma'=sum(lumped$lumped_uncertainty, na.rm=T))

	#wrangle df into ggplot-friendly format
  	forPlot <- tidyr::gather(numbers, key=key, value=value, c('Distributed', 'Lumped_full'))
  	forPlot[forPlot$key != 'Distributed',]$cal_sigma <- NA #don't apply to upscaling model
	forPlot[forPlot$key != 'Lumped_full',]$lumped_sigma <- NA #don't apply to transport model
  	forPlot$key <- factor(forPlot$key, levels=c('Lumped_full', 'Distributed'))

	#build plot
  	bars <- ggplot(forPlot, aes(x=key, y=value, fill=key)) +
  		geom_col(size=1.5, color='black', size=1.5) +
  		geom_errorbar(aes(ymin=value-cal_sigma, ymax=value+cal_sigma), width=.15, size=1.75) +
  		geom_errorbar(aes(ymin=value-lumped_sigma, ymax=value+lumped_sigma), width=.15, size=1.75) +
  		scale_fill_manual(values=c('#006e90', '#fe7f2d'))+
  		scale_x_discrete(labels=c('Upscaling', 'Transport'))+
  		labs(tag='C')+  		
  		xlab('')+
  		ylab(expression(bold(FCO[2]~(Tg-C/yr)))) +
  		theme(legend.position='none') +
  		theme(axis.title = element_text(size=26, face='bold', color='black'), axis.text = element_text(size=20, color='black'))+ #axis text settings
    	theme(plot.tag = element_text(size=26,face='bold'))


    #SETUP GLORICH COMPARISON----------------------------------------------------------------
    #hydraulic geometry parameters
  	depAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/depAHG.rds') #these are hard coded here. the models are available at the ref in the paper
  	widAHG <- readr::read_rds('/nas/cee-water/cjgleason/craig/RSK600/cache/widAHG.rds')
  	glorich$a <- widAHG$coefficients[1]
  	glorich$b <- widAHG$coefficients[2]
  	glorich$c <- depAHG$coefficients[1]
  	glorich$f <- depAHG$coefficients[2]

  	#get w, d, v treating all as rivers
  	glorich$D <- exp(glorich$c) * glorich$nhdQ_cms^glorich$f
  	glorich$W <- exp(glorich$a) * glorich$nhdQ_cms^glorich$b
  	glorich$V <- glorich$nhdQ_cms / (glorich$D*glorich$W)

	#get eD and k600
  	glorich$eD <- 9.8*glorich$V*glorich$nhd_slope #[m2/s3] Ulseth et al 2019
  	glorich$k600_m_dy <- ifelse(glorich$eD <= 0.02, exp(3.10+0.35*log(glorich$eD)), exp(6.43+1.18*log(glorich$eD))) #Ulseth etal 2019

	#filter for in situ data within our snapping threshold of 10m
  	glorich$snap_distance_m_num <- as.numeric(glorich$snap_distance_m)
  	glorich <- dplyr::filter(glorich, snap_distance_m_num <= 10) %>%
  		dplyr::filter(pco2 > 400)

	#build shapefile from these data (for inset map)
	glorich_shp <- readr::read_csv('data/glorich_rocher_ros_2019.csv')
	glorich_shp <- sf::st_as_sf(x=glorich_shp,
                coords = c("Longitude", "Latitude"),
           		crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
			dplyr::filter(STAT_ID %in% glorich$STAT_ID)

	# read in CONUS boundary for inset map
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

  	#BUILD ACTUAL INSET MAP------------------------------------------
	map_world <- ggplot(glorich_shp, aes(color=STAT_ID)) +
		geom_sf(color='#102542')+
    	geom_sf(data=states, #conus boundary
        	    color='black',
            	linewidth=0.25,
            	alpha=0) +
    	coord_sf(expand = FALSE) +
    	labs(tag='B')+
    	theme(axis.title = element_text(face = "bold", size = 18),
    		  axis.text = element_text(size = 18),
              axis.text.x = element_text(angle = 90),
              plot.tag = element_text(size=26,
            	                  face='bold'))

    #WRANGLE LUMPED UPSCALING MODEL-----------------------------------
    lumpedModel <- dplyr::select(ourModel, c('lumped_k600_m_dy', 'lumped_CO2')) %>%
    	dplyr::mutate(model='Upscaling') %>%
    	dplyr::select('model', 'lumped_k600_m_dy', 'lumped_CO2') %>%
    	dplyr::distinct(lumped_k600_m_dy, .keep_all = TRUE)
    colnames(lumpedModel) <- c('model', 'k600_m_dy', 'CO2_ppm')

    #WRANGLE OUR TRANSPORT MODEL-----------------------------------
    ourModel <- dplyr::select(ourModel, c('k600_m_s', 'CO2_ppm')) %>%
    	dplyr::mutate(k600_m_dy = k600_m_s*86400,
    								model='Transport') %>%
    	dplyr::select(c('model','k600_m_dy', 'CO2_ppm'))

    #WRANGLE IN SITU DATA------------------------------------------
    glorich <- dplyr::select(glorich, c('k600_m_dy', 'pco2'))
    glorich$model <- 'In situ data'
    colnames(glorich) <- c('k600_m_dy', 'CO2_ppm','model')
    glorich <- dplyr::select(glorich, c('model', 'k600_m_dy', 'CO2_ppm'))

    #BUILD FCO2 VARIABLE SPACE (adapted from https://github.com/rocher-ros/co2_domains_publication/blob/master/co2_domains_figs_stats.R). Thanks!!------------------------------------------------------
	pco2 <- seq(from=4600, to= 401, by=-10)
	k600 <- seq(from= 1, to=850, length.out = length(pco2) )
	kpco2 <- list( k600, pco2)

	x <- matrix( nrow=length(pco2), ncol=length(pco2), dimnames = kpco2) 

	#We use k600, which is CO2 at 20 degrees (and 400 ppm assumed for atmospheric)
	Henrys <- henry_func(20)

	for(i in 1:length(pco2)){
  	for( j in 1:length(k600)){
    	x[i,j] <- (k600[i]*(pco2[j]-400)*Henrys*1e-6)*(1/0.001)*12.01 #g-C/m2/dy
  	  }
	}

	tx <- setNames(reshape2::melt(x), c('k', 'co2', 'fco2'))
	tx100<- subset(tx, fco2 < 100)

	#make map bins for fco2
	tx<-tx %>%
    		dplyr::mutate(fco2_col = dplyr::case_when(
      			fco2*1e-3 <= 0.1 ~ '0-0.1'
      			,fco2*1e-3 <= 0.25 ~ '0.1-0.25'
      			,fco2*1e-3 <= 0.5 ~ '0.25-0.5'
      			,fco2*1e-3 <= 1 ~ '0.5-1'
      			,fco2*1e-3 <= 5 ~ '1-5'
      			,TRUE ~ '5+'
    			)) 


    #COMBINE ALL THREE CO2/k600 PAIRINGS INTO A SINGLE DF---------------------------------------------------
    forPlot <- rbind(ourModel, glorich, lumpedModel)
    cols <- c("In situ data"="#102542","Transport"="#fe7f2d","Upscaling"="#006e90") #custom color scheme

    #PLOT 2D KERNEL DENSITY DIAGRAM COMPARING MODELS AND IN SITU DISTRIBUTIONS---------------------------------------------
	#build plot
  	glorichPlot <- ggplot(forPlot)+
  	    geom_tile(data=tx, aes(x=k, y=co2, fill=fco2_col), alpha=0.7, show.legend= T) +
  		geom_density_2d(data=forPlot[forPlot$model == 'In situ data',], aes(x=k600_m_dy, y=CO2_ppm, color='In situ data', linewidth=..level..), linetype='dashed', size=1.5, bins=5, contour_var='density')+
		geom_density_2d(data=forPlot[forPlot$model == 'Transport',], aes(x=k600_m_dy, y=CO2_ppm, color='Transport', linewidth=..level..), linetype='solid', size=1.5, bins=5, contour_var='density')+  		
  		geom_density_2d(data=forPlot[forPlot$model == 'Upscaling',], aes(x=k600_m_dy, y=CO2_ppm, color='Upscaling', linewidth=..level..), linetype='solid', size=1.5, bins=5, contour_var='density')+  		
  		scale_color_manual(name=expression(bold(Distribution)),values=cols, guide=guide_legend(keywidth=unit(1.5, 'cm'), override.aes=list(linetype=c('dashed','solid', 'solid'), linewidth=5, fill='grey')))+
  		scale_fill_grey(name=expression(atop(bold(FCO[2]~(Kg-C/m^2/yr)), (at~20~degrees))), start=0.95, end=0.2)+
  		labs(tag='A')+
		theme(legend.position=c(0.75, 0.75),
			  legend.box.just = "right",
  	 		  legend.text=element_text(size=24),
  	 		  legend.title=element_text(size=26),
			  legend.background = element_rect(fill=alpha('white', 0.7)))+
    	theme(axis.title = element_text(face = "bold", size = 24),
    		  axis.text = element_text(size = 22),
        	  plot.tag = element_text(size=26,
            	                  face='bold'))+
    	xlab('')+
    	xlab(expression(bold(k[600]~(m/dy))))+
    	ylab(expression(bold(CO[2]~(ppm)))) +
    	guides(linewidth='none')

  	#PLOT DESIGN------------------------------------------------------------
  	design <- "
  		AAAB
  		AAAC
  	"

	#BUILD COMBO PLOT---------------------------------------------------------
  	comboPlot <- patchwork::wrap_plots(A=glorichPlot, B=map_world, C=bars,design=design)

	#WRITE TO FILE------------------------------------------------------------
  	ggsave('cache/figures/modelsCompare.jpg', comboPlot, width=18, height=14)

	#RETURN UNCERTAINTIES (just for some manual checking and verification)
  	return(numbers)
}






#' produce figure comparing transport and upscaling models
#'
#' @name conceptualPlot
#'
#' @param final_model: our model for x basin
#' @param glorich_data: CO2 inputs for upscaling model
#' @param huc4_id: basin ID
#'
#' @import dplyr
#' @import patchwork
#' @import sf
#' @import ggsn
#' @import cowplot
#' @import stringr
#' @import ggplot2
#'
#' @return print statement (figure written to file)
conceptualPlot <- function(final_model, glorich_data, huc4_id){
	#set ggplot theme
	theme_set(theme_classic())

	#get huc2 id
	huc2 <- substr(huc4_id, 1, 2)

	##READ IN BASIN SHAPEFILE AND FILTER FOR THE BASIN OF INTEREST-------------------------------------------------------------------------------------
	final_model <- dplyr::select(final_model, c('NHDPlusID', 'StreamOrde', 'waterbody', 'Water_temp_c','Q_m3_s','W_m','lakeSA_m2', 'k600_m_s','k_co2_m_s', 'CO2_ppm', 'FCO2_gC_m2_yr'))
  	network <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4_id, '_HU4_GDB/NHDPLUS_H_', huc4_id, '_HU4_GDB.gdb'), layer='NHDFlowline')
  	network <- dplyr::left_join(network, final_model, 'NHDPlusID') %>%
  		sf::st_zm() %>%
  		dplyr::filter(is.na(waterbody)==0)

  	basin <- sf::st_read(paste0(path_to_data, '/HUC2_', huc2, '/WBD_', huc2, '_HU2_Shape/Shape/WBDHU4.shp')) %>%
  		dplyr::filter(huc4 == huc4_id)

  	basin_name <- basin$name


  	##CALCULATE UPSCALING MODEL FOR THE BASIN----------------------------------------------------------------------------------------
  	riverCO2 <- glorich_data[glorich_data$HUC2 == huc2,]$river #[ppm]
  	lakeCO2 <- glorich_data[glorich_data$HUC2 == huc2,]$lake #[ppm]
  	temp_c <- mean(network$Water_temp_c, na.rm=T) #regional average water temperature following raymond 2013
  	henry <- henry_func(temp_c)
  	sc <- 1911-118.11*temp_c+3.453*temp_c^2-0.0413*temp_c^3 #Raymond2012/Wanninkof 1991

	###RIVER CALCULATION
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

	####LAKES/RESERVOIRS
  	lakes_by_area <- dplyr::filter(network, waterbody == 'Lake/Reservoir') %>% #build combined lakes dataset for lake scaling
    	dplyr::group_by(WBArea_Permanent_Identifier) %>%
    	dplyr::summarise(area_skm = sum(lakeSA_m2, na.rm=T)*1e-6) %>%
    	dplyr::mutate(k600_m_dy = ifelse(area_skm < 0.1, 0.54, #[m/dy] Read etal 2012
        	                ifelse(area_skm < 1, 1.16,
            	              ifelse(area_skm < 3.16, 1.32,
                	            ifelse(area_skm < 10, 1.9, 1.9))))) %>%
    	dplyr::mutate(kco2_m_dy = k600_m_dy/(600/sc)^-0.5) %>% #[m/dy]
    	dplyr::mutate(kco2_m_s = (kco2_m_dy)/(24*60*60))  #[m/s]

  	#calculate a basin fco2
  	lakes_by_area$lakes_FCO2_lumped <- ((lakeCO2-400)*henry*1e-6)*lakes_by_area$kco2_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]
  	lakes_FCO2_lumped_total <- sum(lakes_by_area$lakes_FCO2_lumped*lakes_by_area$area_skm*1e6, na.rm=T) #[g-C/yr]

  	#COMBINED EMISSIONS FOR RIVERS + LAKES/RESERVOIRS
  	lumpedFCO2_gC_C_m2_yr <- rivers_FCO2_lumped + sum(lakes_by_area$lakes_FCO2_lumped, na.rm=T)
  	network$lumped_FCO2_gC_m2_yr <- lumpedFCO2_gC_C_m2_yr

  	##CALCULATE LUMPED CO2 MODEL FOR BASIN (see methods)----------------------------------------------------------------------------------------
  	network$semi_FCO2_gC_m2_yr <- ifelse(network$waterbody == 'River', ((riverCO2-400)*henry*1e-6)*network$k_co2_m_s*(1/0.001)*12.01*(60*60*24*365), ((lakeCO2-400)*henry*1e-6)*network$k_co2_m_s*(1/0.001)*12.01*(60*60*24*365)) #[g-C/m2/yr]
  	network$semi_FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$semi_FCO2_gC_m2_yr*network$W_m*network$LengthKM*1000, network$semi_FCO2_gC_m2_yr*network$lakeSA_m2) #[g-C/yr]

	##CALCULATE LUMPED K MODEL FOR BASIN (see methods)----------------------------------------------------------------------------------------
  	network$semi2_FCO2_gC_m2_yr <- ifelse(network$waterbody == 'River', ((network$CO2_ppm-400)*henry*1e-6)*rivers_k_co2_lumped_m_s*(1/0.001)*12.01*(60*60*24*365), ((network$CO2_ppm-400)*henry*1e-6)*network$k_co2_m_s*(1/0.001)*12.01*(60*60*24*365)) #[g-C/m2/yr]
  	network$semi2_FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$semi2_FCO2_gC_m2_yr*network$W_m*network$LengthKM*1000, network$semi2_FCO2_gC_m2_yr*network$lakeSA_m2) #[g-C/yr]

  	#CALCULATE TOTAL TRANSPORT MODEL FLUX----------------------------------------------------------------------------------------------
	network$FCO2_gC_yr <- ifelse(network$waterbody == 'River', network$FCO2_gC_m2_yr*network$W_m*network$LengthKM*1000, network$FCO2_gC_m2_yr*network$lakeSA_m2) #[g-C/yr] 	

  	## AGGREGATE TO WHOLE-BASIN FLUXES----------------------------------------------------------------------------------------------
  	lumpedFCO2_GgC_C_yr <- (rivers_FCO2_lumped_total + lakes_FCO2_lumped_total) * 1e-9 #[Gg-C-yr]
  	semiDistFCO2_GgC_C_yr <- sum(network$semi_FCO2_gC_yr, na.rm=T)*1e-9 #[Gg-C-yr]
  	semi2DistFCO2_GgC_C_yr <- sum(network$semi2_FCO2_gC_yr, na.rm=T)*1e-9 #[Gg-C-yr]
  	distFCO2_GgC_yr <- sum(network$FCO2_gC_yr, na.rm=T)*1e-9 #[Gg-C-yr]

	#BUILD BASIN FLUX LABELS AS PLOTS--------------------------
  	plotA <- patchwork::wrap_elements(grid::textGrob(paste0('Statistical Upscaling\n',round(lumpedFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))

  	plotB <- patchwork::wrap_elements(grid::textGrob(paste0('Semi-Upscaling 1\n', round(semi2DistFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))

  	plotC <- patchwork::wrap_elements(grid::textGrob(paste0('Semi Upscaling 2\n', round(semiDistFCO2_GgC_C_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))  	

  	plotD <- patchwork::wrap_elements(grid::textGrob(paste0('Mechanistic Transport\n',round(distFCO2_GgC_yr,0), ' Gg-C/yr'), y=0.6, gp=grid::gpar(col="black", fontsize=34)))


  	## K VS. CO2 PLOTS----------------------------------------------------------------------------------------
  	network$lumpedCO2_ppm <- ifelse(network$waterbody == 'River', riverCO2, lakeCO2)

	#PLOT UPSCALING
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
    	xlab(expression(bold(k[600]~(m/dy))))+
    	ylab(expression(bold(CO[2]~(ppm))))


	#PLOT LUMPED K
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
    	xlab(expression(bold(k[600]~(m/dy))))+
    	ylab('')

	#PLOT LUMPED CO2
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
    	xlab(expression(bold(k[600]~(m/dy))))+
    	ylab('')

	#PLOT TRANSPORT MODEL
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
    	xlab(expression(bold(k[600]~(m/dy))))+
    	ylab('')


  	##RIVER NETWORK MAPS----------------------------------------------------------------------------------------
	#PLOT UPSCALING
    network <- network %>%
    		dplyr::mutate(forPlot = dplyr::case_when(
      			lumped_FCO2_gC_m2_yr*1e-3 <= 0.5 ~ '0-0.5'
      			,lumped_FCO2_gC_m2_yr*1e-3 <= 1 ~ '0.5-1'
      			,lumped_FCO2_gC_m2_yr*1e-3 <= 2.5 ~ '1-2.5'
      			,lumped_FCO2_gC_m2_yr*1e-3 <= 5 ~ '2.5-5'
      			,lumped_FCO2_gC_m2_yr*1e-3 <= 10 ~ '5-10'
      			,TRUE ~ '10+'
    			))

    network$forPlot <- factor(network$forPlot, levels = c('0-0.5', '0.5-1', '1-2.5', '2.5-5', '5-10', '10+')) #[Kg-C/m2/yr]

    plotI <- ggplot() +
    	geom_sf(data=network, aes(color=forPlot, linewidth=Q_m3_s))+
    	ggsn::scalebar(data=network,location='bottomleft', dist = 25, dist_unit = "km",transform = TRUE, model = "WGS84",st.dist=0.05) +  
    	scale_color_brewer(palette='RdYlBu', direction=-1,name=expression(bold(FCO[2]~(Kg-C/m^2/yr))))+
    	scale_linewidth_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10,100),
            	          range=c(0.4,2),
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

	#PLOT LUMPED K
    network <- network %>%
    		dplyr::mutate(forPlot = dplyr::case_when(
      			semi2_FCO2_gC_m2_yr*1e-3 <= 0.5 ~ '0-0.5'
      			,semi2_FCO2_gC_m2_yr*1e-3 <= 1 ~ '0.5-1'
      			,semi2_FCO2_gC_m2_yr*1e-3 <= 2.5 ~ '1-2.5'
      			,semi2_FCO2_gC_m2_yr*1e-3 <= 5 ~ '2.5-5'
      			,semi2_FCO2_gC_m2_yr*1e-3 <= 10 ~ '5-10'
      			,TRUE ~ '10+'
    			))

    network$forPlot <- factor(network$forPlot, levels = c('0-0.5', '0.5-1', '1-2.5', '2.5-5', '5-10', '10+')) #[Kg-C/m2/yr]

    plotJ <- ggplot() +
    	geom_sf(data=network, aes(color=forPlot, linewidth=Q_m3_s))+
    	ggsn::scalebar(data=network,location='bottomleft', dist = 25, dist_unit = "km",transform = TRUE, model = "WGS84",st.dist=0.05) +      	
    	scale_color_brewer(palette='RdYlBu', direction=-1,name=expression(bold(FCO[2]~(Kg-C/m^2/yr))))+
    	scale_linewidth_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10,100),
            	          range=c(0.4,2),
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


	#PLOT LUMPED CO2
    network <- network %>%
    		dplyr::mutate(forPlot = dplyr::case_when(
      			semi_FCO2_gC_m2_yr*1e-3 <= 0.5 ~ '0-0.5'
      			,semi_FCO2_gC_m2_yr*1e-3 <= 1 ~ '0.5-1'
      			,semi_FCO2_gC_m2_yr*1e-3 <= 2.5 ~ '1-2.5'
      			,semi_FCO2_gC_m2_yr*1e-3 <= 5 ~ '2.5-5'
      			,semi_FCO2_gC_m2_yr*1e-3 <= 10 ~ '5-10'
      			,TRUE ~ '10+'
    			))

    network$forPlot <- factor(network$forPlot, levels = c('0-0.5', '0.5-1', '1-2.5', '2.5-5', '5-10', '10+')) #[Kg-C/m2/yr]

    plotK <- ggplot() +
    	geom_sf(data=network, aes(color=forPlot, linewidth=Q_m3_s))+
    	ggsn::scalebar(data=network,location='bottomleft', dist = 25, dist_unit = "km",transform = TRUE, model = "WGS84",st.dist=0.05) +
    	scale_color_brewer(palette='RdYlBu', direction=-1,name=expression(bold(FCO[2]~(Kg-C/m^2/yr))))+
    	scale_linewidth_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10,100),
            	          range=c(0.4,2),
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


	#PLOT TRANSPORT MODEL
    network <- network %>%
    		dplyr::mutate(forPlot = dplyr::case_when(
      			FCO2_gC_m2_yr*1e-3 <= 0.5 ~ '0-0.5'
      			,FCO2_gC_m2_yr*1e-3 <= 1 ~ '0.5-1'
      			,FCO2_gC_m2_yr*1e-3 <= 2.5 ~ '1-2.5'
      			,FCO2_gC_m2_yr*1e-3 <= 5 ~ '2.5-5'
      			,FCO2_gC_m2_yr*1e-3 <= 10 ~ '5-10'
      			,TRUE ~ '10+'
    			))

    network$forPlot <- factor(network$forPlot, levels = c('0-0.5', '0.5-1', '1-2.5', '2.5-5', '5-10', '10+')) #[Kg-C/m2/yr]

    plotL <- ggplot() +
    	geom_sf(data=network, aes(color=forPlot, linewidth=Q_m3_s))+
    	ggsn::scalebar(data=network,location='bottomleft', dist = 25, dist_unit = "km",transform = TRUE, model = "WGS84",st.dist=0.05) +    	
    	scale_color_brewer(palette='RdYlBu', direction=-1,name=expression(bold(FCO[2]~(Kg-C/m^2/yr))))+
    	scale_linewidth_binned(name='Discharge [cms]',
        	              breaks=c(0.001, 0.01, 0.1, 1, 10,100),
            	          range=c(0.4,2),
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


    ##EXTRACT SHARED LEGEND-----------------------------------------------
    legend <- cowplot::get_legend(plotL +
                            labs(tag = '')+
                            theme(legend.position = "bottom",
                                  legend.text = element_text(size=22),
                                  legend.title = element_text(size=24, face='bold'),
                                  legend.box="vertical",
                                  legend.margin=margin(),
                                  legend.spacing.x = unit(0.3, 'cm')) +
                            guides(color = guide_legend(override.aes = list(linewidth=20),
                            							title.position="top",
                            							title.hjust = 0.5,
                            							nrow = 1)))

    ##EXTRACT BASIN NAME----------------------------
    basin_name <- stringr::str_wrap(paste0(basin_name, ' River'), 30) #wrap to twenty characters, seems to fit nicely
    plotN <- patchwork::wrap_elements(grid::textGrob(basin_name, y=0.6, gp=grid::gpar(col="black", fontsize=34)))

  	#PLOT DESIGN----------------------------
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

	#BUILD COMBO PLOT----------------------------
  	comboPlot <- patchwork::wrap_plots(A=plotA, B=plotB, C=plotC, D=plotD,
  									   E=plotE+theme(legend.position=c(0.5, 0.8)), F=plotF+theme(legend.position='none'), G=plotG+theme(legend.position='none'), H=plotH+theme(legend.position='none'),
  									   I=plotI+theme(legend.position='none'), J=plotJ+theme(legend.position='none'), K=plotK+theme(legend.position='none'), L=plotL+theme(legend.position='none'),
  									   M=legend, N=plotN,

  									   design=design)

	#WRITE TO FILE----------------------------------------
  	ggsave(paste0('cache/figures/conceptualCompare_', huc4_id, '.jpg'), comboPlot, width=28, height=20)

	#RETURN FILEPATH
  	return(paste0('see cache/figures/conceptualCompare_', huc4_id, '.jpg'))
}









#' build figure of sources of CO2 emissions (figure 3)
#'
#' @name sourcesMap
#'
#' @param path_to_data: path to data repo
#' @param results: model results shapefile
#' @param combined_sources_by_order: df of results by stream order
#'
#' @import dplyr
#' @import sf
#' @import tidyr
#' @import cowplot
#' @import patchwork
#' @import ggplot2
#'
#' @return print statement (figure written to file)
sourcesMap <- function(path_to_data, results, combined_sources_by_order){
	#set ggplot theme
	theme_set(theme_classic())
  
  	# READ IN CONUS BOUNDARY -------------------------------------------
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

	#SETUP-----------------------------------------------------------------
	#remove great lakes
  	results <- dplyr::filter(results, is.na(contribGW_TgC_yr)==0 & is.na(lakeFCO2_TgC_yr)==0)

  	#setup results to map
  	results$perc_GW <- round((results$contribGW_TgC_yr/(results$contribGW_TgC_yr+results$contribWC_TgC_yr+results$contribBZ_TgC_yr))*100,0) #setup percent
  	results$perc_WC <- round((results$contribWC_TgC_yr/(results$contribGW_TgC_yr+results$contribWC_TgC_yr+results$contribBZ_TgC_yr))*100,0) #setup percent
  	results$perc_BZ <- round((results$contribBZ_TgC_yr/(results$contribGW_TgC_yr+results$contribWC_TgC_yr+results$contribBZ_TgC_yr))*100,0) #setup percent

	#BUILD BASIN-SCALE MAPS-----------------------------------------------
	# plot %GW
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
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
    		  axis.text.x = element_text(angle = 90),
        	  legend.title = element_text(face = "bold", size = 18),
          	  legend.text = element_text(family = "Futura-Medium", size = 18),
          	  plot.tag = element_text(size=26,
            	                      face='bold'),
          	  plot.title = element_text(size=26,
            	                      face='bold'))+
    	xlab('')+
    	ylab('')

  	# plot %WC
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
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
    		  axis.text.x = element_text(angle = 90),
        	  legend.title = element_text(face = "bold", size = 18),
          	  legend.text = element_text(family = "Futura-Medium", size = 18),
          	  plot.tag = element_text(size=26,
            	                      face='bold'),
          	plot.title = element_text(size=26,
            	                      face='bold'))+
    	xlab('')+
    	ylab('')

  	# plot %BZ
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
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
    		  axis.text.x = element_text(angle = 90),
        	  legend.title = element_text(face = "bold", size = 18),
          	  legend.text = element_text(family = "Futura-Medium", size = 18),
          	  plot.tag = element_text(size=26,
                                   face='bold'),
          	  plot.title = element_text(size=26,
                                   face='bold'))+
    	xlab('')+
    	ylab('')

	#extract legend
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


  ####SOURCES BY ORDER--------------------------------------
  #bin by stream order
  combined_results_by_order <- combined_sources_by_order %>%
  	dplyr::filter(is.na(percGW_reach_median)==0 & is.finite(percGW_reach_median)==1) %>% #remove Great Lakes
   	tidyr::gather(key=key, value=value, c('percGW_reach_median', 'percBZ_reach_median', 'percWC_reach_median'))

   #build plot
   plotSources_by_order <- ggplot(combined_results_by_order, aes(fill=key, x=StreamOrde, y=value*100)) +
     geom_boxplot(color='black', size=1.2)+
     xlab('Stream Order') +
     ylab('Median % of emissions')+
     scale_fill_manual(name='',
     				   labels=c('Hyporheic zone respiration', 'Groundwater', 'Net water-column respiration'),
                       values=c('#edae49', '#d1495b', '#00798c'))+
     ylim(0,100)+ #these can't go beyond 100%, so this is ok
     labs(tag='D')+
     theme(axis.title = element_text(size=26, face='bold'),
           axis.text = element_text(size=24,face='bold'),
           plot.tag = element_text(size=26,
                                   face='bold'),
           legend.position='bottom',
           legend.text = element_text(size=24))	


  	#PLOT DESIGN-------------------------------------------
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
	#BUILD COMBO PLOT-----------------------------------------
  	comboPlot <- patchwork::wrap_plots(A=mapBZ + theme(legend.position='none'), B=mapGW + theme(legend.position='none'), C=mapWC + theme(legend.position='none'), D=legend, E=plotSources_by_order, design=design)

	#WRITE TO FILE----------------------------------------------
  	ggsave('cache/figures/mapSources.jpg', comboPlot, width=20, height=20)

	#RETURN FILEPATH---------------------------------------------
  	return('see cache/figures/mapSources.jpg')
}






#' build paper figure of lake CO2 emissions
#'
#' @name lakesMap
#'
#' @param path_to_data: character path to data repo
#' @param results: model results df
#'
#' @import dplyr
#' @import sf
#' @import gpplot2
#'
#' @return print statement (figure written to file)
lakesMap <- function(path_to_data, results){
	#set ggplot theme
	theme_set(theme_classic())
  
  	# READ IN CONUS BOUNDARY--------------------------------------------
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

	#SETUP--------------------------------------------------------------
	#remove great lakes
  	results <- dplyr::filter(results, is.na(contribGW_TgC_yr)==0 & is.na(lakeFCO2_TgC_yr)==0)

  	#setup results to map
  	results$perc_Lakes <- round((results$lakeFCO2_TgC_yr/results$sumFCO2_TgC_yr )*100,0) #calculate percent
  	results[!is.na(results$lakeFCO2_TgC_yr) & results$lakeFCO2_TgC_yr < 0,]$perc_Lakes <- 0 #if lakes are a sink, set to 0

	#PERC LAKES MAP-------------------------------------------------
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
    	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
    	theme(legend.position = c(0.2, 0.1),
        	  legend.key.size = unit(2, 'cm'))+ 
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
        	  legend.title = element_text(face = "bold", size = 18),
          	  legend.text = element_text(family = "Futura-Medium", size = 18),
          	  plot.tag = element_text(size=26,
            	                      face='bold'))+
    	xlab('')+
    	ylab('')

	#WRITE TO FILE----------------------------------------------------
  	ggsave('cache/figures/mapLakes.jpg', mapLakes, width=20, height=15)

	#RETURN FILEPATH---------------------------------------------------
  	return('see cache/figures/mapLakes.jpg')
}






#' prep sf objects for mapping for fco2 map. This is done basin by basin so that sf processing (this function) cna be ran in parallel and signficiantly speed everything up
#'
#' @name indvRiverMaps
#'
#' @param results: model df
#' @param huc4: basin ID
#'
#' @import dplyr
#' @import sf
#'
#' @return sf object with legend for x basin
indvRiverMaps <- function(results, huc4){
	#get basin IDs
	huc2 <- substr(huc4, 1, 2)
	huc4 <- ifelse(nchar(huc4)==5,substr(huc4,1,4),huc4)

	#skip great lakes
	if(huc4 %in% c('0418', '0419', '0424', '0426', '0428')){
		return(NA)
	}
	#all other basins
	else{
    	# read in shapefile
  		shapefile <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4, '_HU4_GDB/NHDPLUS_H_', huc4, '_HU4_GDB.gdb'), layer='NHDFlowline') %>%
  			sf::st_zm() %>%
  			dplyr::left_join(results, by='NHDPlusID') %>%
  			dplyr::filter(!is.na(StreamOrde))

  		#fix multicurves (if necessary)
  		shapefile <- fixGeometries(shapefile)
  	
  		# bin fco2 for mapping
  		fin<-shapefile %>%
    		dplyr::mutate(CO2_col = dplyr::case_when(
      			FCO2_gC_m2_yr*1e-3 <= 0.5 ~ '0-0.5'
      			,FCO2_gC_m2_yr*1e-3 <= 1 ~ '0.5-1'
      			,FCO2_gC_m2_yr*1e-3 <= 2.5 ~ '1-2.5'
      			,FCO2_gC_m2_yr*1e-3 <= 5 ~ '2.5-5'
      			,FCO2_gC_m2_yr*1e-3 <= 10 ~ '5-10'
      			,TRUE ~ '10+'
    			)) %>%
    		dplyr::select(c('CO2_col', 'Q_m3_s', ))

    	fin$CO2_col <- factor(fin$CO2_col, levels = c('0-0.5', '0.5-1', '1-2.5', '2.5-5', '5-10', '10+')) #[Kg-C/m2/yr]
	
	#return sf object
  	return(fin)
	}
}






#' CONUS FCO2 map (top half)
#'
#' @name mainMapFunction1
#'
#' @param mapList: combined list of sf objects per basin (makes mapping possible)
#'
#' @import ggplot2
#' @import dplyr
#' @import sf
#' @import RColorBrewer
#'
#' @return print statement (writes figure to file). Illustrator is used to combine into a single figure
mainMapFunction1 <- function(mapList){
	#remove NAs (great lakes)
	mapList <- mapList[!is.na(mapList)]

	#set ggplot theme
	theme_set(theme_classic())

	# READ IN CONUS BOUNDARY--------------------------------------
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

	sf::sf_use_s2(FALSE)

  	#BIG MAIN MAP------------------------------------------
	#unfortuantely, list of sf objects must each be manually specified to do this. Objects are by basin so that each basin can be built in parallel
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
		scale_color_brewer(palette='RdYlBu', direction=-1,name=expression(bold(FCO[2]~(Kg-C/m^2/yr))))+
 		theme(plot.title = element_text(face = "italic", size = 26),
    	  	axis.text = element_text(size = 22),
        	plot.tag = element_text(size=26,
             	                  face='bold'),
        	legend.position=c(0.9,0.15),
 			legend.text = element_text(size=18),
 			legend.title = element_text(size=22,face="bold"),
 			legend.spacing.y = unit(0.1, 'cm'))+
 		guides(color = guide_legend(override.aes = list(linewidth=8), byrow = TRUE))

	#BUILD INSET BOX BOUNDING BOX-------------------------------------
  	#inset centering
   	zoom_to <- c(-79.3000, 39.1399)

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

    # add inset 1 bounding box to map
    bigMap <- bigMap +
    	geom_sf(data=box_1,
    			color='#fca311',
    			linewidth=3,
    			alpha=0)

	#WRITE TO FILE---------------------------------------------------------
	ggsave(filename="cache/figures/mainMap_1.jpg",plot=bigMap,width=20,height=15)
	
	#RETURN FILEPATH-------------------------------------------
	return('see cache/figures/')
}








#' CONUS FCO2 map (second half))
#'
#' @name mainMapFunction2
#'
#' @param map_0205: sf object for basin for inset mapping
#' @param map_0206: sf object for basin for inset mapping
#' @param map_0207: sf object for basin for inset mapping
#' @param map_0208: sf object for basin for inset mapping
#' @param map_0502: sf object for basin for inset mapping
#' @param map_0503: sf object for basin for inset mapping
#' @param map_0501: sf object for basin for inset mapping
#' @param map_0505: sf object for basin for inset mapping
#'
#' @import dplyr
#' @import sf
#' @import ggsn
#' @import patchwork
#' @import RColorBrewer
#'
#' @return print statement (writes figure to file). Illustrator is used to combine into a single figure
mainMapFunction2 <- function(map_0205, map_0206,map_0207,map_0208,map_0502, map_0503, map_0501, map_0505){
	#set ggplot theme
	theme_set(theme_classic())

	#BUILD INSET BOUNDING BOXES-------------------------------------
  	#setup centering
   	zoom_to <- c(-79.3000, 39.1399)

	# INSET 1
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

	#INSET 2
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

	#INSET 3
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

	#INSET 4
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


  	#BUILD INSET MAPS----------------------------------------
	#build complete inset river network
  	insetNet <- rbind(map_0205, map_0206,map_0207,map_0208,map_0502, map_0503, map_0501, map_0505)

  	#custom color scale
	myColors <- rev(RColorBrewer::brewer.pal(6,"RdYlBu"))
	names(myColors) <- levels(insetNet$CO2_col)
	colScale <- scale_colour_manual(name = "CO2_col",values = myColors)
	fillScale <- scale_fill_manual(name = "CO2_col",values = myColors)

  	#plot inset 1
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
    	theme(legend.position='none',
    		  panel.background = element_rect(fill = "black"))

   	#plot inset 2
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
     	theme(legend.position='none',
     		  panel.background = element_rect(fill = "black"))

  	#plot inset 3
  	insetShp3 <- sf::st_crop(insetNet, xmin=lon_bounds_3[1], xmax=lon_bounds_3[2], ymin=lat_bounds_3[1], ymax=lat_bounds_3[2])
  	waterbodies <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_02/NHDPLUS_H_0207_HU4_GDB/NHDPLUS_H_0207_HU4_GDB.gdb'), layer='NHDWaterbody') %>%
  		dplyr::filter(FType %in% c(390, 436))
    waterbodies <- sf::st_crop(waterbodies, xmin=lon_bounds_3[1], xmax=lon_bounds_3[2], ymin=lat_bounds_3[1], ymax=lat_bounds_3[2])
    waterbodies <- sf::st_join(waterbodies, insetShp3,  join = st_contains)
    waterbodies <- dplyr::filter(waterbodies, is.na(CO2_col)==0) #match the actual modle run hydrography

  	inset3 <- ggplot() +
  		geom_sf(data = insetShp3, aes(color = CO2_col,linewidth=Q_m3_s)) +
		geom_sf(data = waterbodies, aes(fill = CO2_col)) +  		
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
     	theme(legend.position='none',
     		  panel.background = element_rect(fill = "black"))

  	#plot inset 4
  	insetShp4 <- sf::st_crop(insetNet, xmin=lon_bounds_4[1], xmax=lon_bounds_4[2], ymin=lat_bounds_4[1], ymax=lat_bounds_4[2])
  	waterbodies <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_02/NHDPLUS_H_0207_HU4_GDB/NHDPLUS_H_0207_HU4_GDB.gdb'), layer='NHDWaterbody') %>%
  		dplyr::filter(FType %in% c(390, 436))
    waterbodies <- sf::st_crop(waterbodies, xmin=lon_bounds_4[1], xmax=lon_bounds_4[2], ymin=lat_bounds_4[1], ymax=lat_bounds_4[2])
    waterbodies <- sf::st_join(waterbodies, insetShp4,  join = st_contains)
    waterbodies <- dplyr::filter(waterbodies, is.na(CO2_col)==0) #match the actual model run hydrography

  	inset4 <- ggplot() +
  		geom_sf(data = insetShp4, aes(color = CO2_col,linewidth=Q_m3_s)) +
		geom_sf(data = waterbodies, aes(fill = CO2_col)) +  		
	  	scale_linewidth_binned(name='Discharge [cms]',
        	breaks=c(0.01,0.1, 1, 10,100,1000),
            range=c(0.3,2.5))+  		
    	ggsn::scalebar(data=insetShp4,location='bottomleft', dist = 0.5, dist_unit = "km",transform = TRUE, model = "WGS84", box.fill=c('white','red'),st.color='white',st.dist=0.05,border.size=0.05) +  
  		xlab('')+
  		ylab('')+
  		coord_sf(datum=NA) +  							
     	theme(legend.position='none',
     		  panel.background = element_rect(fill = "black"))

  	# PLOT DESIGN-----------------------------------------------------------------------
  	 design <- "
  	 	BCDE
  	 "

	#BUILD COMBO PLOT--------------------------------------------------------------------
  	comboPlot <- patchwork::wrap_plots(B=inset4 + colScale + fillScale, C=inset3 + colScale + fillScale, D=inset2 + colScale, E=inset1 + colScale, design=design)

	#WRITE TO FILE---------------------------------------------------
	ggsave(filename="cache/figures/mainMap_2.jpg",plot=comboPlot,width=20,height=5)
	
	#RETURN FILEPATH--------------------------------------------------------------------
	return('see cache/figures/')
}







#' prep sf objects for mapping for k600 map. This is done basin by basin so that sf processing (this function) cna be ran in parallel and signficiantly speed everything up
#'
#' @name indvRiverMaps_k600
#'
#' @param results: sf object of model results
#' @param huc4: basin ID
#'
#' @import dplyr
#' @import sf
#'
#' @return sf object map for x basin
indvRiverMaps_k600 <- function(results, huc4){
	#get basin IDs
	huc2 <- substr(huc4, 1, 2)
	huc4 <- ifelse(nchar(huc4)==5,substr(huc4,1,4),huc4)

	#skip great lakes
	if(huc4 %in% c('0418', '0419', '0424', '0426', '0428')){
		return(NA)
	}
	#all other basins
	else{
    	# read in shapefile
  		shapefile <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4, '_HU4_GDB/NHDPLUS_H_', huc4, '_HU4_GDB.gdb'), layer='NHDFlowline') %>%
  			sf::st_zm() %>%
  			dplyr::left_join(results, by='NHDPlusID') %>%
  			dplyr::filter(!is.na(StreamOrde))

  		#fix multicurves (if necessary)
  		shapefile <- fixGeometries(shapefile)
  	
  		# bin fco2 for mapping
  		fin<-shapefile %>%
    		dplyr::mutate(k_col = dplyr::case_when(
      			k600_m_s*86400 <= 2.5 ~ '0-2.5'
      			,k600_m_s*86400 <= 5 ~ '2.5-5'
      			,k600_m_s*86400 <= 10 ~ '5-10'
      			,k600_m_s*86400 <= 15 ~ '10-15'
      			,k600_m_s*86400 <= 50 ~ '15-50'
      			,TRUE ~ '50+'
    			)) %>%
    		dplyr::select(c('k_col', 'Q_m3_s', ))

    	fin$k_col <- factor(fin$k_col, levels = c('0-2.5', '2.5-5', '5-10', '10-15', '15-50', '50+')) #[m/dy]
	
	#return sf object
  	return(fin)
	}
}






#' CONUS k600 map
#'
#' @name mainMapFunction_k600
#'
#' @param mapList: combined list of sf objects per basin (makes mapping easier)
#'
#' @import ggplot2
#' @import dplyr
#' @import sf
#' @import RColorBrewer
#'
#' @return print statement (writes figure to file)
mainMapFunction_k600 <- function(mapList){
	#remove NAs (great lakes)
	mapList <- mapList[!is.na(mapList)]

	#set ggplot theme
	theme_set(theme_classic())

	# READ IN CONUS BOUNDARY--------------------------------------
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

	sf::sf_use_s2(FALSE)

  	#BIG MAIN MAP------------------------------------------
	#unfortuantely, list of sf objects must each be manually specified to do this. Objects are by basin so that each basin can be built in parallel
	bigMap <- ggplot()+
		geom_sf(data=mapList[[1]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[2]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[3]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[4]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[5]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[6]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[7]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[8]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[9]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[10]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[11]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[12]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[13]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[14]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[15]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[16]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[17]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[18]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[19]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[20]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[21]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[22]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[23]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[24]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[25]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[26]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[27]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[28]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[29]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[30]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[31]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[32]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[33]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[34]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[35]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[36]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[37]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[38]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[39]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[40]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[41]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[42]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[43]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[44]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[45]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[46]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[47]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[48]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[49]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[50]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[51]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[52]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[53]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[54]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[55]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[56]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[57]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[58]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[59]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[60]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[61]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[62]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[63]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[64]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[65]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[66]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[67]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[68]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[69]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[70]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[71]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[72]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[73]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[74]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[75]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[76]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[77]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[78]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[79]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[80]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[81]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[82]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[83]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[84]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[85]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[86]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[87]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[88]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[89]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[90]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[91]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[92]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[93]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[94]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[95]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[96]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[97]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[98]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[99]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[100]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[101]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[102]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[103]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[104]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[105]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[106]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[107]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[108]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[109]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[110]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[111]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[112]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[113]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[114]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[115]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[116]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[117]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[118]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[119]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[120]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[121]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[122]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[123]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[124]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[125]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[126]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[127]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[128]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[129]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[130]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[131]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[132]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[133]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[134]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[135]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[136]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[137]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[138]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[139]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[140]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[141]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[142]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[143]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[144]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[145]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[146]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[147]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[148]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[149]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[150]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[151]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[152]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[153]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[154]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[155]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[156]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[157]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[158]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[159]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[160]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[161]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[162]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[163]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[164]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[165]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[166]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[167]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[168]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[169]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[170]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[171]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[172]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[173]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[174]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[175]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[176]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[177]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[178]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[179]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[180]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[181]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[182]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[183]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[184]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[185]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[186]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[187]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[188]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[189]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[190]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[191]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[192]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[193]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[194]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[195]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[196]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[197]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[198]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[199]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[200]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[201]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[202]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[203]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[204]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[205]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[206]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data = mapList[[207]], aes(color = k_col),linewidth=0.1) +
		geom_sf(data=states, #conus boundary
        	    color='black',
            	linewidth=0.75,
            	alpha=0)+
		scale_color_brewer(palette='RdYlBu', direction=-1,name=expression(bold(k[600]~(m/d))))+
 		theme(plot.title = element_text(face = "italic", size = 26),
    	  	axis.text = element_text(size = 22),
        	plot.tag = element_text(size=26,
             	                  face='bold'),
        	legend.position=c(0.9,0.15),
 			legend.text = element_text(size=18),
 			legend.title = element_text(size=22,face="bold"),
 			legend.spacing.y = unit(0.1, 'cm'))+
 		guides(color = guide_legend(override.aes = list(linewidth=8), byrow = TRUE))


	#WRITE TO FILE---------------------------------------------------------
	ggsave(filename="cache/figures/mainMap_k600.jpg",plot=bigMap,width=20,height=15)
	
	#RETURN FILEPATH-------------------------------------------
	return('see cache/figures/')
}





#' prep sf objects for mapping for k600 map. This is done basin by basin so that sf processing (this function) cna be ran in parallel and signficiantly speed everything up
#'
#' @name indvRiverMaps_pco2
#'
#' @param results: sf object of model results
#' @param huc4: basin ID
#'
#' @import dplyr
#' @import sf
#'
#' @return sf object map for x basin
indvRiverMaps_pco2 <- function(results, huc4){
	#get basin IDs
	huc2 <- substr(huc4, 1, 2)
	huc4 <- ifelse(nchar(huc4)==5,substr(huc4,1,4),huc4)

	#skip great lakes
	if(huc4 %in% c('0418', '0419', '0424', '0426', '0428')){
		return(NA)
	}
	#all other basins
	else{
    	# read in shapefile
  		shapefile <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4, '_HU4_GDB/NHDPLUS_H_', huc4, '_HU4_GDB.gdb'), layer='NHDFlowline') %>%
  			sf::st_zm() %>%
  			dplyr::left_join(results, by='NHDPlusID') %>%
  			dplyr::filter(!is.na(StreamOrde))

  		#fix multicurves (if necessary)
  		shapefile <- fixGeometries(shapefile)
  	
  		# bin fco2 for mapping
  		fin<-shapefile %>%
    		dplyr::mutate(CO2_col = dplyr::case_when(
      			CO2_ppm <= 500 ~ '0-500'
      			,CO2_ppm <= 1000 ~ '500-1000'
      			,CO2_ppm <= 2500 ~ '1000-2500'
      			,CO2_ppm <= 5000 ~ '2500-5000'
      			,CO2_ppm <= 10000 ~ '5000-10000'
      			,TRUE ~ '10000+'
    			)) %>%
    		dplyr::select(c('CO2_col', 'Q_m3_s', ))

    	fin$CO2_col <- factor(fin$CO2_col, levels = c('0-500', '500-1000', '1000-2500', '2500-5000', '5000-10000', '10000+')) #[ppm]
	
	#return sf object
  	return(fin)
	}
}





#' CONUS map for pCO2
#'
#' @name mainMapFunction_pco2
#'
#' @param mapList: combined list of sf objects per basin (makes mapping easier)
#'
#' @import ggplot2
#' @import dplyr
#' @import sf
#' @import RColorBrewer
#'
#' @return print statement (writes figure to file)
mainMapFunction_pco2 <- function(mapList){
	#remove NAs (great lakes)
	mapList <- mapList[!is.na(mapList)]

	#set ggplot theme
	theme_set(theme_classic())

	# READ IN CONUS BOUNDARY--------------------------------------
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

	sf::sf_use_s2(FALSE)

  	#BIG MAIN MAP------------------------------------------
	#unfortuantely, list of sf objects must each be manually specified to do this. Objects are by basin so that each basin can be built in parallel
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
		scale_color_brewer(palette='RdYlBu', direction=-1,name=expression(bold(pCO[2]~(ppm))))+
 		theme(plot.title = element_text(face = "italic", size = 26),
    	  	axis.text = element_text(size = 22),
        	plot.tag = element_text(size=26,
             	                  face='bold'),
        	legend.position=c(0.9,0.15),
 			legend.text = element_text(size=18),
 			legend.title = element_text(size=22,face="bold"),
 			legend.spacing.y = unit(0.1, 'cm'))+
 		guides(color = guide_legend(override.aes = list(linewidth=8), byrow = TRUE))


	#WRITE TO FILE---------------------------------------------------------
	ggsave(filename="cache/figures/mainMap_pco2.jpg",plot=bigMap,width=20,height=15)
	
	#RETURN FILEPATH-------------------------------------------
	return('see cache/figures/')
}





#' build map comparing upscaling to transport (extended data figure 1)
#'
#' @name compareAgainstLumped
#'
#' @param path_to_data: path to data repo
#' @param results: sf object of model results
#'
#' @import dplyr
#' @import sf
#' @import ggplot2
#'
#' @return print statement (writes figure to file)
compareAgainstLumped <- function(path_to_data, results){
	#set ggplot theme
	theme_set(theme_classic())
  
  	# READ IN CONUS BOUNDARY-----------------------------------------------------------
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
  
	#PREP--------------------------------------------------------------------------------
  	#round results
  	results$perc_diff <- round(((results$sumFCO2_TgC_yr - results$sumFCO2_lumped_TgC_yr)/results$sumFCO2_lumped_TgC_yr )*100,0) #calculate percent

  	#BUILD PERC DIFFERENCE MAP---------------------------------------------------------------
  	map <- ggplot(results) +
    	geom_sf(aes(fill=perc_diff), #actual map
        	    color='black',
            	size=0.5) +
    	geom_sf(data=states, #conus boundary
        	    color='black',
            	linewidth=0.75,
            	alpha=0)+
    	scale_fill_gradientn(name='Percent difference in CO2 emissions\nfrom statistical upscaling',
        	                 colors=c('#e63946', 'white', '#1d3557'),
            	             limits=c(-100,100),
                	         guide = guide_colorbar(direction = "horizontal",
                    	                            title.position = "bottom"))+
    	theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
    	theme(legend.position = 'bottom',
			  legend.key.size=unit(2, 'cm'))+
    	theme(text = element_text(family = "Futura-Medium"), #legend text settings
        	  legend.title = element_text(size = 20),
          	  legend.text = element_text(family = "Futura-Medium", size = 18),
          	  plot.tag = element_text(size=26,
            	                      face='bold'))+
    	xlab('')+
    	ylab('')

	#WRITE TO FILE-----------------------------------------------------------
    ggsave('cache/figures/lumpedRegionalCompare.jpg', map, width=10, height=10)

	#RETURN FILEPATH-----------------------------------------------------------
    return('see cache/figures/comparisonHUC2.jpg')
}









#' Build map of CO2 sources by order and by region (Extended Data Fig 8)
#'
#' @name sources_by_order_regional
#'
#' @param combined_results_by_order: df of median % sources by stream order by basin
#'
#' @import dplyr
#' @import patchwork
#' @import ggplot2
#' @import tidyr
#'
#' @return print statement (writes figure to file)
sources_by_order_regional <- function(combined_results_by_order){
  #set ggplot theme
  theme_set(theme_classic())

  #remove great lakes
  combined_results_by_order <- dplyr::filter(combined_results_by_order, is.na(percGW_reach_median)==0 & is.na(percBZ_reach_median)==0 & is.na(percWC_reach_median)==0)

  #DEFINE AND FILTER BY REGION---------------------------------------------
  combined_results_by_order$huc2 <- substr(combined_results_by_order$method, 18, 19)
  combined_results_by_order$huc4 <- substr(combined_results_by_order$method, 18, 21)
  east <- c('0101', '0102', '0103', '0104', '0105', '0106', '0107', '0108', '0109', '0110', #all basins east of the Mississippi River (determined visually)
            '0202', '0203', '0206', '0207', '0208', '0204', '0205',
            '0301', '0302', '0303', '0304', '0305', '0306', '0307', '0308', '0309', '0310', '0311', '0312', '0313', '0314', '0315', '0316', '0317', '0318',
            '0401', '0402', '0403', '0404', '0405', '0406', '0407', '0408', '0409', '0410', '0411', '0412', '0413', '0414', '0420', '0427', '0429', '0430',
            '0501', '0502', '0503', '0504', '0505', '0506', '0507', '0508', '0509', '0510', '0511', '0512', '0513', '0514',
            '0601', '0602', '0603', '0604',
            '0701', '0703', '0704', '0705', '0707', '0709', '0712', '0713', '0714',
            '0801', '0803', '0806', '0807', '0809',
            '0901', '0902', '0903', '0904')
  westDF <- combined_results_by_order[!(combined_results_by_order$huc4 %in% c(east)),]
  eastDF <- combined_results_by_order[combined_results_by_order$huc4 %in% c(east),]

  ####SOURCES BY ORDER EAST------------------------------------------------
  #prep data
  eastDF <- eastDF %>%
   	tidyr::gather(key=key, value=value, c('percGW_reach_median', 'percBZ_reach_median', 'percWC_reach_median'))

  #plot
  plotSources_by_orderEast <- ggplot(eastDF, aes(fill=key, x=StreamOrde, y=value*100)) +
    geom_boxplot(color='black', size=1.2)+
    xlab('') +
    ylab('Median % of emissions')+
    scale_fill_manual(name='',
   					  labels=c('Hyporheic zone respiration', 'Groundwater', 'Net water-column respiration'),
                      values=c('#edae49', '#d1495b', '#00798c'))+
    ylim(0,100)+ #can't go over 100% so this is fine
    labs(tag='A')+
    ggtitle('Basins east of the Mississippi River')+
    theme(axis.title = element_text(size=26, face='bold'),
          axis.text = element_text(size=24,face='bold'),
          plot.tag = element_text(size=26,
                                  face='bold'),
          legend.position='none',
          legend.text = element_text(size=24),
      	  title = element_text(size=26, face='bold'))	

  ####SOURCES BY ORDER WEST---------------------------------------------------
  #prep data
  westDF <- westDF %>%
   	tidyr::gather(key=key, value=value, c('percGW_reach_median', 'percBZ_reach_median', 'percWC_reach_median'))

  #plot
  plotSources_by_orderWest <- ggplot(westDF, aes(fill=key, x=StreamOrde, y=value*100)) +
    geom_boxplot(color='black', size=1.2)+
    xlab('Stream Order') +
    ylab('Median % of emissions')+
    scale_fill_manual(name='',
    				  labels=c('Hyporheic zone respiration', 'Groundwater', 'Net water-column respiration'),
                      values=c('#edae49', '#d1495b', '#00798c'))+
    ylim(0,100)+ #can't go over 100% so this is fine
    labs(tag='B')+
    ggtitle('Basins west of the Mississippi River')+
    theme(axis.title = element_text(size=26, face='bold'),
          axis.text = element_text(size=24,face='bold'),
          plot.tag = element_text(size=26,
                                  face='bold'),
          legend.position='bottom',
          legend.text = element_text(size=24),
		  title = element_text(size=26, face='bold'))


  	# PLOT DESIGN-------------------------------------------
  	 design <- "
  	 	A
  	 	B
  	 "

	# BUILD COMBO PLOT--------------------------------------
  	comboPlot <- patchwork::wrap_plots(A=plotSources_by_orderEast, B=plotSources_by_orderWest, design=design)

	#WRITE TO FILE------------------------------------------------
	ggsave(filename="cache/figures/sources_regional_plot.jpg",plot=comboPlot,width=20,height=20)
	
	#RETURN FILEPATH------------------------------------------------
	return('see cache/figures/')
}






#' build nhd discharge validation figure
#'
#' @name pseudoValidation
#'
#' @param combined_pseudoValidation: df of all pseudovalidation results, i.e. glorich samples joined to NHD (and various QAQC attributes)
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return pseudo pCO2 validation figure written to file
pseudoValidation <- function(combined_pseudoValidation){
	#set ggplot theme
	theme_set(theme_classic())

	#wrangle
	forPlot <- combined_pseudoValidation %>%
		dplyr::mutate(huc2 = substr(huc4,1,2))%>%
		dplyr::group_by(huc2)%>%
		dplyr::summarise(pCO2_calib_ppm = sum(pCO2_calib_ppm, na.rm=T),
						pCO2_model_ppm = sum(pCO2_model_ppm, na.rm=T),
						n=n()) %>%
		tidyr::drop_na() %>%
		dplyr::mutate(pCO2_calib_ppm = pCO2_calib_ppm/(n()*2),
						pCO2_model_ppm = pCO2_model_ppm/(n()*2))

	#BUILD PLOT------------------------------------------------
  	plot <- ggplot(forPlot, aes(x=pCO2_calib_ppm, y=pCO2_model_ppm)) +
    	geom_abline(linetype='dashed', color='darkgrey', size=2)+
    	geom_point(size=8)+
		#scale_color_brewer(palette='Set2', name='', labels=c('Lake/Reservoir', 'River'))+
    	xlab('HUC2 median in situ pCO2 [ppm]')+
    	ylab('HUC2 median modeled pCO2 [ppm]')+
    	scale_y_log10()+
    	scale_x_log10()+
    	theme(axis.text=element_text(size=20),
        	  axis.title=element_text(size=24,face="bold"),
          	legend.text = element_text(size=20),
          	legend.position='bottom',
          	plot.title = element_text(size = 30, face = "bold"))

	#WRITE TO FILE-------------------------------------------------
  	ggsave('cache/figures/modelPseudoValidation.jpg', plot, width=10, height=10)
  	
	#RETURN FILE PATH--------------------------------------------
	return(paste('see cache/figures/modelPseudoValidation.jpg'))
}