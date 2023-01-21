## Calculates Raymond CO2 flux using their CO2 and their hydrography
## Winter 2022
## Craig Brinkerhoff/Brian Saccardi


#' Runs rayond upscaling model per huc2 region
#'
#' @name runRaymondModel
#'
#' @param path_to_data: path to NHD geodatabases
#' @param HUC2:
#' @param glorich_data: obsered CO2 concentrations per region used to upscale
#' @param Catm: atmospheric CO2 constant [ppm]
#' @param final_x: hydrography objects/routing files
#'
#' @import dplyr
#' @import terra
#' @import dplyr
#'
#' @return final upscaled CO2 flux estimates for a given HUC2 region
runRaymondModel <- function(path_to_data, HUC2, glorich_data,hydrographyList) {
  ##READ IN HYDROSHEDS AND SET UP RIVER NETWORK PETE STYLE------------------
  hydrosheds <- terra::vect(paste0(path_to_data, '/HUC_', HUC2, '/hydrosheds_', HUC2, '.shp'))
  dem <- terra::rast(paste0(path_to_data, '/HUC_', HUC2, '/GMTED2010_', HUC2, '.tif'))
  lakes <- terra::vect(paste0(path_to_data, '/HUC_', HUC2, '/glwd_', HUC2, '.shp')) #pre-filtered to >= 3.16km2 and no rivers
  runoff <- read.csv('data/2008gb003281-ds01.csv')

  #get HUC and coscat IDs and Fekete runoff
  coscat <- glorich_data[glorich_data$HUC2 == HUC2,]$coscat #get coscat Brian associated with HUC
  runoff <- dplyr::filter(runoff, River.id..Fekete..et.al...2002. == coscat) #get Fekete runoff from coscat
  yield <- exp(runoff[1,]$Ln.Precipitation...mm.day.1.)/86400*0.001 #[m3/s]

  #get glorich co2
 # riverCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$river #[ppm]
#  lakeCO2 <- glorich_data[glorich_data$HUC2 == HUC2,]$lake #[ppm]

  #Use our water temperature data to get a basin average schmidt number
  results <- do.call("rbind", hydrographyList) #make HUC2 river network
  riverCO2 <- median(results[results$waterbody == 'River',]$CO2_ppm) #use calibrated value to remove uncertainties from CO2s
  lakeCO2 <- median(results[results$waterbody == 'Lake/Reservoir',]$CO2_ppm) #use calibrated value to remove uncertainties from CO2s
  temp_c <- mean(results$Water_temp_c, na.rm=T)
  henry <- henry_func(mean(results$Water_temp_c, na.rm=T))
  sc <- 1911-118.11*mean(temp_c, na.rm=T)+3.453*mean(temp_c, na.rm=T)^2-0.0413*mean(temp_c, na.rm=T)^3 #Raymond2012/Wanninkof 1991

  #average Q over the entire domain
  yield <- mean(hydrosheds$DIS_AV_CMS/(hydrosheds$UPLAND_SKM*1e6), na.rm=T) #[m/s]

  #Calculate slope by order using GMTED2010 DEM------
  max_elev <- terra::extract(dem, hydrosheds, fun=function(x){max(x, na.rm=T)})
  colnames(max_elev) <- c('row', 'max_elv_m')
  min_elev <- terra::extract(dem, hydrosheds, fun=function(x){min(x, na.rm=T)})
  colnames(min_elev) <- c('row', 'min_elv_m')

  slopes <- dplyr::left_join(max_elev, min_elev, by='row') %>%
    dplyr::select(!c('row'))

  hydrosheds$slope <- (slopes$max_elv_m - slopes$min_elv_m) / (hydrosheds$LENGTH_KM*1000)
  hydrosheds <- as.data.frame(hydrosheds)

  slope_by_order <- dplyr::group_by(hydrosheds, ORD_STRA) %>%
    dplyr::summarise(slope = median(slope, na.rm=T))

  ratio <- NA
  for(i in 2:3){ #for orders 2 to 4
    ratio[i] <- slope_by_order[i,]$slope / slope_by_order[i+1,]$slope
  }
  ratio <- mean(ratio, na.rm=T)

  slope_by_order$ORD_STRA <- slope_by_order$ORD_STRA+1
  slope_by_order <- rbind(slope_by_order, c(1, NA, NA, NA, NA, NA))
  slope_by_order <-  slope_by_order[order(slope_by_order$ORD_STRA),]

  for(i in 1){
    slope_by_order[i,]$slope <- slope_by_order[i+1,]$slope*(ratio)
  }
  for(i in 5:7) {
    slope_by_order[i,]$slope <- slope_by_order[i-1,]$slope*(1/ratio)
  }

  #Calculate river network length by order----------
  lengths_by_order <- dplyr::group_by(hydrosheds, ORD_STRA) %>%
    dplyr::summarise(order_length_km = sum(LENGTH_KM))

  ratio <- NA
  for(i in 1:3){ #for orders 1 to 4
    ratio[i] <- lengths_by_order[i,]$order_length_km / lengths_by_order[i+1,]$order_length_km
  }
  ratio <- mean(ratio)

  lengths_by_order$ORD_STRA <- lengths_by_order$ORD_STRA+1
  lengths_by_order <- rbind(lengths_by_order, c(1, lengths_by_order[1,]$order_length_km*ratio))
  lengths_by_order <-  lengths_by_order[order(lengths_by_order$ORD_STRA),]

  #calculate river network width by order--------------
  width_by_order <- dplyr::group_by(as.data.frame(hydrosheds), ORD_STRA) %>%
    dplyr::summarise(DA_skm = mean(UPLAND_SKM, na.rm=T))

  width_by_order[1:3,]$DA_skm <- NA

  #scale DA, get Q, get w
  ratio <- NA
  for(i in 4:5){ #for orders 4 to 6
    ratio[i] <- width_by_order[i+1,]$DA_skm / width_by_order[i,]$DA_skm
  }
  ratio <- mean(ratio, na.rm=T)

  width_by_order$ORD_STRA <- width_by_order$ORD_STRA+1
  width_by_order <- rbind(width_by_order, c(1, NA))
  width_by_order <-  width_by_order[order(width_by_order$ORD_STRA),]

  for(i in 4:1){ #scale DA for order 1-4
    width_by_order[i,]$DA_skm <- width_by_order[i+1,]$DA_skm*(1/ratio)
  }

  width_by_order$Q_cms <- width_by_order$DA_skm*1e6*yield #[m3/s]

  width_by_order$eq1 <- exp(0.423*log(width_by_order$Q_cms) + 2.56) #Raymond 2012
  width_by_order$eq2 <- exp(0.51*log(width_by_order$Q_cms) + 1.86) #Raymond 2013
  width_by_order$width_m <- apply(width_by_order[,4:5], 1, mean)

  n_order <- nrow(width_by_order)
  eph_v <- c(c(0.055, 0.036, 0.026, 0.02, 0.014), rep(0, n_order-5)) #ephemeral fractions from raymond 2013, only done to orders 1-5
  pete_network <- data.frame('order'=width_by_order$ORD_STRA,
                             'width_m'=width_by_order$width_m,
                             'length_km'=lengths_by_order$order_length_km,
                             'slope'=slope_by_order$slope,
                             'frac_ephemeral'=eph_v,
                             'Q_cms'=width_by_order$Q_cms)

  pete_network$SA_m2 <- pete_network$width_m*pete_network$length_km*1000
  pete_network$SA_m2 <- pete_network$SA_m2*(1-pete_network$frac_ephemeral) #account for ephemeral streams

  #Calculate a regional k
  pete_network$velocity_2012 <- exp(-1.64 + 0.285*log(pete_network$Q_cms))
  pete_network$velocity_2013 <- exp(-1.06 + 0.12*log(pete_network$Q_cms))
  pete_network$velocity <- rowMeans(pete_network[,c('velocity_2012', 'velocity_2013')], na.rm=TRUE)
  pete_network$k_600 <- pete_network$slope*pete_network$velocity*2841.6+2.03 #[m/day]
  pete_network$k_co2 <- pete_network$k_600/(600/sc)^-0.5
  pete_network$k_co2_m_s <- (pete_network$k_co2)/(24*60*60) #[m/s]
  rivers_k_co2_RG_m_s <- weighted.mean(pete_network$k_co2_m_s, pete_network$SA_m2) #normalize by surface area for each order

  #calculate a regional fco2
  rivers_FCO2_RG <- ((riverCO2-390)*henry*1e-6)*rivers_k_co2_RG_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]

  #calculate regional carbon emissions rate
  rivers_FCO2_RG_total <- rivers_FCO2_RG*sum(pete_network$SA_m2) #[g-C/yr]

  #lakes (per stream order)---------------------------------------------------------------------------------------
  #pareto lake distribution by lake size bin
  c <- 0.85 #Raymond 2013

  #Scaling below lake minimum of 3.16km2
  log_cumm_abund <- NA
  binned_avg_lake_area <- NA
  lake_bins <- c(0.001, 0.1, 1, 3.16) #[km2]
  for (i in 2:4){
    Amax <- lake_bins[i]
    Amin <- lake_bins[i-1]
    upper_int <- nrow(lakes[lakes$AREA_SKM > Amax,]) + 1 #GLWD

    #Pareto distriubtion for # of lakes
    log_cumm_abund[i] <- log(upper_int) + (log(Amax)-log(Amin))*c

    #Downing etal 2006 equation 8
    binned_avg_lake_area[i] <- c*((-Amax*Amin^c+Amax^c*Amin)/((c-1)*(Amax^c-Amin^c))) #[km2]

  }
  log_cumm_abund <- log_cumm_abund[-1]
  binned_avg_lake_area <- binned_avg_lake_area[-1]

  #total huc lake/reservoir surface area [km2]
  pete_lake_reservoir <- data.frame('area_bin'=c(0.1, 1, 3.16),
                                    'mean_area_km2'=binned_avg_lake_area,
                                    'binned_num_lakes'=round(exp(log_cumm_abund),0),
                                    'obs_surface_area_km2'=NA) #[km2])

  #GLWD lake areas (no scaling)
  lakes$area_bin <- ifelse(lakes$AREA_SKM <= 10, 10, max(lakes$AREA_SKM))
  lakes <- dplyr::group_by(as.data.frame(lakes), area_bin) %>%
    dplyr::summarise('mean_area_km2'=NA,
              'binned_num_lakes'=NA,
              'obs_surface_area_km2'=sum(AREA_SKM))

  pete_lake_reservoir <- rbind(pete_lake_reservoir, lakes)

  pete_lake_reservoir$surface_area_fin_skm <- ifelse(is.na(pete_lake_reservoir$obs_surface_area_km2)==1, pete_lake_reservoir$mean_area_km2*pete_lake_reservoir$binned_num_lakes, pete_lake_reservoir$obs_surface_area_km2)

  pete_lake_reservoir$k600_m_dy <- c(0.54, 1.16, 1.32, 1.32, 1.9) #[m/dy] Read etal 2012
  pete_lake_reservoir$kco2 <- pete_lake_reservoir$k600_m_dy/(600/sc)^-0.5
  pete_lake_reservoir$k_co2_m_s <- (pete_lake_reservoir$kco2)/(24*60*60) #[m/s]

  #calculate a regional fco2
  pete_lake_reservoir$lakes_FCO2_RG <- ((lakeCO2-390)*henry*1e-6)*pete_lake_reservoir$k_co2_m_s*(1/0.001)*12.01*(60*60*24*365) #[g-C/m2/yr]

  #calculate regional carbon emissions rate
  lakes_FCO2_RG_total <- sum(pete_lake_reservoir$lakes_FCO2_RG*pete_lake_reservoir$surface_area_fin_skm*1e6) #[g-C/yr]

  #save results to file-----------------------------------
  network_fluxes_RG <- data.frame('huc2'=HUC2,
                                  'sumFCO2_RG_TgC_yr' = c(rivers_FCO2_RG_total*1e-12, lakes_FCO2_RG_total*1e-12),
                                  'waterbody'=c('River', 'Lake/Reservoir'))
 return(network_fluxes_RG)

}
