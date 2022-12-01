#use google earth engine to add worldclim air temps per HUC12 (csvs) and then we add to this hydrography--------------------------------------------------------
#dsnPathHUC12 <- paste0(path_to_data, '/HUC2_', huc2, '/WBD_', huc2, '_HU2_Shape/Shape/WBDHU12.shp')
#NHD_HUC12 <- sf::st_read(dsnPathHUC12) #HUC 12 basins
#NHD_HUC12$HUC12 <- as.numeric(as.character(NHD_HUC12$huc12))
#airTemp_HU12 <- read.csv(paste0('data/airTemp_means_', huc2, '.csv')) #read in mean annual air temps values per HUC 12
#hucAirTemp <- dplyr::left_join(NHD_HUC12, airTemp_HU12, by=c('HUC12'='huc12')) #join
#hucAirTemp <- dplyr::select(hucAirTemp, 'mean')
#colnames(hucAirTemp) <- c('airTemp_mean_c', 'geometry')
#hucAirTemp$airTemp_mean_c <- hucAirTemp$airTemp_mean_c * 0.1 #saved with scale factor of 10 in dataset
#nhd <- sf::st_intersection(nhd, hucAirTemp) #intersect rivers with HUC12s with air temps.
