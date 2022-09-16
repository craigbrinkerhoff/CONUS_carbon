path_to_data <- '/nas/cee-water/cjgleason/craig/CONUS_ephemeral_data' #path to data repo (separate from code repo)
source('src/utils.R')
options(scipen=999)
lookup <- readr::read_csv('data/HUC4_lookup.csv')

ids <- which(is.na(lookup$toBasin) == 0)

df <- data.frame()
for(i in ids){
  huc4_d <- lookup[i,]$toBasin
  huc4 <- lookup[i,]$HUC4

  print(huc4)

  indiana_hucs <- c('0508', '0509', '0514', '0512', '0712', '0404', '0405', '0410') #indiana-effected basins

  sf::sf_use_s2(FALSE)

  #get current basin
  huc2 <- substr(huc4, 1, 2)
  dsnPath <- paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4, '_HU4_GDB/NHDPLUS_H_', huc4, '_HU4_GDB.gdb')
  if(huc4 %in% indiana_hucs) {
    nhd <- sf::st_read(paste0(path_to_data, '/HUC2_', huc2, '/indiana/indiana_fixed_', huc4, '.shp'))
    nhd <- sf::st_zm(nhd)
    colnames(nhd)[10] <- 'WBArea_Permanent_Identifier'
    nhd$NHDPlusID <- round(nhd$NHDPlusID, 0) #some of these have digits for some reason......
  }
  else{
    nhd <- sf::st_read(dsn=dsnPath, layer='NHDFlowline', quiet=TRUE)
    nhd <- sf::st_zm(nhd)
    nhd <- fixGeometries(nhd)
  }
  NHD_HR_VAA <- sf::st_read(dsn = dsnPath, layer = "NHDPlusFlowlineVAA", quiet=TRUE) #additional 'value-added' attributes
  NHD_HR_EROM <- sf::st_read(dsn = dsnPath, layer = "NHDPlusEROMMA", quiet=TRUE) #mean annual flow table

  nhd <- dplyr::left_join(nhd, NHD_HR_VAA)
  nhd <- dplyr::left_join(nhd, NHD_HR_EROM)

  nhd$StreamOrde <- nhd$StreamCalc #stream calc handles divergent streams correctly: https://pubs.usgs.gov/of/2019/1096/ofr20191096.pdf
  nhd$Q_cms <- nhd$QEMA * 0.0283 #cfs to cms
  nhd <- dplyr::filter(nhd, Q_cms > 0) #remove streams with no flow
  nhd <- dplyr::filter(nhd, StreamOrde > 0 & is.na(HydroSeq)==0 & FlowDir == 1)



  #get downstream basin
  huc2 <- substr(huc4_d, 1, 2)
  dsnPath <- paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', huc4_d, '_HU4_GDB/NHDPLUS_H_', huc4_d, '_HU4_GDB.gdb')
  if(huc4_d %in% indiana_hucs) {
    nhd_d <- sf::st_read(paste0(path_to_data, '/HUC2_', huc2, '/indiana/indiana_fixed_', huc4_d, '.shp'))
    nhd_d <- sf::st_zm(nhd_d)
    colnames(nhd_d)[10] <- 'WBArea_Permanent_Identifier'
    nhd_d$NHDPlusID <- round(nhd_d$NHDPlusID, 0) #some of these have digits for some reason......
  }
  else{
    nhd_d <- sf::st_read(dsn=dsnPath, layer='NHDFlowline', quiet=TRUE)
    nhd_d <- sf::st_zm(nhd_d)
    nhd_d <- fixGeometries(nhd_d)
  }
  NHD_HR_VAA <- sf::st_read(dsn = dsnPath, layer = "NHDPlusFlowlineVAA", quiet=TRUE) #additional 'value-added' attributes
  NHD_HR_EROM <- sf::st_read(dsn = dsnPath, layer = "NHDPlusEROMMA", quiet=TRUE) #mean annual flow table

  nhd_d <- dplyr::left_join(nhd_d, NHD_HR_VAA)
  nhd_d <- dplyr::left_join(nhd_d, NHD_HR_EROM)

  nhd_d$StreamOrde <- nhd_d$StreamCalc #stream calc handles divergent streams correctly: https://pubs.usgs.gov/of/2019/1096/ofr20191096.pdf
  nhd_d$Q_cms <- nhd_d$QEMA * 0.0283 #cfs to cms
  nhd_d <- dplyr::filter(nhd_d, Q_cms > 0) #remove streams with no flow
  nhd_d <- dplyr::filter(nhd_d, StreamOrde > 0 & is.na(HydroSeq)==0 & FlowDir == 1)


  nhd$huc4 <- huc4
  nhd_d$huc4 <- huc4_d

  #check routing
  if(huc4 == '0707' & huc4_d == '0706'){nhd[nhd$NHDPlusID == 22001100000027,]$ToNode <- 22000400010646}
  if(huc4 == '0804' & huc4_d == '0807'){nhd[nhd$NHDPlusID == 20000800014282,]$ToNode <- 20000300026950}
  if(huc4 == '0514' & huc4_d == '0801'){nhd[nhd$NHDPlusID == 24000100569580,]$ToNode <- 22000100085737}


  nhd_filt <- dplyr::filter(nhd, ToNode %in% nhd_d$FromNode)
  exportedCO2_ppm <- nhd_filt$CO2_ppm
  exported_ToNode <- nhd_filt$ToNode
  exported_Q <- nhd_filt$Q_m3_s

  temp <- as.data.frame(dplyr::filter(nhd_d, FromNode %in% exported_ToNode))

  if(nrow(temp)==0){
    print(paste0('broke! Basin: ', huc4, ' and downstream basin: ', huc4_d))
    temp <- data.frame('huc4'=huc4, 'FromNode'=NA, 'huc_up'=NA)
    df <- rbind(df, temp)
  }
  else{
    temp <- select(temp, c('huc4', 'FromNode'))
    temp$huc_up <- huc4
    df <- rbind(df, temp)
  }
  write.csv(df, 'test.csv')
}
