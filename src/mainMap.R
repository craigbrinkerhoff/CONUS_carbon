library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)
library(classInt)
#library(viridis)

path_to_data <- '/nas/cee-water/cjgleason/craig/CONUS_ephemeral_data' #path to data repo (separate from code repo)

start<-Sys.time()
# Read shapefile with sf 
codes<-c("0101")
for (i in codes){
  huc2 <- substr(i, 1, 2)
  results <- tar_read(paste0('final_',i))

  print(paste0('HUC4 ', i, ' running'))
  # Import shapefile
#  shpPath <- paste0("NHD", i,"F")
  shapefile <- sf::st_read(dsn = paste0(path_to_data, '/HUC2_', huc2, '/NHDPLUS_H_', i, '_HU4_GDB/NHDPLUS_H_', i, '_HU4_GDB.gdb'), layer='NHDFlowline')
  #shapefile <- read_sf(dsn = paste0(path_to_data, '/H'), layer = shpPath)
  shapefile<-shapefile %>% filter(!is.na(StrmOrd))
  # Adding column based on other column:
  shapefile<-shapefile %>%
    mutate(CO2_col = case_when(
      CO2_ppm <= 1000 ~ '0-1000'
      ,CO2_ppm <= 1500 ~ '1000-1500'
      ,CO2_ppm <= 2500 ~ '1500-2500'
      ,CO2_ppm <= 4000 ~ '2500-4000'
      ,TRUE ~ '4000-16000'
    ))
  shapefile<-shapefile[,c("geometry","CO2_col","StrmOrde")] 
}  

# Map the spatial data
map<-ggplot() +
  geom_sf(data = shapefile, aes(color = CO2_col, size = factor(StrmOrd) ,alpha = factor(StrmOrd))) +
 # scale_color_viridis(discrete=TRUE) +
  scale_color_brewer(palette='RdBu', discrete=TRUE)+
  scale_size_manual(values = c(
    "1" = .2, "2" = .3, "3" = .4, "4" = .5,
    "5" = .8, "6" = 1.3, "7" = 1.7, "8" = 2.3, 
    "9" = 2.9, "10" = 3.6, "11" = 4.4, "12" = 5.2))+
  scale_alpha_manual(values = c(
    "1" = 1, "2" = 1, "3" = .8, "4" = .6,
    "5" = .4, "6" = .3, "7" = .3, "8" = .3, 
    "9" = .3, "10" = .3, "11" = .3, "12" = .3))+
  theme_classic()

rm(list=ls()[ ls() %in% c("shapefile")])


codes<-c("0102","0103","0104","0105","0106","0107","0108")#,"0109","0110",
         #"0202","0203","0204","0205","0206","0207","0208",
         #"0301","0302","0303","0304","0305","0306","0307","0308","0309","0310","0311","0312","0313","0314","0315","0316","0317","0318",
         #"0401","0402","0403","0404","0405","0406","0407","0408","0409","0410","0411","0412","0413","0414","0418","0419","0420","0424","0426","0427","0429","0430",
         #"0501","0502","0503","0504","0505","0506","0507","0508","0509","0510","0511","0512","0513","0514",
         #"0601","0602","0603","0604",
         #"0701","0702","0703","0704","0705","0706","0707","0708","0709","0710","0712","0713",
         #"0804","0805","0808",
         #"0901","0902","0903","0904",
         #"1002","1003","1004","1005","1006","1007","1008","1009","1010","1011","1012","1013","1014","1015","1016","1017","1018","1019","1020","1021","1022","1023","1024","1025","1026","1027","1028","1029",
         #"1101","1102","1103","1104","1105","1106","1107","1108","1109","1110","1111","1112","1113","1114",
         #"1201","1202","1203","1204","1205","1206","1207","1208","1209","1210","1211",
         #"1301","1302","1303","1304","1305","1306","1307","1308","1309",
         #"1401","1402","1403","1404","1405","1406","1407","1408",
         #"1501","1502","1503","1504","1505","1506","1507","1508",
         #"1601","1602","1603","1604","1605","1606",
         #"1701","1702","1703","1704","1705","1706","1707","1708","1709","1710a","1710","1711","1712",
         #"1801","1802","1803","1804","1805","1806","1807","1808","1809","1810")

for (i in codes){
  print(paste0('HUC4 ', i, ' running'))
  # Import shapefile
  shpPath <- paste0("NHD", i,"F")
  shapefile <- read_sf(dsn = "R merged shpfiles/", layer = shpPath)
  shapefile<-shapefile %>% filter(!is.na(StrmOrd))
  # Adding column based on other column:
  shapefile<-shapefile %>%
    mutate(CO2_col = case_when(
      CO2_ppm <= 1000 ~ '0-1000'
      ,CO2_ppm <= 1500 ~ '1000-1500'
      ,CO2_ppm <= 2500 ~ '1500-2500'
      ,CO2_ppm <= 4000 ~ '2500-4000'
      ,TRUE ~ '4000-16000'
    ))
  shapefile<-shapefile[,c("geometry","CO2_col","StrmOrd")] 
  map<-map + geom_sf(data = shapefile, aes(color = CO2_col, size = factor(StrmOrd),alpha = factor(StrmOrd))) 
  rm(list=ls()[ ls() %in% c("shapefile")])
} 
time2<-Sys.time()
start-time2
#write to pdf png or svg
ggsave(filename="cache/figures/mainMap.jpg",plot=map,width=24,height=18,units="in")
done<-Sys.time()

start-time2
time2-done
