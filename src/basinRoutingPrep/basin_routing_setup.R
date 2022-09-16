library(dplyr)
library(tidyr)


HUC4_calibration_txt <- read.csv("C:/Users/craig/Downloads/HUC4_calibration_txt.csv")

HUC4_calibration_txt$HUC4 <- substr(HUC4_calibration_txt$HUC4,2,5)
HUC4_calibration_txt$HUC2 <- ifelse(nchar(HUC4_calibration_txt$HUC2)==1, paste0('0',HUC4_calibration_txt$HUC2), HUC4_calibration_txt$HUC2)
HUC4_calibration_txt$X <- substr(HUC4_calibration_txt$X,2,5)
HUC4_calibration_txt$X.1 <- substr(HUC4_calibration_txt$X.1,2,5)
HUC4_calibration_txt$X.2 <- substr(HUC4_calibration_txt$X.2,2,5)
HUC4_calibration_txt$X.3 <- substr(HUC4_calibration_txt$X.3,2,5)
HUC4_calibration_txt$X.4 <- substr(HUC4_calibration_txt$X.4,2,5)
HUC4_calibration_txt$X.5 <- substr(HUC4_calibration_txt$X.5,2,5)

df <- gather(HUC4_calibration_txt, key=key, value=value, c('X', 'X.1', 'X.2', 'X.3', 'X.4', 'X.5'))

colnames(df) <- c('toBasin', 'HUC2', 'River', 'Lake', 'key', 'HUC4')

df2 <- select(df, c('HUC4', 'HUC2', 'toBasin'))

df2 <- df2[!duplicated(df2),]
df2$HUC4 <- ifelse(df2$HUC4 == "", df2$toBasin, df2$HUC4)

g <- group_by(df2, HUC4) %>%
  summarise(n=n())
df2 <- left_join(df2, g)

df2 <- filter(df2, n == 1 | (n > 1 & toBasin != HUC4))

df2$toBasin <- ifelse(df2$toBasin == df2$HUC4, NA, df2$toBasin)
df2 <- select(df2, !'n')

#assign basin processing levels
df2$level <- ifelse(is.na(df2$toBasin), 0,NA)
while(any(is.na(df2$level))==1){ #to handle downstream basins
  for(i in 1:nrow(df2)){
    ids <- which(df2$toBasin == df2[i,]$HUC4)
    df2[i,]$level <- ifelse(length(ids) > 0, max(df2[ids,]$level) + 1, 0)
  }
}



write.csv(df2, 'C:/Users/craig/Downloads/HUC4_lookup.csv')
