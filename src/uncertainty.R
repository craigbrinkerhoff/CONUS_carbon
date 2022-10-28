rm(list=ls())

model <- tar_read(final_0107)

logpCO2_err <- rnorm(100, 0.009, 0.42)
logQ_err <- rnorm(100, -0.022, 1.22)

model$k_co2_m_s*(((exp(model$CO2_ppm+logQ_err) - Catm)*model$henry)/1000000)*(1/0.001)*12.01*(60*60*24*365)
model$FCO2_gC_yr <- ifelse(model$waterbody == 'River',
                                 model$FCO2_gC_m2_yr * model$W_m * model$LengthKM * 1000, #river g-C/yr
                                 model$FCO2_gC_m2_yr * model$lakeSA_m2) #lake/reservoir g-C/yr

#given river
#W <- matrix(nrow=nrow(temp), ncol=100)
#for(i in 1:nrow(temp)){
#  for(n in 1:100){
#      W[i,n] <- rnorm(1, mean=12, sd=2)*temp[i,]$Q_m3_s^(rnorm(1, mean=0.4, sd=0.1)) #m
#  }
#}
