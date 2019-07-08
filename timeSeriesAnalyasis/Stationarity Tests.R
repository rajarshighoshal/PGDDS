#install.packages("tseries")

library(tseries)

#Stationarity test
exch <- read.csv("exchange-rate-twi.csv", stringsAsFactors=FALSE)

#ADF test
adf.test(exch$Exchange.Rate.TWI, alternative="stationary")

#KPSS test
kpss.test(exch$Exchange.Rate.TWI)
