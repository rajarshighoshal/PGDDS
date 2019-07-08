library(graphics)
library(forecast)
library(tseries)

exchange_rate_data <- read.csv("exchange-rate-twi.csv", header = T, sep = ',')

nrow(exchange_rate_data)

timeser <- ts(exchange_rate_data$Exchange.Rate.TWI)
plot(timeser)

model <- lm(Exchange.Rate.TWI ~ ., data = exchange_rate_data)

globalpred <- predict(model, exchange_rate_data)
localpred <- exchange_rate_data$Exchange.Rate.TWI - globalpred

# Compute and plot the ACF for the localpred
acf(localpred, level=95, lag.max=40, main="ACF Plot for localprde")

# Compute and plot PACF for the localpred
pacf(localpred, level = 95,lag.max = 40, main = "PACF Plot for localpred")

armapred <- arima.sim(model=list(ar=0.96),n=304)

resi <- localpred - armapred

# KPSS test of resi
kpss.test(resi)
