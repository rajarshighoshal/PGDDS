#Loading the required libraries

library(graphics)
library(forecast)

#loading the file into R

bitcoin_price_data <- read.csv("bitcoin_price_historical_data.csv", header = T, sep = ',')

nrow(bitcoin_price_data)

#Converting dataframe to time series

timeser <- ts(bitcoin_price_data$Price)
plot(timeser)

#Building final model in R

bitcoin_price_data$log_Price <- log(bitcoin_price_data$Price)

globalfit<- lm(log_Price ~ Months, data = bitcoin_price_data)
global_pred<-predict(globalfit)

log_timeser <- log(timeser)

plot(log_timeser, col="black")
lines(global_pred, col="red")

local_pred <- log_timeser-global_pred
plot(local_pred, col="red")
