#Loading the required libraries

library(graphics)
library(forecast)

#loading the file into R

bitcoin_price_data <- read.csv("bitcoin_price_historical_data.csv", header = T, sep = ',')

nrow(bitcoin_price_data)

#Converting dataframe to time series

indata <- bitcoin_price_data$Price[1:28]
timeser <- ts(indata)
plot(timeser)

#Building final model in R

autoarimafit<- auto.arima(timeser)
in_data_pred<-fitted(autoarimafit)

plot(autoarimafit$x, col="black")
lines(in_data_pred, col="red")

#Let's evaluate the model using MAPE

fcast_auto_arima <- predict(autoarimafit, n.ahead = 4)

outdata <- bitcoin_price_data$Price[29:32]

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

out_data_pred <- ts(fcast_auto_arima$pred)
total_timeser <- ts(bitcoin_price_data$Price)
plot(total_timeser, col = "black", lwd = 2)
lines(c(in_data_pred, out_data_pred), col = "blue", lwd = 2)
lines(in_data_pred, col="red", lwd = 2)

# Second attempt
# Converting dataframe to time series

indata <- bitcoin_price_data$Price[1:29]
timeser <- ts(indata)
plot(timeser)

#Building final model in R

autoarimafit<- auto.arima(timeser)
in_data_pred<-fitted(autoarimafit)

plot(autoarimafit$x, col="black")
lines(in_data_pred, col="red")

#Let's evaluate the model using MAPE

fcast_auto_arima <- predict(autoarimafit, n.ahead = 3)

outdata <- bitcoin_price_data$Price[30:32]

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

out_data_pred <- ts(fcast_auto_arima$pred)
total_timeser <- ts(bitcoin_price_data$Price)
plot(total_timeser, col = "black", lwd = 2)
lines(c(in_data_pred, out_data_pred), col = "blue", lwd = 2)
lines(in_data_pred, col="red", lwd = 2)
