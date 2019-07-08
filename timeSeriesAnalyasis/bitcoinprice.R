library(forecast)

bitcoin_price_data <- read.csv("bitcoin_price_historical_data.csv", header = T)

timestamp <- ts(bitcoin_price_data$Price)
plot(timestamp)

lmfit <- lm(log(Price) ~ ., data = bitcoin_price_data)
globalpred <- predict(lmfit, bitcoin_price_data)
plot(globalpred, col='red', type = "l")

log_timestamp <- log(timestamp)
plot(log_timestamp, col="blue")
lines(globalpred, col="red")

localpred <- log(bitcoin_price_data$Price) - globalpred
plot(localpred, col="red", type="l")
