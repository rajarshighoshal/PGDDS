############################ GLOBAL MART SALES FORECASTING #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
# 5. Model Evaluation

#####################################################################################

# 1. Business Understanding: 

# Retail-Giant Sales Forecasting: 
# "Global Mart" is an online store super giant having worldwide operations. 
# The store caters to 7 different market segments and in 3 major categories. 
# Objective: To find out 2 most profitable (and consistent) segment from these (7X 3) 21 buckets 
# and forecast the sales and demand for these segments at granular level.

#####################################################################################

# 2. Data Understanding: 

# Loading Neccessary libraries

library(tidyverse)
library(forecast)
library(tseries)
require(graphics)
library(lubridate)
library(ggthemes)
library(gridExtra)
library(forecast)

# Loading Data

#setwd("C:/Users/Pritha/Desktop/DS C7 UPGRAD IIITB COURSE/Course 4/Case study")
getwd()
global_mart_data <- read.csv("Global Superstore.csv",sep=",",header = T)

# Understanding Dimensions
dim(global_mart_data)
# 51290 records of 24 attributes

# View the dataset
#View(global_mart_data)

#Structure of the dataset
str(global_mart_data)

# The dataset has the transaction level data.
# Each row represents a particular order made on the online store with 24 attributes related to each transaction. 
# The different attributes are:  
# Row ID - Unique for each row
# Order ID, Order Date, Customer ID, Segment, City, Product ID, Category, Product Name, Profit etc.
# Sales is our prediction variable

#####################################################################################

# 3. Data Preparation

# For checking for missing value, ceating a duplicate dataset with stringsAsFactors = F
global_mart_data_char <- read.csv("Global Superstore.csv",sep=",",stringsAsFactors = F,header = T)
sum(sapply(global_mart_data_char, function(x) length(which(x == "")))) 
#0 missing Values

# Checking for NA
sapply(global_mart_data, function(x) sum(is.na(x)))

# Postal.Code has 41296 NA values
# Removing Postal.Code from our dataset, as it is not required
global_mart_data <- global_mart_data[,-12]

# checking for duplicates

sum(duplicated(global_mart_data)) 
# 0. No 

# Change Order.Date from factor to Date
global_mart_data$Order.Date<-as.Date(global_mart_data$Order.Date,"%d-%m-%Y")

# Change Ship.Date from factor to Date
global_mart_data$Ship.Date<-as.Date(global_mart_data$Ship.Date,"%d-%m-%Y")

# Arranging the dataset in ascending order of order.date
global_mart_data <- global_mart_data[order(global_mart_data$Order.Date),]

# The dataset is arranged as per Order.Date. We have sales records from Jan2011 to Dec 2014

# Creating timestamp based on month of ordering starting from 01/2011 as month 1
global_mart_data$timestamp <- month(global_mart_data$Order.Date) + 
  (year(global_mart_data$Order.Date) - min(year(global_mart_data$Order.Date))) * 12
# Total 48 Timestamps

View(global_mart_data)
# The seven market details
summary(global_mart_data$Market)
# Africa   APAC  Canada   EMEA     EU   LATAM     US 
#  4587  11002     384   5029  10000    10294   9994 

# The three market Segment details
summary(global_mart_data$Segment)
# Consumer   Corporate  Home Office 
#  26518       15429        9343 

# The three category details
summary(global_mart_data$Category)
# Furniture Office Supplies      Technology 
#  9876           31273           10141 


# Data Visualisation: 

# Finding Sales for different market

Plot1<- ggplot(global_mart_data, aes(x = Market, fill = Sales)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Market Sales", x = "Market", y = "Total Sales") + 
  scale_fill_brewer(palette = "Pastel1")+theme_stata()

# Finding Sales for different segments

Plot2<- ggplot(global_mart_data, aes(x = Segment, fill = Sales)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Total Sales for different Segments", x = "Segments", y = "Total Sales") + 
  scale_fill_brewer(palette = "Pastel1")+theme_stata()

# Finding Sales for different categories

Plot3<- ggplot(global_mart_data, aes(x = Category, fill = Sales)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Total Sales for different Categoies", x = "Category", y = "Total Sales") + 
  scale_fill_brewer(palette = "Pastel1")+theme_stata()

grid.arrange(Plot1, Plot2, Plot3)
# It can be observed that the consumer segment has maximum total sales. 
# EU, APAC, LATAM are the markets for maximum total sales

# Finding Profit for different market

Plot4<- ggplot(global_mart_data, aes(x = Market, fill = Profit)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Market Profit", x = "Market", y = "Total Profit") + 
  scale_fill_brewer(palette = "Pastel1")+theme_stata()

# Finding Profit for different Segments

Plot5<- ggplot(global_mart_data, aes(x = Segment, fill = Profit)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Total Profit of Each Segment", x = "Segment", y = "Total Profit") + 
  scale_fill_brewer(palette = "Pastel1")+theme_stata()

# Finding Profit for different Categories

Plot6<- ggplot(global_mart_data, aes(x = Category, fill = Profit)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Total Profit of Each Category", x = "Category", y = "Total Profit") + 
  scale_fill_brewer(palette = "Pastel1")+theme_stata()


grid.arrange(Plot4, Plot5, Plot6)
# It can be observed that the consumer segment has maximum total profit. 
# EU, APAC, LATAM are the markets for maximum total profit


## create a colummn combining market and category
global_mart_data$market_seg <- paste(global_mart_data$Market,global_mart_data$Segment, sep = "_")
summary(as.factor(global_mart_data$market_seg))
# The 21 market segment buckets are created

# for every market_segment, three attributes is required over the order date 
# to arrive at the monthly values for these attributes
# Retaining only the useful columns for further analysis

usefulCols <- c("timestamp", "Sales", "Quantity", "Profit", "market_seg")
global_mart_data_useful <- global_mart_data[, colnames(global_mart_data) %in% usefulCols]


# Segmenting and storing each df in a list
X <- split(global_mart_data_useful, global_mart_data_useful$market_seg)


# Summarising profit, sales and quantity for each segment and update the list 
for(i in 1:length(X)) {
  X[[i]] <- X[[i]] %>% group_by(timestamp) %>% summarise(Monthly.Profit = sum(Profit), 
                                                         Monthly.Sales = sum(Sales), Monthly.Quantity = sum(Quantity))
}


# calculate coefficient of variation of the Profit for all 21 market segments
# CV (Coefficient of Variation) can be calculated as ratio of total profit and mean profit
# this will give a measure of consistency

CV <- list()
for(i in 1:length(X)) {
  CV[i] <- sd(X[[i]]$Monthly.Profit)/mean(X[[i]]$Monthly.Profit)
}
CV <- setNames(CV,  names(X))

CV <- stack(CV)
colnames(CV) <- c("cv_profit", "market_seg")

# calculate total profit for all 21 market segments

total.Profit <- list()
for(i in 1:length(X)) {
  total.Profit[i] <- sum(X[[i]]$Monthly.Profit)
}
total.Profit <- setNames(total.Profit,  names(X))

total.Profit <- stack(total.Profit)
colnames(total.Profit) <- c("total_profit", "market_seg")

# create a df combining CV of profit and total profit for each market segment 
seg_sel_metric <- cbind(total.Profit,CV)[-2]

#Sorting w.r.t. increasing CV
seg_sel_metric <- seg_sel_metric[order(seg_sel_metric$cv_profit),]
# " EU_Consumer and APAC_Consumer are two of the best segments 

#Sorting w.r.t. decreasing profit
seg_sel_metric <- seg_sel_metric[order(seg_sel_metric$total_profit, decreasing = T),]
# " APAC_Consumer and EU_Consumer are two of the best segments 

#### Final Conclusion of top two market segments as per Lowest CV and highest Profit ####
" APAC_Consumer and EU_Consumer are going to be our target segment for time series analysis "

# plot the CV and Profit of the segments
plot(seg_sel_metric$cv_profit, col = "red", type = "l")
plot(seg_sel_metric$total_profit, col = "blue", type = "l")


# Filtering out the data for APAC_Consumer and EU_Consumer
EU.consumer <- X$EU_Consumer[, c(-2)] #Removing the "Profit" column
APAC.consumer <- X$APAC_Consumer[, c(-2)] #Removing the "Profit" column


##############################################################################################
######################### Time-Series Modelling for APAC Consumer ###########################

# Data Preparation for Model Building 
#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

indata <- APAC.consumer[1:42,]
outdata <- APAC.consumer[43:48,]

nrow(indata) # 42
nrow(outdata) # 6

################################ APAC.Consumer: Sales Timeseries ########################################
sales.timeser <- ts(indata$Monthly.Sales)
plot(sales.timeser, col = "red", lwd = 3)

# smoothing the timeseries - Moving Average Method
# weight with 2
w <- 2
smoothed.salests <- stats::filter(sales.timeser, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
lines(smoothed.salests, col = "green", lwd = 2)
# weight with 1
w <- 1
smoothed.salests <- stats::filter(sales.timeser, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
lines(smoothed.salests, col = "blue", lwd = 2)

#choosing w = 2 leads to smoothing too much

# Hence, selecting  w = 1 for smoothing
# Smoothing left end of the time series

diff <- smoothed.salests[w+2] - smoothed.salests[w+1]
for (i in seq(w,1,-1)) {
  smoothed.salests[i] <- smoothed.salests[i+1] - diff
}

#Smoothing right end of the time series

n <- length(sales.timeser)
diff <- smoothed.salests[n-w] - smoothed.salests[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed.salests[i] <- smoothed.salests[i-1] + diff
}

# Plot the smoothed time series against the original ts
plot(sales.timeser, col = "red", lwd = 3)
lines(smoothed.salests, col="blue", lwd=2)

# Create a smoothed df 
smoothed.salesdf <- as.data.frame(cbind(indata$timestamp, as.vector(smoothed.salests)))
colnames(smoothed.salesdf) <- c("month", "sales")


#### Classical Decomposition Method ####

# fit using lm to get global trend line
lmfit.apac.sales <- lm(sales ~ sin(.5*month)  + cos(.5*month)
                       + month, data = smoothed.salesdf)
global_pred.sales <- predict(lmfit.apac.sales, month = smoothed.salesdf$month)
summary(global_pred.sales)
lines(smoothed.salesdf$month, global_pred.sales, col='green', lwd=2)

# local pred
local_pred.sales <- sales.timeser - global_pred.sales
plot(local_pred.sales, col = "coral", type = "l")
acf(local_pred.sales)
acf(local_pred.sales, type = "partial")
armafit.sales <- auto.arima(local_pred.sales)
tsdiag(armafit.sales)
armafit.sales
# ARIMA(0,0,0) with zero mean 
# sigma^2 estimated as 108886979:  log likelihood=-448.22
# AIC=898.44   AICc=898.54   BIC=900.17

# check if the residual series is white noise
resi.sales <- local_pred.sales - fitted(armafit.sales)

adf.test(resi.sales,alternative = "stationary")
# Dickey-Fuller = -4.5687, Lag order = 3, p-value = 0.01

kpss.test(resi.sales)
# KPSS Level = 0.033492, Truncation lag parameter = 1, p-value = 0.1
" both kpss and Dicky Fuller test suggests that the residual is a pure white noise "

#### Evaluate the Model using MAPE ####
global_fcast.sales <- predict(lmfit.apac.sales, data.frame(month = outdata$timestamp))
MAPE.class_dec.sales <- accuracy(global_fcast.sales, outdata$Monthly.Sales)[5]
MAPE.class_dec.sales
" 22.21365 "

## plot of predicted value along with original values
total_global_fcast.sales <- c(ts(global_pred.sales), ts(global_fcast.sales))
plot(ts(APAC.consumer$Monthly.Sales), col = "red")
lines(total_global_fcast.sales, col = "blue")

#### Auto ARIMA Method ####
autoarima.sales <- auto.arima(sales.timeser)
autoarima.sales
tsdiag(autoarima.sales)
plot(autoarima.sales$x, col = "red")
lines(fitted(autoarima.sales), col = "blue")

# residual series
resi.sales.autoarima <- sales.timeser - fitted(autoarima.sales)
plot(resi.sales.autoarima)

adf.test(resi.sales.autoarima,alternative = "stationary")
# Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01
kpss.test(resi.sales.autoarima)
# KPSS Level = 0.042734, Truncation lag parameter = 1, p-value = 0.1
" both of the tests shows that the residual series is a stationary series "

#### Evaluate the Model using MAPE ####
fcast_autoarima.sales <- predict(autoarima.sales, n.ahead = 6)
MAPE.autoarima.sales <- accuracy(fcast_autoarima.sales$pred, outdata$Monthly.Sales)[5]
MAPE.autoarima.sales
" 27.68952 "

# plot of predicted value along with original values
total_autoarima_fcast.sales <- c(fitted(autoarima.sales), ts(fcast_autoarima.sales$pred))
plot(ts(APAC.consumer$Monthly.Sales), col = "red")
lines(total_autoarima_fcast.sales, col = "blue")

" based on the accuracy we are choosing classical decomposition method for forcasting future data "

################################ APAC.Consumer: Quantity Timeseries ########################################

quantity.timeser <- ts(indata$Monthly.Quantity)
plot(quantity.timeser, col = "red", lwd = 3)
## smoothing the ts
# weight with 2
w <- 2
smoothed.quantityts <- stats::filter(quantity.timeser, 
                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                     method='convolution', sides=2)
lines(smoothed.quantityts, col = "green", lwd = 2)
# weight with 1
w <- 1
smoothed.quantityts <- stats::filter(quantity.timeser, 
                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                     method='convolution', sides=2)
lines(smoothed.quantityts, col = "blue", lwd = 2)

# choosing w = 2 isn't capturing all the variations 

## Hence, smoothing with w = 1

# Smoothing left end of the time series

diff <- smoothed.quantityts[w+2] - smoothed.quantityts[w+1]
for (i in seq(w,1,-1)) {
  smoothed.quantityts[i] <- smoothed.quantityts[i+1] - diff
}

#Smoothing right end of the time series

n <- length(quantity.timeser)
diff <- smoothed.quantityts[n-w] - smoothed.quantityts[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed.quantityts[i] <- smoothed.quantityts[i-1] + diff
}

# Plot the smoothed time series
plot(quantity.timeser, col = "red", lwd = 3)
lines(smoothed.quantityts, col="blue", lwd=2)

# Create a smoothed df 
smoothed.quantitydf <- as.data.frame(cbind(indata$timestamp, as.vector(smoothed.quantityts)))
colnames(smoothed.quantitydf) <- c("month", "quantity")

#### Classical Decomposition Method ####
# fit using lm to get global trend line
lmfit.APAC.quantity <- lm(quantity ~ sin(.5*month) + cos(.5*month) 
            + month, data = smoothed.quantitydf)
global_pred.quantity <- predict(lmfit.APAC.quantity, month = smoothed.quantitydf$month)
summary(global_pred.quantity)
lines(smoothed.quantitydf$month, global_pred.quantity, col='green', lwd=2)

## local pred
local_pred.quantity <- quantity.timeser - global_pred.quantity
plot(local_pred.quantity, col = "coral", type = "l")
acf(local_pred.quantity)
acf(local_pred.quantity, type = "partial")
armafit.quantity <- auto.arima(local_pred.quantity)
tsdiag(armafit.quantity)
armafit.quantity
# ARIMA(0,0,0) with zero mean 
# sigma^2 estimated as 13492:  log likelihood=-259.3
# AIC=520.61   AICc=520.71   BIC=522.34

## check if the residual series is white noise
resi.quantity <- local_pred.quantity - fitted(armafit.quantity)

adf.test(resi.quantity,alternative = "stationary")
# Dickey-Fuller = -5.1137, Lag order = 3, p-value = 0.01
kpss.test(resi.quantity)
# KPSS Level = 0.026909, Truncation lag parameter = 1, p-value = 0.1
# both kpss and Dicky Fuller test suggests that the residual is a pure white noise 

#### Evaluate the Model using MAPE ####
global_fcast.quantity <- predict(lmfit.APAC.quantity, data.frame(month = outdata$timestamp))
MAPE.class_dec.quantity <- accuracy(global_fcast.quantity, outdata$Monthly.Quantity)[5]
MAPE.class_dec.quantity
" 22.94291 "

# Let's also plot the predictions along with original values, to
#get a visual feel of the fit

total_global_fcast.quantity <- c(ts(global_pred.quantity), ts(global_fcast.quantity))
plot(ts(APAC.consumer$Monthly.Quantity), col = "red")
lines(total_global_fcast.quantity, col = "blue")

#### Auto ARIMA Method ####
autoarima.quantity <- auto.arima(quantity.timeser)
autoarima.quantity
tsdiag(autoarima.quantity)
plot(autoarima.quantity$x, col = "red")
lines(fitted(autoarima.quantity), col = "blue")

## residual series
resi.quantity.autoarima <- quantity.timeser - fitted(autoarima.quantity)
plot(resi.quantity.autoarima)

adf.test(resi.quantity.autoarima,alternative = "stationary")
# Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
kpss.test(resi.quantity.autoarima)
# KPSS Level = 0.031535, Truncation lag parameter = 1, p-value = 0.1
" both of the tests shows that the residual series is a stationary series "

#### Evaluate the Model using MAPE ####
fcast_autoarima.quantity <- predict(autoarima.quantity, n.ahead = 6)
MAPE.autoarima.quantity <- accuracy(fcast_autoarima.quantity$pred, outdata$Monthly.Quantity)[5]
MAPE.autoarima.quantity
" 26.24458 "

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

total_autoarima_fcast.quantity <- c(fitted(autoarima.quantity), ts(fcast_autoarima.quantity$pred))
plot(ts(APAC.consumer$Monthly.Quantity), col = "red")
lines(total_autoarima_fcast.quantity, col = "blue")

" based on the accuracy we are choosing classical decomposition method for forcasting future data "


###########################################################################################
######################## Time-Series Modelling for EU Consumer ############################

EU.consumer.sales <- EU.consumer[, c(-3)]
EU.consumer.quantity <- EU.consumer[, c(-2)]

################################ EU.Consumer: Sales Timeseries ########################################

nrow(EU.consumer.sales)
#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

Sales_timeser <- ts(EU.consumer.sales$Monthly.Sales)
indata.sales <- EU.consumer.sales[1:42,]
outdata.sales <- EU.consumer.sales[43:48,]
timeser.sales <- ts(indata.sales$Monthly.Sales)
plot(timeser.sales, col = "red", lwd = 3)

##Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries.sales <- stats::filter(timeser.sales, 
                                      filter=rep(1/(2*w+1),(2*w+1)), 
                                      method='convolution', sides=2)


#Smoothing left end of the time series
diff <- smoothedseries.sales[w+2] - smoothedseries.sales[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries.sales[i] <- smoothedseries.sales[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeser.sales)
diff <- smoothedseries.sales[n-w] - smoothedseries.sales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries.sales[i] <- smoothedseries.sales[i-1] + diff
}

##Plot the smoothed time series
timevals_in.sales <- indata.sales$timestamp
lines(smoothedseries.sales, col="darkgreen", lwd=2)


## Building a model on the smoothed time series using Classical Decomposition
## First, let's convert the time series to a dataframe

smootheddf.sales <- as.data.frame(cbind(timevals_in.sales, as.vector(smoothedseries.sales)))
colnames(smootheddf.sales) <- c('Month', 'Sales')

## Now, let's fit using lm to get global trend line

lmfit.EU.sales <- lm(Sales ~ sin(0.5*Month) + cos(0.5*Month)
            + Month, data=smootheddf.sales)
global_pred.sales <- predict(lmfit.EU.sales, Month=timevals_in.sales)
summary(global_pred.sales)
lines(timevals_in.sales, global_pred.sales, col='blue4', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred.sales <- timeser.sales - global_pred.sales
plot(local_pred.sales, col='coral', type = "l")
acf(local_pred.sales)
acf(local_pred.sales, type="partial")
armafit.sales <- auto.arima(local_pred.sales)

tsdiag(armafit.sales)
armafit.sales
#ARIMA(0,0,0) with zero mean 
#sigma^2 estimated as 106202616:  log likelihood=-447.69
#AIC=897.39   AICc=897.49   BIC=899.12

##We'll check if the residual series is white noise
resi <- local_pred.sales - fitted(armafit.sales)

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -3.9154, Lag order = 3, p-value = 0.02257
kpss.test(resi)
#KPSS Level = 0.093143, Truncation lag parameter = 1, p-value = 0.1
" Both kpss and Dicky Fuller test suggests that the residual is a pure white noise "

##Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
global_fcast.sales<- predict(lmfit.EU.sales, data.frame(Month = outdata.sales$timestamp))
MAPE.class_dec.sales <- accuracy(global_fcast.sales, outdata.sales$Monthly.Sales)[5]
MAPE.class_dec.sales
" 25.85999 "

##Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred.sales <- c(ts(global_pred.sales),ts(global_fcast.sales))
plot(Sales_timeser, col = "black")
lines(class_dec_pred.sales, col = "red")


##So, that was classical decomposition, now let's do an ARIMA fit
autoarima.sales <- auto.arima(timeser.sales)
autoarima.sales
#ARIMA(2,1,0) 
#sigma^2 estimated as 168564623:  log likelihood=-445.84
#AIC=897.67   AICc=898.32   BIC=902.81

tsdiag(autoarima.sales)
plot(autoarima.sales$x, col="black")
lines(fitted(autoarima.sales), col="red")

##Again, let's check if the residual series is white noise
resi_auto_arima.sales <- timeser.sales - fitted(autoarima.sales)

adf.test(resi_auto_arima.sales,alternative = "stationary")
#Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01
kpss.test(resi_auto_arima.sales)
#KPSS Level = 0.05314, Truncation lag parameter = 1, p-value = 0.1
" Both kpss and Dicky Fuller test suggests that the residual is a pure white noise "

##Also, let's evaluate the model using MAPE
fcast_auto_arima.sales <- predict(autoarima.sales, n.ahead = 6)

MAPE_auto_arima.sales <- accuracy(fcast_auto_arima.sales$pred,outdata.sales$Monthly.Sales)[5]
MAPE_auto_arima.sales
" 28.9226 "

##Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred.sales <- c(fitted(autoarima.sales),ts(fcast_auto_arima.sales$pred))
plot(Sales_timeser, col = "black")
lines(auto_arima_pred.sales, col = "red")

" based on the accuracy we are choosing classical ddcomposition method for forcasting future data "


################################ EU.Consumer: Quantity Timeseries ########################################

nrow(EU.consumer.quantity)
#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

Quantity_timeser <- ts(EU.consumer.quantity$Monthly.Quantity)
indata.quantity <- EU.consumer.quantity[1:42,]
outdata.quantity <- EU.consumer.quantity[43:48,]
timeser.quantity <- ts(indata.quantity$Monthly.Quantity)
plot(timeser.quantity, col = "red", lwd = 3)

##Smoothening the series - Moving Average Smoothing
# tried assigning w=2 and w=1. Finally selecting w=1 for smoothening
w <- 1
smoothedseries.quan <- stats::filter(timeser.quantity, 
                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                     method='convolution', sides=2)


#Smoothing left end of the time series
diff <- smoothedseries.quan[w+2] - smoothedseries.quan[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries.quan[i] <- smoothedseries.quan[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeser.quantity)
diff <- smoothedseries.quan[n-w] - smoothedseries.quan[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries.quan[i] <- smoothedseries.quan[i-1] + diff
}

# Plot the smoothed time series
timevals_in.quantity <- indata.quantity$timestamp
lines(smoothedseries.quan, col="darkgreen", lwd=2)


# Building a model on the smoothed time series using Classical Decomposition
# First, let's convert the time series to a dataframe

smootheddf.quan <- as.data.frame(cbind(timevals_in.quantity, as.vector(smoothedseries.quan)))
colnames(smootheddf.quan) <- c('Month', 'Quantity')

# Now, let's fit using lm to get global trend line

lmfit.EU.quantity <- lm(Quantity ~ (sin(.5*Month) * poly(Month, 3)  + cos(.5*Month) * poly(Month, 3))
            + Month, data=smootheddf.quan)
global_pred.quantity <- predict(lmfit.EU.quantity, Month=timevals_in.quantity)
summary(global_pred.quantity)
lines(timevals_in.quantity, global_pred.quantity, col='yellowgreen', lwd=2)

##Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred.quantity <- timeser.quantity - global_pred.quantity
plot(local_pred.quantity, col='coral', type = "l")
acf(local_pred.quantity)
acf(local_pred.quantity, type="partial")
armafit.quantity <- auto.arima(local_pred.quantity)

tsdiag(armafit.quantity)
armafit.quantity
#ARIMA(2,0,0) with zero mean 
#sigma^2 estimated as 7284:  log likelihood=-245.89
#AIC=497.79   AICc=498.42   BIC=503

##We'll check if the residual series is white noise
resi <- local_pred.quantity-fitted(armafit.quantity)

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -6.6825, Lag order = 3, p-value = 0.01
kpss.test(resi)
#KPSS Level = 0.023531, Truncation lag parameter = 1, p-value = 0.1
" Both kpss and Dicky Fuller test suggests that the residual is a pure white noise "

##Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
global_fcast.quantity<- predict(lmfit.EU.quantity, data.frame(Month = outdata.quantity$timestamp))
MAPE.class_dec.quantity <- accuracy(global_fcast.quantity, outdata.quantity$Monthly.Quantity)[5]
MAPE.class_dec.quantity
" 30.39741 "

##Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred.quantity <- c(ts(global_pred.quantity),ts(global_fcast.quantity))
plot(Quantity_timeser, col = "black")
lines(class_dec_pred.quantity, col = "red")


## So, that was classical decomposition, now let's do an ARIMA fit

autoarima.quantity <- auto.arima(Quantity_timeser)
autoarima.quantity
#ARIMA(2,1,0) 
#sigma^2 estimated as 25099:  log likelihood=-304.31
#AIC=614.63   AICc=615.18   BIC=620.18

tsdiag(autoarima.quantity)
plot(autoarima.quantity$x, col="black")
lines(fitted(autoarima.quantity), col="red")

##Again, let's check if the residual series is white noise
resi_auto_arima.quantity <- Quantity_timeser - fitted(autoarima.quantity)

adf.test(resi_auto_arima.quantity,alternative = "stationary")
#Dickey-Fuller = -3.3355, Lag order = 3, p-value = 0.07718
kpss.test(resi_auto_arima.quantity)
#KPSS Level = 0.085039, Truncation lag parameter = 1, p-value = 0.1

# kpss is suggestiung residual is stationary whereas Dickey-Fuller is suggesting it
# is not stationary with slightly more p-value than .05; so, in case of conflict as we choose kpss 
#  we can saty that the residual series is stationary "


##Also, let's evaluate the model using MAPE
fcast_auto_arima.quantity <- predict(autoarima.quantity, n.ahead = 6)

MAPE_auto_arima.quantity <- accuracy(fcast_auto_arima.quantity$pred,outdata.quantity$Monthly.Quantity)[5]
MAPE_auto_arima.quantity
" 26.54859 "

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred.quantity <- c(fitted(autoarima.quantity),ts(fcast_auto_arima.quantity$pred))
plot(Quantity_timeser, col = "black")
lines(auto_arima_pred.quantity, col = "red")

" based on the accuracy we are choosing auto.arima method for forcasting future data "


###############################################################################################
#################### Forecasting: Prediction for future 6 months ######################

future_period<- c(49:54)
future_months <- c("Jan.2015", "Feb.2015", "Mar.2015", "Apr.2015", "May.2015", "June.2015")

                          ######## APAC Consumer Sales ########

#MAPE value for Classical Decomposition: 22.94291
#MAPE value for Auto-Arima: 27.68952 

## hence, Classical Decomposition method provides a 19.77% better MAPE value for the APAC
## consumer sales.

#Creating a Time Series on the whole data set
sales.timeser <- ts(APAC.consumer$Monthly.Sales)

## smoothing the whole series
smoothed.salests <- stats::filter(ts(APAC.consumer$Monthly.Sales), 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
## Smoothing left end of the time series
diff <- smoothed.salests[w+2] - smoothed.salests[w+1]
for (i in seq(w,1,-1)) {
  smoothed.salests[i] <- smoothed.salests[i+1] - diff
}
#Smoothing right end of the time series
n <- length(sales.timeser)
diff <- smoothed.salests[n-w] - smoothed.salests[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed.salests[i] <- smoothed.salests[i-1] + diff
}
# Create a smoothed df 
smoothed.salesdf <- as.data.frame(cbind(APAC.consumer$timestamp, as.vector(smoothed.salests)))
colnames(smoothed.salesdf) <- c("month", "sales")

## create model and predict
APAC.cons.sales.model <- lm(sales ~ sin(.5*month)  + cos(.5*month)
                            + month, data = smoothed.salesdf)
predict_APAC.cons.sales <- predict(APAC.cons.sales.model, data.frame(month = APAC.consumer$timestamp))
forecast_APAC.cons.sales <- predict(APAC.cons.sales.model, data.frame(month = future_period))

## Plotting the future 6 months forecast
fcast.APAC.cons.sales <-  c(ts(predict_APAC.cons.sales), ts(forecast_APAC.cons.sales))
plot(ts(APAC.consumer$Monthly.Sales), col = "red")
lines(fcast.APAC.cons.sales, col = "blue")
# Forecast (Blue Line) shows fall of sales of APAC.Consumer for upcoming months

########################## APAC Consumer Quantity #####################################

#MAPE value for Classical Decomposition: 22.21365 
#MAPE value for Auto-Arima: 26.24458

## hence, Classical Decomposition method provides a 4.03% better MAPE value for the APAC
## consumer quantity.

#Creating a Time Series on the whole data set
quantity.timeser <- ts(APAC.consumer$Monthly.Quantity)
## smoothing the whole series
smoothed.quantityts <- stats::filter(ts(APAC.consumer$Monthly.Quantity), 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
## Smoothing left end of the time series
diff <- smoothed.quantityts[w+2] - smoothed.quantityts[w+1]
for (i in seq(w,1,-1)) {
  smoothed.quantityts[i] <- smoothed.quantityts[i+1] - diff
}
#Smoothing right end of the time series
n <- length(quantity.timeser)
diff <- smoothed.quantityts[n-w] - smoothed.quantityts[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed.quantityts[i] <- smoothed.quantityts[i-1] + diff
}
# Create a smoothed df 
smoothed.quantitydf <- as.data.frame(cbind(APAC.consumer$timestamp, as.vector(smoothed.quantityts)))
colnames(smoothed.quantitydf) <- c("month", "quantity")

## create model and predict
APAC.cons.quantity.model <- lm(quantity ~ sin(.5*month)  + cos(.5*month)
                            + month, data = smoothed.quantitydf)
predict_APAC.cons.quantity <- predict(APAC.cons.quantity.model, data.frame(month = APAC.consumer$timestamp))
forecast_APAC.cons.quantity <- predict(APAC.cons.quantity.model, data.frame(month = future_period))

## Plotting the future 6 months forecast
fcast.APAC.cons.quantity <-  c(ts(predict_APAC.cons.quantity), ts(forecast_APAC.cons.quantity))
plot(ts(APAC.consumer$Monthly.Quantity), col = "red")
lines(fcast.APAC.cons.quantity, col = "blue")

# Forecast (Blue Line) shows fall of quantity of APAC.Consumer for upcoming months


######################## EU Consumer Sales ########################

#MAPE value for Classical Decomposition: 25.85999  
#MAPE value for Auto-Arima: 28.9226 

## hence, Classical Decomposition method provides a 3.06% better MAPE value for the EU
## consumer sales.

#Creating a Time Series on the whole data set
sales.timeser <- ts(EU.consumer$Monthly.Sales)
## smoothing the whole series
smoothed.salests <- stats::filter(ts(EU.consumer$Monthly.Sales), 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
## Smoothing left end of the time series
diff <- smoothed.salests[w+2] - smoothed.salests[w+1]
for (i in seq(w,1,-1)) {
  smoothed.salests[i] <- smoothed.salests[i+1] - diff
}
#Smoothing right end of the time series
n <- length(sales.timeser)
diff <- smoothed.salests[n-w] - smoothed.salests[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothed.salests[i] <- smoothed.salests[i-1] + diff
}
# Create a smoothed df 
smoothed.salesdf <- as.data.frame(cbind(EU.consumer$timestamp, as.vector(smoothed.salests)))
colnames(smoothed.salesdf) <- c("month", "sales")

## create model and predict
EU.cons.sales.model <- lm(sales ~ sin(.5*month)  + cos(.5*month)
                            + month, data = smoothed.salesdf)
predict_EU.cons.sales <- predict(EU.cons.sales.model, data.frame(month = EU.consumer$timestamp))
forecast_EU.cons.sales <- predict(EU.cons.sales.model, data.frame(month = future_period))

## Plotting the future 6 months forecast
fcast.EU.cons.sales <-  c(ts(predict_EU.cons.sales), ts(forecast_EU.cons.sales))
plot(ts(EU.cons$Monthly.Sales), col = "red")
lines(fcast.EU.cons.sales, col = "blue")

# Forecast (Blue Line) shows fall of sales of EU.Consumer for upcoming months

                        ######## EU Consumer Quantity ########

#MAPE value for Classical Decomposition: 30.39741
#MAPE value for Auto-Arima: 26.54859

## hence, AUTO-ARIMA method provides a 3.84% better MAPE value for the EU consumer quantity.

#Creating a Time Series on the whole data set
quantity.timeser <- ts(APAC.consumer$Monthly.Quantity)
## create model using auto.arima
EU.cons.autoarima.quantity <- auto.arima(quantity.timeser)
EU.cons.autoarima.quantity
## predict
fcast.EU.cons.quantity <- predict(EU.cons.autoarima.quantity, n.ahead = 6)

## Plotting the future 6 months forecast
forecast.EU.cons.quantity <-  c(fitted(EU.cons.autoarima.quantity), ts(fcast.EU.cons.quantity$pred))
plot(ts(EU.cons$Monthly.Quantity), col = "red")
lines(forecast.EU.cons.quantity, col = "blue")

# Forecast (Blue Line) shows rise of Quantity of EU.Consumer for upcoming months
