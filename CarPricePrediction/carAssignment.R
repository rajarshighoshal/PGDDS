################################################################################################
######################### Loading required libraries ########################################
################################################################################################
library(tidyverse)
library(gridExtra)
library(corrplot)
library(MASS)
library(car)

################################################################################################
######################### Loading data into environment ########################################
################################################################################################
carData <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)
str(carData)
" 26 variables: 10 charater based, 16 numerical based
                so, there are lot of categorical variables
                car_ID seems like row numbers so can remove it"


################################################################################################
####################### Cleaning data and preparing for analysis ###############################
################################################################################################
## removing car_ID
carData <- carData[-1]
## check for NA
sum(is.na(carData))
" no NA values "
## check for duplicates
sum(duplicated(carData))
" no duplicate values "
##################### making plots for better understanding ##################################
##### categorical variables #####
## symboling 
ggplot(carData) + geom_bar(aes(x = symboling)) +
  scale_x_continuous(breaks = seq(-3, 3, 1)) + labs(x = "Symboling", y = "Count")
## carName
# split the carName in 2 parts: manufacturer and model name
# model name can be ignored but we shouldn't ignore manufacturer name to understand brand impact
carData <- carData %>% tidyr::separate(CarName, c("carManufacturer", "carModel"), sep = " ") %>%
  dplyr::select(-carModel)
## check discripencies of carManufacturer
levels(as.factor(carData$carManufacturer))
" 1) maxda should be mazda
  2) Nissan and nissan are same, sticking with nissan
  3) porcshce should be porsche 
  4) toyouta should be toyota
  5) vokswagen and vw should be volkswagen "
carData[which(carData$carManufacturer == "maxda"), 'carManufacturer'] <- "mazda"
carData[which(carData$carManufacturer == "Nissan"), 'carManufacturer'] <- "nissan"
carData[which(carData$carManufacturer == "porcshce"), 'carManufacturer'] <- "porsche"
carData[which(carData$carManufacturer == "toyouta"), 'carManufacturer'] <- "toyota"
carData[which(carData$carManufacturer == "vokswagen"), 'carManufacturer'] <- "volkswagen"
carData[which(carData$carManufacturer == "vw"), 'carManufacturer'] <- "volkswagen"
## recheck after cleaning
levels(as.factor(carData$carManufacturer))
# plot
ggplot(carData) + geom_bar(aes(x = carManufacturer), fill = "orange") +
  labs(x = "Car Manufacturer", y = "Count")
## fuel type
ggplot(carData) + geom_bar(aes(x = fueltype), fill = "orange") +
  labs(x = "Fuel Type", y = "Count")
" most cars are of gas type "
## aspiration
ggplot(carData) + geom_bar(aes(x = aspiration), fill = "orange") +
  labs(x = "Aspiuration", y = "Count")
" std is more common "
## doornumber
ggplot(carData) + geom_bar(aes(x = doornumber), fill = "orange") +
  labs(x = "No. of doors", y = "Count")
" four doors are more common though difference is very less "
## carbody
ggplot(carData) + geom_bar(aes(x = carbody), fill = "orange") +
  labs(x = "Car body type", y = "Count")
" sedan is most common "
## drivewheel
ggplot(carData) + geom_bar(aes(x = drivewheel), fill = "orange") +
  labs(x = "Drive Wheel", y = "Count")
" fwd is most common; 4wd is rare "
## enginelocation
ggplot(carData) + geom_bar(aes(x = enginelocation), fill = "orange") +
  labs(x = "Location of Engine", y = "Count")
" rear located engine is very rare "
## enginetype
ggplot(carData) + geom_bar(aes(x = enginetype), fill = "orange") +
  labs(x = "Engine Type", y = "Count")
" car engines are of these types:
      a) SOHC, b) DOHC, c) l type engine, d) rotary engine
  So, we need to replace some values "
carData$enginetype[(which(carData$enginetype  %in% c("ohcf","ohcv")))] <- "ohc"
carData$enginetype[(which(carData$enginetype  %in% c("dohcv")))] <- "dohc"
##
ggplot(carData) + geom_bar(aes(x = enginetype), fill = "orange") +
  labs(x = "Engine Type", y = "Count")
" ohc is most common engine "
## cylindernumber
ggplot(carData) + geom_bar(aes(x = cylindernumber), fill = "orange") +
  labs(x = "No. of cylinders", y = "Count")
" three and twelve numbered cylinders seems very rare; four is extremely common "
## fuelsystem
ggplot(carData) + geom_bar(aes(x = fuelsystem), fill = "orange") +
  labs(x = "Fuel System", y = "Count")
" mfi, spfi rare "
##### numerical variables #####
## wheelbase
g1 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = wheelbase), fill = "light yellow") +
  labs(x = "", y = "wheelbase")
## carlength
g2 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = carlength), fill = "light yellow") +
  labs(x = "", y = "car length")
## carwidth
g3 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = carwidth), fill = "light yellow") +
  labs(x = "", y = "car width")
## carheight
g4 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = carheight), fill = "light yellow") +
  labs(x = "", y = "car height")
## curbweight
g5 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = curbweight), fill = "light yellow") +
  labs(x = "", y = "curb weight")
## enginesize
g6 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = enginesize), fill = "light yellow") +
  labs(x = "", y = "engine size")
## boreratio
g7 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = boreratio), fill = "light yellow") +
  labs(x = "", y = "boreratio")
## stroke
g8 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = stroke), fill = "light yellow") +
  labs(x = "", y = "stroke")
## compressionratio
g9 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = compressionratio), fill = "light yellow") +
  labs(x = "", y = "compression ratio")
## horsepower
g10 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = horsepower), fill = "light yellow") +
  labs(x = "", y = "horse power")
## citympg
g11 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = citympg), fill = "light yellow") +
  labs(x = "", y = "city mpg")
## highwaympg
g12 <- ggplot(carData) + geom_boxplot(aes(x = 1, y = highwaympg), fill = "light yellow") +
  labs(x = "", y = "highway mpg")
##
grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12)

##### outliers #####
## engine size
quantile(carData$enginesize, seq(0, 1, 0.01))
" jump at 97%ile, replaceing everything above 209 with 209 "
carData$enginesize[which(carData$enginesize > 209)] <- 209
## stroke
quantile(carData$stroke, seq(0, 1, 0.01))
" jump at 2%ile and 95%ile, replaceing everything below 2.64 with 2.64 AND above 3.82 with 3.82 "
carData$stroke[which(carData$stroke < 2.64)] <- 2.64
carData$stroke[which(carData$stroke > 3.82)] <- 3.82
## compression ratio
quantile(carData$compressionratio, seq(0, 1, 0.01))
" jump at 3%ile and 91%ile, replaceing everything below 7.5 with 7.5 AND above 10.94 with 10.94 "
carData$compressionratio[which(carData$compressionratio < 7.5)] <- 7.5
carData$compressionratio[which(carData$compressionratio > 10.94)] <- 10.94
## horse power
quantile(carData$horsepower, seq(0, 1, 0.01))
" jump at 97%ile, replaceing everything above 184 with 184 "
carData$horsepower[which(carData$horsepower > 184)] <- 184
##### some bivariate analysis #####
## fuel type vs curbweight
ggplot(carData) + geom_boxplot(aes(x = fueltype, y = curbweight, fill = "light yellow")) +
  labs(x = "Fuel Type", y = "Curb Weight")
## fuel type vs compression ratio
ggplot(carData) + geom_boxplot(aes(x = fueltype, y = compressionratio, fill = "light yellow")) +
  labs(x = "Fuel Type", y = "Compression Ratio")
" correlation "
## enginesize vs horsepower
ggplot(carData, aes(x = enginesize, y = horsepower)) + geom_point(col = "blue") +
  labs(x = "Engine Size", y = "Horse Power") + geom_smooth(method = "lm")
" have some correlation "
### make a corplot ###
carCorMat <- 
  cor(carData %>% 
        dplyr::select(peakrpm,wheelbase,carlength,carwidth,carheight,curbweight,enginesize,boreratio,stroke,compressionratio,horsepower,citympg,highwaympg,symboling))
corrplot(carCorMat, method = "number")
" citympg and highwaympg have correlation of 0.97; so we could safely drop highwaympg;
  as citympg will reflect it appropriately "
carData <- carData %>% dplyr::select(-highwaympg)
"curbweight has strong correlation with many other variables, so we can safely drop it "
carData <- carData %>% dplyr::select(-curbweight)
##
carCorMat <- 
  cor(carData %>% 
        dplyr::select(peakrpm,wheelbase,carlength,carwidth,carheight,enginesize,boreratio,stroke,compressionratio,horsepower,citympg,symboling))
corrplot(carCorMat, method = "number")
"carlength and wheelbase have strong correlation, 
 so we can only use wheelbase as a replacement of carlength "
carData <- carData %>% dplyr::select(-carlength)
###################### Dealing with categorical variables #####################################
### making all categorical variables as factor
str(carData)
cols <- c("carManufacturer", "fueltype", "aspiration", "doornumber", "carbody", "drivewheel", "enginelocation",
          "enginetype", "cylindernumber", "fuelsystem")
carData[cols] <- lapply(carData[cols], factor)
### making dummy variables and adding them to create a new dataframe
carDataNumeric <- carData
## carManufacturer
dummy1 <- model.matrix(~carManufacturer, data = carDataNumeric)
carDataNumeric <- cbind(carDataNumeric %>% dplyr::select(-carManufacturer), dummy1[,-1])
## fueltype
# gas = 1, disel = 0
carDataNumeric$fueltype <- ifelse(carDataNumeric$fueltype == "disel", 0, 1)
## aspiration
# std = 1, turbo = 0
carDataNumeric$aspiration <- ifelse(carDataNumeric$aspiration == "turbo", 0, 1)
## doornumber
# twodoor = 1, fourdoor = 0
carDataNumeric$doornumber <- ifelse(carDataNumeric$doornumber == "four", 0, 1)
## carbody
dummy2 <- model.matrix(~carbody, data = carDataNumeric)
carDataNumeric <- cbind(carDataNumeric %>% dplyr::select(-carbody), dummy2[,-1])
## drivewheel
dummy3 <- model.matrix(~drivewheel, data = carDataNumeric)
carDataNumeric <- cbind(carDataNumeric %>% dplyr::select(-drivewheel), dummy3[,-1])
## enginelocation
# front = 1, rear = 0
carDataNumeric$enginelocation <- ifelse(carDataNumeric$enginelocation == "rear", 0, 1)
## enginetype
dummy4 <- model.matrix(~enginetype, data = carDataNumeric)
carDataNumeric <- cbind(carDataNumeric %>% dplyr::select(-enginetype), dummy4[,-1])
## cylindernumber
dummy5 <- model.matrix(~cylindernumber, data = carDataNumeric)
carDataNumeric <- cbind(carDataNumeric %>% dplyr::select(-cylindernumber), dummy5[,-1])
## fuelsystem
dummy6 <- model.matrix(~fuelsystem, data = carDataNumeric)
carDataNumeric <- cbind(carDataNumeric %>% dplyr::select(-fuelsystem), dummy6[,-1])

################################################################################################
################################### Model creation #############################################
################################################################################################

##### setting seed to achieve reproducibility 
set.seed(1)
##### creating training and testing dataset
trainindices <- sample(1:nrow(carDataNumeric), 0.7 * nrow(carDataNumeric))
train <- carDataNumeric[trainindices,]
test <- carDataNumeric[-trainindices,]

##### model 
model1 <-  lm(price~., data = train)
summary(model1)
## stepAIC
stepAIC(model1, direction = "both")
model2 <- lm(formula = price ~ enginelocation + wheelbase + carwidth + 
               carheight + enginesize + compressionratio + horsepower + 
               peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
               carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
               carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
               carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
               carManufacturerporsche + carManufacturerrenault + carManufacturersubaru + 
               carManufacturertoyota + carManufacturervolkswagen + carManufacturervolvo + 
               carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemidi, data = train)
summary(model2)
## VIF checking
vif(model2)
" enginesize has very high vif; also its p-value is high too "
model3 <- lm(formula = price ~ enginelocation + wheelbase + carwidth + 
               carheight + compressionratio + horsepower + 
               peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
               carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
               carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
               carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
               carManufacturerporsche + carManufacturerrenault + carManufacturersubaru + 
               carManufacturertoyota + carManufacturervolkswagen + carManufacturervolvo + 
               carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemidi, data = train)
summary(model3)
vif(model3)
" carheight has very high p-value and relatively higher vif, lets drop it "
model4 <- lm(formula = price ~ enginelocation + wheelbase + carwidth + 
               compressionratio + horsepower + 
               peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
               carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
               carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
               carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
               carManufacturerporsche + carManufacturerrenault + carManufacturersubaru + 
               carManufacturertoyota + carManufacturervolkswagen + carManufacturervolvo + 
               carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemidi, data = train)
summary(model4)
vif(model4)
" wheelbase has very high p-value, remove it "
model5 <- lm(formula = price ~ enginelocation + carwidth + 
               compressionratio + horsepower + 
               peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
               carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
               carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
               carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
               carManufacturerporsche + carManufacturerrenault + carManufacturersubaru + 
               carManufacturertoyota + carManufacturervolkswagen + carManufacturervolvo + 
               carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemidi, data = train)
summary(model5)
vif(model5)
" removing enginetypeohc "
model6 <- lm(formula = price ~ enginelocation + carwidth + 
               compressionratio + horsepower + 
               peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
               carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
               carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
               carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
               carManufacturerporsche + carManufacturerrenault + carManufacturersubaru + 
               carManufacturertoyota + carManufacturervolkswagen + carManufacturervolvo + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemidi, data = train)
summary(model6)
vif(model6)
" cylindernumbersix "
model7 <- lm(formula = price ~ enginelocation + carwidth + 
               compressionratio + horsepower + 
               peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
               carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
               carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
               carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
               carManufacturerporsche + carManufacturerrenault + carManufacturersubaru + 
               carManufacturertoyota + carManufacturervolkswagen + carManufacturervolvo + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               cylindernumberfive + cylindernumberfour + 
               fuelsystem2bbl + fuelsystemidi, data = train)
summary(model7)
vif(model7)
" remove fuelsystem2bbl "
model8 <- lm(formula = price ~ enginelocation + carwidth + 
               compressionratio + horsepower + 
               peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
               carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
               carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
               carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
               carManufacturerporsche + carManufacturerrenault + carManufacturersubaru + 
               carManufacturertoyota + carManufacturervolkswagen + carManufacturervolvo + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               cylindernumberfive + cylindernumberfour + fuelsystemidi, data = train)
summary(model8)
vif(model8)
" removing carManufacturerporsche "
model9 <- lm(formula = price ~ enginelocation + carwidth + 
               compressionratio + horsepower + 
               peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
               carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
               carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
               carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
               carManufacturerrenault + carManufacturersubaru + 
               carManufacturertoyota + carManufacturervolkswagen + carManufacturervolvo + 
               carbodyhatchback + carbodysedan + carbodywagon + 
               cylindernumberfive + cylindernumberfour + fuelsystemidi, data = train)
summary(model9)
vif(model9)
" cylindernumberfour "
model10 <- lm(formula = price ~ enginelocation + carwidth + 
                compressionratio + horsepower + 
                peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
                carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
                carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
                carManufacturerrenault + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + carManufacturervolvo + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model10)
vif(model10)
" carManufacturervolvo  "
model11 <- lm(formula = price ~ enginelocation + carwidth + 
                compressionratio + horsepower + 
                peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
                carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
                carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
                carManufacturerrenault + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model11)
vif(model11)
" compressionratio "
model12 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + 
                peakrpm + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
                carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
                carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
                carManufacturerrenault + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model12)
vif(model12)
" peakrpm "
model13 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
                carManufacturerhonda + carManufacturerisuzu + carManufacturerjaguar + 
                carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
                carManufacturerrenault + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model13)
vif(model13)
" carManufacturerisuzu  "
model14 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
                carManufacturerhonda + carManufacturerjaguar + 
                carManufacturermazda + carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
                carManufacturerrenault + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model14)
vif(model14)
" carManufacturermazda "
model15 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
                carManufacturerhonda + carManufacturerjaguar + 
                carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + carManufacturerpeugeot + carManufacturerplymouth + 
                carManufacturerrenault + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model15)
vif(model15)
" carManufacturerpeugeot "
model16 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
                carManufacturerhonda + carManufacturerjaguar + 
                carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + carManufacturerplymouth + 
                carManufacturerrenault + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model16)
vif(model16)
" carManufacturerplymouth "
model17 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + carManufacturerdodge + 
                carManufacturerhonda + carManufacturerjaguar + 
                carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + 
                carManufacturerrenault + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model17)
vif(model17)
" carManufacturerdodge "
model18 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerhonda + carManufacturerjaguar + 
                carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + 
                carManufacturerrenault + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model18)
vif(model18)
" carManufacturerhonda "
model19 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + 
                carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + 
                carManufacturerrenault + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model19)
vif(model19)
" carManufacturerrenault "
model20 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + 
                carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + carManufacturersubaru + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model20)
vif(model20)
" carManufacturersubaru "
model21 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + 
                carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + 
                carManufacturertoyota + carManufacturervolkswagen + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model21)
vif(model21)
" carManufacturervolkswagen "
model22 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + 
                carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan + 
                carManufacturertoyota +  
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model22)
vif(model22)
" carManufacturertoyota "
model23 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + 
                carManufacturermercury + carManufacturermitsubishi + 
                carManufacturernissan +  
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model23)
vif(model23)
" carManufacturernissan  "
model24 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + 
                carManufacturermercury + carManufacturermitsubishi +  
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model24)
vif(model24)
" carManufacturermercury "
model25 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + carManufacturermitsubishi +  
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model25)
vif(model25)
" carbodysedan "
model26 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + carManufacturermitsubishi +  
                carbodyhatchback + carbodywagon + 
                cylindernumberfive + fuelsystemidi, data = train)
summary(model26)
vif(model26)
" carbodywagon "
model27 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + carManufacturermitsubishi +  
                carbodyhatchback + cylindernumberfive + fuelsystemidi, data = train)
summary(model27)
vif(model27)
" carbodyhatchback "
model28 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + carManufacturermitsubishi +  
                cylindernumberfive + fuelsystemidi, data = train)
summary(model28)
vif(model28)
" try removing carwidth "
model29 <- lm(formula = price ~ enginelocation + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + carManufacturermitsubishi +  
                cylindernumberfive + fuelsystemidi, data = train)
summary(model29)
" huge drop in r-squared; not acceptable "
" fuelsystemidi "
model29 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + carManufacturermitsubishi +  
                cylindernumberfive, data = train)
summary(model29)
vif(model29)
" cylindernumberfive "
model30 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar + carManufacturermitsubishi, data = train)
summary(model30)
vif(model30)
" carManufacturermitsubishi "
model31 <- lm(formula = price ~ enginelocation + carwidth + 
                horsepower + carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar, data = train)
summary(model31)
vif(model31)
" horsepower "
model32 <- lm(formula = price ~ enginelocation + carwidth + 
                carManufacturerbmw + carManufacturerbuick + 
                carManufacturerjaguar, data = train)
summary(model32)
" huge drop in adjusted r-squared; not acceptable "

" accepting model31 for now "

## prediction 
prediction1 <- predict(model31, test %>% dplyr::select(-price))
test$testPrice1 <- prediction1
rsquared <- cor(test$price,test$testPrice1)^2
rsquared
" 0.9203 "

### try the model31 with replacing horsepower with enginesize as they have high correlation 
model31_2 <- lm(formula = price ~ enginelocation + carwidth + 
                  enginesize + carManufacturerbmw + carManufacturerbuick + 
                  carManufacturerjaguar, data = train)
summary(model31_2)
vif(model31_2)
## prediction
prediction2 <- predict(model31_2, test %>% dplyr::select(-price))
test$testPrice2 <- prediction2
rsquared <- cor(test$price,test$testPrice2)^2
rsquared
" 0.8403 "
" model with horsepower is better "


#################################################################################################
" we don't have any control over manufacturer as a new compaqny; so in the next models
 removing manufacturers and building new models "
model32 <- lm(formula = price ~ enginelocation + wheelbase + carwidth + 
               carheight + enginesize + compressionratio + horsepower + 
               peakrpm + carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemidi, data = train)
summary(model32)
vif(model32)
stepAIC(model32, direction = "both")
model33 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                horsepower + peakrpm + carbodyhatchback + carbodysedan + 
                carbodywagon + enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemidi, data = train)
summary(model33)
vif(model33)
cor(train$enginesize, train$horsepower)
" 0.874 : high correlation, should choose one among them " 
" removing enginesize "
model34 <- lm(formula = price ~ enginelocation + wheelbase + 
                horsepower + peakrpm + carbodyhatchback + carbodysedan + 
                carbodywagon + enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemidi, data = train)
summary(model34)
vif(model34)
" very high drop, not acceptable "
" removing horsepower "
model34 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                peakrpm + carbodyhatchback + carbodysedan + 
                carbodywagon + enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemidi, data = train)
summary(model34)
vif(model34)
" accepting "
" removing carbodysedan "
model35 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                peakrpm + carbodyhatchback + 
                carbodywagon + enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemidi, data = train)
summary(model35)
vif(model35)
" removing fuelsystemidi "
model36 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                peakrpm + carbodyhatchback + 
                carbodywagon + enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train)
summary(model36)
vif(model36)
" enginetypeohc "
model37 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                peakrpm + carbodyhatchback + 
                carbodywagon + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train)
summary(model37)
vif(model37)
" try removing wheelbase "
model38 <- lm(formula = price ~ enginelocation + enginesize + 
                peakrpm + carbodyhatchback + 
                carbodywagon + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train)
summary(model38)
vif(model38)
" huge drop, not acceptable "
" carbodywagon "
model38 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                peakrpm + carbodyhatchback + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train)
summary(model38)
vif(model38)
" carbodyhatchback "
model39 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                peakrpm +
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train)
summary(model39)
vif(model39)
" try removing cylindernumberfour "
model40 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                peakrpm +
                cylindernumberfive + 
                cylindernumbersix, data = train)
summary(model40)
vif(model40)
" not acceptable "
" try removing cylindernumbersix"
model40 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                peakrpm +
                cylindernumberfive + cylindernumberfour, data = train)
summary(model40)
vif(model40)
" accepting "
" cylindernumberfive "
model41 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                peakrpm + cylindernumberfour, data = train)
summary(model41)
vif(model41)
" peakrpm "
model42 <- lm(formula = price ~ enginelocation + wheelbase + enginesize + 
                cylindernumberfour, data = train)
summary(model42)
vif(model42)
" stopping and avaluating model42 "

## prediction 
prediction3 <- predict(model42, test %>% dplyr::select(-price))
test$testPrice3 <- prediction3
rsquared <- cor(test$price,test$testPrice3)^2
rsquared
" 0.7382 "
" 10% drop "

#################################################################################################
########################### Predicting and Model selection ######################################
#################################################################################################
test$errorModel31 <- test$price - test$testPrice1
test$errorModel31_2 <- test$price - test$testPrice2
test$errorModel42 <- test$price - test$testPrice3
## noise plot
g1 <- ggplot(test) + geom_line(aes(x = c(1:nrow(test)),y = 0)) +
  geom_point(aes(x = c(1:nrow(test)),y = errorModel31)) +
  labs(title = "Noise in prediction1",x = "Number of observations",y = "Error")
g2 <- ggplot(test) + geom_line(aes(x = c(1:nrow(test)),y = 0)) +
  geom_point(aes(x = c(1:nrow(test)),y = errorModel31_2)) +
  labs(title = "Noise in prediction2",x = "Number of observations",y = "Error")
g3 <- ggplot(test) + geom_line(aes(x = c(1:nrow(test)),y = 0)) +
  geom_point(aes(x = c(1:nrow(test)),y = errorModel42)) +
  labs(title = "Noise in prediction3",x = "Number of observations",y = "Error")
grid.arrange(g1,g2,g3)
" in prediction 2 and 3 there seems to be some patter present "
## prediction plot
g4 <- ggplot(test) + geom_line(aes(x = c(1:nrow(test)),y = price,col = "blue")) +
  geom_line(aes(x = c(1:nrow(test)),y = testPrice1,col = "red")) +
  scale_color_discrete(name = "Price", labels = c("Actual Price","Predicted Price")) +
  labs(title="Actual Vs Predicted Price model31",x="Number of observations",y="Price")
g5 <- ggplot(test) + geom_line(aes(x = c(1:nrow(test)),y = price,col = "blue")) +
  geom_line(aes(x = c(1:nrow(test)),y = testPrice2,col = "red")) +
  scale_color_discrete(name = "Price", labels = c("Actual Price","Predicted Price")) +
  labs(title="Actual Vs Predicted Price model31_2",x="Number of observations",y="Price")
g6 <- ggplot(test) + geom_line(aes(x = c(1:nrow(test)),y = price,col = "blue")) +
  geom_line(aes(x = c(1:nrow(test)),y = testPrice3,col = "red")) +
  scale_color_discrete(name = "Price", labels = c("Actual Price","Predicted Price")) +
  labs(title="Actual Vs Predicted Price model42",x="Number of observations",y="Price")
grid.arrange(g4,g5,g6)
" model 31 is predicting properly, others are missing some trend. So, we will choose model 31 "





finalmodel <- model31
summary(finalmodel)
" adjuste r-squaqred : .9106 "
## prediction using final model
prediction <- predict(model31, carDataNumeric %>% dplyr::select(-price))
carData$predictedPrice <- prediction
rsquared <- cor(carData$price,carData$predictedPrice)^2
rsquared
" 0.917  "

" So, moast influetial factors that we can focus on for predicting car price in 
  the given market are :- 
          1) engine location 
          2) car width
          3) horse power
      Also, these three brands have influence on price :- 
          1) BMW
          2) Buick
          3) Jaguar 



  The model based on above 6 variables is predicting price quite accurately with a 
  r-squared value of 91.7%. "


