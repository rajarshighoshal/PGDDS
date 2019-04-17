
#-----------------------Multiple Linear regression-----------------------------------------

carprice <- read.csv("Carprice_Assignment.csv",stringsAsFactors = F)
str(carprice)

# Identify factor variables
carprice$symboling <-as.factor(carprice$symboling)
carprice$cylindernumber<-as.factor(carprice$cylindernumber)
carprice$enginetype <- as.factor(carprice$enginetype)
carprice$fuelsystem<-as.factor(carprice$fuelsystem)
carprice$fueltype<-as.factor(carprice$fueltype)
carprice$aspiration<-as.factor(carprice$aspiration)
carprice$doornumber<-as.factor(carprice$doornumber)
carprice$carbody <-as.factor(carprice$carbody)
carprice$drivewheel<-as.factor(carprice$drivewheel)
carprice$enginelocation <- as.factor(carprice$enginelocation)

# CarName is a categorical variable
# Lets examine factors
summary(as.factor(carprice$CarName))
# There are multiple levels in CarName. Let us try to reduce the variables by taking only the carCompany

carprice$carCompany <-gsub("\\ .*", "", carprice$CarName)
str(carprice$carCompany)
carprice$carCompany <- as.factor(carprice$carCompany)
summary(carprice$carCompany)
levels(carprice$carCompany)

levels(carprice$carCompany)[10] <- "mazda"
levels(carprice$carCompany)

levels(carprice$carCompany)[14] <- "nissan"
levels(carprice$carCompany)

levels(carprice$carCompany)[16] <- "porsche"
levels(carprice$carCompany)

levels(carprice$carCompany)[21] <- "toyota"
levels(carprice$carCompany)

levels(carprice$carCompany)[21] <- "volkswagen"
levels(carprice$carCompany)

levels(carprice$carCompany)[23] <- "volkswagen"
levels(carprice$carCompany)

# check for missing values
sum(is.na(carprice))
# no missing values

# check duplicated
which(duplicated(carprice))
# no duplicated

# Create the dummy variables

# For carCompany
dummy_1 <- data.frame(model.matrix( ~carCompany, data = carprice))
dummy_1<-dummy_1[,-1]


# For carbody
dummy_2 <- data.frame(model.matrix( ~carbody, data = carprice))
dummy_2<-dummy_2[,-1]


# Drivewheel 
dummy_3 <- data.frame(model.matrix( ~drivewheel, data = carprice))
dummy_3<-dummy_3[,-1]


#Engine type
dummy_4 <- data.frame(model.matrix( ~enginetype, data = carprice))
dummy_4<-dummy_4[,-1]


#cylindernumber
dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = carprice))
dummy_5<-dummy_5[,-1]


# Fuelsystem
dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = carprice))
dummy_6<-dummy_6[,-1]

# Symboling
dummy_7 <- data.frame(model.matrix( ~symboling, data = carprice))
dummy_7<-dummy_7[,-1]
#-------------------------------------------------------------------------------------------

# Variable having 2 levels.

# for fueltype
levels(carprice$fueltype)<-c(1,0)
# assigning 1 to diesel and 0 to gas
carprice$fueltype<- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

#-------------------------------------------------------------------------------------------

# for aspiration
levels(carprice$aspiration)<-c(1,0)
# Assigning 1 to "std" and 0 to "turbo"
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]

#-------------------------------------------------------------------------------------------

# For doornumber
levels(carprice$doornumber)<-c(1,0)
# Assigning 1 if the number of doors is 4, and 0 if the number of doors is 2.
carprice$doornumber<- as.numeric(levels(carprice$doornumber))[carprice$doornumber]

#-------------------------------------------------------------------------------------------

# Enginelocation
levels(carprice$enginelocation)<-c(1,0)
# Assigning 1 if the engine is front and 0 if in rear
carprice$enginelocation<- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

#-------------------------------------------------------------------------------------------

# Combine the dummy variables and the numeric columns of carprice dataset.

carprice_1 <- cbind(carprice[ , c(1,4:6,9:14,17,19:26)], dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6, dummy_7)

#-------------------------------------------------------------------------------------------

# View the new dataset carprice_1

View(carprice_1)

#-------------------------------------------------------------------------------------------

# Divide you data in 70:30 

set.seed(100)
indices= sample(1:nrow(carprice_1), 0.7*nrow(carprice_1))

train=carprice_1[indices,]
test = carprice_1[-indices,]

#-------------------------------------------------------------------------------------------


#------------------------------Multiple Linear regression-----------------------------------
#-------------------------------------------------------------------------------------------

# Develop the first model 

model_1 <-lm(price~.,data=train[,-1])
summary(model_1)


#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")

#-------------------------------------------------------------------------------------------


# Run the step object

step

#-------------------------------------------------------------------------------------------

model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_2)
vif(model_2)

#####
#carlength has a high VIF and is insignificant. Thus, removing

model_3 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_3)
vif(model_3)

# remove citympg
model_4 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_4)
vif(model_4)

# remove fuelsystemmpfi 
model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 fuelsystem2bbl + symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_5)
vif(model_5)


# remove carcompanyporsche
model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 fuelsystem2bbl + symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_6)
vif(model_6)

# remove fuelsystem2bbl
model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_7)
vif(model_7)

# remove symboling0
model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 symboling.1 + symboling3, data = train[, -1])
summary(model_8)
vif(model_8)

# remove symboling.1 
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 symboling3, data = train[, -1])
summary(model_9)
vif(model_9)


# remove carcompanymercury

model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                  symboling3, data = train[, -1])
summary(model_10)
vif(model_10)

# remove symboling3

model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_11)
vif(model_11)


# remove carbodyhardtop

model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                  carbodyhatchback + carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_12)
vif(model_12)


# remove carbodyhatchback

model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                  carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_13)
vif(model_13)

# remove carbodysedan

model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                  carbodywagon + 
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_14)
vif(model_14)

# remove carbodywagon


model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_15)
vif(model_15)

# All variables are significant now
# Variable curbweight and enginesize have high VIFs
# curbweight is very less significant as compared to enginesize
# remove curbweight

model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_16)
vif(model_16)

# remove cylindernumberfive

model_17 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_17)
vif(model_17)


# remove peakrpm (higher VIF, higher p-value as compared to othe variables in model)
model_18 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_18)
vif(model_18)


# remove carCompanysaab
model_19 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_19)
vif(model_19)

# remove carcompanyhonda
model_20 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_20)
vif(model_20)


# remove carCompany renault
model_21 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + 
                  carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_21)
vif(model_21)


# remove drivewheelrwd
model_22 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + 
                  carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_22)
vif(model_22)

# remove carCompanyvolkswagen
model_23 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_23)
vif(model_23)


# remove carCompanydodge
model_24 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_24)
vif(model_24)



# remove carCompanyplymouth
model_25 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_25)
vif(model_25)

# remove carCompanynissan
model_26 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanypeugeot + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_26)
vif(model_26)

# remove enginetypeohc
model_27 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanypeugeot + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetyperotor, data = train[, -1])
summary(model_27)
vif(model_27)

# remove carCompanymitsubishi
model_28 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanypeugeot + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetyperotor, data = train[, -1])
summary(model_28)
vif(model_28)

# remove carCompanytoyota
model_29 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanypeugeot + 
                  carCompanysubaru +  
                  enginetyperotor, data = train[, -1])
summary(model_29)
vif(model_29)

# remove carcompanyPeugeot
model_30 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + 
                  carCompanysubaru +  
                  enginetyperotor, data = train[, -1])
summary(model_30)
vif(model_30)
# adjusted R-squared = 0.9437

# carwidth, enginesize have high VIF. Lets see what happens when we remove these
# removing carwidth
model_31a <- lm(formula = price ~ aspiration + enginelocation + 
                  enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + 
                  carCompanysubaru +  
                  enginetyperotor, data = train[, -1])
summary(model_31a)
vif(model_31a)

# adjusted R-squared decreased from 0.9437 to 0.9224, a decline of 2 percent.

# removing enginesize
model_31b <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + 
                  carCompanysubaru +  
                  enginetyperotor, data = train[, -1])
summary(model_31b)
vif(model_31b)

# adjusted R-squared decreases from 0.9437 to 0.8926
# these are fairly large decreases.

# Thus, we remove variables which are comparatively less significant
# remove aspiration
model_31 <- lm(formula = price ~  enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + 
                  carCompanysubaru +  
                  enginetyperotor, data = train[, -1])
summary(model_31)
vif(model_31)


# remove carcompanysubaru
model_32 <- lm(formula = price ~  enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + 
                  enginetyperotor, data = train[, -1])
summary(model_32)
vif(model_32)

# remove enginetyperotor
model_33 <- lm(formula = price ~  enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar, data = train[, -1])
summary(model_33)
vif(model_33)


# remove stroke
model_34 <- lm(formula = price ~  enginelocation + 
                  carwidth + enginesize +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar, data = train[, -1])
summary(model_34)
vif(model_34)

# remove carcompany jaguar
model_35 <- lm(formula = price ~  enginelocation + 
                  carwidth + enginesize + 
                  carCompanybmw + carCompanybuick, data = train[, -1])
summary(model_35)
vif(model_35)

# Now we have 5 variables in the model.
# Test the model on test dataset
Predict_1 <- predict(model_35,test[,-c(1,20)])

#-------------------------------------------------------------------------------------------

# Add a new column "test_predict" into the test dataset

test$test_price <- Predict_1

#-------------------------------------------------------------------------------------------

# calculate the test R2 

cor(test$price,test$test_price)
cor(test$price,test$test_price)^2