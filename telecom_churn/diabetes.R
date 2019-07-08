library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

data <- read.csv("pima_indian_diabetes.csv")

data[, c(-8)] <- data.frame(lapply(data[, c(-8)], function(x) scale(x)))

summary(data)


set.seed(100)
indices = sample.split(data$Diabetes, SplitRatio = 0.7)
train <- data[indices,]
test <- data[!(indices),]


model1 <- glm(Diabetes~., data = train, family = "binomial")
summary(model1)

step <- stepAIC(model1)
step

model2 <- glm(formula = Diabetes ~ No_Times_Pregnant + Plasma_Glucose + 
                BMI, family = "binomial", data = train)
summary(model2)
vif(model2)
