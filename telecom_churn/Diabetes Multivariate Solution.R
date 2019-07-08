# Loading data file

pima_indian_diabetes<-read.csv("pima_indian_diabetes.csv")

summary(pima_indian_diabetes)

# scaling of continuous variables

pima_indian_diabetes$No_Times_Pregnant<- scale(pima_indian_diabetes$No_Times_Pregnant)
pima_indian_diabetes$Plasma_Glucose<- scale(pima_indian_diabetes$Plasma_Glucose)
pima_indian_diabetes$Diastolic_BP<- scale(pima_indian_diabetes$Diastolic_BP)
pima_indian_diabetes$Triceps<- scale(pima_indian_diabetes$Triceps)
pima_indian_diabetes$Insulin<- scale(pima_indian_diabetes$Insulin)
pima_indian_diabetes$BMI<- scale(pima_indian_diabetes$BMI)
pima_indian_diabetes$Age<- scale(pima_indian_diabetes$Age)

# Splitting data into training and testing data sets

set.seed(100)

indices <- sample.split(pima_indian_diabetes$Diabetes, SplitRatio = 0.7)

train <- pima_indian_diabetes[indices,]

test <- pima_indian_diabetes[!(indices),]

# Creating model using training data

model_1 = glm(Diabetes ~ ., data = train, family = "binomial")

summary(model_1)

# Variable selection using stepwise AIC algorithm

library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Checking VIF

vif(model_2)

# No variable with VIF greater than 2. Also, all variables have low p value. Hence, there is no need for further variable selection