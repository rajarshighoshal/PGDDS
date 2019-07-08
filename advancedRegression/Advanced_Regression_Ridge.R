#------------------------Ridge Regression-------------------------------------
#-----------------------------------------------------------------------------

# Load the dataset "carPrice_1 in the working directory"


carPrice<-read.csv("carPrice.csv")


# Create a matrix "x" of all independent variables
# and store dependent variable in "y".

x <- model.matrix(price~.,data=carPrice)[,-1]
y <-carPrice$price

# Divide you data in 70:30 

set.seed(1)
train= sample(1:nrow(x), 0.7*nrow(x))

# Store indices into test which is not present in train . 
test = (-train)

y.test = y[test]

#-----------------------------------------------------------------------------

library(glmnet)

# Cross Validation

cv.out <- cv.glmnet(x[train,],y[train],alpha=0)

# Plot cv.out

plot(cv.out)

# Optimal lamda 

minlamda <- cv.out$lambda.min

minlamda

# Apply model on train dataset at lambda equal to minlamda

ridge.mod <- glmnet(x[train,],y[train],alpha=0,lambda =minlamda)

# Prediction on test dataset

ridge.pred <- predict(ridge.mod,s=minlamda,newx=x[test,])


# MSE with ridge 

mean((ridge.pred-y.test)^2)


# Apply model on train dataset at lambda equal to zero

ridge.mod <- glmnet(x[train,],y[train],alpha=0,lambda = 0)

# Linear Regression model
ridge.pred_0 <- predict(ridge.mod,s=0,newx=x[test,])


# MSE Linear regression model

mean((ridge.pred_0-y.test)^2)

# Final model Coefficents 

ridge_coef <-predict(ridge.mod,type="coefficients",s=minlamda)

ridge_coef

#----------------------------------------------------------------------------------