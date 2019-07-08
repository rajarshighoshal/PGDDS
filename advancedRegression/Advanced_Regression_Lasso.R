
#------------------------------- Lasso Regression----------------------------------

# Prediction from Lasso 

cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

# Optimal lamda store it into "minlamda_lasso" object

minlamda_lasso <-cv.out$lambda.min
minlamda_lasso

# But when we check in the plot, MSE value is constant upto log(6)

lasso.mod <- glmnet(x[train,],y[train],alpha=1,lambda = 403.4)

lasso.pred <- predict(lasso.mod,s= 403.4,newx=x[test,])


# MSE 
mean((lasso.pred-y.test)^2)

# All the coefficents from the model at optimal lamda, s=403.4
lasso.coef <- predict(lasso.mod,type="coefficients",s=403.4)

lasso.coef


lasso.coef <- predict(lasso.mod,type="coefficients",s=403.4)[1:65,]

# Non zero coefficients in final model
lasso.coef[lasso.coef!=0]

#-----------------------------------------------------------------------------


