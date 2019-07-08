library(caTools)
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)
library(kernlab)
# The following code provides a basic model building template.
# Feel free to tweak the code as this is just for practice purpose.
# But keep in mind that the aim is to compare all the three models based on accuracies.

# Read data
consumers <- read.csv("ecommerce_consumers.csv")
str(consumers)

# EDA and data Visualisation

# Modelling -------------------------------------------------

## Prepare data for glm
consumer_num <- consumers
consumer_num$label <- ifelse(consumer_num$label=="male",1,0)
consumer_num$time <- scale(consumer_num$time)
consumer_num$ratio <- scale(consumer_num$ratio)
## Divide the data into train and test in 75:25 ratio.
set.seed(100)
indices = sample.split(consumer_num$label, SplitRatio = 0.75)
train = consumer_num[indices,]
test = consumer_num[!(indices),]
## Build a logistic regression model
logistic_model <- glm(label ~ ., data = train, family = "binomial")
pred_num <- predict(logistic_model, type = "response", newdata = test[,-3])
logistic_actual <- factor(ifelse(test$label == 1,"male","female"))
perform_fn <- function(cutoff) 
{
  predicted_label <- factor(ifelse(pred_num >= cutoff, "male", "female"))
  conf <- confusionMatrix(predicted_label, logistic_actual, positive = "male")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


summary(pred_num)

s = seq(.5,.75,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2]) == min(abs(OUT[,1]-OUT[,2])))]

test_cutoff <- factor(ifelse(pred_num >= cutoff, "female", "male"))

test$label <- factor(ifelse(test$label==1,"male","female"))
conf_final <- confusionMatrix(test_cutoff, test$label)

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

conf_final
" accuracy = .64 "
## Build a decision tree model and try different values of hyperparameters
set.seed(10)
indices = sample.split(consumers$label, SplitRatio = 0.75)
train = consumers[indices,]
test = consumers[!(indices),]
tree_model <- rpart(label ~ ., train)
tree_predictions <- predict(tree_model, test, type = "class")
tree_confusion_matrix <- confusionMatrix(tree_predictions, test$label) 
tree_confusion_matrix
" accuracy = 1 "
## Build an SVM model with different kernels and try different values of hyperparameters
svm_model <- svm(label ~ ., data = train, kernel = "radial")
svm_predictions <- predict(svm_model, test)
svm_confusion_matrix <- confusionMatrix(svm_predictions, test$label) 
svm_confusion_matrix
" accuracy = .94 "
  
  
  