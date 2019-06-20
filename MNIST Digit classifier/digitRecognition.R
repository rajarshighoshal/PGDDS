##### Loading required libraries for analysis #####
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(doParallel)
library(parallelSVM)

##### Loading datasets in the environment #####
mnist_train <- read_csv("mnist_train.csv", col_names = F)
mnist_test <- read_csv("mnist_test.csv", col_names = F)

##### Data understanding and preperation #####
## understand dimension
dim(mnist_train)
" 60000 X 785 "
dim(mnist_test)
" 10000 X 785"

## structure of data
str(mnist_train)
str(mnist_test)
" everything is integer as they should be "

## checking missing values
which(sapply(mnist_train, function(x) sum(is.na(x))) != 0)
" no NA"
which(sapply(mnist_test, function(x) sum(is.na(x))) != 0)
"no NA "

## remove columns with same value throughout rows
same_value_train <- which(sapply(mnist_train, function(x) length(unique(x)) == 1))
same_value_test <- which(sapply(mnist_test, function(x) length(unique(x)) == 1))
same_value_test_train <- intersect(same_value_test, same_value_train)
" 65 values "
mnist_train <- mnist_train %>% select(-same_value_test_train)
mnist_test <- mnist_test %>% select(-same_value_test_train)

## check values in column 1 if they are all in between 0 and 9
which(mnist_train[1] < 0 | mnist_train[1] > 9)
" no such row"
which(mnist_test[1] < 0 | mnist_test[1] > 9)
" no such row"

### all the values except column 1 are the pixel values of that particular block 
### so it should be between 0 to 255
## check values in columns (except column 1) if greater than 255
which(mnist_train[-1] < 0 | mnist_train[-1] > 255)
" no such value "
which(mnist_train[-1] < 0 | mnist_train[-1] > 255)
" no such value "

## make X1 factor
mnist_train$X1 <- as.factor(mnist_train$X1)
mnist_test$X1 <- as.factor(mnist_test$X1)

##### Model Creation #####

### enable parallel processing
cl <- makeCluster(detectCores())
registerDoParallel(cl)

## model 1 : linear svm
Model_1 <- parallelSVM(X1 ~ ., data = mnist_train, scale = FALSE, kernel = "linear")
Eval_1 <- predict(Model_1, mnist_test)

# confusion matrix - Linear Kernel
confusionMatrix(Eval_1,mnist_test$X1)
" Accuracy : 0.9324 "
## see model 1 details
Model_1
" cost = 1; gamma = 0.00139"

## model 2: RBF svm
Model_2 <- ksvm(X1~ ., data = mnist_train, scale = FALSE, kernel = "rbfdot")
Eval_2 <- predict(Model_2, mnist_test)

# confusion matrix - RBF Kernel
confusionMatrix(Eval_2, mnist_test$X1)
" Accuracy : 0.9772 "
## see model 2 details
Model_2
" cost = 1, sigma = 1.63593975515263e-07  "


## model 3 : polynomial svm
Model_3 <- parallelSVM(X1 ~ ., data = mnist_train, scale = FALSE, kernel = "polynomial")
Eval_3 <- predict(Model_3, mnist_test)

# confusion matrix - Polynomial Kernel
confusionMatrix(Eval_3,mnist_test$X1)
" Accuracy : 0.963 "
## see model 3 details
Model_3
" cost = 1; gamma = 0.00139"

" choosing rbf kernel based on accuracy level "

##### Hyperparameter tuning and Cross-validation #####
trainControl <- trainControl(method = "cv", number = 5)
## take accuracy as metric
metric <- "Accuracy"

## create grid for testing with different values
grid1 <- expand.grid(.sigma=seq(.0000001, .0000005, by=.0000001), .C=seq(1, 5, by=1))

## get train data as 15% of the total train data
set.seed(100)
train.indices = sample(1:nrow(mnist_train), 0.15*nrow(mnist_train))
train = mnist_train[train.indices, ]
## fit svm using train
set.seed(100)
fit.svm1 <- train(X1 ~ ., data = train, method = "svmRadial", metric = metric, 
                 tuneGrid = grid1, trControl = trainControl)

print(fit.svm1)
plot(fit.svm1)
" choosing sigma = 5e-07 and C = 2 "

" as there was no sign of decrease of accuracy with increase of sigma 
  so we should try with some more values starting from 4e-07 "
set.seed(100)
grid2 <- expand.grid(.sigma=seq(.0000004, .0000009, by=.0000001), .C=c(1,2,3))
fit.svm2 <- train(X1 ~ ., data = train, method = "svmRadial", metric = metric, 
                  tuneGrid = grid2, trControl = trainControl)

print(fit.svm2)
plot(fit.svm2)
" so  sigma = 5e-07 is the optimum sigma and C = 2 is optimul cost "


#### final model ####
final_model <- parallelSVM(X1~ ., data = mnist_train, scale = FALSE, kernel = "radial",
                    gamma = .0000005, cost = 2)

final_eval <- predict(final_model, mnist_test)

## final comfusion matrix
confusionMatrix(final_eval, mnist_test$X1)
" accuracy : 0.9739 
  so, in case of rbfkernel with sigma = .0000005 and C = 2 we get 97.39% accurate model "
### stopping parallel processing
stopCluster(cl)

