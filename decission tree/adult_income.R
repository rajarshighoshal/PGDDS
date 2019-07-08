#install.packages(c("rpart", "rpart.plot", "caret", "ggplot2"))
rm(list = ls())

# get data
#filename and filepath corresponding to the adult income dataset that you would have downloaded
income <- read.csv("adult_dataset.csv")
str(income)

# baseline accuracy
prop.table(table(income$income))  # 0.76

# divide into train and test set
set.seed(123)
split.indices <- sample(nrow(income), nrow(income)*0.8, replace = F)
train <- income[split.indices, ]
test <- income[-split.indices, ]

# Classification Trees
library(rpart)
library(rpart.plot)
library(caret)

#1 build tree model- default hyperparameters
tree.model <- rpart(income ~ .,                     # formula
                    data = train,                   # training data
                    method = "class")               # classification or regression

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test$income, positive = ">50K")  # 0.8076

#2 Change the algorithm to "information gain" instead of default "gini" ----------------------
tree.model <- rpart(income ~ .,                     # formula
                    data = train,                   # training data
                    method = "class",               # classification or regression
                    parms = list(split = "information")
                    )

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test$income, positive = ">50K")  # 0.8076

#3 Tune the hyperparameters ----------------------------------------------------------
tree.model <- rpart(income ~ .,                                # formula
                     data = train,                             # training data
                     method = "class",                         # classification or regression
                     control = rpart.control(minsplit = 1000,  # min observations for node
                                             minbucket = 1000, # min observations for leaf node
                                             cp = 0.05))       # complexity parameter

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test$income, positive = ">50K")  # 0.77740

#4 A more complex tree -----------------------------------------------------------------
tree.model <- rpart(income ~ .,                                # formula
                     data = train,                             # training data
                     method = "class",                         # classification or regression
                     control = rpart.control(minsplit = 1,     # min observations for node
                                             minbucket = 1,    # min observations for leaf node
                                             cp = 0.001))      # complexity parameter

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test$income, positive = ">50K") 

#5 Cross test to choose CP ------------------------------------------------------------
library(caret)

# set the number of folds in cross test to 5
tree.control = trainControl(method = "cv", number = 5)

# set the search space for CP
tree.grid = expand.grid(cp = seq(0, 0.02, 0.0025))

# train model
tree.model <- train(income ~ .,
                     data = train,
                     method = "rpart",
                     metric = "Accuracy",
                     trControl = tree.control,
                     tuneGrid = tree.grid,
                     control = rpart.control(minsplit = 50,
                                             minbucket = 20))

# look at cross validated model results
tree.model

# look at best value of hyperparameter
tree.model$bestTune

# make predictions on test set
tree.predict <- predict.train(tree.model, test)

# accuracy
confusionMatrix(tree.predict, test$income)  

# plot CP vs Accuracy
library(ggplot2)
accuracy_graph <- data.frame(tree.model$results)
ggplot(data = accuracy_graph, aes(x = cp, y = Accuracy*100)) +
        geom_line() +
        geom_point() +
        labs(x = "Complexity Parameter (CP)", y = "Accuracy", title = "CP vs Accuracy")



