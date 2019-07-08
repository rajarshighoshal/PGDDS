library(rpart)
library(rpart.plot)


heart <- read.csv("heart.csv")
heart <- heart[-15]
heart$heart.disease <- as.factor(heart$heart.disease)
tree = rpart(heart.disease ~ ., data = heart)
prp(tree)

