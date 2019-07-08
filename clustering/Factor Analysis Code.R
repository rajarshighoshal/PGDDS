#Load the dataset
marks <- read.csv("marks.csv", stringsAsFactors = FALSE)

#Plot the correlations
library(corrplot)
corrplot(cor(marks[-1]))

#Scale the data
marks_stand <- as.data.frame(scale(marks[-1]))

#Factor Analysis
fa1 <- factanal(marks_stand, factors = 2,rotation = "varimax")
fa1_load <- fa1$loadings

#Sum of Squares of factor loadings
eig1_load <- sum((fa1_load)^2)

#Display Results
fa1_load
eig1_load

