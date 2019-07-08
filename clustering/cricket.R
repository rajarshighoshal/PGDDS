## load data
cricket <- read.csv("Cricket.csv", stringsAsFactors = F)
## choose columns for clustering
data <- cricket[,c("Ave", "SR")]
## scale
data$Ave <- scale(data$Ave)
data$SR <- scale(data$SR)
## find optimal value of k
r_sq <- rnorm(20)
for(k in 1:20)
{
  clus <- kmeans(data, centers = k, nstart = 50)
  r_sq[k] <- clus$betweenss/clus$totss
}
plot(r_sq)
## choosing k = 4
clus4 <- kmeans(data, centers = 4, nstart = 50)
cricket <- cbind(cricket, clus4$cluster)
colnames(cricket)[14] <- "ClusterID"

library(ggplot2)
ggplot(cricket, aes(x = SR, y = Ave, colour = as.factor(cricket$ClusterID), label = Player)) +
  geom_point() + geom_text(size = 3)



#### hc
hc <- hclust(dist(data), method = "complete")
plot(hc)
cluster <- cutree(hc, k = 4)
cricket <-cbind(cricket,cluster)
ggplot(cricket, aes(x = SR, y = Ave, colour = as.factor(cricket$cluster), label = Player)) + 
  geom_point() + geom_text(size = 3)
