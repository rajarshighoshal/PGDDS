## load data
Online.Retail <- read.csv("Online Retail.csv", stringsAsFactors=FALSE)
## remove na
order_wise <- na.omit(Online.Retail)
####
Amount <- order_wise$Quantity * order_wise$UnitPrice
order_wise <- cbind(order_wise,Amount)
order_wise <- order_wise[order(order_wise$CustomerID),]
monetary <- aggregate(Amount~CustomerID, order_wise, sum)
####
frequency <- order_wise[,c(7,1)]
temp<-table(as.factor(frequency$CustomerID))
temp<-data.frame(temp)
colnames(temp)[1]<-c("CustomerID")
RFM <-merge(monetary,temp,by="CustomerID")
recency <- order_wise[,c(7,5)]
recency$InvoiceDate<-as.Date(recency$InvoiceDate,"%d/%m/%y %H:%M")
maximum <- max(recency$InvoiceDate)
maximum <- maximum+1
maximum$diff <- maximum-recency$InvoiceDate
recency$diff <- maximum$diff
recency <- aggregate(recency$diff,by=list(recency$CustomerID),FUN="min")
colnames(recency)[1]<- "CustomerID"
colnames(recency)[2]<- "Recency"
RFM <- merge(RFM, recency, by = ("CustomerID"))
RFM$Recency <- as.numeric(RFM$Recency)
####
box <- boxplot.stats(RFM$Amount)
out <- box$out
RFM1 <- RFM[ !RFM$Amount %in% out, ]
RFM <- RFM1
box <- boxplot.stats(RFM$Freq)
out <- box$out
RFM1 <- RFM[ !RFM$Freq %in% out, ]
RFM <- RFM1
box <- boxplot.stats(RFM$Recency)
out <- box$out
RFM1 <- RFM[ !RFM$Recency %in% out, ]
RFM <- RFM1

####
RFM_norm1 <- RFM[ , -1]
RFM_norm1$Amount <- scale(RFM_norm1$Amount)
RFM_norm1$Freq <- scale(RFM_norm1$Freq)
RFM_norm1$Recency <- scale(RFM_norm1$Recency)


####
clus <- kmeans(RFM_norm1, centers = 3, nstart = 50)
str(clus)
###
r_sq <- rnorm(20)

for(number in 1:20)
{
  clus <- kmeans(RFM_norm1, centers = number, nstart = 50)
  r_sq[number] <- clus$betweenss/clus$totss
}

plot(r_sq)


##
clus5 <- kmeans(RFM_norm1, centers = 5, nstart = 50)
RFM_km <- cbind(RFM, clus5$cluster)
colnames(RFM_km)[5] <- "ClusterID"

####
library(tidyverse)
km_clusters <- group_by(RFM_km, ClusterID)

tab1 <- summarise(km_clusters, Mean_amount = mean(Amount), 
                  Mean_freq = mean(Freq), Mean_recency = mean(Recency))
g1 <- ggplot(tab1, aes(x= factor(ClusterID), y=Mean_amount)) + geom_bar(stat = "identity")
g2 <- ggplot(tab1, aes(x= factor(ClusterID), y=Mean_freq)) + geom_bar(stat = "identity")
g3 <- ggplot(tab1, aes(x= factor(ClusterID), y=Mean_recency)) + geom_bar(stat = "identity")
library(gridExtra)
grid.arrange(g1, g2, g3)
