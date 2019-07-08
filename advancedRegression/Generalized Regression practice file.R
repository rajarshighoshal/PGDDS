# Load Dataset
cdata <- read.csv("total-electricity-consumption-us.csv")

# Explore the data 

plot(cdata, xlab='Year', ylab='Consumption', type='b')
fitseq <- seq(1920, 1970, by=1)
lr <- lm(cdata$Consumption ~ poly(cdata$Year, 3), data=cdata)
lines(fitseq, predict(lr, data.frame(x=fitseq)), col='red', lwd=2)
