filename <- c("exchange-rate-twi.csv")

cols <- c("red", "blue")

labels <- c("Raw", "Smoothed")

ylab1 <- c("Exchange Rate TWI")

xlab1 <- c("Months from May 1970")

title <- c("Exchange Rate TWI: May 1970 to Aug 1995")


width <- 51

rawdata <- read.csv(filename)

timeser <- ts(rawdata[,2])

plot(timeser, main=title, xlab = xlab1, 
     ylab = ylab1, col=cols[1])

smoothedseries <- filter(timeser, filter=rep(1/width, width),
                         method="convolution", sides=2)

lines(smoothedseries, col=cols[2], lwd=2)

legend("bottomleft", labels, col=cols, lwd=2)




plot(timeser, main=title, xlab = xlab1, ylab = ylab1)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.02, 0.1, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(timeser, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

