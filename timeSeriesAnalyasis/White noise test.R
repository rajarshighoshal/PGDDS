# Generate a sequence of Gaussian random numbers and
# convert the sequence into a time-series object
noise <- ts(rnorm(200, mean = 0, sd = 1))


# Code for the histogram
hist(noise, freq=FALSE, prob=T,ylim=c(0,0.5),xlim=c(-5,5),col="red")
mu <- mean(noise)
sigma <- sd(noise)
x<-seq(-5,5,length=100)
y<-dnorm(x,mu,sigma)
lines(x,y,lwd=2,col="blue")


# Code for the QQ Plot
qqnorm(noise)
abline(0,1, col="red")