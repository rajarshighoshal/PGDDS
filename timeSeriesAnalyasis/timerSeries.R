# generate  a sequence of gaussian random numbers 
# and convert the sequence into a timne series object
noise <- ts(rnorm(200, mean = 0, sd = 1))


# code for histogram
hist(noise, freq = F, prob = T, ylim = c(0,0.5), xlim = c(-5,5), col = "red")


# comparing with normal distribution
mu <- mean(noise)
sigma <- sd(noise)
x <- seq(-5, 5, length = 100)
y <- dnorm(x, mu, sigma)
lines(x, y, lwd = 2, col = "blue")


# code for the QQ plot
qqnorm(noise)
abline(0,1,col="red")
