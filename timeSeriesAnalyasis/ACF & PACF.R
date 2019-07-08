
# Generate a sequence of Gaussian random numbers and
# convert the sequence into a time-series object
noise <- ts(rnorm(200, mean = 0, sd = 1))


# Plot the time series
plot.ts(noise)


# Compute and plot the ACF for the time series
acf(noise, level=95, lag.max=40, main="ACF Plot for White Noise")


# Compute and plot the ACF for the time series
pacf(noise, level=95, lag.max=40, main="PACF Plot for White Noise")
