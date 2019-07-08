library(forecast)
# guessp, guessq - Initial guesses for p and q (from ACF/PACF plots)
# delta - window in which we want to change guessp and guessq to
# look for alternative ARMA models
# timeseries - the one we want to analyze

tryArma <- function(delta, guessp, guessq, timeseries) {
  df <- data.frame()
  # generate all possible ARMA models
  for (p in max(0,(guessp-delta)):(guessp+delta)) {
    for (q in max(0,(guessq-delta)):(guessq+delta)) {
      order <- c(p,0,q)
      # Fit a maximum likelihood ARMA(p,q) model
      armafit <- Arima(timeseries, order=order, method="ML")
      # Add the results to the dataframe
      df <- rbind(df, c(p, q, armafit$loglik, armafit$aic, armafit$aicc, armafit$bic))
    }
  }
  names(df) <- c("p","q","log.likelihood", "AIC", "AICc", "BIC")
  return(df)
}


# Simulate a ARMA(2,2) series
arma22<-arima.sim(model=list(ar=c(.9,-.2),ma=c(-.7,.1)),n=200)
df <- tryArma(2,1,1,arma22)

df
