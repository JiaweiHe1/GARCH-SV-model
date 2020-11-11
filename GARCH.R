library(rugarch)
# library(rmgargh)
library(quantmod)

MSFT <- read.csv("MSFT.csv", header = T)
#log return
lr_MSFT <- diff(log(MSFT[, "Adj.Close"])) * 100

train_MSFT <- lr_MSFT[1:3773]
test_MSFT <- lr_MSFT[3774:5050]

# fit MSFT
library(forecast)
arima_MSFT <- auto.arima(train_MSFT, seasonal = TRUE, max.p = 10, max.q = 10, max.order = 10, ic = "aic",trace = T)

library(rugarch)
myspec_MSFT<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), submodel = NULL, 
                                             external.regressors = NULL, variance.targeting = FALSE),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, archm = FALSE, 
                                         archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                         archex = FALSE),
                       distribution.model = "norm")
# distribution="std"


train_MSFT_fit <- ugarchfit(spec = myspec_MSFT, data=train_MSFT, solver="solnp")

train_MSFT_fit@fit$coef
train_MSFT_var <- train_MSFT_fit@fit$var
train_MSFT_res2 <- (train_MSFT_fit@fit$residuals)^2

plot(train_MSFT_res2, type = "l")
lines(train_MSFT_var, col = "green") # green line: conditional variances

plot(train_MSFT_fit)

qqnorm(train_MSFT_fit@fit$residuals, main='Residuals(MSFT)')
qqline(train_MSFT_fit@fit$residuals)
par(mfrow = c(1, 2))
acf(na.omit(train_MSFT_res2, main='ACF Squared Residuals(MSFT)'))
pacf(na.omit(train_MSFT_res2, main='PACF Squared Residuals(MSFT)'))


# forecast MSFT_1
test_MSFT_fore <- ugarchforecast(train_MSFT_fit, n.ahead = length(test_MSFT))
plot(as.matrix(test_MSFT), type = "l")
lines(1:length(test_MSFT_fore@forecast$seriesFor), test_MSFT_fore@forecast$seriesFor, col = "green")

#forcast_method_2
total_MSFT <- c(train_MSFT, test_MSFT)
MSFT_windowLength = length(train_MSFT)
MSFT_foreLength = length(test_MSFT)
MSFT_forecasts <- vector(mode="character", length=MSFT_foreLength)

# Model to compute the best ARIMA(p,d,q) model + GARCH(1,1)
for (d in 0:MSFT_foreLength) {
  MSFT_ReturnsOffset = total_MSFT[(1+d):(MSFT_windowLength+d)]
  
  # Fit the ARIMA model
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:5) for (q in 0:5) {
    if ( p == 0 && q == 0) {
      next
    }
    
    arimaFit = tryCatch(arima(MSFT_ReturnsOffset, order=c(p, 0, q)), error=function(err) FALSE,
                        warning=function(err) FALSE)
    if(!is.logical(arimaFit)) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) {
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        final.arima <- arima(MSFT_ReturnsOffset, order=final.order)
      }
    } else {
      next
    }
  }
  myspec = ugarchspec(variance.model=list(garchOrder=c(1, 1)),
                      mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
                      distribution.model="norm")   # skewed generalized error distribution
  
  MSFT_fit = tryCatch(ugarchfit(myspec, MSFT_ReturnsOffset, solver = 'hybrid'), 
                    error=function(e) e, warning=function(w) w)
  
  # If the GARCH model does not converge, set the direction to "long" else
  # choose the correct forecast direction based on the returns prediction
  # Output the results to the screen and the forecasts vector
  
  if(is(MSFT_fit, "warning")) {
    MSFT_forecasts[d+1] = paste(index(MSFT_ReturnsOffset[MSFT_windowLength]), 1, sep=",")
    print(paste(index(MSFT_ReturnsOffset[MSFT_windowLength]), 1, sep=","))
  } else {
    MSFT_fore = ugarchforecast(MSFT_fit, n.ahead=1)
    ind = MSFT_fore@forecast$seriesFor
    MSFT_forecasts[d+1] = ind[1]
    print(MSFT_forecasts[d+1])
  }
}

#write.csv(MSFT_forecasts, file="MSFT_forecastsarima_garch.csv", row.names=FALSE)
MSFT_forecasts <- read.csv("MSFT_forecasts_arima_garch.csv", header = TRUE)
MSFT_forecasts <- as.vector(MSFT_forecasts[, "x"])
MSFT_forecasts[MSFT_forecasts == '1,1'] <- 0
MSFT_forecasts <- as.numeric(MSFT_forecasts)

plot(as.matrix(test_MSFT), type = "l", ylab = "log return of MSFT", xlab = "date", main = "log return of MSFT", xaxt="n")
axis(1, c(0, 255, 511, 766, 1021, 1278), c(2015, 2016, 2017, 2018, 2019, 2020), las = 1)
lines(1:1277, MSFT_forecasts[1:1277], col = "green")

# plot(MSFT_forecasts, type = 'l')


#forcast_method_3
total_MSFT <- c(train_MSFT, test_MSFT)
MSFT_windowLength = length(train_MSFT)
MSFT_foreLength = length(test_MSFT)
MSFT_forecasts_garch <- vector(mode="character", length=MSFT_foreLength)

# Model to compute the GARCH(1,1)
for (d in 0:MSFT_foreLength) {
  MSFT_ReturnsOffset = total_MSFT[(1+d):(MSFT_windowLength+d)]
  
  final.order <- c(0,0,0)
  
  myspec = ugarchspec(variance.model=list(garchOrder=c(1, 1)),
                      mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
                      distribution.model="norm")   # skewed generalized error distribution
  
  MSFT_fit = tryCatch(ugarchfit(myspec, MSFT_ReturnsOffset, solver = 'hybrid'), 
                      error=function(e) e, warning=function(w) w)
  
  # If the GARCH model does not converge, set the direction to "long" else
  # choose the correct forecast direction based on the returns prediction
  # Output the results to the screen and the forecasts vector
  
  if(is(MSFT_fit, "warning")) {
    MSFT_forecasts_garch[d+1] = paste(index(MSFT_ReturnsOffset[MSFT_windowLength]), 1, sep=",")
    print(paste(index(MSFT_ReturnsOffset[MSFT_windowLength]), 1, sep=","))
  } else {
    MSFT_fore = ugarchforecast(MSFT_fit, n.ahead=1)
    ind = MSFT_fore@forecast$seriesFor
    MSFT_forecasts_garch[d+1] = ind[1]
    print(MSFT_forecasts_garch[d+1])
  }
}

write.csv(MSFT_forecasts_garch, file="MSFT_forecasts_garch.csv", row.names=FALSE)

plot(as.matrix(test_MSFT), type = "l", ylab = "log return of MSFT")
lines(1:length(MSFT_forecasts_garch), MSFT_forecasts_garch, col = "green")



# fit MSFT with GARCH-t model
library(forecast)
arima_MSFT <- auto.arima(train_MSFT, seasonal = TRUE, max.p = 10, max.q = 10, max.order = 10, ic = "aic",trace = T)

library(rugarch)
myspec_MSFT_t<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), submodel = NULL, 
                                               external.regressors = NULL, variance.targeting = FALSE),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, archm = FALSE, 
                                           archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                           archex = FALSE),
                         distribution.model = "std")


train_MSFT_fit_t <- ugarchfit(spec = myspec_MSFT_t, data=train_MSFT, solver="solnp")

train_MSFT_fit_t@fit$coef
train_MSFT_var <- train_MSFT_fit_t@fit$var
train_MSFT_res2 <- (train_MSFT_fit_t@fit$residuals)^2

plot(train_MSFT_res2, type = "l")
lines(train_MSFT_var, col = "green") # green line: conditional variances

plot(train_MSFT_fit_t)







# EUR_USD
EUR_USD_1 <- read.csv("EUR_USD_1.csv", header = T)
EUR_USD_2 <- read.csv("EUR_USD_2.csv", header = T)
EUR_USD <- rbind(EUR_USD_2, EUR_USD_1)
EUR_USD <- EUR_USD[nrow(EUR_USD):1, ]
#log return
lr_EUR_USD <- diff(log(EUR_USD[, "Price"])) * 100

train_EUR_USD <- lr_EUR_USD[1:3913]
test_EUR_USD <- lr_EUR_USD[3914:5240]

# fit EUR_USD
library(forecast)
arima_EUR_USD <- auto.arima(train_EUR_USD, seasonal = TRUE, max.p = 10, max.q = 10, max.order = 10, ic = "aic",trace = T)

library(rugarch)
myspec_EUR_USD<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), submodel = NULL, 
                                               external.regressors = NULL, variance.targeting = FALSE),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, archm = FALSE, 
                                           archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                           archex = FALSE),
                         distribution.model = "norm")
# distribution="std"


train_EUR_USD_fit <- ugarchfit(spec = myspec_EUR_USD, data=train_EUR_USD, solver="solnp")

train_EUR_USD_fit@fit$coef
train_EUR_USD_var <- train_EUR_USD_fit@fit$var
train_EUR_USD_res2 <- (train_EUR_USD_fit@fit$residuals)^2

plot(train_EUR_USD_res2, type = "l")
lines(train_EUR_USD_var, col = "green") # green line: conditional variances

plot(train_EUR_USD_fit)

qqnorm(train_EUR_USD_fit@fit$residuals, main='Residuals(EUR_USD)')
qqline(train_EUR_USD_fit@fit$residuals)
par(mfrow = c(1, 2))
acf(na.omit(train_EUR_USD_res2, main='ACF Squared Residuals(EUR_USD)'))
pacf(na.omit(train_EUR_USD_res2, main='PACF Squared Residuals(EUR_USD)'))





# Crude Oil futures
Oil_1 <- read.csv("Oil_1.csv", header = T)
Oil_2 <- read.csv("Oil_2.csv", header = T)
Oil <- rbind(Oil_2, Oil_1)
Oil <- Oil[nrow(Oil):1, ]
#log return
lr_Oil <- diff(log(Oil[, "Price"])) * 100

train_Oil <- lr_Oil[1:3773]
test_Oil <- lr_Oil[3774:5116]

# fit Oil
library(forecast)
arima_Oil <- auto.arima(train_Oil, seasonal = TRUE, max.p = 10, max.q = 10, max.order = 10, ic = "aic",trace = T)

library(rugarch)
myspec_Oil <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), submodel = NULL, 
                                                  external.regressors = NULL, variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, archm = FALSE, 
                                              archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                              archex = FALSE),
                            distribution.model = "norm")
# distribution="std"


train_Oil_fit <- ugarchfit(spec = myspec_Oil, data=train_Oil, solver="solnp")

train_Oil_fit@fit$coef
train_Oil_var <- train_Oil_fit@fit$var
train_Oil_res2 <- (train_Oil_fit@fit$residuals)^2

plot(train_Oil_res2, type = "l")
lines(train_Oil_var, col = "green") # green line: conditional variances

plot(train_Oil_fit)

qqnorm(train_Oil_fit@fit$residuals, main='Residuals(Oil)')
qqline(train_Oil_fit@fit$residuals)
par(mfrow = c(1, 2))
acf(na.omit(train_Oil_res2, main='ACF Squared Residuals(Oil)'))
pacf(na.omit(train_Oil_res2, main='PACF Squared Residuals(Oil)'))



