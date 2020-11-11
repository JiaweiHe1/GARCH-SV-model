MSFT <- read.csv("MSFT.csv", header = T)
#log return
lr_MSFT <- diff(log(MSFT[, "Adj.Close"])) * 100

train_MSFT <- lr_MSFT[1:3773]
test_MSFT <- lr_MSFT[3774:5050]

library(forecast)
arima_MSFT <- auto.arima(train_MSFT, seasonal = TRUE, max.p = 10, max.q = 10, max.order = 10, ic = "aic",trace = T)

library(rugarch)
myspec_MSFT<- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), submodel = NULL, 
                                               external.regressors = NULL, variance.targeting = FALSE),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, archm = FALSE, 
                                           archpow = 1, arfima = FALSE, external.regressors = NULL, 
                                           archex = FALSE),
                         distribution.model = "norm")

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


# forecast basic
test_MSFT_fore <- ugarchforecast(train_MSFT_fit, n.ahead = length(test_MSFT))
plot(as.matrix(test_MSFT), type = "l")
lines(1:length(test_MSFT_fore@forecast$seriesFor), test_MSFT_fore@forecast$seriesFor, col = "green")



# forecast boostrap
bootp_MSFT <- ugarchboot(train_MSFT_fit, method = c("Partial"), n.ahead = 1277, n.bootpred = 1277)
# up:returns, down:series

sig = sigma(bootp_MSFT@forc)
ser = fitted(bootp_MSFT@forc)
MSFT_boostrap_series = cbind(t(as.data.frame(bootp_MSFT, which = "sigma", type = "summary")),  sig)
MSFT_boostrap_sigma = cbind(t(as.data.frame(bootp_MSFT, which = "series", type = "summary")), ser)
MSFT_boostrap_sigma = as.data.frame(MSFT_boostrap_sigma)
MSFT_boostrap_series = as.data.frame(MSFT_boostrap_series)
#MSFT_sigma[, c(2, 4)]
#MSFT_series[, c(2, 4)]
names(MSFT_boostrap_sigma)[6] <- 'forecast'

names(MSFT_boostrap_series)[6] <- 'forecast'


library(ggplot2)
x <- c(1:length(test_MSFT))
plot.data <- data.frame(x=c(x,x,x,x),
                        y=c(MSFT_boostrap_sigma$q.25,MSFT_boostrap_sigma$mean,
                            MSFT_boostrap_sigma$q.75, MSFT_boostrap_sigma$forecast),
                        name = c(rep("q0.25",length(x)), rep("mean",length(x)),
                                 rep("q0.75",length(x)), rep("forecast",length(x))))

ggplot(data = plot.data) +
  geom_line(mapping = aes(x = x, y = y, col= name), alpha = 0.7) +
  labs(title = "Series forecast with bootstrap error bands", x = "n.head", y = "log-returns")


plot.data_series <- data.frame(x=c(x,x,x,x,x),
                        y=c(MSFT_boostrap_series$min, MSFT_boostrap_series$q0.25, MSFT_boostrap_series$mean,
                            MSFT_boostrap_series$q0.75, MSFT_boostrap_series$forecast),
                        name = c(rep("min",length(x)), rep("q0.25",length(x)), rep("mean",length(x)),
                                 rep("q0.75",length(x)), rep("forecast",length(x))))

ggplot(data = plot.data_series) +
  geom_line(mapping = aes(x = x, y = y, col= name)) +
  labs(title = "Sigma forecast with bootstrap error bands", x = "n.head", y = "sigma")



# forecast rolling
cl = makePSOCKcluster(10)
roll_MSFT <- ugarchroll(myspec_MSFT, lr_MSFT, n.start = 3774, refit.every = 25,
                  refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                  VaR.alpha = c(0.01, 0.05), cluster = cl, keep.coef = TRUE)
show(roll_MSFT)
stopCluster(cl)
MSFT_roll_forecast_sigma <- roll_MSFT@forecast$density$Realized

plot(as.matrix(test_MSFT), type = "l", ylab = "log return of MSFT")
lines(1:length(MSFT_roll_forecast_sigma), MSFT_roll_forecast_sigma, col = "green")



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
myspec_EUR_USD<- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), submodel = NULL, 
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
myspec_Oil <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), submodel = NULL, 
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


# kurtosis
x <- seq(1, 10000, 1)
w <- 3*exp((alpha+gamma)^2/(1-beta^2))
for(i in x){
  w <- w * (pnorm(2*beta^i*(alpha + gamma)) + 
            exp(-8*beta^(2*i)*alpha*gamma)*pnorm(2*beta^i*(alpha - gamma)))/((pnorm(beta^i*(alpha + gamma))+exp(-2*beta^(2*i)*alpha*gamma)*pnorm(beta^i*(alpha-gamma)))^2)
}
w

# first-order autocorrelation
((1+gamma^2) * exp(gamma^2*beta/(1-beta^2))-1) / (3*exp(gamma^2/(1-beta^2))-1)


