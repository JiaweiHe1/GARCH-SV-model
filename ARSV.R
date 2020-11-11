MSFT <- read.csv("MSFT.csv", header = T)
#log return
lr_MSFT <- diff(log(MSFT[, "Adj.Close"])) * 100

train_MSFT <- lr_MSFT[1:3773]
test_MSFT <- lr_MSFT[3774:5050]

library(stochvol)
#data(exrates)
#y <- exrates$USD

#y <- MSFT$Adj.Close[1:3773]
y <- train_MSFT

## Fit AR(1)-SV model to train MSFT
res <- svsample(y, designmatrix = "ar1")

# Visulize the result
plot(res, showobs = FALSE)

#res <- updatesummary(res, quantiles = c(0.01, 0.1, 0.5, 0.9, 0.99))
volplot(res, forecast = 1277, dates = MSFT$Date[1:3773])


# a numeric summary of the estimation results
summary(res, showlatent = FALSE)  # showlatent = TRUE


## Use predict.svdraws to obtain predictive volatilities
ahead <- 1277
preds <- predict(res, steps = ahead)
predvol <- preds$h
class(predvol) <- "svpredict"

## Use arpredict to obtain draws from the posterior predictive
preddraws <- arpredict(res, predvol)

## Calculate predictive quantiles
predquants <- apply(preddraws, 2, quantile, c(.1, .5, .9))

## Visualize
ts.plot(y, xlim = c(length(y) - ahead, length(y) + ahead), ylim = range(predquants))
for (i in 1:3) {
  lines((length(y) + 1):(length(y) + ahead), predquants[i,],
        col = 3, lty = c(2, 1, 2)[i])
}


ts.plot(cbind(t(apply(preds$y, 2, quantile, c(0.05, 0.5, 0.95))), 
              test_MSFT), xlab = "Periods ahead", lty = c(rep(1, 3), 2),
        col = c("gray80", "black", "gray80", "red"))




# EUR_USD
EUR_USD_1 <- read.csv("EUR_USD_1.csv", header = T)
EUR_USD_2 <- read.csv("EUR_USD_2.csv", header = T)
EUR_USD <- rbind(EUR_USD_2, EUR_USD_1)
EUR_USD <- EUR_USD[nrow(EUR_USD):1, ]
#log return
lr_EUR_USD <- diff(log(EUR_USD[, "Price"])) * 100

train_EUR_USD <- lr_EUR_USD[1:3913]
test_EUR_USD <- lr_EUR_USD[3914:5240]


library(stochvol)
#data(exrates)
#y <- exrates$USD

y <- train_EUR_USD

## Fit AR(1)-SV model to train EUR/USD
res <- svsample(y, designmatrix = "ar1")

# Visulize the result
plot(res, showobs = FALSE)

#res <- updatesummary(res, quantiles = c(0.01, 0.1, 0.5, 0.9, 0.99))
volplot(res, forecast = 1327, dates = MSFT$Date[1:3913])

# a numeric summary of the estimation results
summary(res, showlatent = FALSE)  # showlatent = TRUE


## Use predict.svdraws to obtain predictive volatilities
ahead <- 1327
preds <- predict(res, steps = ahead)
predvol <- preds$h
class(predvol) <- "svpredict"

## Use arpredict to obtain draws from the posterior predictive
preddraws <- arpredict(res, predvol)

## Calculate predictive quantiles
predquants <- apply(preddraws, 2, quantile, c(.1, .5, .9))

## Visualize
ts.plot(y, xlim = c(length(y) - ahead, length(y) + ahead), ylim = range(predquants))
for (i in 1:3) {
  lines((length(y) + 1):(length(y) + ahead), predquants[i,],
        col = 3, lty = c(2, 1, 2)[i])
}


ts.plot(cbind(t(apply(preds$y, 2, quantile, c(0.05, 0.5, 0.95))), 
              test_MSFT), xlab = "Periods ahead", lty = c(rep(1, 3), 2),
        col = c("gray80", "black", "gray80", "red"))




# Crude Oil futures
Oil_1 <- read.csv("Oil_1.csv", header = T)
Oil_2 <- read.csv("Oil_2.csv", header = T)
Oil <- rbind(Oil_2, Oil_1)
Oil <- Oil[nrow(Oil):1, ]
#log return
lr_Oil <- diff(log(Oil[, "Price"])) * 100

train_Oil <- lr_Oil[1:3773]
test_Oil <- lr_Oil[3774:5116]


library(stochvol)
#data(exrates)
#y <- exrates$USD

y <- train_Oil

## Fit AR(1)-SV model to train Oil
res <- svsample(y, designmatrix = "ar1")

# Visulize the result
plot(res, showobs = FALSE)

#res <- updatesummary(res, quantiles = c(0.01, 0.1, 0.5, 0.9, 0.99))
volplot(res, forecast = 1343, dates = MSFT$Date[1:3773])

# a numeric summary of the estimation results
summary(res, showlatent = FALSE)  # showlatent = TRUE


## Use predict.svdraws to obtain predictive volatilities
ahead <- 1343
preds <- predict(res, steps = ahead)
predvol <- preds$h
class(predvol) <- "svpredict"

## Use arpredict to obtain draws from the posterior predictive
preddraws <- arpredict(res, predvol)

## Calculate predictive quantiles
predquants <- apply(preddraws, 2, quantile, c(.1, .5, .9))

## Visualize
ts.plot(y, xlim = c(length(y) - ahead, length(y) + ahead), ylim = range(predquants))
for (i in 1:3) {
  lines((length(y) + 1):(length(y) + ahead), predquants[i,],
        col = 3, lty = c(2, 1, 2)[i])
}


ts.plot(cbind(t(apply(preds$y, 2, quantile, c(0.05, 0.5, 0.95))), 
              test_MSFT), xlab = "Periods ahead", lty = c(rep(1, 3), 2),
        col = c("gray80", "black", "gray80", "red"))




h <- sigma_2 / (1-phi^2)

k <- 3*exp(h)
k

auto <- (exp(sigma_2 * phi) - 1) / (3 * exp(sigma_2) - 1)
auto
