library(quantmod)
library(forecast)
library(tseries)
library(timeSeries)
library(dplyr)
library(neuralnet)

#Input Data
getSymbols("BMRI", src = "yahoo", from = "2017-01-01")
close_price <- BMRI$Close

#Data Description
summary(close_price)
sd(close_price)

#ACF & PACF
acf(close_price, lag.max = 100, main ="(a) ACF")
pacf(close_price, lag.max = 100, main = "(b) PACF")

#Box-cox
library(MASS)
boxcox(lm(close_price~1))
powerTransform(close_price)

#difference logged data
bmri_diff <- diff(close_price, lag = 1)
bmri_difff <- na.locf(bmri_diff, na.rm = TRUE
                      fromLast = TRUE)
plot(bmri_difff, main ="Diferensiasi d=1", ylab = "BMRI")
adf_diff <- adf.test(bmri_difff, alternative = c("stationary", "explosive"),
                     k = 0)
adf_diff

#ACF & PACF after differencing
diff.acf <- acf(bmri_diff)
diff.apcf <- pacf(bmri_diff)

#forecast
databmri <- as.ts(close_price)
split <- floor(length(databmri)*0.8)
traindata <- window(databmri, end = split)
testdata <- window databmri, start =split+1)
library(forecast)
set.seed(123)
arima_model <- auto.arima(traindata, lambda = 0.05)
arima_model
summary(arima_model)

#check residuals
checkresiduals(arima_model)
adf.test(residu)
ks.test(residu, pnorm, mean(residu, trim = 0.25), sd(residu))

#plot
arima <- arima(testdata, order = c(1, 1, 2))
summary(arima)
forecast1 <- forecast(arima, h=30)
a <- ts(close_price)
forecat_ori %>% autoplot() + autolayer(a)
forecast_ori

#FFNN
#Hidden LAyers creation
alpha <- 1.5^(-10)
hn <- length(traindata)/(alpha*(length(traindata)+30))
lambda <- BoxCox.lambda(traindata)
dnn_pred <- nnetar(traindata, size = hn, lambda = lambda)
dnn_pred

#fitting nnetar
dnn_forecast <- forecast(dnn_pred, h = 30, PI = TRUE)
dnn_forecast
plot(dnn_forecast)
summary(dnn_forecast)

