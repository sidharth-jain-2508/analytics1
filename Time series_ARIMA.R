#Time series ARIMA

setwd("C:/Users/seeeeed/Desktop/doosra saal/Term 6/Python")
getwd()

returns= read.csv("Returns.csv",1)
original=returns
attach(original)

nrow(returns)
breakpoint=floor(nrow(returns)*.95)
breakpoint
Returns=ts(returns[c(1:breakpoint),],frequency = 1)
Returns

library("tseries")
library("TSA")
library("forecast")

kpss.test(Returns)
#data is stationary, hence no differencing to remove trend and seasonality : d=0

acf.returns= acf(returns[c(1:breakpoint),], main="ACF Plot")
#q can be 3,5 and 8

pacf.returns= pacf(returns[c(1:breakpoint),], main= "PACF Plot")
#p can be 3 

fit1=Arima(Returns, order=c(3,0,3))
fit1

fit2=Arima(Returns, order=c(3,0,5))
fit2

fit3=Arima(Returns, order=c(3,0,8))
fit3

#fit 2 has lowest AIC value, hence it is selected

acf(residuals(fit2))
pacf(residuals(fit2))
#residuals are well below the threshold

Box.test(residuals(fit2),lag=1,type="Ljung")

#predicting the value for next 5 days
pred= forecast(fit2, h=5)
plot(pred, xlab="Lags", ylab = "Returns")
pred$mean

#comparing the data sets
lines(original, col="red")

new_series= pred$fitted
plot(Returns, main="Original versus Fitted series", col="blue")
lines(new_series,col="red")
