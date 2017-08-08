# 加载数据包
library(zoo)
library(xts)
library(TTR)


## 1.ARIMA模型
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)
# 绘制时间序列图
plot.ts(kingstimeseries)

# 数据差分
kingstimeseriesdiff1 <- diff(kingstimeseries, difference=1)
plot.ts(kingstimeseriesdiff1)
d <- 1

# 检查平稳时间序列的自相关(ACF)图和偏自相关函数(PACF)图
# ACF
acf(kingstimeseriesdiff1, lag.max=20)
acf(kingstimeseriesdiff1, lag.max=20, plot=FALSE)
p <- 1
# PACF
pacf(kingstimeseriesdiff1, lag.max=20)
pacf(kingstimeseriesdiff1, lag.max=20, plot=FALSE)
q <- 3


library(forecast)
# ARIMA(p,d,q)
kings.arima <- arima(kingstimeseries, order = c(1,1,3))
summary(kings.arima)

# 模型预测未来5天，43-47
kingsarimaforecast <- forecast(kings.arima, h=5)
kingsarimaforecast
plot(kingsarimaforecast)

# 预测误差自相关ACF
acf(kingsarimaforecast$residuals, lag.max=20)
# Ljung-Box检验
Box.test(kingsarimaforecast$residuals, lag=20, type="Ljung-Box")


# auto.arima自动预测
kings.arima1 <- auto.arima(kingstimeseries)
summary(kings.arima1)

kings.arima_new <- arima(kingstimeseries, order=c(0,1,1))
summary(kings.arima_new)

kingsarimaforecast_new <- forecast(kings.arima_new, h=5)
kingsarimaforecast_new
plot(kingsarimaforecast_new)

acf(kingsarimaforecast_new$residuals, lag.max=20)
Box.test(kingsarimaforecast$residuals, lag=20, type="Ljung-Box")



## 2.指数平滑模型
# 简单指数平滑法
rain <-scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat", skip=1)
rainseries <- ts(rain, start=c(1813))
plot.ts(rainseries)

# 调用HoltWinters函数
rainseriesHW <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesHW
plot(rainseriesHW)

# 指数平滑模型预测未来5天
rainseriesforecast <- forecast(rainseriesHW, h=5)
plot(rainseriesforecast)

# 预测误差ACF检验和Ljung-Box检验
acf(rainseriesforecast$residuals[!is.na(rainseriesforecast$residuals)], lag.max=20, plot=T)
Box.test(rainseriesforecast$residuals, lag=20, type="Ljung-Box")



## 3.霍尔特指数平滑
skirts <-scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat", skip=5)
skirtsseries <- ts(skirts, start=c(1866))
plot.ts(skirtsseries)

# 调用HoltWinters函数
skirtsseriesHW <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesHW
plot(skirtsseriesHW)

# 预测未来5年
skirtsseriesforecast <- forecast(skirtsseriesHW, h=5)
plot(skirtsseriesforecast)

# 预测误差ACF检验和Ljung-Box检验
acf(skirtsseriesforecast$residuals[!is.na(skirtsseriesforecast$residuals)], lag.max=20, plot=T)
Box.test(skirtsseriesforecast$residuals, lag=20, type="Ljung-Box")



## 4.Holt-Winters指数平滑法(季节周期性)
souvenir <-scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
plot.ts(souvenirtimeseries)

# 取对数减少极差带来的影响，消除方差不齐
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

# 调用HoltWinter函数
souvenirtimeseriesHW <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesHW
plot(souvenirtimeseriesHW)

# 预测未来12个月
souvenirtimeseriesforecast <- forecast(souvenirtimeseriesHW, h=12)
plot(souvenirtimeseriesforecast)

# 预测误差ACF检验和Ljung-Box检验
acf(souvenirtimeseriesforecast$residuals[!is.na(souvenirtimeseriesforecast$residuals)], lag.max=20, plot=T)
Box.test(souvenirtimeseriesforecast$residuals, lag=20, type="Ljung-Box")
