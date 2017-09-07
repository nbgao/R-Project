## 0. Import data & packages
data<-read.csv("000001.csv")
nrow(data)
ncol(data)
summary(data)

library(tseries)
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch)


## 1. 计算对数收益率
close <- data[,2]    # 上证指数日收盘价
n <- length(close)    # 获取数据的长度
return <- log(close[2:n]) - log(close[1:n-1])   # 上证指数的对数收益率
head(return)    # 查看return的前6行数据

return <- diff(log(close))    # 直接差分求对数收益率
head(return)


## 2. 股票指数的可视化
Close.ts <- ts(close, start=c(2000), freq=250)
Return.ts <- ts(return, start=c(2000), freq=250)

par(mfrow=c(2,1))    # 建立一个以两行一列排列图形的图形窗口
plot(Close.ts, type="l", main="(a) Daily Closing Price of 000001.SH", xlab="Date", ylab="Price", cex.main=0.95, las=1)
plot(Return.ts, type="l", main="(b) Daily Closing Price of 000001.SH", xlab="Date", ylab="Rate", cex.main=0.95, las=1)


## 3. 股票收益率的基本统计量
library(tseries)
# 平均值
u <- sum(return)/n
u
# 标准差
e <- sqrt(sum((return-u)^2)/(n-1))
e
# 偏度
s <- sum((return-u)^3)/((n-1)*e^3)
s
# 峰度
k <- sum((return-u)^4)/((n-1)*e^4)
k
# JB正态性检验
jarque.bera.test(return)


## 4. ACF图和PACF图
par(mfrow=c(2,1))
# 自相关图
acf(return, main='(a) the ACF of Return', xlab="Lag (a)", ylab="ACF", las=1, cex.main=0.95)
# 偏自相关图
pacf(return, main='(b) the PACF of Return', xlab="Lag (b)", ylab="PACF", las=1, cex.main=0.95)
# 收益率序列基本不具有自相关性

# 考察收益率平方的自相关性
par(mfrow=c(2,1))
return.square <- return^2

acf(return.square, main='(a) the ACF of Return Square', xlab="Lag (c)", ylab="ACF", las=1, cex.main=0.95)
pacf(return.square, main='(b) the PACF of Return Square', xlab="Lag (d)", ylab="PACF", las=1, cex.main=0.95)


# 5. ARCH效应检验
library(zoo)
library(FinTS)
install.packages("FinTS")

ArchTest(return, lag=12)


# 6. GARCH模型的估计
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch)

m1 <- garchFit(~1+garch(1,1), data=return, trace=F)
summary(m1)

m2 <- garchFit(~1+garch(1,2), data=return, trace=F)
summary(m2)

m3 <- garchFit(~1+garch(1,3), data=return, trace=F)
summary(m3)

m4 <- garchFit(~1+garch(1,4), data=return, trace=F)
summary(m4)


## 7. GARCH模型的标准化残差分析
resi <- residuals(m1, standardize=T)    # 获得标准化残差
par(mfcol=c(1,1))
plot(resi, xlab='Date', ylab='st.resi', type='l')
par(mfcol=c(2,2))
acf(resi, lag=24)
acf(resi^2, lag=24)
pacf(resi, lag=24)
pacf(resi^2, lag=24)

# 残差平方的滞后10阶自相关检验
Box.test(resi^2, lag=10, type='Ljung')
# 残差平方的滞后15阶自相关检验
Box.test(resi^2, lag=15, type='Ljung')
# 残差平方的滞后20阶自相关检验
Box.test(resi^2, lag=20, type='Ljung')
# 可接受原假设，标准化残差平方不存在序列相关性