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


## 1. �������������
close <- data[,2]    # ��ָ֤�������̼�
n <- length(close)    # ��ȡ���ݵĳ���
return <- log(close[2:n]) - log(close[1:n-1])   # ��ָ֤���Ķ���������
head(return)    # �鿴return��ǰ6������

return <- diff(log(close))    # ֱ�Ӳ�������������
head(return)


## 2. ��Ʊָ���Ŀ��ӻ�
Close.ts <- ts(close, start=c(2000), freq=250)
Return.ts <- ts(return, start=c(2000), freq=250)

par(mfrow=c(2,1))    # ����һ��������һ������ͼ�ε�ͼ�δ���
plot(Close.ts, type="l", main="(a) Daily Closing Price of 000001.SH", xlab="Date", ylab="Price", cex.main=0.95, las=1)
plot(Return.ts, type="l", main="(b) Daily Closing Price of 000001.SH", xlab="Date", ylab="Rate", cex.main=0.95, las=1)


## 3. ��Ʊ�����ʵĻ���ͳ����
library(tseries)
# ƽ��ֵ
u <- sum(return)/n
u
# ��׼��
e <- sqrt(sum((return-u)^2)/(n-1))
e
# ƫ��
s <- sum((return-u)^3)/((n-1)*e^3)
s
# ���
k <- sum((return-u)^4)/((n-1)*e^4)
k
# JB��̬�Լ���
jarque.bera.test(return)


## 4. ACFͼ��PACFͼ
par(mfrow=c(2,1))
# �����ͼ
acf(return, main='(a) the ACF of Return', xlab="Lag (a)", ylab="ACF", las=1, cex.main=0.95)
# ƫ�����ͼ
pacf(return, main='(b) the PACF of Return', xlab="Lag (b)", ylab="PACF", las=1, cex.main=0.95)
# ���������л����������������

# ����������ƽ�����������
par(mfrow=c(2,1))
return.square <- return^2

acf(return.square, main='(a) the ACF of Return Square', xlab="Lag (c)", ylab="ACF", las=1, cex.main=0.95)
pacf(return.square, main='(b) the PACF of Return Square', xlab="Lag (d)", ylab="PACF", las=1, cex.main=0.95)


# 5. ARCHЧӦ����
library(zoo)
library(FinTS)
install.packages("FinTS")

ArchTest(return, lag=12)


# 6. GARCHģ�͵Ĺ���
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


## 7. GARCHģ�͵ı�׼���в����
resi <- residuals(m1, standardize=T)    # ��ñ�׼���в�
par(mfcol=c(1,1))
plot(resi, xlab='Date', ylab='st.resi', type='l')
par(mfcol=c(2,2))
acf(resi, lag=24)
acf(resi^2, lag=24)
pacf(resi, lag=24)
pacf(resi^2, lag=24)

# �в�ƽ�����ͺ�10������ؼ���
Box.test(resi^2, lag=10, type='Ljung')
# �в�ƽ�����ͺ�15������ؼ���
Box.test(resi^2, lag=15, type='Ljung')
# �в�ƽ�����ͺ�20������ؼ���
Box.test(resi^2, lag=20, type='Ljung')
# �ɽ���ԭ���裬��׼���в�ƽ�����������������