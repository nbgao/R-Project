### VARģ�ͷ����������������Ķ�̬��ϵ

setwd("D:\\Project\\R\\VAR_Model")

## 0.����׼��
data <- read.csv("monthdata.csv")  # ��ȡ����
CPI <- data[,1]
NEER <- data[,2]
length(CPI)
length(NEER)
 
## 1.����ʱ��ͼ
# ������ת��Ϊʱ������
CPI.ts <- ts(CPI, start=c(2005,8), end=c(2016,12), freq=12)
NEER.ts <- ts(NEER, start=c(2005,8), end=c(2016,12), freq=12)

par(mfrow=c(2,1))
plot(CPI.ts, type='l', xlab='Date', ylab='CPI')
plot(NEER.ts, type='l', xlab='Date', ylab='NEER')

## 2.ƽ���Լ���
logCPI <- log(CPI)
logNEER <- log(NEER)

library(urca)
urt.logCPI <- ur.df(logCPI, type='trend', selectlags='AIC')
urt.logNEER <- ur.df(logNEER, type='trend', selectlags='AIC')

summary(urt.logCPI)
summary(urt.logNEER)

# logCPI�ļ���ͳ������ֵ -2.0578 �� 1%��5%��10% ��������ˮƽ�¶������ٽ�ֵ(-3.99 -3.43 -3.13)
# ���ܾܾ�ԭ��������ܴ��ڵ�λ���ļ���
# ˵�� logCPI �����ǲ�ƽ�ȵġ�
#��ע�⣬ͳ������ֵ���ٽ�ֵΪ����ͳ������ֵ�����ٽ�ֵ�ǽ���ԭ���裻
# ��ͳ������ֵ���ٽ�ֵΪ��ֵ��ͳ������ֵ�����ٽ�ֵ�Ǿܾ�ԭ���衣��
# ͬ����logNEER �ļ���ͳ������ֵ -3.1902 �� 1% ��5% ��������ˮƽ�¶����ڶ�Ӧ���ٽ�ֵ(-3.99 -3.43 -3.13)��
# ���ܾܾ�ԭ���裬lnneer ����Ҳ�ǲ�ƽ�ȵģ����ڵ�λ����

## һ�ײ�ֺ�ƽ���Լ���
dlogCPI <- diff(logCPI)
dlogNEER <- diff(logNEER)
urt.dlogCPI <- ur.df(dlogCPI, type='trend', selectlags='AIC')
urt.dlogNEER <- ur.df(dlogNEER, type='trend', selectlags='AIC')
summary(urt.dlogCPI)
summary(urt.dlogNEER)

## һ�ײ�ֺ��ʱ������ͼ
par(mfrow=c(2,1))
plot(dlogCPI, type='l', xlab='Date', ylab='diff.CPI')
plot(dlogNEER, type='l', xlab='Date', ylab='diff.CPI')

## 3.Э������
# E-G������:
# ��1��:�ع鷽�̵Ĺ���
fit <- lm(logCPI~logNEER)
fit
summary(fit)
# Э���ع鷽��Ϊ: logCPI = 4.80296 - 0.03666*logNEER + ��t

library(zoo)
library(lmtest)
## �������е��������
dwtest(fit)
# ���� Durbin-Watson test ����� p-value ��ֵ����Ϊ 0������ֵС�� 0.05 ��
# ˵���� 5% ��������ˮƽ�ϲв����в�����������������ԡ�

## ����в����е�ƽ����
error <- residuals(fit)    # ��ȡ�в�����
urt.residuals <- ur.df(error, type='none', selectlags='AIC')
summary(urt.residuals)

# E-G������:
# ��2��:�������ģ�͵Ľ���
error.lag <- error[-c(137,138)]
ecm.fit <- lm(dlogCPI~error.lag + dlogNEER)    # ����������ģ��
summary(ecm.fit)
# Э���ع鷽��:
# dlogCPI(t) = -2.299e-5 - 8.414e-2*ecm(t-1) + 3.008e-2*dlogNEER(t) + ��(t)
dwtest(ecm.fit)


## 4.�ͺ������ȷ��
library(MASS)
library(sandwich)
library(strucchange)
library(vars)

data.new <- data.frame(logCPI,logNEER)   # �ϲ�����
VARselect(data.new, lag.max=10, type="const")   # ��10����ѡ�������ͺ����


## 5.VARģ�͵���Ϻ�Ԥ��
# VARģ�͵����
var <- VAR(data.new, lag.max=2, ic="AIC")
summary(var)
 
coef(var)
plot(var)

# ������Ӧ����
var.irf <- irf(var)
plot(var.irf)

# VAR(2)ģ�͵�Ԥ��
var.predict <- predict(var, n.ahead=10, ci=0.95)
var.predict