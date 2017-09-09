### VAR模型分析联合内生变量的动态关系

setwd("D:\\Project\\R\\VAR_Model")

## 0.数据准备
data <- read.csv("monthdata.csv")  # 读取数据
CPI <- data[,1]
NEER <- data[,2]
length(CPI)
length(NEER)
 
## 1.绘制时序图
# 将数据转化为时间序列
CPI.ts <- ts(CPI, start=c(2005,8), end=c(2016,12), freq=12)
NEER.ts <- ts(NEER, start=c(2005,8), end=c(2016,12), freq=12)

par(mfrow=c(2,1))
plot(CPI.ts, type='l', xlab='Date', ylab='CPI')
plot(NEER.ts, type='l', xlab='Date', ylab='NEER')

## 2.平稳性检验
logCPI <- log(CPI)
logNEER <- log(NEER)

library(urca)
urt.logCPI <- ur.df(logCPI, type='trend', selectlags='AIC')
urt.logNEER <- ur.df(logNEER, type='trend', selectlags='AIC')

summary(urt.logCPI)
summary(urt.logNEER)

# logCPI的检验统计量的值 -2.0578 在 1%、5%、10% 的显著性水平下都大于临界值(-3.99 -3.43 -3.13)
# 则不能拒绝原价设而接受存在单位根的假设
# 说明 logCPI 序列是不平稳的。
#（注意，统计量的值和临界值为负，统计量的值大于临界值是接受原假设；
# 若统计量的值和临界值为正值，统计量的值大于临界值是拒绝原假设。）
# 同理，logNEER 的检验统计量的值 -3.1902 在 1% 、5% 的显著性水平下都大于对应的临界值(-3.99 -3.43 -3.13)，
# 不能拒绝原假设，lnneer 序列也是不平稳的，存在单位根。

## 一阶差分后平稳性检验
dlogCPI <- diff(logCPI)
dlogNEER <- diff(logNEER)
urt.dlogCPI <- ur.df(dlogCPI, type='trend', selectlags='AIC')
urt.dlogNEER <- ur.df(dlogNEER, type='trend', selectlags='AIC')
summary(urt.dlogCPI)
summary(urt.dlogNEER)

## 一阶差分后的时间序列图
par(mfrow=c(2,1))
plot(dlogCPI, type='l', xlab='Date', ylab='diff.CPI')
plot(dlogNEER, type='l', xlab='Date', ylab='diff.CPI')

## 3.协整检验
# E-G两步法:
# 第1步:回归方程的估计
fit <- lm(logCPI~logNEER)
fit
summary(fit)
# 协整回归方程为: logCPI = 4.80296 - 0.03666*logNEER + εt

library(zoo)
library(lmtest)
## 检验序列的自相关性
dwtest(fit)
# 由于 Durbin-Watson test 检验的 p-value 的值几乎为 0，该数值小于 0.05 ，
# 说明在 5% 的显著性水平上残差序列不独立，具有自相关性。

## 检验残差序列的平稳性
error <- residuals(fit)    # 提取残差序列
urt.residuals <- ur.df(error, type='none', selectlags='AIC')
summary(urt.residuals)

# E-G两步法:
# 第2步:误差修正模型的建立
error.lag <- error[-c(137,138)]
ecm.fit <- lm(dlogCPI~error.lag + dlogNEER)    # 拟合误差修正模型
summary(ecm.fit)
# 协整回归方程:
# dlogCPI(t) = -2.299e-5 - 8.414e-2*ecm(t-1) + 3.008e-2*dlogNEER(t) + ε(t)
dwtest(ecm.fit)


## 4.滞后阶数的确定
library(MASS)
library(sandwich)
library(strucchange)
library(vars)

data.new <- data.frame(logCPI,logNEER)   # 合并数据
VARselect(data.new, lag.max=10, type="const")   # 在10以内选择最优滞后阶数


## 5.VAR模型的拟合和预测
# VAR模型的拟合
var <- VAR(data.new, lag.max=2, ic="AIC")
summary(var)
 
coef(var)
plot(var)

# 脉冲相应分析
var.irf <- irf(var)
plot(var.irf)

# VAR(2)模型的预测
var.predict <- predict(var, n.ahead=10, ci=0.95)
var.predict
