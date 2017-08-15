library(Matrix)
library(lme4)
library(pbkrtest)
library(car)

# 导入数据
data <- read.csv("yeardata.csv")
head(data)


### 拟合多元线性回归模型
# 查看变量间的相关性
cor(data)
cor(data[c("GDP", "Consumption", "M0")])
# 散点矩阵图
scatterplotMatrix(data, spread = FALSE, main = "Scatter Plot Matrix via car package")
# spread=FALSE 表示删除了残差正负均方根在平滑曲线上的展开和非对称信息

# 拟合模型
GDP <- data[,1]
Consumption <- data[,2]
M0 <- data[,3]
EM <- data[,4]
Investment <- data[,5]
fit <- lm(GDP~Consumption + M0 + EM + Investment)
fit

fit <- lm(GDP~Consumption + M0 + EM + Investment, data = data)
fit
# 得到的多元回归模型
sprintf('多元回归方程为:')
sprintf('GDP = %f + %f*Consumption + %f*M0 + %f*EM + %f*Investment', fit$coefficients[1], fit$coefficients[2], fit$coefficients[3], fit$coefficients[4], fit$coefficients[5])

# 拟合结果报告
summary(fit)
# Residuals   Min   1Q    Mean    3Q    Max
# Coefficients 估计值Estimate   标准差Std.Error   t值t-value    P值Pr(>|t|)
# 拟合优度R^2(可决系数、判定系数)
# 模型联合显著性F统计量 F-statistic   自由度DF    p-value



### 多元线性回归模型的诊断
par(mfrow=c(2,2)) # 将图形以2*2的形式列在一个图形窗口
plot(fit)
# Residuals vs Fitted 图（左上）主要用于检验假定 1：线性于参数，图中显示结果残差值和拟合值基本没有比较明显的关联，则说明自变量与因变量之间是线性关系，满足假定 1。
# Normal Q-Q 图（右上）主要检验误差项的正态性，若满足正态性的假设，则图上的圆点应落在 45 度角的直线上；反之，则不满足正态性的假设。从图中结果来看，基本满足正态性的假设。
# Scale-Location图（左下）主要检验误差的同方差性的假设，若水平线周围的点随机分布，则满足同方差假设；反之，则不满足。而观察此处结果，满足同方差性的假设。
# Residuals vs Leverage 图（右下）主要用于观察数据中的单个值，不用于假设检验。


### 多元线性回归模型的最优选择
# 赤池信息准则AIC
fit1 <- lm(GDP~Consumption + M0 + EM + Investment, data = data)
fit2 <- lm(GDP~Consumption + M0 + EM, data = data)
fit3 <- lm(GDP~Consumption + M0, data)
fit4 <- lm(GDP~Consumption, data = data)
AIC(fit1, fit2, fit3, fit4)   # 赤池信息准则
# AIC准则结果表明fit2 AIC值最小，拟合结果最好即选择 Consumption、M0、EM 三个变量为自变量的多元线性回归模型为最优模型


### 模型参数解释
summary(fit2)
# 回归方程的截距项是 -761.13
# 消费（Consumption）每变动一个单位引起以 GDP 为代表的经济增长 1.67 个单位
# 货币（M0）每增加一个单位的供应会引起 GDP 增加 1.38 
# 同理进出口总额（EM）每增加一个单位会引起 GDP 增加 0.43
# 也就是说，为了促进我国经济的快速发展，可以采取刺激消费来促进经济增长
# 也可以适当的增加货币供应量但不能过多投放，因为会引起通货膨胀
# 还可以扩大对外开放程度来增加进出口从而促进经济增长

### 模型预测
new <- data.frame(Consumption=330000, M0=65000, EM=250000)   # 导入自变量
lm.pred <- predict(fit2, new, interval="prediction", level=0.95)   # 进行预测
lm.pred
