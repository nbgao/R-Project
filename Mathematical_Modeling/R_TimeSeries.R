library(xlsx)

input_file1 <- "东海3类的平均值.csv"
input_file1_m <- "../图表/第2问/东海_3类_月平均.xlsx"
Data <- read.csv(input_file1, header=T)

data1 <- Data$C1
data2 <- Data$C2
data3 <- Data$C3

data1_ts <- ts(data1, frequency = 365, start = c(2002,4,1))
plot.ts(data1_ts, col='blue', xlab="", ylab="海表温度(℃)", xaxt="n", yaxt="n") 
axis(1, at=c((2002-90/365):(2011-90/365)), labels = c(2002:2011))
axis(2, at=c(18:32), labels = c(18:32))

## Holt-Winters指数平滑法(季节周期性)
# 取对数减少极差带来的影响，消除方差不齐
# data1_ts_log <- log(data1_ts)
# plot.ts(data1_ts_log, col='blue', xlab="", ylab="", xaxt="n")
# axis(1, at=c(2002:2011), labels = c(2002:2011))

# Holt-Winters指数平滑曲线
data1_ts_HW <- HoltWinters(data1_ts)
plot(data1_ts_HW, col='blue', xlab="", ylab="海表温度(℃)", xaxt="n", yaxt="n", main="HoltWinters指数平滑曲线")
axis(1, at=c((2002-90/365):(2011-90/365)), labels = c(2002:2011))
axis(2, at=c(18:32), labels = c(18:32))

# Holt-Winters预测
data1_ts_forecast <- forecast(data1_ts_HW, h=365*3)
plot(data1_ts_forecast, col='blue', xlab="", ylab="海表温度(℃)", xaxt="n")
axis(1, at=c((2002-90/365):(2014-90/365)), labels = c(2002:2014))
# 深灰色部分为80%的置信区间，浅灰色为95的置信区间

# 预测误差ACF检验和Ljung-Box检验
acf(data1_ts_forecast$residuals[!is.na(data1_ts_forecast$residuals)], lag.max=20, plot=T)
Box.test(data1_ts_forecast$residuals, lag=20, type="Ljung-Box")



Data1 <- read.xlsx(input_file1_1, sheetIndex = 1, header = F)
Data1_1 <- Data1[1]
Data1_2 <- Data1[2]
Data1_3 <- Data1[3]

Data1_1_ts <- ts(Data1_1, frequency = 12, start = c(2002,4))
plot.ts(Data1_1_ts, col='blue', xlab="", ylab="海表温度(℃)", xaxt="n", yaxt="n") 
axis(1, at=c(2002:2011), labels = c(2002:2011))
axis(2, at=c(18:32), labels = c(18:32))

# Holt-Winters指数平滑曲线
Data1_1_ts_HW <- HoltWinters(Data1_1_ts)
plot(Data1_1_ts_HW, col='blue', xlab="", ylab="海表温度(℃)", xaxt="n", yaxt="n", main="HoltWinters指数平滑曲线")
axis(1, at=c(2002:2011), labels = c(2002:2011))
axis(2, at=c(18:32), labels = c(18:32))

# Holt-Winters预测
Data1_1_ts_forecast <- forecast(Data1_1_ts_HW, h=12*3)
plot(Data1_1_ts_forecast, col='blue', xlab="", ylab="海表温度(℃)", xaxt="n")
axis(1, at=c(2002:2014), labels = c(2002:2014))
# 深灰色部分为80%的置信区间，浅灰色为95的置信区间

# 预测误差ACF检验和Ljung-Box检验
acf(Data1_1_ts_forecast$residuals[!is.na(Data1_1_ts_forecast$residuals)], lag.max=20, plot=T)
Box.test(Data1_1_ts_forecast$residuals, lag=20, type="Ljung-Box")
