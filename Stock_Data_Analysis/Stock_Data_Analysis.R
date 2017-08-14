### 导入股票数据
library(quantmod)
library(zoo)
library(xts)
library(TTR)

getSymbols("AAPL", from = "2017-01-01", to = Sys.Date(), src = "yahoo")
head(AAPL)
tail(AAPL)

library(tseries)
goog <- get.hist.quote(instrument = "GOOG", start = "2017-01-01", end = "2017-08-01", quote = "AdjClose")
head(goog)



### 绘制股票图
# 以chartSerires为例做K线图
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')

chartSeries(AAPL, name = "AAPLBARCHART", subset = "2017-01-01::2017-08-01", type = "bars")
chartSeries(AAPL, name = "AAPLLINECHART", subset = "2017-01-01::2017-08-01", type = "line")
chartSeries(AAPL, name = "AAPLCANDCHART", subset = "2017-01-01::2017-08-01", type = "candlesticks")

### 技术分析图
# 1.addBBands() 布林线指标
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
# 只画区间还有percent百分比width宽度
addBBands(n=14, sd=2, draw='bands')

# 2.addADX() 平均趋向指标
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addADX()
# ADX>=30    趋势强劲
# 20<=ADX<30 中性读数
# ADX<20     时常动能偏 

# 3.addMACD() 指数平滑异同移动平均线
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addMACD()

# 4.addCCI() 测量股价是否超出常态分布范围
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addCCI()

# 5.addRSI() 测量速度和变化的价格变动
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addRSI()

# 6.addVo() 测量成交量
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addVo()

# 7.addWPR() 表示市场处于超卖还是超买状态
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addWPR()

# 8.addATR() 测量价格的波动性指标
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addATR()

# 9.addSAR() 显示市场价格变化的趋势
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addSAR()

# 10.addDPO() 排除价格趋势的震荡指标
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addDPO()


# 技术图合在一张图上
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white', TA = c(addBBands(), addMACD(), addADX(), addVo()))



### 计算股票日度收益率
# 1.简单收益率
close <- AAPL[,4]
close1 <- lag(close,1)
head(close1)

calclose <- merge(close, close1)
simplerate <- (close-close1)/close1
names(simplerate) = "simplerate"
calrate = merge(calclose, simplerate)
head(calrate)

# 2.对数收益率
library(PerformanceAnalytics)
rate = periodReturn(close, period = "daily", type="log")
head(rate)



### 抓取多支股票
library(quantmod)
new.environment <- new.env()
getSymbols(c("AAPL","ORCL","MSFT","GOOG"), src = "yahoo", env = new.environment)

# 查看抓取的全景数据情况
str(get("AAPL", env = new.environment))
str(get("ORCL", env = new.environment))
str(get("MSFT", env = new.environment))
str(get("GOOG", env = new.environment))

# 股票总成交量
getSymbols("AAPL", src = "yahoo", from = "2017-01-01", to = "2017-07-14")
summary(AAPL)
sum(Vo(AAPL))



### 分析股票暴涨暴跌的时间点
# 查看各公司涨跌超过2%的情况
AAPL <- Delt(Cl(get("AAPL", env = new.environment)))
length(AAPL[which(AAPL > 0.02), ])
plot(AAPL[which(AAPL > 0.02), ])

ORCL <- Delt(Cl(get("ORCL", env = new.environment)))
length(ORCL[which(ORCL > 0.02), ])
plot(ORCL[which(ORCL > 0.02), ])

MSFT <- Delt(Cl(get("MSFT", env = new.environment)))
length(MSFT[which(MSFT > 0.02), ])
plot(MSFT[which(MSFT > 0.02), ])

GOOG <- Delt(Cl(get("GOOG", env = new.environment)))
length(GOOG[which(GOOG > 0.02), ])
plot(GOOG[which(GOOG > 0.02), ])



### 相关性判断
# 调整数据
periodicity(get("GOOG", env = new.environment))
getSymbols(c("AAPL", "ORCL", "MSFT", "GOOG"), src = "yahoo", env = new.environment, from = "2017-01-03", to = "2017-08-14")
# 将4家公司股票的每天调整价格整理在一个数据框中
m <- cbind(Ad(get("AAPL", env = new.environment)), Ad(get("ORCL", env = new.environment)), Ad(get("MSFT", env = new.environment)), Ad(get("GOOG", env = new.environment)))

library(psych)
corr.test(as.data.frame(m))

# 绘制相关性图
library(cor)