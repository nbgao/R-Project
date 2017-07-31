# import base library
library(quantmod)
library(zoo)
library(xts)
library(TTR)

# import stock data of "SINA"
getSymbols("SINA")
# getSymbols("SINA",src = "yahoo")
head(SINA)
tail(SINA)
dim(SINA)

# import stock data of "SINA" with time
getSymbols("SINA" , from = "2017-01-03" , to = "2017-07-31")
dim(SINA)
head(SINA)
tail(SINA)
summary(SINA)

# Stock Price Time Series Chart
chartSeries(SINA , from = "2017-01-03" , to = "2017-07-01")
chartSeries(SINA , them = "white" , from = "2017-01-03" , to = "2017-07-31")

# Stock Profit Rate Time Series Chart
SINA.Profit = diff(log(SINA$SINA.Adjusted))
dim(SINA.Profit)
chartSeries(SINA.Profit , theme = "white" )

# Stock Profit Kernal Density Chart
library(fBasics)
library(timeDate)
library(timeSeries)

SINA.Pro = na.omit(SINA.Profit)   # Drop NA Value

de = density(SINA.Pro)
range(SINA.Pro)
X = seq(-.17 , .17 , .001)

# Draw Density Chart
plot(de$x , de$y , xlab = 'X' , ylab = 'density' , type = 'l')

# Draw Normal distribution Curve
ys = dnorm(X , mean(SINA.Pro) , stdev(SINA.Pro))
lines(X , ys , lty = 2)

# Stock Stats
basicStats(SINA.Pro)

# Normal Test
normalTest(SINA.Pro , method = 'jb')  # Jarquebera(JB) Test
normalTest(SINA.Pro , method = 'sw')  # Shapiro-Wilk's(SW) Test
normalTest(SINA.Pro , method = 'ks')  # Kolmogorov-Smirnov(KS) Test
normalTest(SINA.Pro , method = 'da')  # D'Alostino(DA) Test
