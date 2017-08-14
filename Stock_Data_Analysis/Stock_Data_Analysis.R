### �����Ʊ����
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



### ���ƹ�Ʊͼ
# ��chartSeriresΪ����K��ͼ
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')

chartSeries(AAPL, name = "AAPLBARCHART", subset = "2017-01-01::2017-08-01", type = "bars")
chartSeries(AAPL, name = "AAPLLINECHART", subset = "2017-01-01::2017-08-01", type = "line")
chartSeries(AAPL, name = "AAPLCANDCHART", subset = "2017-01-01::2017-08-01", type = "candlesticks")

### ��������ͼ
# 1.addBBands() ������ָ��
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
# ֻ�����仹��percent�ٷֱ�width����
addBBands(n=14, sd=2, draw='bands')

# 2.addADX() ƽ������ָ��
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addADX()
# ADX>=30    ����ǿ��
# 20<=ADX<30 ���Զ���
# ADX<20     ʱ������ƫ 

# 3.addMACD() ָ��ƽ����ͬ�ƶ�ƽ����
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addMACD()

# 4.addCCI() �����ɼ��Ƿ񳬳���̬�ֲ���Χ
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addCCI()

# 5.addRSI() �����ٶȺͱ仯�ļ۸�䶯
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addRSI()

# 6.addVo() �����ɽ���
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addVo()

# 7.addWPR() ��ʾ�г����ڳ������ǳ���״̬
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addWPR()

# 8.addATR() �����۸�Ĳ�����ָ��
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addATR()

# 9.addSAR() ��ʾ�г��۸�仯������
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addSAR()

# 10.addDPO() �ų��۸����Ƶ���ָ��
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white')
addDPO()


# ����ͼ����һ��ͼ��
chartSeries(AAPL, up.col = 'red', dn.col = 'green', theme = 'white', TA = c(addBBands(), addMACD(), addADX(), addVo()))



### �����Ʊ�ն�������
# 1.��������
close <- AAPL[,4]
close1 <- lag(close,1)
head(close1)

calclose <- merge(close, close1)
simplerate <- (close-close1)/close1
names(simplerate) = "simplerate"
calrate = merge(calclose, simplerate)
head(calrate)

# 2.����������
library(PerformanceAnalytics)
rate = periodReturn(close, period = "daily", type="log")
head(rate)



### ץȡ��֧��Ʊ
library(quantmod)
new.environment <- new.env()
getSymbols(c("AAPL","ORCL","MSFT","GOOG"), src = "yahoo", env = new.environment)

# �鿴ץȡ��ȫ���������
str(get("AAPL", env = new.environment))
str(get("ORCL", env = new.environment))
str(get("MSFT", env = new.environment))
str(get("GOOG", env = new.environment))

# ��Ʊ�ܳɽ���
getSymbols("AAPL", src = "yahoo", from = "2017-01-01", to = "2017-07-14")
summary(AAPL)
sum(Vo(AAPL))



### ������Ʊ���Ǳ�����ʱ���
# �鿴����˾�ǵ�����2%�����
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



### ������ж�
# ��������
periodicity(get("GOOG", env = new.environment))
getSymbols(c("AAPL", "ORCL", "MSFT", "GOOG"), src = "yahoo", env = new.environment, from = "2017-01-03", to = "2017-08-14")
# ��4�ҹ�˾��Ʊ��ÿ������۸�������һ�����ݿ���
m <- cbind(Ad(get("AAPL", env = new.environment)), Ad(get("ORCL", env = new.environment)), Ad(get("MSFT", env = new.environment)), Ad(get("GOOG", env = new.environment)))

library(psych)
corr.test(as.data.frame(m))

# ���������ͼ
library(cor)