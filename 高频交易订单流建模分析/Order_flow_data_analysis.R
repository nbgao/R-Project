## 订单数据流描述分析

## 1.订单流数据表示
dat <- read.csv(url("http://labfile.oss.aliyuncs.com/courses/883/pigu.csv"))
summary(dat[,2:ncol(dat)])

head(dat)

format(head(dat)$time, digits=21)

as.POSIXct(head(dat)$time[1], origin="1970-01-01", tz="America/Chicago")


library(tidyverse)
library(lubridate)

bisect_lower_bound <- function(x){
  date <- as.POSIXct(x[1], origin="1970-01-01", tz="America/Chicago") + days(1)
  hour(date) <- 8
  minute(date) <- 30
  second(date) <- 0
  k <- as.numeric(date)
  
  l = -1
  r = length(x) + 1
  while(r-l > 1){
    mid = round((l+r)/2)
    if(x[mid] >= k)
        r = mid
    else
        l = mid
  }
  r
}

bisect_higher_bound <- function(x){
  date <- as.POSIXct(x[1], origin="1970-01-01", tz="America/Chicago") + days(1)
  hour(date) <- 15
  minute(date) <- 0
  second(date) <- 0
  k <- as.numeric(date)
  
  l = -1
  r = length(x) + 1
  while(r-l > 1){
    mid = round((l+r)/2)
    if(x[mid] <= k)
      l = mid
    else
      r = mid
  }
  l
}

start <- bisect_lower_bound(dat$time)
end <- bisect_higher_bound(dat$time)
trade <- dat[start:end,]


## 2.订单间隔分析
time <- trade$time
length(time)

time_diff <- diff(time)
time_diff <- time_diff[time_diff>0]
summary(time_diff)
# 75分位数和均值大约为0.02s，说明标准500期货的交易非常频繁，流动性非常好

# 订单间隔的分布图
hist(time_diff[time_diff<0.1], breaks=seq(0,0.2,0.001), xlim=c(0,0.1), probability=TRUE, col="blue", border="white", xlab="empirical")


library(MASS)

time_diff_p <- time_diff[time_diff<0.04]
exp_fit <- fitdistr(time_diff_p, densfun="exponential")
exp_ran <- rexp(length(time_diff_p), rate=exp_fit$estimate)
plot(density(exp_ran), ylim=c(0,700), main="inter-arrival time: empirical vs exponential", xlab="inter-arrival time")
lines(density(time_diff_p), col="red")
# 用指数分布去拟合小于 0.04 的订单间隔，发现实际数据衰减的速度远远高于对应最优参数模拟指数分布衰减的速度。
# 由于指数分布的衰减速度是非常快的，这更说明小间隔的比例有多么大，可以说这是实实在在的“高频”交易。


## 做市商的存在证据
action <-trade$action_type

pre <- action[1:(length(action)-1)]
aft <- action[2:length(action)]
market_maker <- intersect(which(pre=="T"), which(aft=="M"))
mm_diff <- time[(market_maker+1)] - time[market_maker]
mm_diff <- mm_diff[mm_diff>0]

plot(density(time_diff), col="red", xlim=c(0,0.01), ylim=c(0,5000), main="The evidence of market maker")
lines(density(mm_diff), col="green")
# 我们筛选出了这种特殊的间隔，即前面的事件是市价单，后面的事件是限价单,
# 我们把这种间隔的分布用绿色的线画出来，可以看到这种类型的跟单速度比普通的跟单速度快得多，
# 其峰值小于1毫秒，真是称得上是快如闪电。


## 3.信息率的平稳性
# 观察较高的分位数对应的时间数
quantile(c(table(cut(time, breaks=seq(min(time)-1, max(time)+1, by=1)))), c(.9,.99,.999,.9999,.99999))
quantile(c(table(cut(time, breaks=seq(min(time)-0.01, max(time)+0.01, by=0.01)))), c(.9,.99,.999,.9999,.99999))
# 我们选取的是 1s 和 0.01s ， 如果是平稳的那么 0.01s 对应的分位数应该是 1s 的百分之一左右。
# 但是实际上在越高的分位数上这个规律就越不成立 ，在 99.99 分位和 99.999 分位上甚至超过了十分之一。
# 这说明事件的到来有高度聚集的特征，不能简单地用泊松过程来刻画。


## 4.流动性研究
plot(time, trade$ask_price, col=adjustcolor("red", alpha=0.2), type='l', ylab="bid and ask", xlab="time")
lines(time, trade$bid_price, col=adjustcolor("blue", alpha=0.2))

# spread的分布
table(trade$ask_price - trade$bid_price)
# 可以看到绝大多数时候spread都是25(1个tick),说明标普 500 期货的流动性非常之好。


## 5.限价单相对价格分析
ask_pre <- trade$ask_price
ask_idx <- which(trade$action_item=="ask")
relative_ask <- (trade$price[ask_idx] - trade$ask_price[(ask_idx-1)])/25

bid_idx <- which(trade$action_item=="bid")
bid_idx <- bid_idx[2:length(bid_idx)]
relative_bid <- -(trade$price[bid_idx] - trade$bid_price[(bid_idx-1)])/25

plot(density(relative_ask), col="red", main="relative price for bid and ask", xlab="relative tick")
lines(density(relative_bid), col="blue")
# 从图中我们可以看到,买单和卖单的相对价格分布基本相同,大多限价单的相对价格都在0,1,2左右。

prop.table(table(relative_ask))

prop.table(table(relative_bid))
