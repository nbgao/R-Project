## 订单数据流描述分析
## 订单流数据表示
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


## 订单间隔分析
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


