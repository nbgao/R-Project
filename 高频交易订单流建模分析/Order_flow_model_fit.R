## 订单流模型拟合

library(tidyverse)
library(lubridate)

dat <- read.csv(url("http://labfile.oss.aliyuncs.com/courses/883/pigu.csv"))

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


# 1.指数核Hawkes过程拟合
market_order <- trade[trade$action_type=="T",]
head(market_order)

m <- market_order %>% group_by(time) %>% summarise(sum(vol))
time <- m$time - m$time[1]
vol <- m$`sum(vol)`

log_likelihood <- function(params, event){
  mu = params[1]
  alpha = params[2]
  beta = params[3]
  
  n = length(event)
  t_n = event[n]
  
  kernel_sum <- numeric(n)
  
  for(i in 2:n){
    kernel_sum[i] = (kernel_sum[i-1]+alpha)*exp(-beta*(event[i]-event[i-1]))
  }
  lambda <- kernel_sum + mu
  
  L = sum(log(lambda)) - mu*t_n - alpha*n/beta + alpha/beta*sum(exp(-beta*(t_n-event)))
  
  -L
}

nlminb(c(1,1,1), log_likelihood, event=time)
# 拟合得到的 beta 的值非常大，说明前面事件对后面事件的影响衰减得非常快。
# 注意 1/beta 被定义为Hawkes过程的记忆时间,超过这个时间的后续事件基本不受这个事件的影响。
# 这里我们可以看到影响周期小于一毫秒，说明频率确实非常快。


## 2.正反馈强度分析
time_cut <- cut(time, breaks=seq(-0.01, time[length(time)]+0.01, length.out=14))
reflexity <- as.data.frame(cbind(time, time_cut))
reflexity_res <- list()
for (i in 1:13){
  ref_time <- time[reflexity$time_cut==i]
  ref_time <- ref_time - ref_time[1]
  ref_res <- nlminb(c(1,1,1), log_likelihood, event=ref_time)
  reflexity_res[[i]] <- ref_res
}

mu_vec <- c()
alpha_vec <- c()
beta_vec <- c()
for(i in 1:13){
  mu_vec <- c(mu_vec, reflexity_res[[i]]$par[1])
  alpha_vec <- c(alpha_vec, reflexity_res[[i]]$par[2])
  beta_vec <- c(beta_vec, reflexity_res[[i]]$par[3])
}

time_vec <- seq(market_order$time[1]-0.01, market_order$time[nrow(market_order)]+0.01, length.out=14)
time_vec <- as.POSIXct(time_vec, origin="1970-01-01", tz="America/Chicago")

par(mfrow=c(2,1))
plot(time_vec[1:(length(time_vec)-1)], mu_vec, col="blue", type="b", xlab="time", ylab="mu", main="back groud intensity in different periods")
plot(time_vec[1:(length(time_vec)-1)], col="red", type="b", alpha_vec/beta_vec, xlab="time", ylab="alpha/beta", main="reflexity in different periods")
# 我们把背景强度和reflexity分别画出来,可以看到背景强度形成了一个碗状,说明开盘和收盘时的事件的背景强度较大。
# 而reflexity则在各个时间段基本一致,基本围绕 0.4 波动。


## 3. 考虑订单数量
# 整个时间段的订单强度
plot(time, vol, type="l", ylab="volume", xlab="time", main="market order intensity")

# 建模
log_likelihood_volume <- function(params, event, vol){
  mu = params[1]
  alpha = params[2]
  beta = params[3]
  
  n = length(event)
  t_n = event[n]
  
  kernel_sum <- numeric(n)
  
  for(i in 2:n){
    kernel_sum[i] = (kernel_sum[i-1]+alpha*vol[i-1])*exp(-beta*(event[i]-event[i-1]))
  }
  
  lambda <- kernel_sum + mu
  L = sum(log(lambda)) - mu*t_n - alpha/beta*sum(vol) + alpha/beta*sum(vol*exp(-beta*(t_n-event)))
  -L
}

nlminb(c(1,1,1), log_likelihood_volume, event=time, vol=vol)


## 4. 订单数量的幂指数
log_likelihood_volume_exponent <- function(params, event, vol){
  mu = params[1]
  alpha = params[2]
  beta = params[3]
  k = params[4]
  
  n = length(event)
  t_n = event[n]
  
  kernel_sum <- numeric(n)
  
  vol <- vol^k
  
  for(i in 2:n){
    kernel_sum[i] = (kernel_sum[i-1]+alpha*vol[i-1])*exp(-beta*(event[i]-event[i-1]))
  }
  
  lambda <- kernel_sum + mu
  L = sum(log(lambda)) - mu*t_n - alpha/beta*sum(vol) + alpha/beta*sum(vol*exp(-beta*(t_n-event)))
  -L
}

nlminb(c(1,1,1,1), log_likelihood_volume_exponent, event=time, vol=vol)
# 从参数我们可以看到k的值大约是0.5,说明订单的影响大约是数量的根号,并不是线性增长的。
# 这是非常有趣的一个现象,说明订单量的冲击是边际递减的。
