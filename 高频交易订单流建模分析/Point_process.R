## posion process
x <- cumsum(rexp(50, rate = 0.5))
y <- rep(0,50)
plot(x,y,main = "event arrival")

y_cum <- cumsum(c(0,rep(1,50)))
plot(stepfun(x,y_cum),do.points = F,main = "cumsum of events")


## Hawkes Process
set.seed(728)
n <- 100
params <- c(0.5,0.3,1.2)
mu <- params[1]
alpha <- params[2]
beta <- params[3]

lambda_intensity <- function(event, t, params){
  mu <- params[1]
  alpha <- params[2]
  beta <- params[3]
  
  partial <- event[event < t]
  lambda_int <- mu + sum(alpha * exp(-beta*(t-partial)) )
  
  lambda_int
}

event_time <- numeric(n)

for (i in 1:n){
  if(i==1){
    lambda_star <- mu
    event_time[1] <- rexp(1,rate=lambda_star)
  }else{
    lambda_star <- lambda_intensity(event_time, event_time[i-1], params) + alpha
    event_time[i] <- event_time[i-1] + rexp(1,rate=lambda_star)
    while(runif(1) > lambda_intensity(event_time, event_time[i], params)/lambda_star){
      lambda_star <- lambda_intensity(event_time, event_time[i], params)
      event_time[i] <- event_time[i-1] + rexp(1,rate=lambda_star)
    }
  }
}

time <- seq(min(event_time), max(event_time), 0.001)
intensity <- unlist(lapply(time, function(x) lambda_intensity(event_time, x, params)))

plot(event_time, rep(0,n), col="blue", ylim=c(-1,5), main="simulated hawkes process", ylab="lambda intensity")
lines(time, intensity, type='l', col="red")


## Hawkes过程参数估计
## 指数核Hawkes过程模拟优化
set.seed(728)
n <- 100000
params <- c(0.5, 0.3, 1.2)
mu <- params[1]
alpha <- params[2]
beta <- params[3]
# recusively update intensity
event_time <- numeric(n)
last_kernel_sum <- numeric(n)

for (i in 1:n){
  if(i==1){
    lambda_star <- mu
    event_time[1] <- rexp(1,rate=lambda_star)
  }else{
    lambda_last <- mu + last_kernel_sum[i-1]
    lambda_star <- lambda_last + alpha
    
    event_time[i] <- event_time[i-1] + rexp(1,rate=lambda_star)
    last_kernel_sum[i] <- (last_kernel_sum[i-1] + alpha) * exp(-beta*(event_time[i] - event_time[i-1]))
    lambda_this <- mu + last_kernel_sum[i]
    
    while(runif(1) > lambda_this/lambda_star){
      # thinning procedure, delete this point and make new
      lambda_star <- lambda_this
      
      event_time[i] <- event_time[i] + rexp(1,rate=lambda_star)
      last_kernel_sum[i] <- (last_kernel_sum[i-1]+alpha)*exp(-beta*(event_time[i]-event_time[i-1]))
      lambda_this <- mu + last_kernel_sum[i]
    }
  }
}

## Hawkes过程参数估计
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

nlminb(c(1,1,1), log_likelihood, event=event_time)
