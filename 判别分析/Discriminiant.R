setwd("D:\\Project\\R\\判别分析")

## 1. 数据准备
bankruptcy <- read.csv("bankruptcy.csv", header = T, sep = ",")
trainX1 <- data.frame(x1=bankruptcy[1:17,1], x2=bankruptcy[1:17,2], x3=bankruptcy[1:17,3], x4=bankruptcy[1:17,4])
trainX2 <- data.frame(x1=bankruptcy[18:38,1], x2=bankruptcy[18:38,2], x3=bankruptcy[18:38,3], x4=bankruptcy[18:38,4])
testX <- data.frame(x1=bankruptcy[39:46,1], x2=bankruptcy[39:46,2], x3=bankruptcy[39:46,3], x4=bankruptcy[39:46,4])

## 2. 距离判别
# 构造函数
discriminiant.distance <- function(TrnX1, TrnX2, TstX=NULL, var.equal=FALSE){
  if(is.null(TstX) == TRUE)    # 判断数据结构，并进行数据转换
    TstX <- rbind(TrnX1, TrnX2)
  if(is.vector(TstX) == TRUE)
    TstX <- t(as.matrix(TstX))
  else if(is.matrix(TstX) != TRUE)
    TstX <- as.matrix(TrnX1)
  if(is.matrix(TrnX1) != TRUE)
    TrnX1 <- as.matrix(TrnX1)
  if(is.matrix(TrnX2) != TRUE)
    TrnX2 <- as.matrix(TrnX2)
  
  nx <- nrow(TstX)
  blong <- matrix(rep(0,nx), nrow=1, byrow=TRUE, dimnames=list("blong", 1:nx))
  mu1 <- colMeans(TrnX1)    # 求取梅列均值，即样本中心
  mu2 <- colMeans(TrnX2)
  
  # 判断两个样本协方差阵是否相同
  if(var.equal==TRUE || var.equal==T){
    S <- var(rbind(TrnX1, TrnX2))
    w <- mahalanobis(TstX, mu2, S) - mahalanobis(TstX, mu1, S)    # 调用mahalanobis距离判别公式
  }else{
    S1 <- var(TrnX1)
    S2 <- var(TrnX2)
    w <- mahalanobis(TstX, mu2, S2) - mahalanobis(TstX, mu1, S1)
  }
  
  for(i in 1:nx){
    if(w[i]>0)
      blong[i] <- 1
    else
      blong[i] <- 2
  }
  blong
}


## 3. Bayes判别
# 构造函数
discriminiant.bayes <- function(TrnX1, TrnX2, rate=1, TstX=NULL, var.equal=FALSE){
  if(is.null(TstX) == TRUE)    # 判断数据结构，并进行数据转换
    TstX <- rbind(TrnX1, TrnX2)
  if(is.vector(TstX) == TRUE)
    TstX <- t(as.matrix(TstX))
  else if(is.matrix(TstX) != TRUE)
    TstX <- as.matrix(TstX)
  if(is.matrix(TrnX1) != TRUE)
    TrnX1 <- as.matrix(TrnX1)
  if(is.matrix(TrnX2) != TRUE)
    TrnX2 <- as.matrix(TrnX2)
  
  nx <- nrow(TstX)
  blong <- matrix(rep(0, nx), nrow=1, byrow=TRUE, dimnames=list("blong", 1:nx))
  mu1 <- colMeans(TrnX1)
  mu2 <- colMeans(TrnX2)
  
  # 判断两个样本方差是否相同
  if(var.equal==TRUE || var.equal==T){
    S <- var(rbind(TrnX1, TrnX2))
    beta <- 2*log(rate)    # 计算beta值
    w <- mahalanobis(TstX, mu2, S) - mahalanobis(TstX, mu1, S)
  }else{
    S1 <- var(TrnX1)
    S2 <- var(TrnX2)
    beta <- 2*log(rate) + log(det(S1)/det(S2))
    w <- mahalanobis(TstX, mu2, S2) - mahalanobis(TstX, mu1, S1)
  }
  # 循环输出样本判断结果的类型
  for(i in 1:nx){
    if(w[i]>beta)
      blong[i] <- 1
    else
      blong[i] <- 2
  }
  blong
}


## 4. Fisher判别
# 构造函数
discriminiant.fisher <- function(TrnX1, TrnX2, TstX=NULL){
  if(is.null(TstX) == TRUE)    # 判断数据结构，并进行数据转换
    TstX <- rbind(TrnX1, TrnX2)
  if(is.vector(TstX) == TRUE)
    TstX <- t(as.matrix(TstX))
  else if(is.matrix(TstX) != TRUE)
    TstX <- as.matrix(TstX)
  if(is.matrix(TrnX1) != TRUE)
    TrnX1 <- as.matrix(TrnX1)
  if(is.matrix(TrnX2) != TRUE)
    TrnX2 <- as.matrix(TrnX2)
  
  nx <- nrow(TstX)
  blong <- matrix(rep(0,nx), nrow=1, byrow=TRUE, dimnames=list("blong", 1:nx))
  n1 <- nrow(TrnX1)
  n2 <- nrow(TrnX2)
  mu1 <- colMeans(TrnX1)
  mu2 <- colMeans(TrnX2)
  S <- (n1-1)*var(TrnX1) + (n2-1)*var(TrnX2)
  mu <- n1/(n1+n2)*mu1 + n2/(n1+n2)*mu2
  # 构造判别函数， %o% 外积， %*% 矩阵乘法
  w <- (TstX - rep(1,nx) %o% mu) %*% solve(S,mu2-mu1)
  
  for(i in 1:nx){
    if(w[i] <= 0)
      blong[i] <- 1
    else
      blong[i] <- 2
  }
  blong
}



## 5. 判对率
library(MASS)
train <- bankruptcy[1:38,]
head(train)

# 5.1 线性判对率
distance.lda = lda(类别~., data=train)
table <- table(train$类别, predict(distance.lda, train)$class)
table
sum(diag(prop.table(table)))

# 5.2 Bayes判对率
bayes.lda = lda(类别~., data=train, prior=c(21,17,8)/46)
table <- table(train$类别, predict(bayes.lda, train)$class)
table
sum(diag(prop.table(table)))

## 6. 用LDA预测股票涨跌
install.packages("ISLR")
library(ISLR)
library(MASS)
attach(Smarket)

lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit

Smarket.2005 = subset(Smarket, Year==2005)
lda.pred = predict(lda.fit, Smarket.2005)$class
data.frame(lda.pred)[1:5,]
table <- table(lda.pred, Smarket.2005$Direction)
# 计算判对率
sum(diag(prop.table(table)))
