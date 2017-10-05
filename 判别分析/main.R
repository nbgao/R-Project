#　source('discriminiant.distance.R')
# 默认情况样本协方差不同
discriminiant.distance(trainX1, trainX2)    

# 协方差相同
discriminiant.distance(trainX1, trainX2, var.equal = T)    

# 调用测试集进行预测
discriminiant.distance(trainX1, trainX2, testX)


# source('discrimiant.bayes.R')
discriminiant.bayes(trainX1, trainX2, rate=21/17)
discriminiant.bayes(trainX1, trainX2, rate=21/17, var.equal=T)
discriminiant.bayes(trainX1, trainX2, rate=21/17, testX, var.equal = T)


# source('discriminiant.fisher.R')
discriminiant.fisher(trainX1, trainX2)
discriminiant.fisher(trainX1, trainX2, testX)
