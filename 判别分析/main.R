#　source('discriminiant.distance.R')
# 默认情况样本协方差不同
discriminiant.distance(trainX1, trainX2)    

# 协方差相同
discriminiant.distance(trainX1, trainX2, var.equal = T)    

# 调用测试集进行预测
discriminiant.distance(trainX1, trainX2, testX)
