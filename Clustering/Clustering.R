
## 1. 数据获取
setwd("D:\\Project\\R\\Clustering")

coms <- read.csv("comsume.csv", header=T, sep=',')
coms <- as.data.frame(coms[-1], row.names=c("北京市","天津市","河北省","山西省","内蒙古自治区","辽宁省","吉林省","黑龙江省","上海市","江苏省","浙江省","安徽省","福建省","江西省","山东省","河南省","湖北省","湖南省","广东省","广西壮族自治区","海南省","重庆市","四川省","贵州省","云南省","西藏自治区","陕西省","甘肃省","青海省","宁夏回族自治区","新疆维吾尔自治区"))


## 2. 数据整理
# 2.1 做标准化变化
coms.scale <- scale(coms)

# 2.2 生成距离结构
d <- dist(coms.scale, method="euclidean")


## 3. 系统聚类法
# 3.1 最短距离法(single)
data.single <- hclust(d, method="single")
plot(data.single, hang=-1, cex=.8, main="Single Linkage Clustering")

# 3.2 最长距离法(complete)
data.complete <- hclust(d, method="complete")
plot(data.complete, hang=-1, cex=.8, main="Complete Linkage Clustering")

# 3.3 类平均法(average)
data.average <- hclust(d, method="average")
plot(data.average, hang=-1, cex=.8, main="Average Linkage Clustering")

# 3.4 重心法(centroid)
data.centroid <- hclust(d, method="centroid")
plot(data.centroid, hang=-1, cex=.8, main="Centroid Linkage Clustering")

# 3.5 离差平方和法(ward法)
data.ward <- hclust(d, method="ward")
plot(data.ward, hang=-1, cex=.8, main="Ward Linkage Clustering")

# 3.6 选择聚类个数
library(NbClust)
devAskNewPage(ask=TRUE)
nc <- NbClust(coms.scale, distance="euclidean", min.nc=2, max.nc=15, method="average")

table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clustering", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")

# 3.7 获取最终聚类方法
# 查看分配
clusters <- cutree(data.average, k=2)
table(clusters)

# 描述聚类
aggregate(as.data.frame(coms.scale), by=list(cluster=clusters), median)

# 重新绘图
plot(data.average, hang=-1, cex=.8, main="Average Linkage Clustering\n Cluster Solution")
rect.hclust(data.average, k=2)


## 4. 划分聚类法
# 4.1 K-Means均值法
km <- kmeans(coms.scale, 5, nstart=20)
km

sort(km$cluster)

plot(coms.scale, col=km$cluster)
points(km$centers, col=1:5, pch=8, cex=2)

# 4.2 K-medoids聚类
library(cluster)
coms.result <- pam(coms, 5, stand=TRUE)
coms.result
summary(coms.result)

plot(coms.result$data, col=coms.result$clustering)
