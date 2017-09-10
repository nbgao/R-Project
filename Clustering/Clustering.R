
## 1. ���ݻ�ȡ
setwd("D:\\Project\\R\\Clustering")

coms <- read.csv("comsume.csv", header=T, sep=',')
coms <- as.data.frame(coms[-1], row.names=c("������","�����","�ӱ�ʡ","ɽ��ʡ","���ɹ�������","����ʡ","����ʡ","������ʡ","�Ϻ���","����ʡ","�㽭ʡ","����ʡ","����ʡ","����ʡ","ɽ��ʡ","����ʡ","����ʡ","����ʡ","�㶫ʡ","����׳��������","����ʡ","������","�Ĵ�ʡ","����ʡ","����ʡ","����������","����ʡ","����ʡ","�ຣʡ","���Ļ���������","�½�ά���������"))


## 2. ��������
# 2.1 ����׼���仯
coms.scale <- scale(coms)

# 2.2 ���ɾ���ṹ
d <- dist(coms.scale, method="euclidean")


## 3. ϵͳ���෨
# 3.1 ��̾��뷨(single)
data.single <- hclust(d, method="single")
plot(data.single, hang=-1, cex=.8, main="Single Linkage Clustering")

# 3.2 ����뷨(complete)
data.complete <- hclust(d, method="complete")
plot(data.complete, hang=-1, cex=.8, main="Complete Linkage Clustering")

# 3.3 ��ƽ����(average)
data.average <- hclust(d, method="average")
plot(data.average, hang=-1, cex=.8, main="Average Linkage Clustering")

# 3.4 ���ķ�(centroid)
data.centroid <- hclust(d, method="centroid")
plot(data.centroid, hang=-1, cex=.8, main="Centroid Linkage Clustering")

# 3.5 ���ƽ���ͷ�(ward��)
data.ward <- hclust(d, method="ward")
plot(data.ward, hang=-1, cex=.8, main="Ward Linkage Clustering")

# 3.6 ѡ��������
library(NbClust)
devAskNewPage(ask=TRUE)
nc <- NbClust(coms.scale, distance="euclidean", min.nc=2, max.nc=15, method="average")

table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clustering", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")

# 3.7 ��ȡ���վ��෽��
# �鿴����
clusters <- cutree(data.average, k=2)
table(clusters)

# ��������
aggregate(as.data.frame(coms.scale), by=list(cluster=clusters), median)

# ���»�ͼ
plot(data.average, hang=-1, cex=.8, main="Average Linkage Clustering\n Cluster Solution")
rect.hclust(data.average, k=2)


## 4. ���־��෨
# 4.1 K-Means��ֵ��
km <- kmeans(coms.scale, 5, nstart=20)
km

sort(km$cluster)

plot(coms.scale, col=km$cluster)
points(km$centers, col=1:5, pch=8, cex=2)

# 4.2 K-medoids����
library(cluster)
coms.result <- pam(coms, 5, stand=TRUE)
coms.result
summary(coms.result)

plot(coms.result$data, col=coms.result$clustering)