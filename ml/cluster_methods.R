
## iris dataset
# k-means cluster
iris2 <- iris[,1:4]
nrow(iris2)
iris.kmeans <- kmeans(iris2, 3)
iris.kmeans$cluster
# 查看每个类有多少元素
table(iris$Species, iris.kmeans$cluster)
# 查看聚类结果
plot(iris2$Sepal.Length,iris2$Sepal.Width,col=iris.kmeans$cluster,pch="*")
points(iris.kmeans$centers,pch="X",cex=1.5,col=4)

# Hierarchical Clustering
dim(iris)
idx<-sample(1:dim(iris)[1],40)  # 抽取40个元素
iris3<-iris[idx,-5]  # 去除第五列
# 1-cor(t(iris3))
hc<-hclust(dist(iris3)),method = "average")  #注意hcluster里边传入的是dist返回值对象
## 画聚类图
plot(hc,hang=-1,labels=iris$Species[idx])  #这里的hang=-1使得树的节点在下方对齐
#将树分为3块
rect.hclust(hc,k=3)  
groups<-cutree(hc,k=3) 
