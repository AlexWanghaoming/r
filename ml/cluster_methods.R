##  玉米分类数据集（3类效果最佳）
aa <- read.csv("/Users/alexwang/Desktop/maize_cluster.csv", header = F)
a <- scale(aa) # 标准化

colnames(a) <- c("root_length","shoot_DW","grain_N")
rownames(a) <- c("BMY","JHH","ZD2","TK5","ND108","ZD958")
dd2 <- dist(a) # 计算距离矩阵

# 分别用三种方法层次聚类
hc_ward1 <- hclust(dd2,method = "ward.D")
hc_average1 <- hclust(dd2,method = "average")
hc_centroid1 <- hclust(dd2, method = "centroid")
p1 <- plot(hc_ward1,hang = 1)
p2 <- plot(hc_average1, hang = 1)
p3 <- plot(hc_centroid1, hang=1)

## kmeans聚类
library(fpc)
kk <- kmeans(a,centers = 3)
plotcluster(scale(bb), kk1$cluster) # kmeans绘图
# kmeans聚类画出轮廓系数图评估K的最佳取值

K <- 2:4 # 分别计算 K取2，3，4哪个最优
round <- 30  # 30次防治局部最优
rst <- sapply(K, function(i){  
  print(paste("K=",i))
  mean(sapply(1:round,function(r){
    print(paste("Round",r))
    result <- kmeans(a, i)
    stats <- cluster.stats(dist(a), result$cluster)
    stats$avg.silwidth
  }))
})
plot(K, rst, type='l', main='outline & R relation', ylab='outline coefficient') # 轮廓系数绘图

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
