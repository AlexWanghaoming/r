# common courses grades
common1 <- sample(x = 50:100, size = 10000, replace = T)
common2 <- sample(x = 50:100, size = 10000, replace = T)
common3 <- sample(x = 60:100, size = 10000, replace = T)
common4 <- sample(x = 60:100, size = 10000, replace = T)
# professional basic(pb) courses grades
pb <- (common3 + common4)/2 + 5; pb <- ifelse(pb>99, pb-5, pb)
# professional courses grades
pc <- (common3+common4+pb)/3 + sample(x = 1:10, size=10000, replace = T); pc <- ifelse(pc>99, round(pc -3), round(pc))

## 学生分数定量变量 -> 定性变量（A，B，C，D，E）的转换
# ll=list()
# k=0
# for (i in list(common1, common2, pb, pc)) {
#   k=k+1
#   level.df <- data.frame(start=c(50,60,70,80,90), end=c(60,70,80,90,110), levels=c("E","D","C","B","A"))
#   df1 <- as.data.frame(i)
#   ll[[k]] <- level.df$levels[findInterval(df1$i, level.df$start)]
# }

data1 <- data.frame(common1, common2, common3, common4, pb)  # 公共课预测专业基础课
data2 <- data.frame(common1, common2, common3, common4, pb, pc) # 公共课和专业基础课 预测 专业课

index<-sample(nrow(data),0.7*nrow(data)) 
data1.train <- data1[index, ]
data1.test <- data1[-index, ]
data2.train <- data2[index, ]
data2.test <- data2[-index, ]


# 训练模型预测专业基础课成绩
library(rpart)
pb.tree1<-rpart(pb~., data=data1.train, method="anova",
               parms=list(split="gini"))  # 使用CART算法时， split = “gini”
printcp(pb.tree1)  # 输出每个分类节点的复杂度参数CP
pb.tree2 <- prune(pb.tree1,cp=pb.tree1$cptable[which.min(pb.tree1$cptable[,"xerror"]),"CP"])  # 利用最小复杂度剪枝

par(mfrow=c(1,2))
library(rpart.plot)
rpart.plot(pb.tree1, branch=1, type=2, fallen.leaves=T,cex=0.8, sub="剪枝前")
rpart.plot(pb.tree2, branch=1, type=4,fallen.leaves=T,cex=0.8, sub="剪枝后")

df <- data.frame("common1"=55, "common2"=52, "common3"=60, "common4"=63)  # 输出学生公共课成绩得到预测结果
predtree <- predict(pb.tree2, newdata = df)  #利用测试数据集进行预测
# 计算模型的均方误差
rmse1 <- sum((predtree - data1.test$pb)^2) / length(data1.test$pb)  

##------------------------------

# 训练模型预测专业课成绩
pc.tree1<-rpart(pc~., data=data2.train, method="anova",
                parms=list(split="gini"))  # 使用CART算法时， split = “gini”
printcp(pc.tree1)  # 输出每个分类节点的复杂度参数CP
pc.tree2 <- prune(pc.tree1,cp=pc.tree1$cptable[which.min(pc.tree1$cptable[,"xerror"]),"CP"])  # 利用最小复杂度剪枝

par(mfrow=c(1,2))
library(rpart.plot)
rpart.plot(pc.tree1, branch=1, type=2, fallen.leaves=T,cex=0.8, sub="剪枝前")
rpart.plot(pc.tree2, branch=1, type=4, fallen.leaves=T,cex=0.8, sub="剪枝后")


df2 <- data.frame("common1"=79, "common2"=86, "common3"=91, "common4"=92,"pb"=96)
predtree2 <- predict(pc.tree2, newdata = df2)  #利用测试数据集进行预测
# 计算均方误差
rmse2 <- sum((predtree2 - data2.test$pc)^2) / length(data2.test$pc)  




