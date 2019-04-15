# decision tree - > classification
loc<-"http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds<-"breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url<-paste(loc,ds,sep="")
data<-read.table(url,sep=",",header=F,na.strings="?")
names(data)<-c("编号","肿块厚度","肿块大小","肿块形状","边缘黏附","单个表皮细胞大小",
               "细胞核大小","染色质","细胞核常规","有丝分裂","类别")
data$类别[data$类别==2]<- "good"
data$类别[data$类别==4]<- "bad"
data <- data[,-1]
train<-sample(nrow(data),0.7*nrow(data)) 
tdata<-data[train,] 
vdata<-data[-train,] 

## rpart.control对树设置向前剪枝的条件 
## xval是10折交叉验证  
## minsplit是最小分支节点数，这里指大于等于20，那么该节点会继续分划下去，否则停止  
## minbucket：叶子节点最小样本数  
## maxdepth：树的深度  
## cp全称为complexity parameter，指某个点的复杂度，对每一步拆分,模型的拟合优度必须提高的程度  
ct <- rpart.control(xval=10, minsplit=20, cp=0.1)  # 该数据集不用设置向前剪枝的条件

library(rpart)
dtree<-rpart(类别~., data=tdata,method="class",
               parms=list(split="information"), control = ct)  # 使用ID3算法时候，split = “information”基于信息熵分类
                                                 # 使用CART算法时， split = “gini”
printcp(dtree)  # 输出每个分类节点的复杂度参数CP

## xerror是交叉验证误差，选择交叉验证误差最小的cp进行剪枝
## prune函数可以实现最小代价复杂度剪枝法
tree<-prune(dtree,cp=dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"])

par(mfrow=c(1,2))
library(rpart.plot)
rpart.plot(dtree,branch=1,type=2, fallen.leaves=T,cex=0.8, sub="剪枝前")
rpart.plot(tree,branch=1, type=4,fallen.leaves=T,cex=0.8, sub="剪枝后")

## test
predtree<-predict(tree,newdata=vdata,type="class")  #利用预测集进行预测
table(vdata$类别,predtree,dnn=c("真实值","预测值")) #输出混淆矩阵  accuracy rate：（73+122）/ 210 = 0.9285714






