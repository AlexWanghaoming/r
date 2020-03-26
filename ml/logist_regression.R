data2 <- read.csv("/Users/alexwang/Downloads/接单/pressure2.csv")
data2 <- data2[,c(2,3,5,6,8,9,4)]  # label值为二分类变量
data2 <- na.omit(data2)

# 划分测试集和训练集
s <- sample(nrow(data2),floor(nrow(data2)*0.8),replace = F)
train_df <- data2[s,]
test_df <- data2[-s,]

# 建立逻辑回归模型
logist_model <- glm(formula = Y ~ ., data = train_df, family = binomial)
summary(logist_model)
# step <- step(logist_model,direction = 'both')
# summary(step)

library(pROC)
pred <- predict.glm(logist_model,type='response',newdata = train_df)
head(pred)
fitted.r <- ifelse(pred>0.5,1,0)
# 模型的精度
accuracy <- table(fitted.r,test_df$Y)
#做出roc的图像
roc <- roc(test_df$Y,pred)
roc
plot(roc)
