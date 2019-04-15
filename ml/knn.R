a <- read.table("/Users/alexwang/r_env/ml/BondTelco_Customers.csv", sep = ",", header = T,
                stringsAsFactors = T)

## dataframe factor -> numeric 数据库数据格式转换
## college
# a[a$COLLEGE == "zero",1] = 0
# a[a$COLLEGE == "one",1] = 1
# a[,1] <- as.numeric(a[,1])
# ## reported_satisfaction
# a[a$REPORTED_SATISFACTION == "very_sat",9] = 0
# a[a$REPORTED_SATISFACTION == "sat",9] = 1
# a[a$REPORTED_SATISFACTION == "avg",9] = 2
# a[a$REPORTED_SATISFACTION == "unsat",9] = 3
# a[a$REPORTED_SATISFACTION == "very_unsat",9] = 4
# a[,9] <- as.numeric(a[,9])
# 
# ## reported_usage_level
# a[a$REPORTED_USAGE_LEVEL == "very_little",10] = 0
# a[a$REPORTED_USAGE_LEVEL == "little",10] = 1
# a[a$REPORTED_USAGE_LEVEL == "avg",10] = 2
# a[a$REPORTED_USAGE_LEVEL == "high",10] = 3
# a[a$REPORTED_USAGE_LEVEL == "very_high",10] = 4
# a[,10] <- as.numeric(a[,10])
# 
# ## consider_change_of_plan
# a[a$CONSIDERING_CHANGE_OF_PLAN == "never_thought",11] = 0
# a[a$CONSIDERING_CHANGE_OF_PLAN == "no",11] = 1
# a[a$CONSIDERING_CHANGE_OF_PLAN == "perhaps",11] = 2
# a[a$CONSIDERING_CHANGE_OF_PLAN == "considering",11] = 3
# a[a$CONSIDERING_CHANGE_OF_PLAN == "actively_looking_into_it",11] = 4
# a[,11] <- as.numeric(a[,11])
# ## leave
# a[a$LEAVE == "STAY",12] = 0
# a[a$LEAVE == "LEAVE",12] = 1
# a[,12] <- as.numeric(a[,12])


### 
library(caret)
#Split the data into 75% train and 25% test
nrow(a)
set.seed(1)
SplitIndex <- sample(x = c("Train", "Test"),size = nrow(a),replace = TRUE, prob = c(0.75,0.25)) 
TrainData <- a[SplitIndex == "Train", ]
TestData <- a[SplitIndex == "Test", ]

## ***************************************************************
# ### 方法1: 用Learning Vector Quantization（LVQ）模型进行特征选择
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model <- train(LEAVE~., data=a, method="lvq", preProcess="scale", trControl=control)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)

### 方法2: 递归特征消除（Recursive Feature Elimination）RFE
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
a$LEAVE <- as.factor(a$LEAVE)  # 将第12列转为因子型
# aa <- a[1:2000,]
results <- rfe(a[,1:11], a[,12], sizes=c(1:11), rfeControl=control)
print(results) # summarize the results
predictors(results) # list the chosen features
plot(results, type=c("g", "o"))

### 方法3: 用子集选择方法
library(leaps)
regfit <- regsubsets(LEAVE~., a)   # summary(regfit)
regfit.fwd <- regsubsets(LEAVE~., a, method="forward", nvmax = 10)
regfit.bwd <- regsubsets(LEAVE~., a, method="backward", nvmax = 10)
## 三种子集选择的方法得到类似的结果，对于限制4个预测变量而言结果一致
coef(regfit,4)

# coef(regfit.bwd,4)
# coef(regfit.fwd,4)



## *****************************************

## 选择最佳的4个特征（预测变量）
# names(TrainData)
train.data <- TrainData[,c(2,3,4,5,12)]
train.data$LEAVE <- as.factor(train.data$LEAVE) 
# attributes(a1$LEAVE)
KnnMode2 <- train(form = LEAVE~.,
                  data = train.data,
                  method = 'knn',
                  trControl=trainControl(
                    method='repeatedcv',
                    number=10,
                    repeats=15))

KnnMode1

## 画ROC曲线
library("pROC")
test.data <- TestData[,c(2,3,4,5,12)]
KnnProbs <- predict(object = KnnMode2, newdata = test.data, type = "prob") 
# head(KnnProbs)
KnnProbs <- KnnProbs[,2] #Only want one probability for each row
#Generate the ROC
KnnROC <- roc(response = test.data$LEAVE, predictor = KnnProbs) 
plot(KnnROC, print.auc = T)
  
## calculate success rate of prediction. 看看在测试数据集上的准确率
value <- ifelse(KnnProbs>0.5, "STAY","LEAVE")
true.value <- test.data$LEAVE
cc <- 1:length(value)
length(cc[value==true.value]) / length(value)
## 判断测试数据集的每一个预测值是否正确
test.data$true_or_false <- (value==true.value)
# test.data




