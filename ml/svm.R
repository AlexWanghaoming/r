key_word <- read.delim("/Users/alexwang/Desktop/key_word.csv",sep = ",")

# make dataframe for training SVM model.
df.list <- list()
for (col in 1:ncol(key_word)) {
  elements <- key_word[,col]
  ll <- list()
  i=0
  for (k in elements) {
    if(k!=""){
    i=i+1
    k <- c(0,0,0,0,0,0)
    k[col] <- 1
    ll[[i]] <- k
    }
  }
  df <- as.data.frame(ll)
  col.names <- key_word[,col]
  col.names <- col.names[which(col.names!="")]
  colnames(df) <- col.names
  df.list[[col]] <- df
}
data <- do.call(cbind,df.list)

a <- lapply(split.default(data, names(data)), function(x) Reduce("+",x)) # 用 split.default() 合并列名重复的特征

train.data <- as.data.frame(a)
train.data$type <- colnames(key_word)
# training SVM model
library("e1071")
model <- svm(type ~ ., data = train.data, type="C-classification",kernel="radial")

## processing test data

target <- "苏州市,交巡警,队,苏,E99E91,车主,桐泾,路段,修路,下班,西环路,改走,解放西路,绕行,西环路,解放西路,路口,不知,连续,收到,次,违章,记录,登录,网站,查询,违章,图片,并未,发现自己,闯红灯,请,相关,部门,给予,解释,第一次,违法,编号,第二次,违法,编号"

aa <- unlist(strsplit(target,","))  # split target sentences
ll=list()
k=0
for (i in colnames(train.data)) {
  k=k+1
  x <- sum(aa==i)
  ll[[k]]=x
}
test <- unlist(ll)
test.df <- as.data.frame(t(as.data.frame(test)))
colnames(test.df) <- colnames(train.data)

# predict result
as.vector(predict(model, test.df))





