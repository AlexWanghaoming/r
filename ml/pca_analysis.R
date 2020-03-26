library(readxl)
data <- readxl::read_xlsx("/Users/alexwang/Desktop/homework6_rawdata.xlsx",col_names = T)
data <- data[,2:ncol(data)]
# 数据标准化
df1 <- scale(data)

# 对标准化后的数据进行PCA分析，cor=TRUE 指定运用相关系数矩阵，否则用协方差矩阵
pc_analysis <- princomp(df1,cor = TRUE)
summary(pc_analysis,loadings = TRUE)

# 输出前五个主成分的载荷矩阵
pc_analysis$loadings[,1:5]

# 计算相关系数矩阵的特征值和特征向量
y <- eigen(cor(df1))
