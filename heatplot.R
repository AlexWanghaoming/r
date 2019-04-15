data <- read.delim("/Users/alexwang/Downloads/GPCR-1-NJtree.csv", sep = ",")
data <- column_to_rownames(data, "X")  # 将表达量数据的第一列转换为列名

# 过滤表达量全部是0的基因
library(tidyverse)
mat <- data %>%
    rownames_to_column('gene') %>%
    filter_if(is.numeric, any_vars(. != 0)) %>%
    column_to_rownames('gene')

co <- cor(t(mat), method="pearson" ) # 计算皮尔逊相关系数
hr <- hclust(as.dist(1-co), method="average")  # 层次聚类
## 画聚类图
plot(hr,hang=-1)  #这里的hang=-1使得树的节点在下方对齐

#将树分为3部分
rect.hclust(hr,k=3)  
# groups<-cutree(hr,k=3)

# 画热图
library(gplots)
heatmap.2(as.matrix(mat), scale = "row", dendrogram = "row", Rowv = as.dendrogram(hr), Colv = F, 
          col = bluered(256), srtCol = 0,adjCol = 0.5,cexCol = 1.1, offsetRow = 0, density.info = "none", 
          trace = "none", margins = c(2,7),lhei = c(1,3))
