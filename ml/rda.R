library(ggvegan)
library(ggrepel)
# 读入72个样本-测量指标数据,相当于物种
dd <- readxl::read_xlsx("/Users/alexwang/Downloads/RDA.xlsx", col_names = T)
se <- data.frame(type = c(rep("pe",24), rep("bio",24), rep("mp",24)),
                 con = c(rep("ck",6),rep("0.1",6),rep("0.5",6),rep("1",6),rep("ck",6),rep("0.1",6),
                         rep("0.5",6),rep("1",6),rep("ck",6),rep("0.1",6),rep("0.5",6),rep("1",6)))

# dd <- decostand(dd, method = "hellinger") #对响应变量做标准化（除以行和再取平方根）

### 1 dca分析
# decorana(dd)
decorana()
res <- rda(dd ~ . , se)
# vif.cca(res)  # VIF 进行方差膨胀因子分析，要求各个变量小于10
summary(res)

## RDA plot
centroids <- as.data.frame(res$CCA$centroids[,c(1,2)])
rownames(centroids) <- c("bio","mp","pe","con-0.1","con-0.5","con-1","con-ck")
rda.v <- as.data.frame(res$CCA$v[,c(1,2)])
rda.v$name = row.names(rda.v)

arrow_data <- data.frame(x=rda.v[,1], y = rda.v[,2], x_end=0, y_end=0, name=rda.v[,3])

ggplot(data = centroids, aes(x = RDA1,y=RDA2)) + geom_point() +
  geom_segment(data = arrow_data, aes(x=0, y=0, xend=x, yend=y), 
               arrow = arrow(length = unit(0.1,"inches"))) + 
  ggrepel::geom_text_repel(aes(RDA1,RDA2,label=rownames(centroids))) +
  ggrepel::geom_text_repel(data = arrow_data, aes(x,y,label=name))

