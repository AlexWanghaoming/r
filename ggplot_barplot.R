## barplot
# 在条形图上加横线
df <- data.frame(chr=c("1","1","2"), length=c(10,10,15), bar_pos=c(3,8,10))
library(ggplot2)
p <- ggplot(df, aes(x=chr, y=length)) + geom_bar(stat = "identity", position = position_dodge()) # position_dodge2
                                                                                                 # 可以将分为多个
p+geom_errorbar(aes(y=bar_pos,ymin=bar_pos, ymax=bar_pos), colour="green")


## boxplot
library(readxl)
a <- readxl::read_xlsx("/Users/alexwang/Downloads/protein length-expression - 副本.xlsx")

protein_length <- a[,c(1,2)]
colnames(protein_length) <- c("other","orph")
protein_exp <- a[,c(4,5)]
colnames(protein_exp) <- c("other","orph")
library(tidyverse)
df1 <- na.omit(gather(protein_length))
df2 <- na.omit(gather(protein_exp))
df2$value <- df2$value+1   # 基因表达量 加1，否则后续坐标轴变化无法取log

# scales 为后续转换坐标轴刻度
# library(scales)

library(ggsignif)
p1 <- ggplot(df1,aes(x=key,y=value)) + geom_violin() + geom_boxplot(fill=c("orange","lightblue"),width=0.25) + 
  scale_y_continuous(trans="log10",breaks = c(1,25,50,100,200,400,600,1000,2000,5000,10000))+
  scale_x_discrete(label=c("orph","other"))+
  xlab("Gene Category") + ylab("Protein length (aa)")+
  geom_signif(comparisons = list(c("other","orph"))) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) # 去掉网格并加上边框


p2 <- ggplot(df2,aes(x=key,y=value)) + geom_violin() + geom_boxplot(fill=c("orange","lightblue"),width=0.25) + 
  scale_y_continuous(trans = "log10",breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000,10000,20000,50000))+
  scale_x_discrete(label=c("orph","other"))+
  xlab("Gene Category") + ylab("Maximum gene expression (TPM+1)")+
  geom_signif(comparisons = list(c("other","orph"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

library(cowplot)
plot_grid(p1,p2,align = "h",labels = c("a","b"))
