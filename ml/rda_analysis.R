# rda analysis
library(readxl)
data1 <- read_xlsx("/Users/alexwang/Downloads/rda_analysis.xlsx",sheet = 1)[,c(-1,-2)]  #response variable
data2 <- read_xlsx("/Users/alexwang/Downloads/rda_analysis.xlsx", sheet = 2)    # constrain variable
data1 <- decostand(data1,method = "hellinger")#对响应变量做转化

res <- rda(data1 ~ treatment, data2)
res <- summary(res)
sp=as.data.frame(res$species[,1:2])/1.5#可根据出图结果，对画图数据做一定的放大或缩小，下同
st=as.data.frame(res$constraints[,1:2])/1.5 #或res$sites，建议两者都探索一下   
yz=as.data.frame(res$biplot[,1:2])/4
row.names(yz) <- gsub("treatment","",rownames(yz))

grp = data2   # 约束变量分组数据

library(ggplot2)
library(ggrepel)

ggplot() +
  #geom_text_repel(data = st,aes(RDA1,RDA2,label=row.names(st)),size=4)+#显示样方
  geom_point(data = st,aes(RDA1,RDA2,shape=grp$treatment),size=4)+
  scale_shape_manual(values = c(1:12))+
  geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "#D45068")+
  geom_text_repel(data = sp,aes(RDA1,RDA2,label=row.names(sp)))+
  geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=2, size=0.6,colour = "#7C9728")+
  geom_text_repel(data = yz,aes(RDA1,RDA2,label=row.names(yz)), segment.size = 4)+
  labs(x=paste("RDA 1 (", format(100 *res$cont[[1]][2,1], digits=4), "%)", sep=""),
       y=paste("RDA 2 (", format(100 *res$cont[[1]][2,2], digits=4), "%)", sep=""))+
  geom_hline(yintercept=0,linetype=3,size=1) + 
  geom_vline(xintercept=0,linetype=3,size=1)+
  guides(shape=guide_legend(title=NULL,color="black"),
         fill=guide_legend(title=NULL))+
  theme_bw()+theme(panel.grid=element_blank())

