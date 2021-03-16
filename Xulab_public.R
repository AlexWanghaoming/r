###################################################### coding for Xulab public analysis########################################################### 
################ Osp24 position on PH-1 chromosome 
chr1 <- c(1, 11760950)
chr2 <- c(1,  8997558)
chr3 <- c(1,  7792947)
chr4 <- c(1,  9407501)
g2s <- read.table("/Users/alexwang/1.script/r/xulab/FG.2speed_block.txt", header = T)

dat1 <- read.table("/Users/alexwang/testtttttt/GENE/7301.txt", header = T)
dat2 <- read.table("/Users/alexwang/testtttttt/GENE/6911.txt", header = T)
#fsc <- c("yellow", "purple")
fsc <- c("#FFF68F", "#CD96CD")

# pdf("FG.Osp_coord.pdf", width = 5, height = 2.5)
par(mar=c(0,1,2,2), xpd = T)
# declare max range
plot(0, 0, xlim = chr1/10^6, ylim = c(0, 5.8), axes = F, pch = NA, xlab = NA, ylab = NA)

# chr1
chr1dat1 <- dat1$center[dat1$chr == 1]/10^6
chr1dat2 <- dat2$center[dat2$chr == 1]/10^6
rect(g2s$start[g2s$chr == 1]/10^6, 4.5+0.02, g2s$end[g2s$chr == 1]/10^6, 5.5-0.02, col = fsc[g2s$colorcode[g2s$chr == 1]], lend = 1, border = NA)
segments(chr1dat1, 4.5+0.02, chr1dat1, 5.5-0.02, col = "seagreen", lend = 1)
segments(chr1dat2, 4.5+0.02, chr1dat2, 5.5-0.02, col = "red", lend = 1)

# chr2
chr2dat1 <- dat1$center[dat1$chr == 2]/10^6
chr2dat2 <- dat2$center[dat2$chr == 2]/10^6
rect(g2s$start[g2s$chr == 2]/10^6, 3+0.02, g2s$end[g2s$chr == 2]/10^6, 4-0.02, col = fsc[g2s$colorcode[g2s$chr == 2]], lend = 1, border = NA)
segments(chr2dat1, 3+0.02, chr2dat1, 4-0.02, col = "seagreen", lend = 1)
segments(chr2dat2, 3+0.02, chr2dat2, 4-0.02, col = "red", lend = 1)

# chr3
chr3dat1 <- dat1$center[dat1$chr == 3]/10^6
chr3dat2 <- dat2$center[dat2$chr == 3]/10^6
rect(g2s$start[g2s$chr == 3]/10^6, 1.5+0.02, g2s$end[g2s$chr == 3]/10^6, 2.5-0.02, col = fsc[g2s$colorcode[g2s$chr == 2]], lend = 1, border = NA)
segments(chr3dat1, 1.5+0.02, chr3dat1, 2.5-0.02, col = "seagreen", lend = 1)
segments(chr3dat2, 1.5+0.02, chr3dat2, 2.5-0.02, col = "red", lend = 1)

# chr4
chr4dat1 <- dat1$center[dat1$chr == 4]/10^6
chr4dat2 <- dat2$center[dat2$chr == 4]/10^6
rect(g2s$start[g2s$chr == 4]/10^6, 0+0.02, g2s$end[g2s$chr == 4]/10^6, 1-0.02, col = fsc[g2s$colorcode[g2s$chr == 4]], lend = 1, border = NA)
segments(chr4dat1, 0+0.02, chr4dat1, 1-0.02, col = "seagreen", lend = 1)
segments(chr4dat2, 0+0.02, chr4dat2, 1-0.02, col = "red", lend = 1)

axis(3, xlim = chr1/10^6, cex.axis = 0.8)
text(12.6, 6.2, "Mb", cex = 0.8)

text(-0.5, 5, "I")
text(-0.5, 3.5, "II")
text(-0.5, 2, "III")
text(-0.5, 0.5, "IV")

legend("right", inset=-0.08, c("fast subgenome", "slow subgenome"), pch = c(15, 15), col = fsc, cex = 0.8, box.col = NA)

dev.off()
##############################################################################
############# boxplot Jiang Cong Nature Microbiology Fig
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
###########################################################################################
############################### 在条形图上加横线
df <- data.frame(chr=c("1","1","2"), length=c(10,10,15), bar_pos=c(3,8,10))
library(ggplot2)
p <- ggplot(df, aes(x=chr, y=length)) + geom_bar(stat = "identity", position = position_dodge()) # position_dodge2
# 可以将分为多个
p+geom_errorbar(aes(y=bar_pos,ymin=bar_pos, ymax=bar_pos), colour="green")

############################################################################################
############################### GO enrichment dotplot parsing Blast2Go results
library(squash)
library(graphics)
Sys.setlocale("LC_TIME","English")
pdf("Hyp12h-0h-up-enrich-specific.pdf",width = 7, height = 12)

layout(matrix(c(1,1,1,4,2,3),2,3,byrow = TRUE),c(1.5,1,1),c(10,1))

go<-read.table("Hyp12h-0h-up-enrich-result-specific.txt", header = T, sep = "\t", check.names = T)
gofold<-(go$Nr_Test/(go$Nr_Test+go$Non_Annot_Test))/(go$Nr_Reference/(go$Nr_Reference+go$Non_Annot_Reference))

gof<--log10(go$FDR)

par(mar=c(2,1,1,1))

plot(gof,seq(1,length(go$GO_Name)),
     xlab="-log10(FDR)",
     ylab="",
     axes=F,
     xlim=c(0,46),
     col="white",
     frame.plot = T,
     adj=0.15
)
axis(1,at=seq(0,max(gof)+1,4))
ngo <- length(go$GO_Category)
nmf <- sum(go$GO_Category == "MOLECULAR_FUNCTION")
ncc <- sum(go$GO_Category == "CELLULAR_COMPONENT")
nbp <- sum(go$GO_Category == "BIOLOGICAL_PROCESS")
abline(h=seq(0.5,50.5,1),lty=3,col="grey75",lwd=0.6)
abline(v=seq(-4,max(gof)+1,1),lty=3,col="grey75",lwd=0.6)
rect(max(gof)+1.2,1-0.5-1,47.63,1+nbp-0.5,col="#FFFFCC", border = "white")
rect(max(gof)+1.2,1+nbp-0.5,47.63,1+nbp+ncc-0.5,col="#CCCCFF", border = "white")
rect(max(gof)+1.2,1+nbp+ncc-0.5,47.63,1+nbp+ncc+nmf-0.5+1,col="#FFCCCC", border = "white")
points(gof,seq(1,length(go$GO_Name)),
       col=jet(ngo)[rank(go$Nr_Test)],
       pch=16,
       cex=1.5+log(gofold,2)#????????С??cex
)
text(max(gof)+2,seq(1,length(go$GO_Name)),paste(go$GO_ID,go$GO_Name),cex=0.8,adj = 0)#Go????
abline(v=45.5,col="white", lwd=2)#??ɫɫ????
abline(v=max(gof)+1.1)#???????ݱ߿?
text(46.5,nbp/2+0.5,"BP",cex=1)#λ?þ???
text(46.5,nbp+ncc/2+0.5,"CC",cex=1)#λ?þ???
text(46.5,nbp+ncc+nmf/2+0.5,"MF",cex=1)#λ?þ???
# legend 1
par(mar=c(1.7,1,1.1,2),xpd = T)
barplot(rep(1,11),border = "NA", space = 0,
        ylab="",
        xlab="",
        xlim=c(0.40,10.54),
        axes = F, col=jet(11) )
box()
text(1,1.6,"Less", cex = 0.9)
text(10,1.6,"More", cex = 0.9)
text(6,-0.8,"Number of enriched genes", adj=0.55, cex = 0.9)
# legend 2
par(mar=c(1.7,1,1.1,2),xpd = T)
plot(c(1:4),rep(1,4),
     ylab="",
     xlab="",
     xlim=c(1,4.2),	
     axes = F,pch=16, cex=2*(0.2+fivenum(log(gofold,2))),col="green")
box(col = FALSE)###ȥ????ɫ?߿?
text(1,1.9,"Low", cex = 0.9)
text(4,1.9,"High", cex = 0.9)
text(3,-0.1,"Enrichment factor (fold)", adj=0.6, cex = 0.9)
# legend main
plot(0,0, axes = F,col="NA")
text(0.01,0.3,"-log10(FDR)", adj=1.8)
dev.off()

