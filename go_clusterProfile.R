library(GenomicFeatures)
library(clusterProfiler)
library(AnnotationHub)
library(enrichplot)

## 删除除了same_merged以外的所有对象
# a <- ls()
# rm(list = a[which(a!="same_merged")])


go.ids <- read.table("/Users/alexwang/0data/xulab_current/go/current/FG.RR.27.GO.ids",skip = 1)
func1 <- function(x){
  term <- strsplit(x[2], ",")[[1]]
  gene <- rep(x[1],length(term))
  subdataframe <- data.frame(term=term, gene=gene)
  return(subdataframe)
}
ss <- apply(go.ids, 1, func1)
goID2gene <- do.call(rbind, ss)

TermId <- as.numeric(as.character(goID2gene[,1])) # 将term转为数值型
TermId <- sprintf("GO:%07d", TermId)        ##### 将每一个GO id 填充为7位
term2gene <- data.frame(term=TermId, gene=goID2gene[,2])
term2name <- read.table("/Users/alexwang/0data/xulab_current/go/term2name.txt", header = F, sep = "\t")[,1]
# head(term2name)
target.gene <- read.table("/Users/alexwang/Downloads/1s_mu_de.down.txt")[,1]
enrichPH1 <- enricher(gene = target.gene, TERM2GENE = term2gene, TERM2NAME = term2name)
head(term2name)
pdf(file = "1s_mu_de.down.pdf", width = 15, height = 5)
dotplot(enrichPH1)
barplot(enrichPH1)
dev.off()

