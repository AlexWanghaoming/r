library(GenomicFeatures)
library(clusterProfiler)
library(AnnotationHub)
library(enrichplot)

## 删除除了same_merged以外的所有对象
# a <- ls()
# rm(list = a[which(a!="same_merged")])

load("/Users/alexwang/r_env/go.RData")
library(tibble)   ## 给数据框加一列
m6A_fra_df2 <- add_column(m6A_fra_df, rownames(m6A_fra_df), .after = 1)
colnames(m6A_fra_df2) <- c("total", "V2","SameDir", "DiffDir", "strand")
same2_fold <- m6A_fra_df2[which(m6A_fra_df2$SameDir > 10*m6A_fra_df2$DiffDir),]
diff2_fold <- m6A_fra_df2[which(m6A_fra_df2$DiffDir > 10*m6A_fra_df2$SameDir),]
same_merged <- merge(same2_fold, geneMapping, by = "V2")
diff_merged <- merge(diff2_fold, geneMapping, by = "V2")

go.ids <- read.table("/Users/alexwang/data/go/current/FG.RR.27.GO.ids",skip = 1)
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
term2name <- read.table("/Users/alexwang/data/go/term2name.txt", header = F, sep = "\t")
# head(term2name)
target.gene <- same_merged$V1
enrichPH1 <- enricher(gene = target.gene, TERM2GENE = term2gene, TERM2NAME = term2name)

### if there's a AnnotationHub db aviliable:
# hub <- AnnotationHub()
# query(hub, "Fusarium graminearum")
# ph1.OrgDb <- hub[["AH67055"]]
# ph1.OrgDb1 <- hub[["AH65060"]]
# ph1.OrgDb2 <- hub[["AH65325"]]
# keys(ph1.OrgDb)
# columns(ph1.OrgDb)
# keytypes(ph1.OrgDb)
#### transform GO -> GID so that it can be suitable for ph1.OrgDb
# trans <- bitr(geneID = gene2term[,2], fromType = "GO", toType = "GID", OrgDb = ph1.OrgDb)
# bitr(geneID = same_merged$V2, fromType = "GENENAME", toType = "GID", OrgDb = ph1.OrgDb)
# geneID <- trans$GID

#### over-representation   fisher精确检验，超几何检验
# enrich_ph1 <- enrichGO(gene = geneID, OrgDb = ph1.OrgDb)
# ##### geneset enrich      
# gsea_ph1 <- gseGO(gene=geneID, OrgDb = ph1.OrgDb)





