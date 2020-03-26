
library(ggplot2)
#### PCA plot
data.matrix <- matrix(nrow = 100,ncol = 10)
colnames(data.matrix) <- c(paste0("wt",1:5), paste0("ko",1:5))
rownames(data.matrix) <- paste0("gene",1:100)
for (i in 1:100) {
  wt.value <- rpois(5,lambda = sample(x=10:1000, size = 1))
  ko.value <- rpois(5,lambda = sample(x=10:1000, size = 1))
  data.matrix[i,] <- c(wt.value, ko.value)
}
pca <- prcomp(t(data.matrix), scale = T, center = T)
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
pca.data <- data.frame(Sample=rownames(pca$x),
                       X = pca$x[,1],
                       Y = pca$x[,2])

ggplot(data = pca.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text() + 
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep = "")) + 
  ylab(paste("PC2 - ",pca.var.per[2], "%", sep = "")) + theme_bw() + ggtitle("PCA Graph")

### MDS/PCoA plot
distance.matrix <- dist(scale(t(data.matrix), center = T, scale = T), method = "euclidean")
mds.stuff <- cmdscale(distance.matrix,eig = T, x.ret = T)
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.value <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.value), X=mds.value[,1], Y=mds.value[,2])
ggplot(data=mds.data, aes(x=X, y=Y,label=Sample)) + 
  geom_text() + 
  theme_bw() + 
  xlab(paste("MDS - ", mds.var.per[1], "%", sep = "")) + 
  ylab(paste("MDS - ", mds.var.per[2], "%", sep = "")) + 
  ggtitle("MDS plot using euclidean distance")


