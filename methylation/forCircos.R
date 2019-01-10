library(GenomicFeatures)
library(Rsamtools)
library(RColorBrewer)
library(circlize)
plot_circos_strandSpecific6mA <- function(windowSize,chrNum) {
  chrLabels = unlist(strsplit(names(ph1_genome[chrNum])," "))[1]
  ran <-
    IRanges(start = seq(1, width(ph1_genome[chrNum]), by = windowSize),
            width = windowSize)
  end(ran[length(ran)]) <- width(ph1_genome[chrNum])
  ph1genome_bins <-
    GRanges(seqnames = rep(chrLabels, length(ran)),
            ranges = ran,
            strand = "*")
  positive_strand_6mA <-
    m6A_strand_specific[which(strand(m6A_strand_specific) == "+")]
  m6A_counts1 <- countOverlaps(ph1genome_bins, positive_strand_6mA)
  negative_strand_6mA <-
    m6A_strand_specific[which(strand(m6A_strand_specific) == "-")]
  m6A_counts2 <- countOverlaps(ph1genome_bins, negative_strand_6mA)
  bins_seq <-
    getSeq(FaFile("Fusarium_graminearum.RR1_with_mito.fa"),
           ph1genome_bins)
  Pos <- (m6A_counts1 / letterFrequency(bins_seq, "A"))
  Neg <- (m6A_counts2 / letterFrequency(bins_seq, "T"))
  mcols(ph1genome_bins) <- cbind(Pos,Neg)
  return(ph1genome_bins)
}
total_info <- c(plot_circos_strandSpecific6mA(50000,1),
              plot_circos_strandSpecific6mA(50000,2),
              plot_circos_strandSpecific6mA(50000,3),
              plot_circos_strandSpecific6mA(50000,4),
              plot_circos_strandSpecific6mA(50000,5))

bedDF <- as.data.frame(total_info)[,c(-4,-5)]  # remove 4th,5th cols

## modified bedDF 1st cols
as.character(bedDF[,1])
bedDF[,1] <- as.character(bedDF[,1])  # factor -> charactors
bedDF[which(bedDF[,1] != "Mt"),][,1] = paste("chr",bedDF[which(bedDF[,1] != "Mt"),][,1], sep = "")

# load("/home/wanghm/wanghm_R/methylation/methy_Circos.RData")
chro <- c("chr1", "chr2", "chr3", "chr4","Mt")
starts <- c(0,0,0,0,0)
ends <- c(11760891, 8997558, 7792947, 9395062, 95638)
genoCir <- data.frame(chr=chro, start=starts, end=ends)

circos.clear()
circos.par(gap.degree = 4, start.degree = 30, track.height = 0.2, cell.padding = c(0,0,0,0))
circos.genomicInitialize(genoCir)
circos.genomicTrackPlotRegion(ylim = c(0,1), 
                              bg.col = brewer.pal(5,"Set3"),
                              bg.border = 1, track.height = 0.05)
f = colorRamp2(breaks = c(0,0.05),colors = c("black","green"))
circos.genomicTrackPlotRegion(bedDF2,stack = T,track.height = 0.2,
                              panel.fun = function(region,value,...){
                              circos.genomicRect(region,value,col = f(value[[1]]),border = NA,...)
                              cell.xlim=get.cell.meta.data("cell.xlim")
                              cell.ylim=get.cell.meta.data("cell.ylim")
                              circos.lines(cell.xlim,col = "white",y = c((cell.ylim[1]+cell.ylim[2])/2,
                                                           (cell.ylim[1]+cell.ylim[2])/2))
})
bedList <- list(bedDF[,-5],bedDF[,-4])
circos.genomicTrackPlotRegion(bedList,ylim = c(-0.05,0.05),track.height = 0.2,
                              panel.fun = function(region,value,...){
                                i = getI(...)
                                if(i == 1){
                                  circos.genomicLines(region,value,col = "#e6b89c",...)
                                }else{
                                  circos.genomicLines(region,-value,col = "#4281a4",...)
                                }
                                cell.xlim=get.cell.meta.data("cell.xlim")
                                circos.lines(cell.xlim,y = c(0,0))
                              })

circos.clear()

