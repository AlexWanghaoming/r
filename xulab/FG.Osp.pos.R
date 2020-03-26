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



