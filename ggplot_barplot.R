# 在条形图上加横线
df <- data.frame(chr=c("1","1","2"), length=c(10,10,15), bar_pos=c(3,8,10))
library(ggplot2)
p <- ggplot(df, aes(x=chr, y=length)) + geom_bar(stat = "identity", position = position_dodge()) # position_dodge2
                                                                                                 # 可以将分为多个
p+geom_errorbar(aes(y=bar_pos,ymin=bar_pos, ymax=bar_pos), colour="green")
