# 1. Fisher exact test
## exp1: Arthritis data
library(vcd)
mytable <- xtabs(~ Treatment + Improved, data = Arthritis)
fisher.test(mytable)   ## 治愈情况和治愈效果是否有关？

## exp2: 高学历和待遇有关系吗？
# 类别             有钱人          整体
# 高学历           10              50
# 一般学历         90              950
# Total          100             1000
mat <- matrix(data = c(10,90,50,950), nrow = 2,byrow = F)
fisher.test(mat, alternative = "greater")   ## P-value 小于0.05，说明学历和待遇有关

# 2. 卡方检验
## 赌场又一个骰子：出现六个面的观察频数依次是：107， 198， 192， 125， 132， 248，问：骰子是否有问题？
## 理论频率都为1/6
p <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
x <- c(107, 198, 192, 125, 132, 248)
chisq.test(x, p = p)   ## p < 0.01 所以拒绝原假设,所以骰子有问题