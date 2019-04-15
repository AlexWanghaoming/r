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

# 3. 当n>40时，一般用卡方检验，fisher检验只能用于检验两个二进制变量的相关性，这里检验s/n，与low/high是否有关
fisher.test(x = c(rep("s",32), rep("n",8)), y = c(rep("s",20), rep("n", 20)))

# 卡方检验
a <- c(rep("yes", 2), rep("no", 4))
b <- c(rep("yes", 30),rep("no", 3))
c <- c(rep("yes", 5), rep("no", 4))
d <- c(rep("yes", 3), rep("no", 5))
con <- c(rep("a", length(a)), rep("b",length(b)), rep("c", length(c)), rep("d", length(d)))
df <- data.frame(value = c(a,b,c,d), con)
ggpiestats(df, main = "value", condition = con)

