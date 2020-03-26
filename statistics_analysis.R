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

####***** 方差分析
wt <- c(7.3, 7.35, 7.85)
fgyck1 <- c(1.6, 1.625, 1.6)
comp3 <- c(7.75, 7.625, 7.55)
comp5 <- c(7.475, 7.475, 7.75)
df <- data.frame(wt, fgyck1, comp3, comp5)
library(tidyverse)
df2 <- gather(df)
# 单因素方差分析
fit <- aov(value ~ key, data = df2)
summary(fit)
# 进行多重比较
TukeyHSD(fit)
library(gplots)
plotmeans(df2$value ~ df2$key, xlab = "group", ylab = "value")

# 作方差分析之前，先进行正态性检验和方差齐性检验
# 夏皮罗威尔克正态检验也叫W检验
shapiro.test(x = pe_54$value)
# 巴特勒特方差齐性检验，用于正态分布的数据，若w检验p>0.05则可以使用否则用Levene检验数据方差齐性
bartlett.test(value~pe5.4, data = pe_54)
car::leveneTest(pe_54$value, pe_54$pe5.4)
# 若方差齐性检验的结果p>0.05,则one-way ANOVA 适用，否则用welch‘s ANOVA进行分析，一般不用Kruskal-Wallis非参数秩和检验
library(onewaytests)
welch.test(value~pe5.4, data = pe_54)

#### ******* permutation test
a<-c(24,43,58,67,61,44,67,49,59,52,62,50,42,43,65,26,33,41,19,54,42,20,17,60,37,42,55,28)
group<-factor(c(rep("A",12),rep("B",16))) # a的前12个是一组，后16个是一组，检验两组数据的分布是否相同
data<-data.frame(group,a)
find.mean<-function(x){
  mean(x[group=="A",2])-mean(x[group=="B",2])  # 构造统计量 mean(a) - mean(b)
}
results<-replicate(999,find.mean(data.frame(group,sample(data[,2])))) # 重复999次
p.value<-length(results[results>mean(data[group=="A",2])-mean(data[group=="B",2])])/1000 #算出p值
# 画出概率分布
hist(results,breaks=20,prob=TRUE) 
lines(density(results))

### 多因素方差分析
pe_54$typr <- "pe"
colnames(pe_54) <- c("concentration","height","type")
bio_54$typr <- "bio"
colnames(bio_54) <- c("concentration","height","type")
mp_54$typr <- "mp"
colnames(mp_54) <- c("concentration","height","type")
df1 <- rbind(pe_54,bio_54, mp_54) # 合并数据框
fit <- aov(height~concentration*type,data = df1)  # 双因素方差分析；探究不同浓度，不同种类的塑料对
# 植物株高带来的影响是否存在差异
summary(fit)

library(gplots)
plotmeans(height~interaction(concentration,type, sep=" "),data = df1,
          connect = list(c(1,5,9), c(2,6,10), c(3,7,11), c(4,8,12)),   # 哪些线需要连接起来
          col = c("red", "darkgreen","blue","yellow"))
library(HH)
interaction2wt(height~concentration*type, data = df1)

## MM +ade  #生长速率 t-test 统计功效
wi <- c(4.1,5.1,4.5,4.6,4.218,4.6,5.3,4.8,4.7)
mu <- c(2.7,2.8,2.9,0.7,0.75,0,2.9,3,2.7)
t.test(wi,mu)
library(pwr)
library(effsize)
cohen.d(wi,mu)
pwr.t.test(n=9,d=3.032318,sig.level=0.05, type="two.sample",alternative="two.sided")
pwr.t.test(d=0.8,sig.level=0.05,power = 0.9,type="two.sample",alternative="two.sided")

## 负二项分布negative binomial distribution 描述一系列伯努利实验中，单次成功的概率是p, 成功指定次数（size）时（之前），失败次数的离散分布
dnbinom(0:2, size=4, p=0.8) # 运动员夺冠的概率时0.8， 该运动员获得4个冠军前，发生0，1，2次失败的概率
pnbinom(2, 4, 0.8) # 运动员夺冠的概率时0.8， 该运动员获得4个冠军前，至多发生两次失败的累计概率
qnbinom(0.9, 4, 0.8)  #百分之90的情况下，该运动员至多失败几次
rnbinom(1000, 4, 0.8) # 1000次模拟，每次失败的次数

## 二项分布是 进行size次伯努利实验，单次成功概率是p，成功x次的分布
size = 6
p = 0.1
dbinom(x=2, size, p)
pbinom()
qnbinom()
rnbinom()





