library(ISLR)
Hitters
x <- model.matrix(Salary~., Hitters)[,-1]   # 转换定性变量为哑变量，并去除Salary列
y <- Hitters$Salary

library(glmnet)
grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid, standardize = T)  # 岭回归建模， 默认进行标准化，lamda范围是100 - 10^1/2