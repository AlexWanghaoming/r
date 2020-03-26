### 1.1；1.2 5th,10th polynomial
set.seed(1)
x <- sort(runif(40)*10^0.5)
y <- sort(runif(40)*10^0.1)
lm.model1<- lm(y~poly(x, degree = 5))  # build 5th polynomial model
lm.model2 <- lm(y~poly(x,degree = 10)) # build 10th polynomial model
# plot
plot(x,y)
lines(x, predict(lm.model1,data.frame(x)),col="red",lwd=2)
lines(x, predict(lm.model2,data.frame(x)),col="green",lwd=2)
legend("topleft",legend = c("5th polynomial", "10th polynomial"), col=c("red","green"),box.col = NA,pch="l")
title("Comparation of 5th and 10th polynomial regression")

### 1.3 comparation of natrual cubic and 9/15 knots cubic 
library(splines)
bs.model1 <- lm(y~bs(x, df = 12)) # build 9 knots cubic model
bs.model2 <- lm(y~bs(x, df = 18)) # build 15 knots cubic model
ns.model <- lm(y~ns(x)) # build natural cubic model 

## check R-square and MSE
# summary(bs.model1)
# summary(bs.model2)
# summary(ns.model)
# plot
plot(x,y)
lines(x, predict(bs.model1, data.frame(x)), col="red", lwd=2)
lines(x, predict(bs.model2, data.frame(x)), col="green", lwd=2)
lines(x, predict(ns.model, data.frame(x)), col="blue", lwd=2)
legend("topleft", legend = c("9 knots cubic spline", "15 knots cubic spline", "natural cubic spline"), col=c("red", "green", "blue"), pch="l", box.col = NA)
title("Comparation of cubic spline regression")

################################################# 2 spline regression comparation
raw1 <- read.table("8A7666FD-4704-47E2-BA26-FD0BD2919EEB.txt", header = T)
raw1 <- raw1[order(raw1$age),]
linear.fit <- lm(strontium.ratio ~ bs(age, df=12, degree = 1), data = raw1) # build linear 11 knots model
quadratic.fit <- lm(strontium.ratio ~ bs(age, df=13, degree = 2), data = raw1) # build quadratic 11 knots model
cubic.fit <- lm(strontium.ratio ~ bs(age, df=14, degree = 3), data = raw1) # build cubic 11 konts model

# plot
plot(raw1$age, raw1$strontium.ratio, xlab = "age", ylab = "strontium.ratio")
lines(raw1$age, predict(linear.fit, raw1), col="red", lwd=2)
lines(raw1$age, predict(quadratic.fit, raw1), col="green", lwd=2)
lines(raw1$age, predict(cubic.fit, raw1), col="blue", lwd=2)
legend(x=100,y=0.70725,legend = c("linear spline", "quadratic spline", "cubic spline"), col=c("red","green","blue"),
       pch = "l", box.col = NA)
title("Cubic spline model of strontium.ratioc ~ age")

################################################ 3 GCV and CV comparation
raw2 <- read.table("CF907FE2-0682-44AB-9FF0-58DCCBB0985A.txt",header = T)
raw2 <- raw2[order(raw2$age),]
smooth.fit.cv <- smooth.spline(raw2$age, raw2$log.income,cv = T) # build CV smooth spline model
smooth.fit.gcv <- smooth.spline(raw2$age, raw2$log.income,cv = F) # build GCV smooth spline model

# plot
plot(raw2$age, raw2$log.income, xlab = "age", ylab = "log.income")
lines(raw2$age, predict(smooth.fit.cv, raw2$age)$y, col="red", lwd=2)
lines(raw2$age, predict(smooth.fit.gcv, raw2$age)$y, col="green", lwd=2)
legend(x=35,y=12,legend=c("CV=0.299,RSS=7.89","GCV=0.293,RSS=8.24"), col=c("red", "green"), pch="l", box.col = NA)
title("Smooth spline of log.income ~ age")




