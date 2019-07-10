rm(list=ls())

setwd("C:\\Users\\User\\Desktop\\Fall 2018\\Applied Stat TP\\HW6")
height <- read.csv("height.csv",header=TRUE, sep=",")


#Q1.a

h1 = data.frame(x = na.exclude(height$Height1), id = c(rep("Height1", length(height$Height1)-sum(is.na(height$Height1)))))
h2 = data.frame(x = height$Height2, id = c(rep("Height2", nrow(height))))     
combine = rbind(h1,h2)
boxplot(x~id, data=combine, notch=TRUE)


#b
qqplot(height$Height1, height$Height2, main = "Height 1 vs Height 2" ,xlab = "Height 1", ylab= "Height 2", ylim=c(160,200))
abline(0,1, col="red")


plot(sort(height$Height2), sort(quantile(height$Height2, probs = ppoints(height$Height1))))

#c
mean_h1 = mean(na.omit(height$Height1))
mean_h2 = mean(height$Height2)

var_h1 = var(na.omit(height$Height1))
var_h2 = var(height$Height2)
n = var(na.omit(height$Height1))+ var(height$Height2)
p1 = var_h1/n
p2 = var_h2/n
c(p1,p2)

set.seed(1)
y = rep(0, 100)
u = runif(100)

for (i in 1:100){
  if(u[i]<p1) {
    y[i] = rnorm(1, mean = mean_h1, sd = sqrt(var_h1))
  }else{y[i] = rnorm(1, mean= mean_h2, sd = sqrt(var_h2))
  }
}


qqplot(na.omit(combine[,1]),y, xlab = "Heights", ylab="Mixture" ,main ="QQ Plot Mixture")
abline(0,1, col="red")


#d
mixture_q <- quantile(y, probs = c(0.25,0.5,0.75), type=5)
height_q <- quantile(combine[,1], probs = c(0.25,0.5,0.75), type=5)
rb <-rbind(mixture_q,height_q)
c(cb)
IQR = rb[,3]-rb[,1]
result = c(rb[,1]-1.5*IQR, rb[,3]+1.5*IQR)

c(result[1],mixture_q,result[3])
c(result[2],height_q,result[4])

#Q2.a

complife  <- read.csv("complife.csv",header=TRUE, sep=",")
drop(t(complife))
n = nrow(complife)
p = (1:n - 1/2)/n
Qp = -log(1-p)
plot(Qp, t(complife), main = "QQ Plot Exponential", xlab = "exponential quantile", ylab = "sample quantile")

#b
plot(qlnorm(p), t(complife), main = "QQ plot LogNormal", xlab = "lognormal quantile", ylab = "Sample quantile")

#c
qqnorm(t(complife))
abline(a=mean(t(complife)), b=sd(t(complife)), col="red")


#3.a

n3 = 100
uni = runif(n3,2,6)
qqnorm(uni)
abline(a=mean(uni), b=sd(uni), col = "red")

#b
t.dist = rt(n3, df = 5)
qqnorm(t.dist)
abline(a=mean(t.dist), b=sd(t.dist), col = "red")

#c
set.seed(1)
y = rep(0, n3)
u = runif(n3)
for (i in 1:n3){
  if(u[i]<0.75) {
    y[i] = rnorm(1, mean = 5, sd = 4)
  }else{y[i] = rnorm(1, mean= 10, sd = 4)
  }
}

qqnorm(y)
abline(a=mean(y), b = sd(y), col = "red")

#d

set.seed(1)
y1 = rep(0, n3)
u1 = runif(n3)
for (i in 1:n3){
  if(u1[i]<0.6) {
    y1[i] = rnorm(1, mean = 3, sd = 1)
  }else{y1[i] = rnorm(1, mean= 3, sd = 6)
  }
}

qqnorm(y1)
abline(a=mean(y1), b = sd(y1), col = "red")
