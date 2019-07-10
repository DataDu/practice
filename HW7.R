#Problem 1

set.seed(0)
n = 1000
lambda = 3
uniform <- runif(n)
x <- -log(1-uniform)/lambda
hist(x,freq=F,main = "Histogram", col = "red")
lines(density(x))

q = qexp(ppoints(length(x)),3)
plot(q,sort(x), main = "Exponential QQ Plot", xlab = "Theoretical Q", ylab = "Sample Q")

#Problem 2
rm(list=ls())
B <- 1000
n <- c(5,10,25,50,100)

#a
for(i in 1:5){
  N <- n[i]
  Ystar = replicate(B, (mean(rexp(N,2))) - 0.5)/(0.5/sqrt(N))
  hist(Ystar, main = paste("Plot from Exp(2) n =",N,sep=""))
  curve(dnorm(x),add=TRUE)
  qqnorm(Ystar, main = paste("Normal QQplot from Exp(2) n=",N,sep=""))
  qqline(Ystar, col ="red")

  }

#b

for(i in 1:5){
  N <- n[i]
  Ystar = replicate(B, (mean(runif(N,-1,1))) - 0.5)/(0.5/sqrt(N))
  hist(Ystar, main = paste("Plot from Exp(2) n =",N,sep=""))
  curve(dnorm(x),add=TRUE)
  qqnorm(Ystar, main = paste("Normal QQplot from Exp(2) n=",N,sep=""))
  qqline(Ystar, col ="red")
  
}