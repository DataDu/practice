rm(list=ls())
setwd("C:\\Users\\User\\Desktop\\Fall 2018\\Applied Stat TP\\HW9")
indus <- read.table("indusacc(4).dat",header=TRUE)
install.packages("optim")
library(optim)

#Q1 (b)
qnorm(0.025,mean=1.85714,sd = 0.5714)
qnorm(0.975,mean=1.85714,sd = 0.5714)



#Q2

L  = qbeta(0.025,3, 9)
U  = qbeta(0.975,3,9)

L_HPD = function(u) plogis(u)
U_HPD = function(u,v) plogis(u)+
  exp(plogis(u,lower.tail=FALSE,log.p=TRUE)+plogis(v,log.p=TRUE))
opt = function(par) {
  x =par[1]
  y =par[2]
  (dbeta(U_HPD(x,y),3,9) -dbeta(L_HPD(x),3,9))^2 +
    (pbeta(U_HPD(x,y),3,9)-pbeta(L_HPD(x),3,9)-0.95)^2
}
fit = optim(par=c(qlogis(L), qlogis((U-L)/(1-L))), fn=opt,method="Nelder-Mead")
#Lower HPD and Upper HPD
c(L_HPD(fit$par[1]),U_HPD(fit$par[1],fit$par[2]))


install.packages("nleqslv")
library(nleqslv)
nonlin = function(x) dbeta(qbeta(pbeta(x,3,9)+0.95,3,9),3,9) -dbeta(x,3,9)
fit_nonlin = nleqslv(0.025,fn=nonlin)

# Lower and Upper HPD solved using nleqslv
c(fit_nonlin$x,qbeta(pbeta(fit_nonlin$x,3,9)+0.95,3,9))

install.packages("boa")
library(boa)
MCMC = rbeta(10000,3,9)
seed(1)
boa.hpd(MCMC,0.05)


#Q3
#from HW8
#a
s1 = 2.78
s2 = 2.97
s3 = 2.875
-2*log(s3^(100*(s2+s1))/((s1^(100*s1))*(s2^(100*s2))))


