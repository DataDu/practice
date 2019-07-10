rm(list=ls())
setwd("C:\\Users\\User\\Desktop\\Fall 2018\\Applied Stat TP\\HW8")
indus <- read.table("indusacc.dat",header=TRUE)

x1 = log(factorial(indus[,1])) + log(indus[,2]) - log(sum(indus[,2]))
x2 = log(factorial(indus[,1])) + log(indus[,4]) - log(sum(indus[,4]))
x3 = log(factorial(indus[,1])) + log(indus[,6]) - log(sum(indus[,6]))

k = 0:10
plot(k,x1, main = "Poisoness Plot", xlab = "Freq", ylab = "N1k")
plot(k,x2, main = "Poisoness Plot", xlab = "Freq", ylab = "N2k")
plot(k,x3, main = "Poisoness Plot", xlab = "Freq", ylab = "N3k")

l1 = sum(k*indus[,2])/sum(indus[,2])
l2 = sum(k*indus[,4])/sum(indus[,4])
l3 = sum(k*indus[,6])/sum(indus[,6])
cbind(l1,l2,l3)

se.l1 = sqrt(l1/sum(indus[,2]))
se.l2 = sqrt(l2/sum(indus[,4]))
se.l3 = sqrt(l3/sum(indus[,6]))
cbind(se.l1,se.l2,se.l3)

CI.l1_L = l1 - 1.96*se.l1
CI.l1_U = l1 + 1.96*se.l1
cbind(CI.l1_L, CI.l1_U)

CI.l2_L = l2 - 1.96*se.l2
CI.l2_U = l2 + 1.96*se.l2
cbind(CI.l1_L, CI.l2_U)

CI.l3_L = l3 - 1.96*se.l3
CI.l3_U = l3 + 1.96*se.l3
cbind(CI.l3_L, CI.l3_U)

#C

cbind(CI.l1_L*exp(-l1), CI.l1_U*exp(-l1))
cbind(CI.l2_L*exp(-l2), CI.l2_U*exp(-l2))
cbind(CI.l3_L*exp(-l3), CI.l3_U*exp(-l3))


#d
#lambda
c("Lambda",sum(indus$k*indus$n1.k.)/100)
#Tau(lambda)
c("Tau(lambda)",indus[2,2]/sum(indus$n1.k.))

life <- read.csv("life2018.csv",header=TRUE)
k = 1:30
sum(k*life)/sum(life)



#Pivotal Quantity
l_low = sum(life$x)/qchisq(0.05,nrow(life))
l_high = sum(life$x)/qchisq(0.95,nrow(life))
cbind(1/l_low,1/l_high)

#MLE
l_mle = 1/mean(life$x)
m_high = l_mle + 1.64*l_mle/sqrt(30)
m_low = l_mle - 1.64*l_mle/sqrt(30)
cbind(m_low, m_high)
