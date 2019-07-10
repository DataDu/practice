rm(list=ls())

#1 (a)
nk <- c(160, 60, 20, 8, 2)
N <- sum(nk)
sum(nk*0:4)/sum(nk)

mle <- log(nk) + log(factorial(0:4)) - log(N)
plot(0:4, mle, xlab = "Count", ylab = "MLE" )

#d
mi <-250*0.5^(0:4)*exp(-0.5)/factorial(0:4)
xi <- c(160,60,20,8,2)
chisq <- sum((xi-mi)^2/mi)



#2a
prop.test(x=c(75,65), n=c(100,100), conf.level =0.95, alternative ="greater")



#b
Group.A<-cbind(c(rep("recover",75),rep("not recover",25)),1)
Group.B<-cbind(c(rep("recover",65),rep("not recover",35)),2)
comb <-rbind( Group.A, Group.B)
Table.A.B <-table( comb[,1], comb[,2])
chisq.test(Table.A.B)


#c
A=0.75
B=0.65
l = A-B-qnorm(0.025,lower.tail =FALSE)*sqrt(A*(1-A)/100+B*(1-B)/100)
u = A-B+qnorm(0.025,lower.tail =FALSE)*sqrt(A*(1-A)/100+B*(1-B)/100)
c(l,u)

#d
odd=75*35/(65*25)
c(exp(log(odd) -qnorm(0.025,lower.tail=FALSE) *sqrt(1/75 + 1/25 + 1/65 + 1/35)),exp(log(odd) +qnorm(0.025,lower.tail=FALSE) *sqrt(1/75 + 1/25 + 1/65 + 1/35)#3.a
faculty <-array(c(11,15,60,30,20,50,61,28),dim=c(2,2,2),
             dimnames=list(Ideology =c("Left","Right"),Tolerance =c("Low","High"),
               College =c("College 1","College 2")))

#b
mantelhaen.test(faculty)


#4.a
cancer.A=515
cancer.B=527
TreatmentA=515+107
TreatmentB=527+95
prop.test(x=c(cancer.A,cancer.B),n=c(TreatmentA,TreatmentB),correct=FALSE)
#4.b
cancer.matchedpair <-matrix(c(510,5,17,90), nrow=2,
                     dimnames=list("6th month"=c("sur","die"),
                                   "immediate"=c("sur","die")))
cancer.matchedpair
mcnemar.test (cancer.matchedpair)
