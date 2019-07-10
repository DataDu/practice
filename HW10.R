rm(list=ls())
setwd("C:\\Users\\User\\Desktop\\Fall 2018\\Applied Stat TP\\HW10")

#1(a)
twins <-read.table("twins.dat", head =T)
twins$dif <- twins[,1] - twins[,2]
qqnorm(twins[,1], main = "Sample1 vs Normal Plot")
qqnorm(twins[,2], main = "Sample2 vs Nomral Plot")
qqnorm(twins[,3], main = "Diff vs Normal Plot")
boxplot(twins[,1], twins[,2], twins[,3], names = c("Sample1", "Sample2", "Diff"))


#(b)
n <- nrow(twins)
shapiro.test(twins$Sample1)
twins_l1 = mean(twins[,1]) - qt(0.995, n-1)*sd(twins[,1])/sqrt(n)
twins_u1 = mean(twins[,1]) + qt(0.995, n-1)*sd(twins[,1])/sqrt(n)
c("Sample1 99% C.I",twins_l1, twins_u1)

shapiro.test(twins$Sample2)
twins_l2 = mean(twins[,2]) - qt(0.995, n-1)*sd(twins[,2])/sqrt(n)
twins_u2 = mean(twins[,2]) + qt(0.995, n-1)*sd(twins[,2])/sqrt(n)
c("Sample2 99% C.I", twins_l2, twins_u2)

#(c)

shapiro.test(twins$dif)
twins_l3 = mean(twins[,3]) - qt(0.995, n-1)*sd(twins[,3])/sqrt(n)
twins_u3 = mean(twins[,3]) + qt(0.995, n-1)*sd(twins[,3])/sqrt(n)
c("Sample3 99% C.I", twins_l3, twins_u3)

#(d)
t.test(twins$dif, alternative = "two.sided", confi.level = 0.99)

#(e)
B = sum(twins$dif>0)
2*min((1/2^20)*sum(choose(n,B:n)),(1/2^20)*sum(choose(n,0:B)))

#(f)
wilcox.test(twins$Sample1,twins$Sample2, paired = TRUE,conf.level = 0.995)

#2 (a)
ratweight <-read.csv("ratweight.csv", head =T)
#ratweight$dif <- ratweight$control[,1] - ratweight[,2]
qqnorm(ratweight$control, main = "Control vs QQ Normal")
qqline(ratweight$control)
shapiro.test(ratweight$control)

qqnorm(ratweight$ozone, main = "Ozone vs QQ Normal")
qqline(ratweight$ozone)
shapiro.test(ratweight$ozone)

n_control=23
n_ozone=22
control_mu=mean(ratweight$control,na.rm=TRUE)
ozone_mu=mean(ratweight$ozone,na.rm=TRUE)
var_control=var(ratweight$control,na.rm=TRUE)
var_ozone=var(ratweight$ozone,na.rm=TRUE)

t_stat=(control_mu-ozone_mu)/sqrt((var_control/n_control)+(var_ozone/n_ozone))
df_uneq=floor({((var_control/n_control)+(var_ozone/n_ozone))^2}/{((var_control/n_control)^2)/(n_control-1)+((var_ozone/n_ozone)^2)/(n_ozone-1)})
t_crit=qt(0.05,df=df_uneq)
c(t_stat,abs(t_crit))
t.test(ratweight$control,ratweight$ozone,alternative="two.sided", conf.level = 0.9)


#b
wilcox.test(ratweight$control, ratweight$ozone, conf.level = 0.9, exact = FALSE)


#3
#a

sodion <-read.table("sodion.dat", head =T)
var.test(sodion[,1], sodion[,2], conf.level =0.9)

S0 = c(log(var(sodion[,1])),log(var(sodion[,2])))
mat = matrix(0,1,30)
mat2 = matrix(0,1,30)
A_mat = matrix(0,1,30)
A_Mat2 = matrix(0,1,30)

for(i in 1:30){
  mat[i] = log(var(sodion[-i,1]))
  mat2[i] = log(var(sodion[-i,2]))
  A_mat[i] = 30*S0[1] - 29*mat[i]
  A_mat2[i] = 30*S0[2] - 29*mat2[i]
}
Plasma_mu = mean(A_mat)
Eryth_mu = mean(A_mat2)
Plasma_V = 1/(30*29)*sum((A_mat-Plasma_mu)^2)
Eryth_V = 1/(30*29)*sum((A_mat2-Eryth_mu)^2)
Q = (Eryth_mu - Plasma_mu)/ sqrt(Plasma_V + Eryth_V)


shapiro.test(sodion$Plasma)
shapiro.test(sodion$Erythrocytes)

#3c
rm(list=ls())
sodion <-read.table("sodion.dat", head =T)

n1= length(sodion$Plasma)
n2 = length(sodion$Erythrocytes)
B = 100000
Y1 = sodion$Plasma
Y2 = sodion$Erythrocytes
boot = function(n1,n2,B){
  # n1, n2 = sample size for two samples
  # B = number of bootstrap samples
  
  # initialize variables
  est=matrix(0,B,2)
  colnames(est) =c("mdiff", "vratio")              # column names of matrix est
  out = vector("list",2)
  names(out)=c("bootstats","actual")
  
  actual = c(mean(Y1)-mean(Y2),var(Y1)/var(Y2))
  names(actual) = c("mdiff", "vratio")  
  for (i in 1:B){                                  # boot loop
    idx1 = sample(1:n1,n1,replace=TRUE)            # select indices for bootstrap sample for Y1
    idx2 = sample(1:n2,n2,replace=TRUE)            # select indices for bootstrap sample for Y2
    Ystar1= Y1[idx1]
    Ystar2= Y2[idx2]   
    est[i,1]=mean(Ystar1)-mean(Ystar2)             # difference of two means
    est[i,2]=var(Ystar1)/var(Ystar2)               # ratio of two variances
  }
  out$bootstats = est
  out$actual    = actual
  return(out)
}


out = boot(n1,n2,10000)
colMeans(out$bootstats)

# 95% CI --  from 1000 bootstrap samples,take 25th and 975th sorted observations
smdiff = sort(out$bootstats[,1], decreasing=FALSE)
cat(smdiff[c(25,975)],"\n")
hist(smdiff,breaks=30)

#Variance ratio
hist(out$bootstats[,2],breaks=30)
abline(v=quantile(out$bootstats[,2],0.95 ,col="blue",lwd=3))
       


# 95% C.I
vardiff = sort(out$bootstats[,2], decreasing=FALSE)
cat(vardiff[c(250,9750)],"\n")
