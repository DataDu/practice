#Q1
#a
pgeom(q=2,prob=.25)


#Q2

ppois(9,4)
1-ppois(9,4)


#Q3
arg <- c(46,15,5,3,5,1,1)
turk <- c(60, 9, 4, 2, 0, 1, 0)

#total number classes
k_arg = length(arg) - 1
k_turk = length(turk) - 1
n = 0:6

meta_arg <- log(arg) + log(factorial(0:k_arg)) - log(sum(arg))
meta_turk <- log(turk) + log(factorial(0:k_turk)) - log(sum(turk))

plot(0:k_arg, meta_arg, xlab='K classes', ylab = 'Count', main = 'Argentina')
lm_arg <- lm(meta_arg~n)
abline(lm_arg$coefficients[1],lm_arg$coefficients[2], col="red")

plot(0:k_turk, meta_turk, xlab='K classes', ylab = 'Count', main = 'Turkey')

#Argentina
p_arg_zero = arg[1]/sum(arg)
p_arg_one = arg[2]/sum(arg)
p_arg_z = p_arg_one/p_arg_zero

p_arg_zero1 = arg[2]/sum(arg)
p_arg_one1 = arg[3]/sum(arg)
p_arg_z1 = p_arg_one1/p_arg_zero1

p_arg_z < p_arg_z1
data.frame(p_arg_z,p_arg_z1)

#Turkey
p_turk_zero = turk[1]/sum(turk)
p_turk_one = turk[2]/sum(turk)
p_turk_z = p_turk_one/p_turk_zero

p_turk_zero1 = turk[2]/sum(turk)
p_turk_one1 = turk[3]/sum(turk)
p_turk_z1 = p_turk_one1/p_turk_zero1

p_turk_z < p_turk_z1
data.frame(p_turk_z,p_turk_z1)

#Q3c
#Sample Mean & Variance
s_mean_arg = sum((n)*arg/sum(arg))
s_mean_turk = sum((n)*turk/sum(turk))
data.frame(s_mean_arg, s_mean_turk)


#Sample Variance
s_var_arg = var(rep(n,arg))
s_var_turk = var(rep(n,turk))
data.frame(s_var_arg, s_var_turk)

#Q3d
s_mean_arg = sum((n)*arg/sum(arg))
s_mean_turk = sum((n)*turk/sum(turk))



#Argentina
nk = 
r.est_arg = ((s_mean_arg)^2)/(sum((n^2)*arg/sum(arg))-s_mean_arg^2-s_mean_arg)
meta_arg_ng <- log(arg) - log(choose(r.est_arg + n -1,n)) - log(sum(arg))
plot(n,meta_arg_ng, xlab = "K", ylab = "Measure", main = "Argentina")

data.frame(s_mean_arg,r.est_arg)


#Turkey
r.est_turk = ((s_mean_turk)^2)/(sum((n^2)*turk/sum(turk))-s_mean_turk^2-s_mean_turk)
meta_turk_ng <- log(turk) - log(choose(r.est_turk + n -1,n)) - log(sum(turk))
plot(n, meta_turk_ng, xlab = "K", ylab = "Measure", main = "Turkey")
data.frame(s_mean_turk,r.est_turk)

#Q3e

arg.freq <- exp(lgamma(n + r.est_arg) - lgamma(r.est_arg) - lgamma(n+1) +
      r.est_arg*(log(r.est_arg) - log(s_mean_arg + r.est_arg)) + n*log(1-r.est_arg/(r.est_arg + s_mean_arg)))

turk.freq <- exp(lgamma(n + r.est_turk) - lgamma(r.est_turk) - lgamma(n+1) +
                  r.est_turk*(log(r.est_turk) - log(s_mean_turk + r.est_turk)) + n*log(1-r.est_turk/(r.est_turk + s_mean_turk)))

arg.argfreq = data.frame(arg, sum(arg)*arg.freq)
turk.turkfreq = data.frame(turk, sum(turk)*turk.freq)

#Q4
setwd("C:\\Users\\User\\Desktop\\Fall 2018\\Applied Stat TP\\HW4")
BinomPois <- read.csv("BinomPois.csv",header=TRUE, sep=",")

#a.1

dbinom(3,10,0.3)
dpois(3,3)

# (ii)
hist(BinomPois$x, col=rgb(1,0,0,.5), xlim = c(0,10), ylim = c(0,300), main = "Comparison X and Y",xlab = "X and Y Variables")
hist(BinomPois$y, col = rgb(0,0,1,.5), add = T)
legend("topright", c("X","Y"), fill = c("red","blue"))
box()

# (iii)
x = table(BinomPois$x)
y = table(BinomPois$y)
kx = as.numeric(names(x))
ky = as.numeric(names(y))
nx = as.vector(x)
ny = as.vector(y)

bin.x <- log(nx) - log(choose(10, kx)) - log(sum(nx))
plot(kx, bin.x, xlab = "K", ylab = "Measure", main = "Binomialness Plot X")
lm.bin = lm(bin.x~kx)
abline(lm.bin$coefficients[1], lm.bin$coefficients[2], col="red")

pois.x <- log(nx) + log(factorial(kx)) - log(sum(nx))
plot(kx, pois.x, xlab = "K", ylab = "Measure", main = "Poissonness Plot X")
lm.pois = lm(pois.x~kx)
abline(lm.pois$coefficients[1], lm.pois$coefficients[2], col="red")


#(b)
#i

dbinom(3, 1000, 0.003)
dpois(3, 3)


#ii
hist(BinomPois$y, col=rgb(1,0,0,.5), xlim = c(0,10), ylim = c(0,300), main = "Comparison Y and Z",xlab = "Y and Z Variables")
hist(BinomPois$z, col = rgb(0,0,1,.5), add = T)
legend("topright", c("Y","Z"), fill = c("red","blue"))
box()


#iii

z = table(BinomPois$z)
kz = as.numeric(names(z))
nz = as.vector(z)

bin.z <- log(nz) - log(choose(1000, kz)) - log(sum(nz))
plot(kz, bin.z, xlab = "K", ylab = "Measure", main = "Binomialness Plot Z")
lm.bin.z = lm(bin.z~kz)
abline(lm.bin.z$coefficients[1], lm.bin.z$coefficients[2], col="red")

pois.z <- log(nz) + log(factorial(kz)) - log(sum(nz))
plot(kz, pois.z, xlab = "K", ylab = "Measure", main = "Poissonness Plot Z")
lm.pois.z = lm(pois.z~kz)
abline(lm.pois.z$coefficients[1], lm.pois.z$coefficients[2], col="red")
