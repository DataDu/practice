setwd("C:\\Users\\User\\Desktop\\Fall 2018\\Applied Stat TP\\HW5")
simd <- read.csv("simdata.txt",header=T, sep=",")[,-1]

XY = simd[,1] + simd[,2]

#Q1
#a
hist(XY, col=rgb(1,0,0,.5), xlim = c(0,20), ylim = c(0,40), main = "Simulated Data XY", xlab = "Variable")
hist(simd[,3] ,col = rgb(0,0,1,.5), add = T)
legend("topright", c("X+Y","Z"), fill = c("red","blue"))
box()

#b
plot(sort(XY), sort(simd[,3]),main = "QQ PLOT", xlab = "XY", ylab = "Z")
abline(0,1)


#c
xxx = 3*simd[,1]
hist(xxx, col=rgb(1,0,0,.5), xlim = c(0,50), ylim = c(0,40), main = "Simulated Data 3X vs W", xlab = "Variable")
hist(simd[,4] ,col = rgb(0,0,1,.5), add = T)
legend("topright", c("3X","W"), fill = c("red","blue"))
box()

#d
plot(sort(xxx), sort(simd[,4]),main = "QQ PLOT", xlab = "3X", ylab = "W")
abline(0,1)


#e
X = simd[,1]
div = X/XY
U = simd[,5]
hist(x/XY, col=rgb(1,0,0,.5), xlim = c(0,1), ylim = c(0,25), main = "Simulated Data X/(X+Y) vs U", xlab = "Variable")
hist(U ,col = rgb(0,0,1,.5), add = T)
legend("topright", c("X/XY","U"), fill = c("red","blue"))
box()

#f
plot(sort(div), sort(U),main = "QQ PLOT", xlab = "X/(X+Y)", ylab = "U")
abline(0,1)

#2 a

prob = 1 - pnorm(16*60,mean = 1200,sd = 160)

#b

qnorm(0.2,mean = 1200, sd = 160)

#c
p = 0.3 + pnorm(1250,1200,160)
qnorm(p,1200,160)


#3 a

pbeta(0.3,2,5) - pbeta(0.2,2,5)

#b

dpois(2,2)-dpois(4,2)
