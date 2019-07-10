library(ggplot2)


setwd("C:\\Users\\User\\Desktop\\Fall 2018\\Applied Stat TP\\HW2")
property <- read.csv("ct.property.csv",header=FALSE, sep=",")
violent <- read.csv("ct.violent.csv",header=FALSE, sep=",")



p_05to09 = property[16,]
p_10to14 = property[17,]

comb <- rbind(p_05to09, p_10to14)
comb1 <- as.matrix(comb[,-c(1,2)])
comb2 <- rbind(as.numeric(comb1[1,]),as.numeric(comb1[2,]))


t <- c("Prop Crime", "Burglary", "Larcency Theft", "Motor Theft")
colnames(comb2) <- t
rownames(comb2) <- c("05-09", "10-14")


#Bar Graph Property
bar <- barplot(height = as.matrix(comb2), col = c("red","blue"), 
               legend = c("05-09","10-14"), beside = TRUE,
              names.arg = c("Prop Crime", "Burglary", "Larcency", "Vehicle"), ylab = "Property", main = "Property Violent in CT")
legend("topright", c("05-09","10-14"), fill = c("red","blue"))


library(plotrix)
pie(comb2[1,-c(1,1)])
pie(comb2[2,-c(1,1)])
pct <- comb2[1,-c(1,1)]/sum(comb2[1,-c(1,1)])*100
pct1 <- round(pct, digits = 2)
lbls <- paste(t[-c(1,1)], pct1)
lbls <- paste(lbls, "%", sep="")
pie3D(comb2[1,-c(1,1)], labels = lbls, explode=0.15, main = "Property Violent in CT 05-09")


pct2 <- comb2[2,-c(1,1)]/sum(comb2[2,-c(1,1)])*100
pct3 <- round(pct2, digits = 2)
lbls2 <- paste(t[-c(1,1)], pct3)
lbls2 <- paste(lbls2, "%", sep="")
pie3D(comb2[1,-c(1,1)], labels = lbls2, explode=0.15, main = "Property Violent in CT 10-14")

#Problem 2
# (b)

bpdrug <- read.csv("BP_Drug.csv",header=TRUE, sep=",")
bf.bp90nd.over = bpdrug$BeforeBP[which(bpdrug$BeforeBP>=90)]
bf.bp.below90 = bpdrug$BeforeBP[which(bpdrug$BeforeBP < 90)]
bf.bp <- cbind(length(bf.bp.below90),length(bf.bp90nd.over))
colnames(bf.bp) <- c("Before BP < 90", "Before BP >= 90")
rownames(bf.bp) <- c("Number")
bf.bp

af.bp90nd.over = bpdrug$AfterBP[which(bpdrug$AfterBP>=90)]
af.bp.below90 = bpdrug$AfterBP[which(bpdrug$AfterBP < 90)]
af.bp <- cbind(length(af.bp.below90), length(af.bp90nd.over))
colnames(af.bp) <- c("After BP < 90", "After BP >= 90")
rownames(af.bp) <- c("Number")
af.bp

#c
min(bpdrug$BeforeBP)
max(bpdrug$BeforeBP)
min(bpdrug$AfterBP)
max(bpdrug$AfterBP)

#d

bp.bf.placebo = bpdrug$BeforeBP[which(bpdrug$Treatment==0)]
bp.bf.drug = bpdrug$BeforeBP[which(bpdrug$Treatment==1)]

bp.af.placebo = bpdrug$AfterBP[which(bpdrug$Treatment==0)]
bp.af.drug = bpdrug$AfterBP[which(bpdrug$Treatment==1)]

hist(bp.bf.placebo, col=rgb(1,0,0,.5), xlim = c(70,120), ylim = c(0,18), main = "Before/After for Placebo/Drug", xlab = "BP Measurement")
hist(bp.bf.drug ,col = rgb(0,0,1,.5), breaks= 10, add = T)
hist(bp.af.placebo, col = rgb(1,1,0.3,0.5), add = T)
hist(bp.af.drug, col = rgb(0.4,0.4,0.4,0.5), breaks = 10, add = T)
legend("topright", c("bf.placebo","bf.drug","af.placebo","af.drug"), fill = c("red","blue","yellow","grey"))
box()



#Before/After Placebo
hist(bp.bf.placebo, col=rgb(1,0,0,.5), xlim = c(80,120), ylim = c(0,10), main = "Before/After for Placebo", xlab = "BP Measurement")
hist(bp.af.placebo ,col = rgb(0,0,1,.5), add = T)
legend("topright", c("bf.placebo","af.placebo"), fill = c("red","blue"))
box()

#Before/After Drug
hist(bp.bf.drug, col=rgb(1,1,0.3,0.5), breaks = 10 ,xlim = c(70,120), ylim = c(0,18), main = "Before/After for Drug", xlab = "BP Measurement")
hist(bp.af.drug, col = rgb(0.4,0.4,0.4,0.5), add = T)
legend("topright", c("bf.drug","af.drug"), fill = c("yellow","grey"))
box()

#e

dif.placebo <- bp.bf.placebo - bp.af.placebo
dif.drug <- bp.bf.drug-bp.af.drug
hist(dif.placebo, col=rgb(1,1,0.3,0.5) ,xlim = c(0,20), ylim = c(0,12), main = "Difference for Placebo/Drug", xlab = "Diff BP Measurement")
hist(dif.drug, col = rgb(0.4,0.4,0.4,0.5), breaks = 10 ,add = T)
legend("topright", c("placebo","drug"), fill = c("yellow","grey"))
box()

#f

bpdrug.m <- subset(bpdrug, Gender=="M", select = c("Gender", "BeforeBP", "AfterBP"))
bpdrug.f <- subset(bpdrug, Gender=="F", select = c("Gender", "BeforeBP", "AfterBP"))

m.bf = bpdrug.m$BeforeBP
m.af = bpdrug.m$AfterBP

f.bf = bpdrug.f$BeforeBP
f.af = bpdrug.f$AfterBP


#Before/After male
hist(m.bf, col=rgb(1,0,0,.5), xlim = c(80,120), ylim = c(0,10), breaks = 10 ,main = "Before/After for Male", xlab = "BP Measurement")
hist(m.af,col = rgb(0,0,1,.5), add = T)
legend("topright", c("bf.male","af.male"), fill = c("red","blue"))
box()

#Before/After female
hist(f.bf, col=rgb(1,1,0.3,0.5) ,xlim = c(70,120), ylim = c(0,10), breaks = 10 , main = "Before/After for Female", xlab = "BP Measurement")
hist(f.af, col = rgb(0.4,0.4,0.4,0.5),breaks = 10,add = T)
legend("topright", c("bf.female","af.female"), fill = c("yellow","grey"))
box()

#Difference
difbp.f <- f.af-f.bf
difbp.m <- m.af-m.bf
hist(difbp.f, col=rgb(1,0,0,.5), xlim =c(-30,10), ylim = c(0,8) , breaks = 10 ,main = "Diff for Male/Female", xlab = "Difference")
hist(difbp.m, col = rgb(0,0,1,.5), breaks = 10 ,add = T)
legend("topleft", c("female","male"), fill = c("red","blue"))

#Problem3

boxplot(BeforeBP~Treatment, data = bpdrug, main = "Blood Press Before by Treatment", xlab= "Treatment", ylab = "Blood Pressure Measure")
bpdrug.bf.drug <- subset(bpdrug, Treatment==1, select = c("BeforeBP"))
bpdrug.bf.placebo <- subset(bpdrug, Treatment==0, select = c("BeforeBP"))
legend("topleft", c("0 Placebo","1 Drug"))
summary(bpdrug.bf.drug)
summary(bpdrug.bf.placebo)



#Notch BoxPlot
long.df = data.frame(x = c(bpdrug$BeforeBP, bpdrug$AfterBP), id = c(rep("BeforeBP", nrow(bpdrug)), rep("AfterBP", nrow(bpdrug))))
boxplot(x ~ id, data = long.df, notch=TRUE)


#problem 4
y <- c(125,136,148,157,160,168,197,206,211,417)

#problem 5

simu <- read.csv("simudat.csv",header=TRUE, sep=",")
datasimu = data.frame(x = c(simu$y1, simu$y2), id = c(rep("y1", nrow(simu)), rep("y2", nrow(simu))))
boxplot(x~id, data= datasimu, notch = TRUE)

dist = seq(from = 0, to =1 , by = 1/length(y1))
y1 = sort(subset(datasimu, id == "y1")[,1], decreasing = FALSE)
y2 = sort(subset(datasimu, id == "y2")[,1], decreasing = FALSE)

qqplot(x=dist, y = y1)
abline(0, 1, col = 'red')
qqplot(x=dist, y = y2)
abline(0, 1, col = 'red')

library(ggplot2)
library(sm)
library(vioplot)
#Notch BoxPlot
long.df = data.frame(x = c(bpdrug$BeforeBP, bpdrug$AfterBP), id = c(rep("BeforeBP", nrow(bpdrug)), rep("AfterBP", nrow(bpdrug))))
boxplot(x ~ id, data = long.df, notch=TRUE)

#violin Plot
df = data.frame(x = c(y1, y2), id = c(rep(("y1"), nrow(y1)), rep(("y2"), nrow(y2))))
vioplot(y1,y2, names=c("y1", "y2"), col = ("gold"),title("Violin Plots of Simudat"))

#Empirical quantile quantile plot

qqplot(x=y1, y=y2, main = "Empiral QQ Plot")
abline(0, 1, col = 'red')
