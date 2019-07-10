setwd("C:\\Users\\User\\Desktop\\Fall 2018\\Applied Stat TP\\HW3")
stereo <- read.csv("stereo.csv",header=FALSE, sep=",")
stereo1 <- read.csv("stereo.csv",header=TRUE, sep=",")



stereo.df = data.frame(x = c(stereo$V1, stereo$V2), id = c(rep("V1", nrow(stereo)), rep("V2", nrow(stereo))))
boxplot(x ~ id, data = stereo.df)





#pdf("boxplot", width = 8, height = 6)
boxplot(stereo1$NV, stereo1$VV, names = c("NV", "VV"), main = "Box plot"
        ,sub = "stereo", col = c("black", "red"))
#dev.off()
