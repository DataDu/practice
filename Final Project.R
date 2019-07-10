rm(list=ls())

load("C:/Users/User/Desktop/Fall 2018/Applied Stat TP/project/Crime.RData")
head(crime)
Data = crime
#P1 Crimes by Category

#Channel(Phone)
Data$Crime.Code.name <- ifelse(Data$Crime.Code == 110, "Hom", 
                            ifelse(Data$Crime.Code == 626, "I.P-S.A",
                                   ifelse(Data$Crime.Code == 624, "BATTERY-S.A",
                                          ifelse(Data$Crime.Code == 210, "ROB",
                                                 ifelse(Data$Crime.Code == 220,"A.ROB",
                                                        ifelse(Data$Crime.Code == 236,"I.P-A.A",
                                                               ifelse(Data$Crime.Code == 946, "OTHER",
                                                                      ifelse(Data$Crime.Code == 756, "Poss/Bomb",0))))))))

                            
n <- Data$Crime.Code.name %in% c("Hom", "I.P-S.A", "BATTERY-S.A",
                                   "ROB", "A.ROB", "I.P-A.A", "OTHER", "Poss/Bomb")
#Crime Date
y <- strptime(Data$Date.Occurred, format="%m/%d/%Y")
Data$Date.Occured.year = as.numeric(format(y, "%y"))

id <- which(n==TRUE)
Data1 <- Data[id, ]

year <- table(Data1$Date.Occured.year)


tab1 <- t(table(Data1$Crime.Code.name, Data1$Date.Occured.year))

par(mfrow = c(2, 2))
barplot(tab1[1,], main="2010 Crimes Type")
barplot(tab1[2,], main="2011 Crimes Type")
barplot(tab1[3,], main="2012 Crimes Type")
barplot(tab1[4,], main="2013 Crimes Type")
par(mfrow = c(2, 2))
barplot(tab1[5,], main="2014 Crimes Type")
barplot(tab1[6,], main="2015 Crimes Type")
barplot(tab1[7,], main="2016 Crimes Type")
barplot(tab1[8,], main="2017 Crimes Type")

par(mfrow = c(1,1))
barplot(tab1[1,], main="2010 Crimes Type")
barplot(tab1[8,], main="2017 Crimes Type")
barplot(tab1[8,], main="2017 Crimes Type",col=c("green","red","yellow","blue","orange","darkblue","purple","black"),
        xlab="Different Type of Crimes", ylab="Number of Crimes")
#P2 Type of Crimes and Victoms of Gender
tab2 <- t(table(Data1$Crime.Code.name, Data1$Victim.Sex))[c(3, 5),]
par(mfrow = c(2, 2))
pie(tab2[,1], main="Homicide")
pie(tab2[,2], main="INTIMATE PARTNER - SIMPLE ASSAULT") #Highest crime
pie(tab2[,3], main="BATTERY - SIMPLE ASSAULT")   # 2nd highest crime
pie(tab2[,4], main="ROBBERY")
par(mfrow = c(2, 2))
pie(tab2[,5], main="ATTEMPTED ROBBERY")  #3rd most often
pie(tab2[,6], main="INTIMATE PARTNER - AGGRAVATED ASSAULT")
pie(tab2[,7], main="OTHER MISCELLANEOUS CRIME")
pie(tab2[,8], main="WEAPONS POSSESSION/BOMBING")

par(mfrow = c(1,3))
pie(tab2[,2], main="Intim.P-SA")  #3rd most often
pie(tab2[,4], main="ROB")
pie(tab2[,3], main="Bat-SA")

#Crimes vs Victim Ethnic Group
tab3 <- t(table(Data1$Crime.Code.name, Data1$Victim.Descent))
tab3 <- tab3[c(3, 4, 9, 12, 14,19), ]
par(mfrow = c(2, 2))
pie(tab3[,1], main="Homicide")
pie(tab3[,2], main="INTIMATE PARTNER - SIMPLE ASSAULT")
pie(tab3[,3], main="BATTERY - SIMPLE ASSAULT")
pie(tab3[,4], main="ROBBERY")
par(mfrow = c(2, 2))
pie(tab3[,5], main="ATTEMPTED ROBBERY")
pie(tab3[,6], main="INTIMATE PARTNER - AGGRAVATED ASSAULT")
pie(tab3[,7], main="OTHER MISCELLANEOUS CRIME")
pie(tab3[,8], main="WEAPONS POSSESSION/BOMBING")


par(mfrow = c(1,3))
pie(tab3[,2], main="INTIMATE PARTNER - SIMPLE ASSAULT")
pie(tab3[,4], main="ROBBERY")
pie(tab3[,3], main="BATTERY - SIMPLE ASSAULT")


#Crimes vs Status
tab4 <- t(table(Data1$Crime.Code.name, Data1$Status.Code))[c(4,5,7,8,9),]
par(mfrow = c(2, 2))
pie(tab4[,1], main="Homicide")
pie(tab4[,2], main="INTIMATE PARTNER - SIMPLE ASSAULT")
pie(tab4[,3], main="BATTERY - SIMPLE ASSAULT")
pie(tab4[,4], main="ROBBERY")
par(mfrow = c(2, 2))
pie(tab4[,5], main="ATTEMPTED ROBBERY")
pie(tab4[,6], main="INTIMATE PARTNER - AGGRAVATED ASSAULT")
pie(tab4[,7], main="OTHER MISCELLANEOUS CRIME")
pie(tab4[,8], main="WEAPONS POSSESSION/BOMBING")

par(mfrow = c(1,3))
pie(tab4[,2], main="INTIMATE PARTNER - SIMPLE ASSAULT")
pie(tab4[,4], main="ROBBERY")
pie(tab4[,3], main="BATTERY - SIMPLE ASSAULT")

# Chi-Square analysis
chi1 <- chisq.test(tab1)
chi2 <- chisq.test(tab2)
chi3 <- chisq.test(tab3)
chi4 <- chisq.test(tab4)

#C.I for Age of Victims
t.test(Data1$Victim.Age)
