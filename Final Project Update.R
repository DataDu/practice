#=============================================================================
# 2018 Travelers Case Competition
#     Method: Logestic Regression
#=============================================================================
install.packages("gplots")
install.packages("lattice")
install.packages("ggplot2")
install.packages("Rcpp")
install.packages("neuralnet")

library("gplots")
library("ROCR")  #to construct ROC curve
library("My.stepwise")  #Stepwise selection
library("mice")       # Impute missing variables
library("lattice")
library("ggplot2")
library("car")  #for VIF
library("rpart")  #for classification tree
library("randomForest")  #for random forest
library("gbm")  #for gbm
rm(list=ls())

# ============== #
# 1. Preparation #
# ============== #

setwd("C:\\Users\\User\\Desktop\\Fall 2018\\Travelers")
train = read.csv("uconn_comp_2018_train.csv")[-1]
train <- na.omit(train) #deleting rows with NA. Can be done by train <- na.omit(train) as well

head(train)


# Data cleaning
train1 <- subset(train, train$fraud != -1)
train1 <- subset(train1, train$annual_income!=-1)
train1 <- subset(train1, train$zip_code !=0)
train1 <- subset(train1, train1$age_of_driver < 90)



# Constructing dummy variables for qualitative variables

#Gender(Female)
train1$gender <- ifelse(train1$gender == "M", 1, 0)

#Channel(Phone)
train1$channel.channelBroker <- ifelse(train1$channel == "Broker", 1, 0)
train1$channel.channelOnline <- ifelse(train1$channel == "Online", 1, 0)

#Living Status(Own)
train1$living_status.Rent <- ifelse(train1$living_status == "Rent", 1, 0)

#Vehicle Category (Medium)
train1$vehicle_category.Compact <- ifelse(train1$vehicle_category == "Compact", 1, 0)
train1$vehicle_category.Large <- ifelse(train1$vehicle_category == "Large", 1, 0)

#Vehicle Color (white)
train1$vehicle_color.black <- ifelse(train1$vehicle_color == "black", 1, 0)
train1$vehicle_color.blue <- ifelse(train1$vehicle_color == "blue", 1, 0)
train1$vehicle_color.gray <- ifelse(train1$vehicle_color == "gray", 1, 0)
train1$vehicle_color.other <- ifelse(train1$vehicle_color == "other", 1, 0)
train1$vehicle_color.red <- ifelse(train1$vehicle_color == "red", 1, 0)
train1$vehicle_color.silver <- ifelse(train1$vehicle_color == "silver", 1, 0)


#Claim Day of Week
train1$claim_day_of_week.Weekend <- ifelse(train1$claim_day_of_week == "Saturday" | train1$claim_day_of_week == "Sunday", 1, 0)
train1$claim_day_of_week.Weekday <- ifelse(train1$claim_day_of_week == "Monday" | train1$claim_day_of_week == "Tuesday" | train1$claim_day_of_week == "Wednesday"
                                           | train1$claim_day_of_week == "Thursday" | train1$claim_day_of_week == "Friday", 1, 0)

#Accident Site (highway)
train1$accident_site.Local <- ifelse(train1$accident_site == "Local", 1, 0)
train1$accident_site.ParkingLot <- ifelse(train1$accident_site == "Parking Lot", 1, 0)


#Zip Code (Co.Denver.S)
train1$AZ.Phoenix <- ifelse(train1$zip_code >= 85000 & train1$zip_code < 86000, 1, 0)
train1$VA.Dules <- ifelse(train1$zip_code >= 20000 & train1$zip_code < 21000, 1, 0)
train1$IA.DesMoine <- ifelse(train1$zip_code >= 50000 & train1$zip_code < 51000, 1, 0)
train1$CO.Denver.N <- ifelse(train1$zip_code >= 80000 & train1$zip_code < 80100, 1, 0)
train1$PA.Pittsburgh <- ifelse(train1$zip_code >= 15000 & train1$zip_code < 16000, 1, 0)

#Claim Date
y <- strptime(train1$claim_date, format="%m/%d/%Y")
train1$claim_date.year = as.numeric(format(y, "%y"))
#Year 2015
train1$year2016 <- ifelse(train1$claim_date.year == 16, 1, 0)

# ============================ #
# 2. Exploratory data analysis #
# ============================ #

# Correlation matrix

train2 <- subset(train1, select = -c(living_status ,gender, claim_day_of_week, accident_site, channel, vehicle_category,
                                     vehicle_color, claim_date, zip_code ))
train1.cov <- cor(train2)


# Stepwise regression

my.variable.list <- c(names(train2))
My.stepwise.glm(Y="fraud", variable.list = my.variable.list, data=train2, sle = 0.05,
                sls = 0.05, myfamily = "binomial")

# reduced model after stepwise selection
mod1 <- glm(fraud ~ past_num_of_claims + accident_site.ParkingLot + high_education_ind + witness_present_ind + 
              marital_status + address_change_ind + annual_income + safty_rating + age_of_vehicle + 
              VA.Dules + accident_site.Local + living_status.Rent + PA.Pittsburgh + year2016 + vehicle_category.Large + claim_est_payout, 
            family=binomial(link = "logit") ,data=train2)
summary(mod1)


# Check to see if there is a multicollinearity problem

vif <- vif(mod1)
vif


# Classification Table
# using p=.18 as threshold for classification:
# this is not based on cross-validated

tab <- table( train2$fraud, mod1$fitted.values>0.165)
addmargins(tab)

#Create results of all models
results <- data.frame(Method = as.numeric(), AUC = as.numeric())

# ROC curve, AUC
pred <- prediction(mod1$fitted.values,train2$fraud)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1)
performance(pred,"auc")

results[1,1] <- "Logistic"
results[1,2] <- performance(pred,"auc")@y.values

#####################Prepare variables for Tree Models################################################
train = read.csv("uconn_comp_2018_train.csv")[-1]
trainf <- train[complete.cases(train[,1:24]),] #deleting rows with NA. Can be done by train <- na.omit(train) as well
trainf <- subset(trainf, trainf$fraud != -1)
trainf <- subset(trainf, trainf$annual_income!=-1)
trainf <- subset(trainf, trainf$zip_code !=0)
trainf <- subset(trainf, trainf$age_of_driver < 90)

#Zip Code
trainf$AZ.Phoenix <- ifelse(trainf$zip_code >= 85000 & trainf$zip_code < 86000, 1, 0)
trainf$VA.Dules <- ifelse(trainf$zip_code >= 20000 & trainf$zip_code < 21000, 1, 0)
trainf$IA.DesMoine <- ifelse(trainf$zip_code >= 50000 & trainf$zip_code < 51000, 1, 0)
trainf$CO.Denver.N <- ifelse(trainf$zip_code >= 80000 & trainf$zip_code < 80100, 1, 0)
trainf$PA.Pittsburgh <- ifelse(trainf$zip_code >= 15000 & trainf$zip_code < 16000, 1, 0)

yf <- strptime(trainf$claim_date, format="%m/%d/%Y")
year = as.numeric(format(yf, "%y"))
trainf <- subset(trainf, select = -c(zip_code, claim_date ))
trainf$year2016 <- ifelse(year == 16, 1, 0)

##############################################################################
#################Prepare variables for Prediction Test Datset#################
##############################################################################
test1 = read.csv("uconn_comp_2018_test.csv")

# Erasing data points that are presumptively wrong
test1$age_of_driver[test1$age_of_driver > 90] <- NA
test1$annual_income[test1$annual_income == -1] <- NA

#year
ytest <- strptime(test1$claim_date, format="%m/%d/%Y")
test1$claim_date.year = as.numeric(format(ytest, "%y"))
#test1$year2015 <- ifelse(test1$claim_date.year == 15, 1, 0)
test1$year2016 <- ifelse(test1$claim_date.year == 16, 1, 0)
#Creating Dummy Variables
test1$accident_site.Local <- ifelse(test1$accident_site == "Local", 1, 0)
test1$accident_site.ParkingLot <- ifelse(test1$accident_site == "ParkingLot", 1, 0)
test1$VA.Dules <- ifelse(test1$zip_code >= 20000 & test1$zip_code < 21000, 1, 0)
test1$PA.Pittsburgh <- ifelse(test1$zip_code >= 15000 & test1$zip_code < 16000, 1, 0)
test1$living_status.Rent <- ifelse(test1$living_status == "Rent", 1, 0)
test1$vehicle_category.Large <- ifelse(test1$vehicle_category == "Large", 1, 0)
test1$AZ.Phoenix <- ifelse(test1$zip_code >= 85000 & test1$zip_code < 86000, 1, 0)
test1$IA.DesMoine <- ifelse(test1$zip_code >= 50000 & test1$zip_code < 51000, 1, 0)
test1$CO.Denver.N <- ifelse(test1$zip_code >= 80000 & test1$zip_code < 80100, 1, 0)


########################################################################################
######################## Classification Tree Models#####################################
########################################################################################
modCT <- rpart(fraud ~ age_of_driver + claim_day_of_week + vehicle_weight +claim_est_payout
               + safty_rating + vehicle_price + past_num_of_claims + liab_prct + accident_site + age_of_vehicle, method="class", cp=0.001, data=trainf)

modCT <- rpart(fraud ~ ., method="class", cp=0.0001, data=trainf)

names(summary(modCT))
printcp(modCT) # display the results 
plotcp(modCT) # visualize cross-validation results 
summary(modCT) # detailed summary of splits

# plot tree 
plot(modCT, uniform=TRUE, main="Classification Tree for Fraud")
text(modCT, use.n=TRUE, all=TRUE, cex=.8)
# ROC curve, AUC
pred2 <- prediction(predict(modCT, trainf)[,2], trainf$fraud)
perf <- performance(pred2, "tpr", "fpr")
plot(perf)
abline(0,1)
performance(pred2,"auc")

results[2,1] <- "Classification Tree"
results[2,2] <- performance(pred2,"auc")@y.values


########################################################################################
################################### Random Forest ######################################
########################################################################################
train_forest <- trainf
train_forest$fraud <- as.factor(train_forest$fraud)
modRF <- randomForest(fraud ~ ., data=train_forest, mtry=10, ntree = 1000, importance = TRUE )
plot(modRF)
summary(modRF)
plot(getTree(modRF, k=2, labelVar=TRUE))


yhat_bag1 <- predict(modRF, train_forest, n.trees = 1000)
dev.new(width=4, height=4)
par(mar=c(6,6,6,6))
plot(trainf$fraud, yhat_bag1, xlab="Fraud", ylab="Bagged Tree model")
abline(0,1)

cforest(fraud ~ ., data=trainf, controls=cforest_control(mtry=2, mincriterion=0))

# ROC curve, AUC
pred4 <- prediction(modRF$votes[,2], train_forest$fraud)
perf <- performance(pred4, "tpr", "fpr")
plot(perf)
abline(0,1)
performance(pred4,"auc")

results[3,1] <- "Random Forest"
results[3,2] <- performance(pred4,"auc")@y.values

#Prediction Not sure
testRF = test1
predRF = predict(modRF, newdata=testRF,type='prob')

# =========== #
# 4. Boosting #
# =========== #


#removing unsignificant variables
modGBM1 <- gbm(fraud~. -IA.DesMoine-policy_report_filed_ind-AZ.Phoenix-vehicle_color-PA.Pittsburgh, data=trainf, 
               shrinkage=0.01, distribution = 'bernoulli', cv.folds=10 ,n.trees=5000, verbose=F)

# check the best iteration number
best.iter = gbm.perf(modGBM1, method="cv")
best.iter
print(best.iter)

pdf("gbm",width=6, height=6)
summary(modGBM1)


# Plot relative influence of each variable
par(mfrow = c(1, 1))
summary(modGBM1, n.trees = best.iter) # using estimated best number of trees
dev.off

summary(modGBM1, n.trees = 1) # using first tree


# ROC curve, AUC
par(mfrow = c(1, 1))

pred5 <- prediction(predict(modGBM1, trainf), trainf$fraud)
perf <- performance(pred5, "tpr", "fpr")
plot(perf)
abline(0,1)
performance(pred5,"auc")

results[4,1] <- "Generalized Boosted Models"
results[4,2] <- performance(pred5,"auc")@y.values


#Ensemble
test_ensemble = test1
test_ensemble$fraud <- (0.25*predGLM + 0.5*predBST + 0.25*predTree2)
test_ensemble$fraud = ifelse(test_ensemble$fraud > 0.165, 1, 0)
test_ensemble <- test_ensemble[,c("claim_number", "fraud")]
write.csv(test_ensemble, file = "Submission Ensemble.csv", row.names = FALSE)
#ensemble package, microsoft data science r package