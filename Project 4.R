setwd("/Users/numerp/Documents/PGP-BABI/Module 5 Predictive Modelling/Project 4")
getwd()
library(readr)
library(readxl)
library(dplyr)
library(psych)
library(car)
library(carData)
library(ggplot2)
library(mice)
library(lattice)
library(nFactors)
library(scatterplot3d)
library(data.table)
library(tidyverse)
library(broom)
library(GGally)
Cellphone=read_xlsx("Cellphone.xlsx",sheet = 2,col_names = TRUE)
head(Cellphone,5)
tail(Cellphone,3)
summary(Cellphone)
str(Cellphone)
attach(Cellphone)
View(Cellphone)
Cellphone$Churn=factor(Cellphone$Churn,levels = c(0,1),
                       labels = c("Customer Using Service","Customer Cancelled Service"))
Cellphone$ContractRenewal=factor(Cellphone$ContractRenewal,levels = c(0,1),
                       labels = c("Customer not renewed","Customer renewed"))
Cellphone$DataPlan=factor(Cellphone$DataPlan,levels = c(0,1),
                                 labels = c("Customer with no data","Customer have data"))
summary(Cellphone)
str(Cellphone)
any(is.na(Cellphone))
Cellphone=na.omit(Cellphone)
dim(Cellphone)
ggpairs(Cellphone[,c("AccountWeeks","DataUsage","CustServCalls","DayMins","DayCalls",
                     "MonthlyCharge","OverageFee","RoamMins")],
        ggplot2::aes(colour=as.factor(Cellphone$Churn)))
ct.data=subset(Cellphone,select = c(Churn,ContractRenewal,DataPlan))
num.data=subset(Cellphone,select = -c(Churn,ContractRenewal,DataPlan))
dim(ct.data)
dim(num.data)
outliers = boxplot(num.data,plot = FALSE)$out
print(outliers)
names(ct.data)
names(num.data)
par(mfrow=c(2,4))
boxplot(AccountWeeks,horizontal = TRUE,
        main = "Active Customers",col = "blue")
boxplot(DataUsage,horizontal = T,
        main = "Gigabytes used by the customers",col = "green")
boxplot(CustServCalls,horizontal = T,
        main = "Number of Calls received by Customer Service",col = "red")
boxplot(DayMins,horizontal = T,
        main = "Customer using Data in Day Time",col = "gold")
boxplot(DayCalls,horizontal = T,
        main = "Customer making calls in Day Time",col = "orange")
boxplot(MonthlyCharge,horizontal = T,
        main = "Customer average month rate",col = "grey")
boxplot(OverageFee,horizontal = T,
        main = "Customer overage fees for last 12 months",col = "brown")
boxplot(RoamMins,horizontal = T,
        main = "Customer average roaming minutes",col = "yellow")
par(mfrow=c(2,4))
hist(AccountWeeks,col = "blue")
hist(DataUsage,col = "green")
hist(CustServCalls,col = "red")
hist(DayMins,col = "gold")
hist(DayCalls,col = "orange")
hist(MonthlyCharge,col = "grey")
hist(OverageFee,col = "brown")
hist(RoamMins,col = "yellow")
#Based on Customer Cancelled Service or not
lr.reg=glm(Churn~AccountWeeks,data = Cellphone,family = binomial)
summary(lr.reg)
lr.reg=glm(Churn~DataUsage,data = Cellphone,family = binomial)
summary(lr.reg)
lr.reg=glm(Churn~CustServCalls,data = Cellphone,family = binomial)
summary(lr.reg)
lr.reg=glm(Churn~DayMins,data = Cellphone,family = binomial)
summary(lr.reg)
lr.reg=glm(Churn~DayCalls,data = Cellphone,family = binomial)
summary(lr.reg)
lr.reg=glm(Churn~MonthlyCharge,data = Cellphone,family = binomial)
summary(lr.reg)
lr.reg=glm(Churn~OverageFee,data = Cellphone,family = binomial)
summary(lr.reg)
lr.reg=glm(Churn~RoamMins,data = Cellphone,family = binomial)
summary(lr.reg)
model=glm(Churn~.,data = Cellphone,family = binomial)
summary(model)
car::vif(model)
par(mfrow=c(1,4))
plot(model)
par(mfrow=c(1,3))
for (a in names(ct.data)) {
  print(a)
  print(round(prop.table(table(Cellphone$Churn,ct.data[[a]])),digits = 3)*100)
  barplot(table(Cellphone$Churn,ct.data[[a]]),
          col = c("blue","yellow"),
          main = names(ct.data[a]))
}
fullmodel=cbind(ct.data,num.data)
names(fullmodel)
names(Cellphone)
#Split train and test dataset
library(caTools)
set.seed(100)
splits=sample.split(fullmodel$Churn,SplitRatio = 0.60)
train=subset(fullmodel,splits==T)
test=subset(fullmodel,splits==F)
prop.table(table(fullmodel$Churn))
prop.table(table(train$Churn))
prop.table(table(test$Churn))
#Train Dataset
cellphonemodel=glm(Churn~.,data = train,family = binomial)
summary(cellphonemodel)
car::vif(cellphonemodel)
library(lmtest)
lrtest(cellphonemodel)
library(pscl)
pR2(cellphonemodel)["McFadden"]
logLik(cellphonemodel)
trainmodel1=glm(Churn~1,data = train,family = binomial)
1-(logLik(cellphonemodel)/logLik(trainmodel1))
logLik(trainmodel1)
#Prediction on train dataset
trainpredict=predict(cellphonemodel,newdata = train,type = "response")
table(train$Churn,trainpredict>0.5)
(1659+51)/nrow(na.omit(train))
#Prediction on test dataset
testpredict=predict(cellphonemodel,newdata = test,type = "response")
table(test$Churn,testpredict>0.5)
(1111+29)/nrow(na.omit(test))
#Logistics Regression
library(rms)
vif(glm(Churn~MonthlyCharge+DayMins,data = Cellphone,family = binomial))
cor(MonthlyCharge,DayMins)
summary(glm(Churn~MonthlyCharge+DayMins,data = Cellphone,family = binomial))
logistic.churn=glm(Churn~MonthlyCharge+DayMins,data = Cellphone,family = binomial)
logistic.churn
logistic.churn$fitted.values                 
exp(0.01338)
churn.predicted=ifelse(logistic.churn$fitted.values<0.20,
                       "Customer with no service","Customer with Service")
table(Churn,churn.predicted)
pROC::roc(Churn,logistic.churn$fitted.values)
par(mfrow=c(1,1))
pROC::plot.roc(Churn,logistic.churn$fitted.values)
#Linear Discriminant Analysis
library(MASS)
lda.churn=lda(Churn~MonthlyCharge+DayMins,data = Cellphone,CV=TRUE)
lda.churn
lda(Churn~MonthlyCharge+DayMins,data = Cellphone)
plot(Churn,lda.churn$posterior[,2])
churn.predicted=ifelse(lda.churn$posterior[,2]<0.15,
                       "Customer Using Service","Customer Cancelled Service")
churn.predicted
table(Churn,churn.predicted)
pROC::plot.roc(Churn,lda.churn$posterior[,2])
table(Churn,lda.churn$class)
#Naive Bayes
library(e1071)
nb.churn=naiveBayes(Churn~MonthlyCharge+DayMins,data = Cellphone)
nb.churn
nb.churn.predict=predict(nb.churn,type = "raw",newdata = Cellphone)
nb.churn.predict
plot(Churn,nb.churn.predict[,2])
#KNN
library(class)
dim(Cellphone)
set.seed(500)
index=sample(3333,2333)
ktrain=Cellphone[index,]
dim(ktrain)
ktest=Cellphone[-index,]
dim(ktest)
names(ktrain)
kchurn=knn(ktrain[,c(7,9)],ktest[,c(7,9)],ktrain$Churn,k=5)
table(ktest$Churn,kchurn)
kchurn=knn(ktrain[,c(7,9)],ktest[,c(7,9)],ktrain$Churn,k=6)
table(ktest$Churn,kchurn)
kchurn=knn(ktrain[,c(7,9)],ktest[,c(7,9)],ktrain$Churn,k=7)
table(ktest$Churn,kchurn)
kchurn=knn(ktrain[,c(7,9)],ktest[,c(7,9)],ktrain$Churn,k=8)
table(ktest$Churn,kchurn)
kchurn=knn(ktrain[,c(7,9)],ktest[,c(7,9)],ktrain$Churn,k=9)
table(ktest$Churn,kchurn)
kchurn=knn(ktrain[,c(7,9)],ktest[,c(7,9)],ktrain$Churn,k=10)
table(ktest$Churn,kchurn)
kchurn=knn(ktrain[,c(7,9)],ktest[,c(7,9)],ktrain$Churn,k=11)
table(ktest$Churn,kchurn)
kchurn=knn(ktrain[,c(7,9)],ktest[,c(7,9)],ktrain$Churn,k=21)
table(ktest$Churn,kchurn)
glm(Churn~MonthlyCharge+DayMins,data = Cellphone,family = binomial)
plot(ktest$Churn,
     predict(glm(Churn~MonthlyCharge+DayMins,data = Cellphone,family = binomial),
             newdata = ktest,type = "response"))
pROC::roc(ktest$Churn,
          predict(glm(Churn~MonthlyCharge+DayMins,data = Cellphone,family = binomial),
                              newdata = ktest,type = "response"))
#AUC and ROC for Train and Test Dataset
library(ROCR)
roctrainpredict=prediction(trainpredict,train$Churn)
as.numeric(performance(roctrainpredict,"auc")@y.values)
perf=performance(roctrainpredict,"tpr","fpr")
plot(perf,col = "black",lty=2,lwd=2)
plot(perf,lwd=3,colorize=TRUE)
roctestpredict=prediction(testpredict,test$Churn)
as.numeric(performance(roctestpredict,"auc")@y.values)
perf=performance(roctestpredict,"tpr","fpr")
plot(perf,col = "blue",lty=2,lwd=2)
plot(perf,lwd=3,colorize=TRUE)
library(blorr)
blr_step_aic_both(cellphonemodel,details = FALSE)
finalcellphonemodel=glm(Churn~ContractRenewal+CustServCalls+DayMins+DataPlan
                        +OverageFee+RoamMins+DayCalls,data = train,family = binomial(link = "logit"))
summary(finalcellphonemodel)
print(exp(finalcellphonemodel$coefficients))
print(exp(cellphonemodel$coefficients))
blr_rsq_mcfadden(finalcellphonemodel)
blr_rsq_mcfadden_adj(finalcellphonemodel)
pR2(finalcellphonemodel)
library(pROC)
myroc=roc(train$Churn,trainpredict)
coords(myroc,"best",ret = "threshold")
myroc1=roc(test$Churn,testpredict)
coords(myroc1,"best",ret = "threshold")
table(train$Churn,trainpredict>0.177)
(1393+317)/nrow(na.omit(train))
table(test$Churn,testpredict>0.127)
(825+315)/nrow(na.omit(test))
library(ineq)
KS=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
KS
AUC=performance(roctrainpredict,"auc")
AUC
AUC=performance(roctestpredict,"auc")
AUC
GINI=ineq(train$Churn,type = "Gini")
GINI
GINI=ineq(test$Churn,type = "Gini")
GINI
library(caret)
confusionMatrix(test$Churn,sample(test$Churn))
confusionMatrix(train$Churn,sample(train$Churn))