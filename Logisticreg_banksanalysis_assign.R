
banks=read.csv(file.choose(),head=TRUE,sep=";")
sum(is.na(banks))
banks <- na.omit(banks) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value

View(banks)
attach(banks)
banks['job']<-as.numeric(factor(banks$job,levels=c("admin.","technician","services","management","retired","entrepreneur","unknown","self-employed","housemaid","student","unemployed","blue-collar"),labels=c(0:11)))
banks['education']<-as.numeric(factor(banks$education,levels=c("primary","secondary","tertiary","unknown"),labels=c("0","1","2","3")))
banks['default']<-as.integer(as.logical(banks$default=="yes"))
banks['housing']<-as.integer(as.logical(banks$housing=="yes"))
banks['y']<-as.integer(as.logical(banks$y=="yes"))
banks['loan']<-as.integer(as.logical(banks$loan=="yes"))
banks['contact']<-as.numeric(factor(banks$contact,levels=c('telephone','cellular','unknown'),labels=c("0","1","2")))
banks['month']<-as.numeric(factor(banks$month,levels=c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'),labels=c(1:12)))
banks['poutcome']<-as.numeric(factor(banks$poutcome,levels=c('success','failure','unknown','other'),labels=c("0","1","2","3")))
banks['marital']<-as.numeric(factor(banks$marital,levels=c('single','married','divorced'),labels=c("0","1","2")))
#as.integer(banks['marital'])
str(banks)

View(banks)
bankslm <-lm(banks$y~.,data=banks)
pred1 <- predict(bankslm,banks)
pred1
plot(banks$y,pred1)
# We can no way use the linear regression technique to classify the data
plot(pred1)


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(banks$y~.,data=banks,family="binomial")

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
prob <- predict(model,banks,type="response")
View(prob)
summary(model)


# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,banks$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 89.5



# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob,banks$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.8))
# More area under the ROC Curve better is the logistic regression model obtained

library(pROC)
roccurve<-roc(banks$y~ prob)
plot(roccurve)

auc=auc(banks$y~prob)
auc
