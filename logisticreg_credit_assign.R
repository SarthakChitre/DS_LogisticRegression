credit <- read.csv(file.choose()) # Choose the Data set
View(credit)
attach(credit)
colnames(credit)
credit <- credit[,-1]# Removing the first column which is is an Index
credit['card']<-as.integer(card=="yes")
credit['owner']<-as.integer(owner=="yes")
credit['selfemp']<-as.integer(owner=="yes")
str(credit)
# Preparing a linear regression 
mod_lm <- lm(card~.,data=credit)
pred1 <- predict(mod_lm,credit)
pred1
plot(credit$card,pred1)
# We can no way use the linear regression technique to classify the data
plot(pred1)

# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(card~.,data=credit,family="binomial")
summary(model)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
prob <- predict(model,credit,type="response")
summary(model)
View(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,card)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #0.86



# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob,credit$card)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

library(pROC)
roccurve<-roc(credit$card~ prob)
plot(roccurve)

auc=auc(credit$card~prob)
auc


