# install necessary packages
install.packages("glmnet")
library(glmnet)
install.packages("randomForest")
library(randomForest)
install.packages("pROC")
library(pROC)
# import data
marketing<-read.csv(file="C:\\Users\\zil20\\Desktop\\Machine Learning 2\\wk1\\marketing.csv")
# split data into train set and test set
set.seed(1)
idx.test=sample(1:nrow(marketing),floor(0.2*nrow(marketing)))
test=marketing[idx.test,]
train=marketing[-idx.test,]
# Part (a)
frac_success=sum(train$y=='yes')/length(train$y)
frac_success
# Part (b)
# Check if there is any missing data
sapply(train, function(x) sum(is.na(x)))
# Look at the summary of all the data
summary(train)
# fit a losgistic regression using all the variables
fit1<-glm(y~age+job+marital+education+default+balance+housing+loan, family=binomial, data=train)
# predict on test data using the model and calculate misclassification rate
fit1prob<-predict(fit1, newdata = test, type = "response")
fit1pred<-rep('no', dim(test)[1])
fit1pred[fit1prob > 0.5] <- 'yes'
misclassrate1<-1-mean(fit1pred==test$y)
misclassrate1
# Part (c)
sillypred<-rep('no', dim(test)[1])
misclassrate2<-1-mean(sillypred==test$y)
misclassrate2
# Part (d)
# 1000 most likely to say yes clients by my logistic model
newtest<-cbind(test, fit1prob)
sortednewtest1000<-newtest[order(-newtest$fit1prob),][1:1000,]
sorted_frac_yes<-sum(sortednewtest1000$y=='yes')/nrow(sortednewtest1000)
sorted_frac_yes
# a random set of 1000 clients
randomnewtest1000<-newtest[sample(nrow(newtest),1000), ]
random_frac_yes<-sum(randomnewtest1000$y=='yes')/nrow(randomnewtest1000)
random_frac_yes
# Part (e)
roc(test$y,fit1prob,plot=TRUE)
# Part (f) 
# fit a random forest using all the variables
fit2<-randomForest(y~age+job+marital+education+default+balance+housing+loan, data=train)
# predict on test data using the model and calculate the misclassification rate
fit2pred<-predict(fit2, newdata=test)
misclassrate3<-1-mean(fit2pred==test$y)
misclassrate3
# analyze the 1000 most likely to say yes clients by random forest model
fit2prob<-predict(fit2, newdata=test, type="prob")
newtest2<-cbind(test, fit2prob[,2])
sortednewtest21000<-newtest2[order(-newtest2$`fit2prob[, 2]`),][1:1000,]
sorted_frac_yes2<-sum(sortednewtest21000$y=='yes')/nrow(sortednewtest21000)
sorted_frac_yes2
# Add the ROC curve for the random forest fit
plot(roc(test$y, fit2prob[,2]), add=TRUE, col="red")
# Part (g)
fit3<-randomForest(y~age+job+marital+education+default+balance+housing+loan, strata=train$y, sampsize=c(sum(train$y=='yes'), sum(train$y=='yes')), data=train)
# predict on test data using the model and calculate the misclassification rate 
fit3pred<-predict(fit3, newdata=test)
misclassrate4<-1-mean(fit3pred==test$y)
misclassrate4
# analyze the 1000 most likely to say yes clients by balanced random forest model
fit3prob<-predict(fit3, newdata=test, type="prob")
newtest3<-cbind(test, fit3prob[,2])
sortednewtest31000<-newtest3[order(-newtest3$`fit3prob[, 2]`),][1:1000,]
sorted_frac_yes3<-sum(sortednewtest31000$y=='yes')/nrow(sortednewtest31000)
sorted_frac_yes3
# Add the ROC curve for the balanced random forest fit
plot(roc(test$y, fit3prob[,2]), add=TRUE, col="blue")
# Part (h)  
varImpPlot(fit3, main="Variable Importance Plot on Balanced Forest")
# Part (i)
partialPlot(fit3, pred.data=train[sample(1:nrow(train), 1000),], x.var='balance', which.class='yes')
partialPlot(fit3, pred.data=train[sample(1:nrow(train), 1000),], x.var='age', which.class='yes')
