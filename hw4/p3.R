#Problem 3
load("C:/Users/zil20/Desktop/Github Repos/MSCFMachineLearning2/hw4/problem3.RData")
summary(train)
head(train)
dim(train)
summary(test)
dim(test)
#Part (a)
#Make the formula
fmlstring="y~X1"
for (i in 3:51)
{
  fmlstring=paste(fmlstring, names(train)[i], sep="+")
}
fml=as.formula(fmlstring)
#Fit the logistic model
glm_fit=glm(fml, family="binomial", data=train)
train_glm_pred=predict(glm_fit, type="response")
train_glm_pred=(train_glm_pred>0.5)
train_glm_mce=mean(train_glm_pred!=train$y)
train_glm_mce
test_glm_pred=predict(glm_fit, newdata=test, type="response")
test_glm_pred=(test_glm_pred>0.5)
test_glm_mce=mean(test_glm_pred!=test$y)
test_glm_mce
#Part (b)
#Fit the logistic lasso
require("glmnet")
lasso_fit=cv.glmnet(as.matrix(train[,2:51]), as.factor(train$y), type.measure="class", alpha=1, family="binomial")
lasso_fit$lambda.1se
lasso_fit$cvm[which(lasso_fit$glmnet.fit$lambda==lasso_fit$lambda.1se)]
opt_lamb_pos=which(lasso_fit$glmnet.fit$lambda==lasso_fit$lambda.1se)
lasso_fit$glmnet.fit$beta[lasso_fit$glmnet.fit$beta[,opt_lamb_pos]!=0 ,opt_lamb_pos]
lasso_fit$glmnet.fit[opt_lamb_pos]
train_lasso_pred=predict(lasso_fit, as.matrix(train[,2:51]), s="lambda.1se", type="response")
train_lasso_pred=(train_lasso_pred>0.5)
train_lasso_mce=mean(train_lasso_pred!=train$y)
train_lasso_mce
test_lasso_pred=predict(lasso_fit, as.matrix(test[,2:51]), s="lambda.1se", type="response")
test_lasso_pred=(test_lasso_pred>0.5)
test_lasso_mce=mean(test_lasso_pred!=test$y)
test_lasso_mce
#Part (c)
test_lasso_mce*0.9
#Part (d)
require("randomForest")
#Make the formula
fmlstring="as.factor(y)~X1"
for (i in 3:51)
{
  fmlstring=paste(fmlstring, names(train)[i], sep="+")
}
fml=as.formula(fmlstring)
rf_fit=randomForest(fml, data=train)
oob_rf_mce=mean(rf_fit$predicted!=train$y)
oob_rf_mce
train_rf_pred=predict(rf_fit, newdata=train)
train_rf_mce=mean(train_rf_pred!=train$y)
train_rf_mce
test_rf_pred=predict(rf_fit, newdata=test)
test_rf_mce=mean(test_rf_pred!=test$y)
test_rf_mce
#Balanced random forest
brf_fit=randomForest(fml, strata=as.factor(train$y), sampsize=c(sum(as.factor(train$y)==0), sum(as.factor(train$y)==0)), data=train)
oob_brf_mce=mean(brf_fit$predicted!=train$y)
oob_brf_mce
train_brf_pred=predict(brf_fit, newdata = train)
train_brf_mce=mean(train_brf_pred!=train$y)
train_brf_mce
test_brf_pred=predict(brf_fit, newdata=test)
test_brf_mce=mean(test_brf_pred!=test$y)
test_brf_mce
#Part (e)
require("mgcv")
gam_fit=gam(as.factor(y)~s(X20)+s(X31)+s(X40), family="binomial", data=train)
train_gam_pred=predict(gam_fit, type="response")
train_gam_pred=(train_gam_pred>0.5)
train_gam_mce=mean(train_gam_pred!=train$y)
train_gam_mce
test_gam_pred=predict(gam_fit, newdata=test, type="response")
test_gam_pred=(test_gam_pred>0.5)
test_gam_mce=mean(test_gam_pred!=test$y)
test_gam_mce
