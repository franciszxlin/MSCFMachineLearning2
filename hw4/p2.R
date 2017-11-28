#Problem 2
summary(train)
dim(train)
summary(test)
dim(test)
#Part (a)
fmlstring="y~X1"
for (i in 3:21)
{
  fmlstring=paste(fmlstring, names(train)[i], sep="+")
}
fml=as.formula(fmlstring)
lm_fit=lm(fml, data=train)
train_lm_mse=mean(lm_fit$residuals^2)
train_lm_mse
lm_test_pred=predict(lm_fit, newdata=test)
test_lm_mse=mean((lm_test_pred-test$y)^2)
test_lm_mse
test_lm_mse*0.1
#Part (b)
cvridgefit=cv.glmnet(as.matrix(train[,2:21]), as.matrix(train[,1]), alpha=0)
cvridgefit$lambda.1se
train_ridge_pred=predict(cvridgefit, newx=as.matrix(train[,2:21]), s="lambda.1se")
train_ridge_mse=mean((train_ridge_pred-as.matrix(train[,1]))^2)
train_ridge_mse
test_ridge_pred=predict(cvridgefit, newx=as.matrix(test[,2:21]), s="lambda.1se")
test_ridge_mse=mean((test_ridge_pred-as.matrix(test[,1]))^2)
test_ridge_mse
#Part (c)
cvlassofit=cv.glmnet(as.matrix(train[,2:21]), as.matrix(train[,1]), alpha=1)
cvlassofit$lambda.1se
train_lasso_pred=predict(cvlassofit, newx=as.matrix(train[,2:21]), s="lambda.1se")
train_lasso_mse=mean((train_lasso_pred-as.matrix(train[,1]))^2)
train_lasso_mse
test_lasso_pred=predict(cvlassofit, newx=as.matrix(test[,2:21]), s="lambda.1se")
test_lasso_mse=mean((test_lasso_pred-as.matrix(test[,1]))^2)
test_lasso_mse

