install.packages("xgboost")
library(xgboost)

#Load the data, split it into a train_full and a test set.  Split the train_full data set further
# into a validation and training set.
#Methods like random forest or logistic regression will get to use the full train_full data set,
#since they don't need a validation set.  Our boosting algorithm will see the train set and use the valid set 
#to tune parameters.
marketing = read.csv('C:\\Users\\zil20\\Desktop\\Machine Learning 2\\wk2\\marketing.csv')
set.seed(1)
idx.test = sample(1:nrow(marketing),floor(0.2*nrow(marketing)))
test = marketing[idx.test,]
train_full = marketing[-idx.test,]
#Split off another piece of our training set as a validation set.  We will use this for tuning
idx.valid = sample(1:nrow(train_full),floor(0.25*nrow(train_full)))
valid = train_full[idx.valid,]
train = train_full[-idx.valid,]

#Fit logistic regression
fitlm = glm(y~.,data = train_full, family='binomial')
guess_lm = predict(fitlm,newdata=test, type='response')

#Fit a balanced random forest
install.packages("randomForest")
library(randomForest)
nsmall = sum(train$y=='yes')
forest_bal = randomForest(train_full[,1:8], train_full[,9], strata=train_full$y, sampsize=c(nsmall,nsmall))
guess_bal = predict(forest_bal, test[,1:8], type='prob')[,2]

#Draw roc curves
library(pROC)
roc(test[,9], guess_lm, col='blue', plot = TRUE, add=FALSE)
roc(test[,9], guess_bal, col='green', plot = TRUE, add=TRUE)

library(Matrix)
#Reformat the data for xgboost
train_expanded = sparse.model.matrix(y ~ .-1, data = train)
valid_expanded = sparse.model.matrix(y ~ .-1, data = valid)
test_expanded = sparse.model.matrix(y ~ .-1, data = test)
train_y = (train$y == 'yes')
valid_y = (valid$y == 'yes')
test_y = (test$y == 'yes')
dtrain = xgb.DMatrix(data=train_expanded, label=train_y)
dvalid = xgb.DMatrix(data=valid_expanded, label=valid_y)
dtest = xgb.DMatrix(data=test_expanded, label=test_y)

#Time to get started!

#Part (a)
xgbfit1<-xgb.train(list(objective='binary:logistic'), dtrain, nround=10, verbose=2)
guess_xgb<-predict(xgbfit1, newdata = dtest, type='response')
xgb_pred<-rep('no', dim(test)[1])
xgb_pred[guess_xgb>0.5]<-'yes'
misclass1<-mean(xgb_pred!=test$y)
misclass1
roc(test[,9], guess_xgb, col='red', plot = TRUE, add=TRUE)


#Part (b) cross validation
watchlist<-list(train=dtrain, validation=dvalid)

vector=c()
nrounds<-c(240, 250, 260)
max_depths<-c(5, 10, 15)
etas<-c(0.02, 0.03, 0.04)
scale_pos_weights<-c(0.4, 0.5, 0.6)
subsamples<-c(0.6, 0.7, 0.8)
for (i in nrounds)
{
  for (j in max_depths)
  {
    for (k in etas)
    {
      for (l in scale_pos_weights)
      {
        for (m in subsamples) 
        {
          fit<-xgb.train(list(objective='binary:logistic', eval_metric='error'), dtrain, nround = i, max_depth = j, eta = k, scale_pos_weight = l, subsample = m, watchlist = watchlist)
          vector<-c(vector, i, j, k, l, m, min(fit$evaluation_log$train_error), min(fit$evaluation_log$validation_error))
        }
      }
    }
  }
}
cv_mat<-matrix(vector, nrow=7)
rownames(cv_mat)<-c('nround', 'max_depth', 'eta', 'scale_pos_weight', 'subsample', 'train error', 'validation error')
cv_mat[,which.min(cv_mat[6,])]
cv_mat[,which.min(cv_mat[7,])]

#Part (c)
best1<-xgb.train(list(objective='binary:logistic'), dtrain, nround=260, max_depth = 15, eta = 0.04, scale_pos_weight = 0.6, subsample = 0.8, verbose = 0)
guess_best1<-predict(best1, newdata = dtest, type='response')
best1_pred<-rep('no', dim(test)[1])
best1_pred[guess_best1>0.5]<-'yes'
misclass2<-mean(best1_pred!=test$y)
misclass2
roc(test[,9], guess_best1, col='hotpink', plot=TRUE, add=TRUE)

best2<-xgb.train(list(objective='binary:logistic'), dtrain, nround=250, max_depth = 15, eta = 0.03, scale_pos_weight = 0.6, subsample = 0.8, verbose = 0)
guess_best2<-predict(best2, newdata = dtest, type='response')
best2_pred<-rep('no', dim(test)[1])
best2_pred[guess_best2>0.5]<-'yes'
misclass3<-mean(best2_pred!=test$y)
misclass3
roc(test[,9], guess_best2, col='yellow', plot=TRUE, add=TRUE)

legend(0.2, 0.8, legend=c('Logistic', 'Random Forest', 'Naive XGB', 'Best XGB 1', 'Best XGB 2'), col=c('blue', 'green', 'red', 'hotpink', 'yellow'), lty=1:1, cex=0.8)

#calculate auc
b1<-xgb.train(list(objective='binary:logistic', eval_metric='auc'), dtrain, nround = 260, max_depth = 15, eta = 0.04, scale_pos_weight = 0.6, subsample = 0.8, verbose = 1, watchlist = watchlist)
max(b1$evaluation_log$train_auc)
b2<-xgb.train(list(objective='binary:logistic', eval_metric='auc'), dtrain, nround = 250, max_depth = 15, eta = 0.03, scale_pos_weight = 0.6, subsample = 0.8, verbose = 1, watchlist = watchlist)
max(b2$evaluation_log$train_auc)

#Part (d)
imp_matrix1<-xgb.importance(colnames(test_expanded), model=best1)
xgb.plot.importance(imp_matrix1, rel_to_first=TRUE, xlab='Relative importance')
imp_matrix2<-xgb.importance(colnames(test_expanded), model=best2)
xgb.plot.importance(imp_matrix2, rel_to_first = TRUE, xlab='Relative importance')
