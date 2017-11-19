#Load the data, split it into a train_full and a test set.  Split the train_full data set further
# into a validation and training set.
#Methods like random forest or logistic regression will get to use the full train_full data set,
#since they don't need a validation set.  Our boosting algorithm will see the train set and use the valid set 
#to tune parameters.
marketing = read.csv('marketing.csv')
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
library(randomForest)
nsmall = sum(train$y=='yes')
forest_bal = randomForest(train_full[,1:8], train_full[,9], strata=train_full$y, sampsize=c(nsmall,nsmall))
guess_bal = predict(forest_bal, test[,1:8], type='prob')[,2]

#Draw roc curves
roc(test[,9], guess_lm, col='blue', plot = TRUE, add=FALSE)
roc(test[,9], guess_bal, col='green', plot = TRUE, add=TRUE)


library(xgboost)
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