install.packages("glmnet")
library(glmnet)
#You get to observe a bunch of data (that turns out to be worthless)
#Returns a data set with 1000 variables and n data points
#The first 1000 columns are the X variables (called X1,...X100)
#The last column is the response (called y)
get_data = function(n){
  p=1000
  X = matrix(rnorm(n*p),ncol=p)
  y = rnorm(n)
  dat = data.frame(X,y)
  dat
}

#Part (a)
#Generate some data
n = 100
set.seed(1)
#Note, your y and X variables are all in this data frame. See the comments for get_data for details.
dataset = get_data(100)
cors<-cor(dataset[,1:1000], dataset[,1001])
hist(cors, main="Correlations between y and each x", xlab="Correlation")
#Part (b)
Xgood<-dataset[, which(abs(cors)>0.25)]
#Part (c)
train<-Xgood[1:(0.8*n),]
test<-Xgood[(0.8*n+1):n,]
cvlassofit<-cv.glmnet(as.matrix(train), as.matrix(dataset[1:(0.8*n),1001]))
plot(cvlassofit)
cvlassofit$lambda.1se
train_predictions<-predict(cvlassofit, newx=as.matrix(train), s="lambda.1se")
train_mse<-mean((train_predictions-as.matrix(dataset[1:(0.8*n),1001]))^2)
train_mse
test_predictions<-predict(cvlassofit, newx=as.matrix(test), s="lambda.1se")
test_mse<-mean((test_predictions-as.matrix(dataset[(0.8*n+1):n,1001]))^2)
test_mse
#Part (d)
anotherdataset=get_data(100)
Xanother<-anotherdataset[, which(abs(cors)>0.25)]
another_predictions<-predict(cvlassofit, newx=as.matrix(Xanother), s="lambda.1se")
another_mse<-mean((another_predictions-as.matrix(anotherdataset[,1001]))^2)
another_mse
