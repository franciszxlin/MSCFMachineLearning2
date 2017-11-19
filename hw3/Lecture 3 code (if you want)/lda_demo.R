library(MASS)
set.seed(1)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Generates good LDA data
get_data = function(n){
  K = 3
  pi0 = c(.5,.3,.2)
  mu = cbind(c(0,0),c(1,2),c(-2,1))*2
  Sigma = matrix(c(1,.5,.5,.9),2)*1.6
  X = rbind(
    mvrnorm(pi0[1]*n,mu[,1],Sigma),
    mvrnorm(pi0[2]*n,mu[,2],Sigma),
    mvrnorm(pi0[3]*n,mu[,3],Sigma)
  )
  y = c(rep(1,pi0[1]*n),rep(2,pi0[2]*n),rep(3,pi0[3]*n))
  X = as.data.frame(X)
  names(X) = c('x1','x2')
  list(X=X,y=y)
}

#Generate training data
train = get_data(200)
test = get_data(1000)

#Plot the points
plot(train$X,col=cbPalette[train$y+1],pch=20)

#Fit LDA on the training set
lda_fit = lda(train$X,train$y)
#See what's in the returned objects
names(lda_fit)
str(lda_fit)

#Suppose that we want to draw the classification regions.
#You can always simply evaluate your classifier at every point
#This line generates all points in a grid
Xgrid = as.matrix(expand.grid(seq(-7,6,length.out=50),seq(-3,7,length.out=50)))
#Make predictions on the grid
b = predict(lda_fit,Xgrid)
#Plot the points
plot(train$X,col=cbPalette[train$y+1],pch=19)
#Draw the predictions across the grid
points(Xgrid[,1],Xgrid[,2],col=cbPalette[as.numeric(b[[1]])+1],pch=4)

#Make predictions on the test data
guesses = predict(lda_fit,newdata=test$X)
#See what the returned object contains
str(guesses)
#Compute the misclassification rate for the test set
mean(guesses$class != test$y)



#What if we want to use QDA?

#Fit qda, also found in the MASS library 
qda_fit = qda(train$X,train$y)

#Plot the prediction region as above
Xgrid = as.matrix(expand.grid(seq(-7,6,length.out=50),seq(-3,7,length.out=50)))
b = predict(qda_fit,Xgrid)
plot(train$X,col=cbPalette[train$y+1],pch=19)
points(Xgrid[,1],Xgrid[,2],col=cbPalette[as.numeric(b[[1]])+1],pch=4)

#Predict on the test set and compute misclassification
guesses = predict(qda_fit,newdata=test$X)
mean(guesses$class != test$y)

#We don't see much improvement, if any

##############################
#Now we will try another setting where QDA should be better (unequal covariance matrices)

#Generates good QDA data
get_data2 = function(n){
  K = 3
  pi0 = c(.5,.3,.2)
  mu = cbind(c(0,0),c(1,2),c(-2,1))*2
  Sigma = matrix(c(1,.8,.8,.9),2)*1.6
  Sigma2 = matrix(c(1,-.7,-.7,.9),2)*1.6
  Sigma3 = matrix(c(1,0,0,.9),2)*1
  
  X = rbind(
    mvrnorm(pi0[1]*n,mu[,1],Sigma2),
    mvrnorm(pi0[2]*n,mu[,2],Sigma),
    mvrnorm(pi0[3]*n,mu[,3],Sigma)
  )
  y = c(rep(1,pi0[1]*n),rep(2,pi0[2]*n),rep(3,pi0[3]*n))
  X = as.data.frame(X)
  names(X) = c('x1','x2')
  list(X=X,y=y)
}

#Generate data
set.seed(1)
train = get_data2(200)
test = get_data2(1000)

#Plot data
plot(train$X,col=cbPalette[train$y+1],pch=20)

#Fit LDA
lda_fit = lda(train$X,train$y)

#Draw the decision regions by classifying a grid of points
Xgrid = as.matrix(expand.grid(seq(-7,6,length.out=50),seq(-3,7,length.out=50)))
b = predict(lda_fit,Xgrid)
plot(train$X,col=cbPalette[train$y+1],pch=19)
points(Xgrid[,1],Xgrid[,2],col=cbPalette[as.numeric(b[[1]])+1],pch=4)

#Make guesses on the test set and compute misclassification error
guesses = predict(lda_fit,newdata=test$X)
mean(guesses$class != test$y)


#Now fit QDA on this data set
qda_fit = qda(train$X,train$y)

#Plot the decision boundaries
Xgrid = as.matrix(expand.grid(seq(-7,6,length.out=50),seq(-3,7,length.out=50)))
b = predict(qda_fit,Xgrid)
plot(train$X,col=cbPalette[train$y+1],pch=19)
points(Xgrid[,1],Xgrid[,2],col=cbPalette[as.numeric(b[[1]])+1],pch=4)

#Make guesses on the test and assess error
guesses = predict(qda_fit,newdata=test$X)
mean(guesses$class != test$y)

