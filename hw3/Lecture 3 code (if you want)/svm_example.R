#install.packages('e1071')
library(e1071)

#This example follows the lab from ISL 9.6.1

#Generate a simple example for our linear kernel to fit
set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y = c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1

#Plot the example
plot(x,col=(3-y))
#Put it in a data frame to pass to the svm function
dat=data.frame(x=x,y=as.factor(y))

#Fit the svm with a linear fit and a cost of 10
#Remember, the cost in this function is the reverse of the C in class
#Big C here means small margin
svmfit=svm(y~.,data=dat,kernel='linear',cost=10,scale=FALSE)
plot(svmfit,dat)
summary(svmfit)

#Let's fit and visualize a plot of the svm
svmfit=svm(y~.,data=dat,kernel='linear',cost=.1,scale=FALSE)
plot(svmfit,dat)
#See what happened to the margin?


#Now follows lab from ISL 9.6.2

#Let's generate a trickier example for the radial kernel
set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y = c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

#and plot the example
plot(x,col=(3-y))


#Fit with radial kernel
svmfit=svm(y~.,data=dat,kernel='radial',gamma=1,cost=1)
plot(svmfit,dat)
summary(svmfit)

#Higher cost gives tighter margins...do we want this?
svmfit=svm(y~.,data=dat,kernel='radial',gamma=1,cost=1e5)
plot(svmfit,dat)
summary(svmfit)

#We can tune by cross-validation
#The cross validation takes a list of parameter values to try.  The rest of the parameters are fixed in the main function call
tune.out = tune(svm, y~., data=dat,kernel='radial',ranges=list(cost=c(.1,1,10,100,1000),gamma=c(.5,1,2,3,4)))
summary(tune.out)
#tune.out$best.model corresponds to the best model found according to cross-validated error.  We can plot it.
plot(tune.out$best.model,dat)
