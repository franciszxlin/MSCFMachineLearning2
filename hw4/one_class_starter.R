install.packages("MASS")
library(MASS)
library(e1071)
set.seed(1)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Generates crazy looking X data.  This has two columns.
get_data = function(n){
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
  X = as.data.frame(X)
  names(X) = c('x1','x2')
  X
}

#Plots the one-class svm results.  Pass in the svm fitted model and a data matrix.
plot_results = function(fit, X){
  xlim = range(X[,1])
  ylim = range(X[,2])
  Xgrid = as.matrix(expand.grid(seq(xlim[1],xlim[2],length.out=50),seq(ylim[1],ylim[2],length.out=50)))
  b = predict(fit,Xgrid)
  plot(X,pch=19, cex=1)
  points(Xgrid[,1],Xgrid[,2],col=cbPalette[as.numeric(b)+1],pch=4,cex=1)
}

#Generate data
set.seed(1)
X = get_data(200)

#Plot data
plot(X,pch=20, cex=3)

### TODO: Really the only thing you have to do is add the SVM fitting function in here 
###       and then play with it

#Part (a) 
fit=svm(X, type='one-classification', kernel='radial', nu='0.1', gamma=0.5, scale=TRUE)

#Draw the decision regions by classifying a grid of points
plot_results(fit, X)

#Part (b)
fitb=svm(X, type='one-classification', kernel='radial', nu='0.1', gamma=2, scale=TRUE)
plot_results(fitb, X)
fitbb=svm(X, type='one-classification', kernel='radial', nu='0.1', gamma=0.1, scale=TRUE)
plot_results(fitbb, X)

#Part (c)
fitc=svm(X, type='one-classification', kernel='radial', nu='0.5', gamma=1, scale=TRUE)
fitcc=svm(X, type='one-classification', kernel='radial', nu='0.0001', gamma=1, scale=TRUE)
plot_results(fitc, X)
plot_results(fitcc, X)