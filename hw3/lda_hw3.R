library(MASS)

#Generate data that's favorable to QDA assumptions
#One class is generated with Sig1 as the covariance, and the other with Sig2
#Sig2 has higher variance and a bit of correlation between all the variables
#Returns a list with an X matrix of variables and a y vector of class factors (classes numbered 1,2)
#Note: The lda/qda functions handle factors just fine, so you shouldn't have to change y
getdata = function(n,p){
  rho = .3   # correlation
  Sig1 = diag(p)   # covariance matrix 1
  Sig2 = matrix(rho,p,p)     #covariance matrix 2
  diag(Sig2)=2
  mu1 = matrix(rep(0,p))   # mean vector 1
  mu2 = matrix(rep(1,p))   # mean vector 2
  X1 = mvrnorm(n/2,mu1,Sig1)
  X2 = mvrnorm(n/2,mu2,Sig2)
  y1 = rep(1,n/2)
  y2 = rep(2,n/2)
  X = rbind(X1,X2)
  y = as.factor(c(y1,y2))
  list(X=X,y=y)  
}

n = 100
p = 20

set.seed(1)
train = getdata(n,p)
test = getdata(n,p)

#Part (a)
palatte = colorRampPalette(c('red', 'blue'))(2)
colors = palatte[as.numeric(cut(as.numeric(test$y), breaks=2))]
plot(test$X[,1], test$X[,2], main="Class y vs. X[,1] & X[,2]", col=colors)
legend(-3, 3, legend=c('y=1', 'y=2'), col=c('red', 'blue'), pch=1, cex=1)

#Part (b)
ldafit<-lda(train$X, train$y)
qdafit<-qda(train$X, train$y)
trnldapred<-predict(ldafit)
misclass1<-mean(train$y!=trnldapred$class)
misclass1
trnqdapred<-predict(qdafit)
misclass2<-mean(train$y!=trnqdapred$class)
misclass2

#Part (c)
tstldapred<-predict(ldafit, newdata=test$X)
misclass3<-mean(test$y!=tstldapred$class)
misclass3
tstqdapred<-predict(qdafit, newdata=test$X)
misclass4<-mean(test$y!=tstqdapred$class)
misclass4