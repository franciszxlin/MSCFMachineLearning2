install.packages('e1071')

get_circle_data = function(n){
  X = matrix(rnorm(2*n),ncol=2)
  Y = as.numeric(X[,1]^2+X[,2]^2<1)
  data.frame(x1=X[,1],x2=X[,2],y=as.factor(ifelse(Y==1,1,-1)))  
}

#Note, you need to have y be a factor for the svm package to realize you're doing classification
library(e1071)

train = get_circle_data(100)
test = get_circle_data(1000)

#Part (a)
#Plot the training and test data. Color the data points by class.
plot(train$x1,train$x2,pch=as.numeric(train$y) + 15,col=train$y, main="Training data")
plot(test$x1,test$x2, pch=as.numeric(test$y) + 15, col=test$y,  main="Testing data")

#Part (b)
svmfit1<-svm(y~., data=train, kernel='linear', cost=1e7)
plot(svmfit1, train)
pred1<-predict(svmfit1, test)
misclass1<-mean(test$y!=pred1)
misclass1

#Part (c)
svmfit2<-svm(y~., data=train, kernel='polynomial', cost=1e7)
plot(svmfit2, train)
pred2<-predict(svmfit2, test)
misclass2<-mean(test$y!=pred2)
misclass2

#Part (d)
svmfit3<-svm(y~., data=train, kernel='polynomial', degree=2, cost=1000)
plot(svmfit3, train)
pred3<-predict(svmfit3, test)
misclass3<-mean(test$y!=pred3)
misclass3

#Part (e) Tuning cost and gamma parameters
tune1<-tune(svm, y~., data=train, kernel='polynomial', degree=2, ranges=list(cost=c(1000, 1e4, 1e5, 1e6, 1e7, 1e8), gamma=c(0.001, 0.005, 0.01, 0.05, 0.1, 1)))
tune1$best.parameters
pred4<-predict(tune1$best.model, test)
misclass4<-mean(test$y!=pred4)
misclass4
plot(tune1$best.model, train)

#Part (f)
svmfit5<-svm(y~., data=train, kernel='radial', cost=1000)
plot(svmfit5, train)
pred5<-predict(svmfit5, test)
misclass5<-mean(test$y!=pred5)
misclass5

#Part (g)
tune2<-tune(svm, y~., data=train, kernel='radial', ranges=list(cost=c(0.1, 1, 10, 100, 1000, 1e4, 1e5, 1e6), gamma=c(0.01, 0.05, 0.1, 0.5, 1, 2, 3)))
tune2$best.parameters
pred6<-predict(tune2$best.model, test)
misclass6<-mean(test$y!=pred6)
misclass6
plot(tune2$best.model, train)

#Part (h) fun with more training data
train1<-get_circle_data(500)

tune3<-tune(svm, y~., data=train1, kernel='radial', ranges=list(cost=c(0.1, 1, 10, 100, 1000, 1e4, 1e5, 1e6), gamma=c(0.01, 0.05, 0.1, 0.5, 1, 2, 3)))
plot(tune3$best.model, train1)
pred7<-predict(tune3$best.model, test)
misclass7<-mean(test$y!=pred7)
misclass7
tune3$best.parameters

tune4<-tune(svm, y~., data=train1, kernel='polynomial', degree=2, ranges=list(cost=c(1000, 1e4, 1e5, 1e6, 1e7, 1e8), gamma=c(0.001, 0.005, 0.01, 0.05, 0.1, 1)))
plot(tune4$best.model, train1)
pred8<-predict(tune4$best.model, test)
misclass8<-mean(test$y!=pred8)
misclass8
tune4$best.parameters
