install.packages("ggplot2")
library(ggplot2)
# Homework #2 set-up
marketing=read.csv('C:\\Users\\zil20\\Desktop\\Machine Learning 2\\wk2\\marketing.csv')
set.seed(1)
idx.test=sample(1:nrow(marketing),floor(0.2*nrow(marketing)))
test=marketing[idx.test,]
train=marketing[-idx.test,]
fitlm=glm(y~., data=train, family='binomial')
score=predict(fitlm, newdata=test, type='link')
# Part (a) 
hist(score, main='Histogram of Score')
# Part (b)
classyes<-which(test$y=='yes')
classno<-which(test$y=='no')
noscore<-as.data.frame(score[classno])
yesscore<-as.data.frame(score[classyes])
colnames(noscore)[1]<-'score'
colnames(yesscore)[1]<-'score'
noscore$class<-'no'
yesscore$class<-'yes'
s<-rbind(noscore, yesscore)
ggplot(s, aes(score, fill=class))+geom_histogram(alpha=0.2, aes(y=..density..), position = 'identity')
# Part (c)
ggplot(s, aes(score, fill=class))+geom_vline(xintercept=as.numeric(-1.5))+scale_x_continuous(breaks=seq(-4,2,0.5))+geom_histogram(alpha=0.2, aes(y=..density..), position = 'identity', bins=30)
