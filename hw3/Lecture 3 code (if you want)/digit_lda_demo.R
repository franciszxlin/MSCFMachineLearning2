setwd('~/Dropbox/Teaching/MSCF_ml2/notes/Lecture3/')

#Load NIST digits as a demo

load('zip.014.Rdata')
source('plot.digit.R')

x.014.tr[1,]
y.014.tr[1]
plot.digit(x.014.tr[1,])

library(MASS)
?lda
lda_fit = lda(x.014.tr,y.014.tr)
names(lda_fit)
str(lda_fit)

guesses = predict(lda_fit,newdata=x.014.te)
#See what the returned object contains
str(guesses)
#Compute the misclassification rate for the test set
mean(guesses$class != y.014.te)


dim(a$scaling)

z = x.014.tr%*%a$scaling
plot(z[,1],z[,2],pch=as.character(y.014.tr))
