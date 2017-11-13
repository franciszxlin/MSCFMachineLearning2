#Load the marketing data set
marketing = read.csv('C:\\Users\\zil20\\Desktop\\Machine Learning 2\\wk2\\marketing.csv')

#To make your life easier in this assignment, we convert the y values to TRUE and FALSE values
#This will let you easily use boolean logic in building your function
marketing$y = (marketing$y=='yes')

#Make training and test sets
set.seed(1)
idx.test = sample(1:nrow(marketing),floor(0.2*nrow(marketing)))
test = marketing[idx.test,]
train = marketing[-idx.test,]

#Fit the logistic model and grab scores
fitlm = glm(y~.,data = train, family='binomial')
guess_lm = predict(fitlm,newdata=test, type='response')


#Write a function to compute sensitivity, specificity, overall loss for a particular classification vector
# Inputs:
## estimate: a boolean vector of estimates, with TRUE for class 0 and FALSE for class 0
## truth: a boolean vector of true classes, with TRUE for class 0 and FALSE for class 0
## loss_FP: The loss for each false positive
## loss_FN: The loss for each false negative
# Outputs: a list containing
## sens: the sensitivity for the estimate
## spec: the specificity for the estimate
## loss: the total loss corresponding to the estimate
eval_set = function(estimate, truth, loss_FP, loss_FN){
  
  #TODO: Calculate sens, spec, loss in here.
  TP<-sum(estimate&truth)
  TN<-sum(!(estimate|truth))
  FP<-sum(estimate&(!truth))
  FN<-sum((!estimate)&truth)
  sens<-TP/(TP+FN)
  spec<-TN/(TN+FP)
  loss<-sum(loss_FP*FP, loss_FN*FN)
  list(sens=sens, spec=spec, loss=loss)
}
#TODO: Check your loss with a classification threshold of 0.3 and the given loss values
guess<-(guess_lm>0.3)
loss_FP<-5
loss_FN<-100
result<-eval_set(guess, test$y, loss_FP, loss_FN)
result$sens
result$spec
result$loss

### Some code for you ###
#For Part b: sorts the score values and finds the midpoints to be used as thresholds later
score_vals = sort(unique(guess_lm))
midpts = (score_vals[-1]+score_vals[-length(score_vals)])/2
#For Part b: Applies your function to this list of values and produces a matrix of results
output = lapply(midpts,FUN=function(x){eval_set(guess_lm>x,test$y,loss_FP,loss_FN)})
eval_values = matrix(unlist(output),ncol=3,byrow = TRUE)#reshape into a matrix
#eval_values has columns sens, spec, loss.
### End code for you ###


#TODO: Finish everything else :)
# Part (b)
plot(1-eval_values[,2], eval_values[,1], main="ROC Curve", xlab="1-Specificity", ylab="Sensitivity")
abline(a=0,b=1)
# Part (c)
palette=colorRampPalette(c('red', 'blue'))(10)
colors=palette[as.numeric(cut(eval_values[,3], breaks=10))]
plot(1-eval_values[,2], eval_values[,1], main="ROC Curve", xlab="1-Specificity", ylab="Sensitivity", col=colors)
abline(a=0,b=1)
# Part (d)
minpos<-which.min(eval_values[,3])
P<-sum(test$y)
N<-length(test$y)-P
slope<-(N*loss_FP)/(P*loss_FN)
plot(1-eval_values[,2], eval_values[,1], main="ROC Curve", xlab="1-Specificity", ylab="Sensitivity")
points(x=1-eval_values[minpos,2], y=eval_values[minpos,1], col='red', pch=19)
abline(a=-slope*(1-eval_values[minpos,2])+eval_values[minpos,1], b=slope)
