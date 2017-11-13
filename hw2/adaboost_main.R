#Load the helper functions.
#You shouldn't need to modify any of the helper functions
#But you should be familiar with what they're doing, and their arguments and outputs
source('C:\\Users\\zil20\\Desktop\\Github Repos\\MSCFMachineLearning2\\hw2\\adaboost_helpers.R')

# Generates data for our example
# Inputs:
## n: number of samples
# Output: list with entries:
## x1: x_1 coordinate, a covariate
## x2: x_2 coordinate, a covariate
## y: class for the point (0,1)
# Example: get_circle_data(500)
get_circle_data = function(n){
  X = matrix(rnorm(2*n),ncol=2)
  Y = as.numeric(X[,1]^2+X[,2]^2<1)
  list(x1=X[,1],x2=X[,2],y=Y)  
}


# Implementation of the adaboost function using single-split trees
# Inputs:
## pts: a data matrix in the format produced by get_circle_data()
## B: The number of trees to produce during boosting
# Outputs:
## trees: a list of the boosted trees
## alphas: a vector of the boosting weights
# Example: my_adaboost(train,B=300)
my_adaboost = function(pts, B=10){
  n = length(pts$y)
  #Observation weights
  wgts = rep(1/n,n)
  #A list to store the trees as we compute them
  trees = vector('list',length=B)
  #A vector to store the tree weights
  alphas = numeric(B)
  
  for(b in 1:B){
    #Find the next split, according to your current weights.
    #Hint: The resulting tree is split$tree
    #Hint: You can get tree predictions with predict(split$tree, type='class')
    split = find_split(pts,wgts)
    
    #TODO: Fill in the rest of the body of this loop to implement the AdaBoost algorithm from class
    #You can introduce whatever you need here.  At the very least, it needs to result in updated wgts and 
    #an additional entry in the trees list and the alphas vector.
    
    pred<-predict(split$tree, type='class')
    e<-sum(wgts*(pts$y!=pred))/sum(wgts)
    alpha<-log((1-e)/e)

    #TODO: You'll probably need some stuff here#
    
    trees[[b]] = split$tree #TODO: Store the new tree#
    alphas[b] = alpha #TODO: Add the new tree's alpha#
    wgts = wgts*exp(alpha*(pts$y!=pred))#TODO: Update your observation weights#
  }
  list(trees=trees, alphas=alphas)
}



#Calculate predictions for boosted trees from my_adaboost
# Inputs:
## btrees: output object from my_adaboost
## pts: a data set to evaluate the performance on.
##   It is assumed that pts has a y field as well, which is used to calculate the error rates
# Outputs: a list containing:
## score: the score sum alpha_b fhat_b(x) for each point (before thresholding)
## predictions: the thresholded scores: our guesses at class.
## test_err: a length B vector of misclassification error at each step (1 tree to B trees)
# Example: predict_ada(boosted_trees, train, train$y)
predict_ada = function(btrees, pts){
  n = length(pts$y)
  answers = pts$y
  #Initialize a score vector to zero.  This will store the ongoing sum alpha_b fhat_b(x) for each point.
  score = numeric(n)
  B = length(btrees$alphas)
  #This will store the misclassification error at each step,
  # so we can see how it changes with the number of trees
  test_err = numeric(B)
  for(b in 1:B){
    #Update your score to include the contribution from tree b
    tree<-btrees$trees[[b]]
    pred<-predict(tree, newdata=pts, type='class')
    pos<-which(pred==1)
    fhat<-rep(-1, n)
    fhat[pos]<-1
    alpha<-btrees$alphas[b]
    score = score + alpha*fhat #TODO: Fill in the updated score#
    #Record the misclassification error for the predictions based on thresholding the current score
    current_prediction<-as.numeric(score>0)
    test_err[b] = mean(current_prediction!=answers) #TODO: Fill in the current misclassification error#
  }
  list(score=score, predictions = as.numeric(score>0), test_err = test_err)
}

#Calculate the partial dependence plot over x1.  We won't bother with x2, because of the symmetry.
# Inputs: 
## btrees: boosted tree output from my_adaboost
## dataset: a data set like the one produced by get_circle_data
# Outputs: a list containing entries
## x: x coordinates corresponding to x1 evaluation points
## pdep: partial dependence values corresponding to the x1 values in x
# Example: calculate_partial_dependence_x1(boosted_trees, dataset)
calculate_partial_dependence_x1 = function(btrees, pts){
  xlim = range(pts$x1)
  x = seq(from=xlim[1], to=xlim[2], length.out=50)
  n = length(x)
  #Somewhere to store the partial dependence values
  pdep = numeric(n)
  #We will fake a dataset and change its x1 values to match each evaluation points
  fake = pts
  for(i in 1:n){
    fake$x1 = rep(x[i],length(fake$x2))
    #TODO: Fill in the actual calculation of the partial dependence at x[i] below#
    #HINT: Evaluate predict_ada on the fake data set and use the resulting predictions
    prediction<-predict_ada(btrees, fake)
    pdep[i] = mean(prediction$predictions) #TODO: Fill in the partial dependence calculation here#
  }
  list(x=x,pdep=pdep)
}

###### End Functions ######

#TODO: The rest of your assignment can go here
# Generate data, make plots, fit boosted trees, make predictions, plot diagnostics, etc
# Part (b)
# Generate the data
train<-get_circle_data(500)
test<-get_circle_data(500)
# Plot the training set
palette = colorRampPalette(c('red', 'blue'))(2)
colors=palette[as.numeric(cut(train$y, breaks=2))]
plot(train$x1, train$x2, xlab='x1', ylab='x2', main='Training Set Circle Data (Red is y=0, Blue is y=1)', col=colors)
# Run boosting method with B=1,2,3 and use the draw_boosted_trees function to plot the decision boundaries and resulting scores 
# B = 1
b1<-1
ada1<-my_adaboost(train, b1)
adapred1<-predict_ada(ada1, train)
draw_boosted_trees(ada1, train, score=adapred1$score)
# B = 2
b2<-2
ada2<-my_adaboost(train, b2)
adapred2<-predict_ada(ada2, train)
draw_boosted_trees(ada2, train, score=adapred2$score)
# B = 3
b3<-3
ada3<-my_adaboost(train, b3)
adapred3<-predict_ada(ada3, train)
draw_boosted_trees(ada3, train, score=adapred3$score)
# Part (c)
# B = 250
b250<-250
ada250<-my_adaboost(train, b250)
adapred250_train<-predict_ada(ada250, train)
adapred250_test<-predict_ada(ada250, test)
# Plot error plot 
plot(adapred250_train$test_err, type='l', main='Misclassification Error as Function of Number of Trees Plot', xlab='Number of trees', ylab='Misclassification Error', col='red')
lines(adapred250_test$test_err, col='blue')
legend(150, 0.3, legend=c('Training set data', 'Testing set data'), col=c('red', 'blue'), lty=1:1, cex=0.8)
# Draw resulting boosted trees
draw_boosted_trees(ada250, train, score=adapred250_train$score)
draw_boosted_trees(ada250, test, score=adapred250_test$score)
# Part (d) Plot the partial dependence plot for x1 
pdp<-calculate_partial_dependence_x1(ada250, test)
plot(pdp$x, pdp$pdep, type='o', main="Partial Dependence Plot for x1", xlab='x1', ylab='Average Prediction')
