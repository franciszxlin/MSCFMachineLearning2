# Helper functions for your personal adaboost implementation #

#This function finds the best single split on one of the x1,x2 variables, while incorporating weights.
#This uses the decision tree library rpart and forces the trees to have exactly one split.
#This will save you from dealing with the library yourself.
# Inputs:
## pts: a data frame with x1, x2, y elements
## wgts: an n-vector of observation weights.
# Returns: a list with the components:
## tree: The object returned by rpart
## variable: which variable was split on (1 or 2) (you don't really need this)
## location: the value at which variable was split (you don't really need this)
find_split = function(pts, wgts){
  n = length(pts$y)
  #Control parameters to force our tree to just be a single split
  control <- rpart.control(maxdepth=1, minsplit=1, minbucket=1, cp=-1, maxcompete = 0, maxsurrogate = 0, xval=0)
  #Fit a tree to find our split
  tree <- rpart(y ~ x1+x2, weights=wgts/sum(wgts), control=control, method="class", data=pts)
  location = tree$splits[4]
  variable = which(rownames(tree$splits)==attr(tree$terms,'term.labels'))
  list(tree=tree, variable=variable, location=location)
}

#Draws the data points and the decision boundaries used by the boosted trees.  
#If score is supplied, the points are also shaded by the score.  Otherwise
#they are colored by the pts$y value.
# Inputs:
## btrees: the list object output by my_adaboost
## pts: a set of points output by get_circle_data
## score: n-vector of scores to color each point (optional)
# Outputs: makes a plot
# Example: draw_boosted_trees(boosted_trees, dataset, prediction$score)
draw_boosted_trees = function(btrees, pts, score=NULL){
  if(!is.null(score)){
    #Build a color palette for the gradient
    rbPal <- colorRampPalette(c('red','blue'))
    #Generate a column of color values
    color = rbPal(10)[as.numeric(cut(score,breaks = 10))]
  } else {
    color = pts$y+1
  }
  plot(pts$x1,pts$x2,col=color, pch=20)
  
  #Grab the trees from our object
  trees = btrees$trees
  linecol = 'gray'
  
  #Loop over the trees and draw each decision boundary
  B = length(trees)
  for(b in 1:B){
    tree = trees[[b]]
    #Location of the split
    location = tree$splits[4]
    #Variable that was split
    variable = which(rownames(tree$splits)==attr(tree$terms,'term.labels'))
    #Draw the corresponding horizontal/vertical line
    if(variable==1){
      abline(v=location, col=color)
    } else if(variable==2) {
      abline(h=location, col=color)
    }
  }
}



