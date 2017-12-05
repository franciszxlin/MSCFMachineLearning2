#This function takes in a group vector returned by cutree on an hclust object
#It also takes in the object loaded from djia_info.RData to look up the info
#It returns a list with several fields:
## symbol
## industry
## company
#Each field is a list.  Suppose you had 3 groups.  You would access each of their industries by
# output$industry[[1]], ..., output$industry[[3]]
# or just output$industry to get them all.
#where output is the object returned by this function.  Same process for company or symbol.
resolve_groups = function(groups, djia_info){
  k = length(unique(groups))
  industry = vector('list',k)
  symbol = vector('list',k)
  company = vector('list',k)
  #Loop over each group
  for(i in 1:k){
    idx = which(djia_info$Symbol %in% names(groups)[which(groups==i)])
    industry[[i]] = as.character(djia_info$Industry[idx])
    symbol[[i]] = as.character(djia_info$Symbol[idx])
    company[[i]] = as.character(djia_info$Company[idx])
  }
  list(industry=industry, symbol=symbol, company=company)
}