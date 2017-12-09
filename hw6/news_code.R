#Problem 1
install.packages("tm")
library(tm)
install.packages("NLP")
library(NLP)
install.packages("topicmodels")
library(topicmodels)
install.packages("SnowballC")
library(SnowballC)
# Load the data.  This will create:
#  stories: a character vector with one article in each element
#  class: a character vector specifying whether each article is about "music" or "art"
load("C:\\Users\\zil20\\Desktop\\Github Repos\\MSCFMachineLearning2\\hw6\\articles.RData")
### Stop here, take a look inside both variables to see what you have.

#Part (a)
#Convert that vector into a "corpus" that the tm package will recognize
corpus = VCorpus(VectorSource(stories))

#Compute a document term matrix.  Set this to remove numbers and punctuation, and to change all words to lowercase.
dtm = DocumentTermMatrix(corpus, control=list(removeNumbers=TRUE, removePunctuation=TRUE, tolower=TRUE))
print(dtm)

#Part (b)
#Make a matrix, so that it's easier to work with as numbers
dtm_mat = as.matrix(dtm)
num_art=colSums(dtm_mat!=0)
hist(num_art, main="Histogram of the Number of Documents Each Words Shows up in", xlab="Numbers of Articles")
which(num_art==max(num_art))

#Part (c)
dtm1 = DocumentTermMatrix(corpus, control=list(removeNumbers=TRUE, removePunctuation=TRUE, tolower=TRUE, stopwords=TRUE, bounds=list(global=c(3, Inf))))
print(dtm1)
dtm1_mat=as.matrix(dtm1)
num_art1=colSums(dtm1_mat!=0)
which(num_art1==max(num_art1))

#Part (d)
colnames(dtm1_mat)[1:10]
dtm2 = DocumentTermMatrix(corpus, control=list(removeNumbers=TRUE, removePunctuation=TRUE, tolower=TRUE, stopwords=TRUE, stemming=TRUE, bounds=list(global=c(3, Inf))))
print(dtm2)
dtm2_mat=as.matrix(dtm2)
colnames(dtm2_mat)[1:10]

#Problem 2
#Part (a)
burnin<-4000
iter<-2000
thin<-500
seed<-list(2003,5,63,100001,765)
nstart<-5
best<-TRUE
#Number of topics (This is the interesting parameter)
k<-2
#Run LDA using Gibbs sampling
lda_out<-LDA(dtm2, k, method="Gibbs", control=list(nstart=nstart, seed=seed, best=best, burnin=burnin, iter=iter, thin=thin))
#docs to topics
lda_topics<-as.matrix(topics(lda_out))
#top 10 terms in each topics
lda_terms<-as.matrix(terms(lda_out, 20))
#probabilities associated with each topic assignment
topic_probabilities <- as.data.frame(lda_out@gamma)
t(lda_terms)

#Part(b)
tab<-table(class, lda_topics)
tab
misclass<-(tab[2,1]+tab[1,2])/sum(tab)
misclass

#Problem 3
#Part (a)
complete_tree=hclust(dist(dtm2_mat), method="complete")
plot(complete_tree)
nchar(stories)
nchar(stories)[19]
nchar(stories)[46]
nchar(stories)[100]
nchar(stories)[52]
nchar(stories)[80]
mean(nchar(stories))

#Part (b)
new_mat=t(scale(t(dtm2_mat), scale=TRUE))
complete_tree1=hclust(dist(new_mat), method="complete")
plot(complete_tree1)

#Part (c)
cut1=cutree(complete_tree1, k=6)
tab2=table(class, cut1)
tab2
misclass1=sum(apply(tab2, 2, min))/sum(tab2)
misclass1

#Problem 4
#Part (a)
require("glmnet")
lasso_fit=cv.glmnet(dtm2_mat, as.factor(class), type.measure="class", alpha=1, family="binomial")
lasso_fit$lambda.1se
lasso_pred=predict(lasso_fit, dtm2_mat, s="lambda.1se", type="response") 
lasso_pred=(lasso_pred>0.5)
lasso_mce=mean(lasso_pred!=(class=="music"))
lasso_mce
require("randomForest")
rf_fit=randomForest(dtm2_mat, as.factor(class))
rf_ooo=mean(rf_fit$predicted!=class)
rf_ooo
pred=predict(rf_fit, dtm2_mat)
rf_mce=mean(pred!=class)
rf_mce

#Part (b)
mat=cbind(dtm2_mat, cut1)
lasso_fit=cv.glmnet(mat, as.factor(class), type.measure="class", alpha=1, family="binomial")
lasso_fit$lambda.1se
lasso_pred=predict(lasso_fit, mat, s="lambda.1se", type="response") 
lasso_pred=(lasso_pred>0.5)
lasso_mce=mean(lasso_pred!=(class=="music"))
lasso_mce
rf_fit=randomForest(mat, as.factor(class))
rf_ooo=mean(rf_fit$predicted!=class)
rf_ooo
pred=predict(rf_fit, mat)
rf_mce=mean(pred!=class)
rf_mce

#Part (c)
k=5
lda_out1<-LDA(dtm2, k, method="Gibbs", control=list(nstart=nstart, seed=seed, best=best, burnin=burnin, iter=iter, thin=thin))
#docs to topics
topic_probabilities1 <- as.matrix(lda_out1@gamma)
mat1=cbind(mat, topic_probabilities1)
lasso_fit=cv.glmnet(mat1, as.factor(class), type.measure="class", alpha=1, family="binomial")
lasso_fit$lambda.1se
lasso_pred=predict(lasso_fit, mat1, s="lambda.1se", type="response") 
lasso_pred=(lasso_pred>0.5)
lasso_mce=mean(lasso_pred!=(class=="music"))
lasso_mce
rf_fit=randomForest(mat1, as.factor(class))
rf_ooo=mean(rf_fit$predicted!=class)
rf_ooo
pred=predict(rf_fit,newdata=as.matrix(mat1))
rf_mce=mean(pred!=class)
rf_mce

#Part (d)
lasso_fit=cv.glmnet(topic_probabilities1, as.factor(class), type.measure="class", alpha=1, family="binomial")
lasso_fit$lambda.1se
lasso_pred=predict(lasso_fit, topic_probabilities1, s="lambda.1se", type="response") 
lasso_pred=(lasso_pred>0.5)
lasso_mce=mean(lasso_pred!=(class=="music"))
lasso_mce
rf_fit=randomForest(topic_probabilities1, as.factor(class))
rf_ooo=mean(rf_fit$predicted!=class)
rf_ooo
pred=predict(rf_fit,newdata=topic_probabilities1)
rf_mce=mean(pred!=class)
rf_mce

