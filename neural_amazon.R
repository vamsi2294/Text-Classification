library(nnet)
library(caret)
library(tm)
install.packages("tm")

train=read.csv("amazon_baby_train.csv")
test=read.csv("amazon_baby_test.csv")

#Removing numbers, stop words and white spaces
preprocess<-function(review_corpus){
  review_corpus = tm_map(review_corpus, content_transformer(tolower))
  review_corpus = tm_map(review_corpus, removeNumbers)
  review_corpus = tm_map(review_corpus, removePunctuation)
  review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
  review_corpus = tm_map(review_corpus, stripWhitespace)
}

#Converting dataframe to corpus
train_review_corpus = Corpus(VectorSource(train$review))
test_review_corpus = Corpus(VectorSource(test$review))

train_review_corpus=preprocess(train_review_corpus)
test_review_corpus=preprocess(test_review_corpus)

#Transforming to document term matrix by performing TF-IDF
train_review_dtm <- DocumentTermMatrix(train_review_corpus, control = list(weighting = weightTfIdf, minWordLength=2, minDocFreq=5))
test_review_dtm <- DocumentTermMatrix(test_review_corpus, control = list(weighting = weightTfIdf, minWordLength=2, minDocFreq=5))

#Setting the sparsity ratio to 0.95
train_review_dtm = removeSparseTerms(train_review_dtm, 0.95)

train_review_dtm<-as.matrix(train_review_dtm)
test_review_dtm<-as.matrix(test_review_dtm)

test_review_dtm<- as.data.frame(test_review_dtm[,intersect(colnames(train_review_dtm),colnames(test_review_dtm))] )
train_review_dtm<- as.data.frame(train_review_dtm )

train_df=cbind(train_review_dtm,train$rating)
test_df=cbind(test_review_dtm,test$rating)

##multinom function  to fit the neural network
p=multinom(train_df$`train$rating`~.,train_df,maxit=100,hidden=c(12,1),decay=1e-06)


##Analysing the accuracy for the Iterations.
i=10
acc=0
while(i<300){
  p=multinom(train_df$`train$rating`~.,train_df,maxit=i,hidden=c(12,1),decay=1e-06)
  pred <- predict(p, type="class", newdata=test_df)
  acc=sum(test_df$`test$rating`==pred)/length(pred)
  acc=c(acc,acc)
  i=i+10
}

##Plotting the accuracy graph
plot(1:30,acc,type="o",col="blue",main="Accuracy VS Iterations",xlab="Iterations",ylab = "Accuracy")
max(k)


##Prediction  function for the train data
pred <- predict(p, type="class", newdata=train_df)
acc=sum(train_df$`train$rating`==pred)/length(pred)

##Prediction  function for the test data
pred <- predict(p, type="class", newdata=test_df)

###Accuracy for the predicted output
acc=sum(test_df$`test$rating`==pred)/length(pred)

table(train_df$`train$rating`,pred)

p=c(1:floor(7/10*length(train_df$`train$rating`)))
cv_ecld=knn(train = train_df[p,],test=train_df[-p,],cl=train_df[p,]$`train$rating`,k=1,l=0,prob = FALSE)

