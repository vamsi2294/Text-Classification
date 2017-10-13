library(tm)
library(rpart)

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

# Classification of data
ctrl=rpart.control(minsplit = 1000,minbucket = 50,cp=29.0e-05)
k=rpart(train_df$`train$rating`~.,train_df,method = "class",control = ctrl)
plotcp(k)
plot(k)
text(k)

# Predicting the accuracy
pre=predict(k,test_df,type = "class")
sum(test_df$`test$rating`==pre)/length(pre)
plotcp(k)
plot(pre,main="predicted output without pruning")



