library(e1071)
library(caret)
library(tm)
#######read csv
train=read.csv("train_df.csv")
test=read.csv("test_df.csv")


train_norm=cbind(normalize(train[1:97]),rating=train$rating)
test_norm=cbind(normalize(test[1:97]),rating=test$rating)
m=1
i=1
acc=NULL
while(i<20){
train2=naiveBayes(as.factor(train$rating)~.,train,laplace=0)
tr=predict(train2,test,type = 'class')
acc[m]=(sum(tr==test$rating)/length(test$rating))
i=i+3
m=m+1
}

prop.table(table(tr,test$rating))
confusionMatrix(tr,test$rating)

train2=naiveBayes(as.factor(train$rating)~.,train,threshold=0.01)
tr=predict(train2,test)
cv=c(sum(tr==test$rating)/length(test$rating))

