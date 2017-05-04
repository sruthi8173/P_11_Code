install.packages("SSL")
library("SSL")
data_set<-read.csv("new_data.csv")
set.seed(600)

library(normtest)
kurtosis.norm.test(data_set)
X <- as.matrix(data_set[, -1])
cl <- unclass(data_set[, 1])
indtrain <- sort(sample(1:569,150))
ind_test2<-sample(indtrain,70)
indtest <- setdiff(1:569, indtrain)
indtrain<-setdiff(indtrain,ind_test2)
write.csv(data_set[indtrain,],"feature.csv")
labels<-data_set[indtrain,1]
model3<-sslGmmEM(X[indtrain,],labels,X[indtest,],seed=0)
labels1<-model3$yu
for(i in indtest)
{ 
  if(!is.na(labels1[i]))
    data_set[i,1]=labels1[i]
  
}
#for( i in indtest)
#{
 # data_set[i,1]=labels1[i]
#}
write.csv(data_set,"new_data_em.csv")
data_set<-read.csv("new_data_em.csv")
data_set<-data_set[,-1]
head(data_set)
for( i in 1:569)
{
  print(i)
  if(data_set[i,1]==1)
    data_set[i,1]='M'
  else 
    data_set[i,1]='B'
}
#svm classifier 1 
library(e1071)
data_set[,1]<-as.factor(data_set[,1])
head(data_set)
svm.model <- svm(diagnosis~., data = data_set[-(ind_test2),])
svm.pred <- predict(svm.model,data_set[ind_test2,-1])
accuracy<-table(pred = svm.pred, true = data_set[ind_test2,1])
print("The accuracy is :")
print(((accuracy[1]+accuracy[4])/(accuracy[1]+accuracy[2]+accuracy[3]+accuracy[4]))*100)

#overfitting checking 
ind<-rbind(data_set[indtrain,],data_set[ind_test2,])
temp<-data_set[-indtrain,]
svm.model <- svm(diagnosis~., data = temp[-(ind_test2),])
svm.pred <- predict(svm.model,data_set[indtrain,-1])
accuracy<-table(pred = svm.pred, true = data_set[indtrain,1])
svm_newpred<-predict(svm.model,data_set[1,-1])
table(pred=svm_newpred,true=data_set[1,1])
print("The accuracy is :")
print(((accuracy[1]+accuracy[4])/(accuracy[1]+accuracy[2]+accuracy[3]+accuracy[4]))*100)
#loss()


#random forest classifier 2
library(randomForest)
rf.model <- randomForest(diagnosis~., data = data_set[-(ind_test2),])
rf.pred <- predict(rf.model,data_set[ind_test2,-1])
accuracy<-table(pred = rf.pred, true = data_set[ind_test2,1])
rf_newpred<-predict(rf.model,data_set[1,-1])
table(pred=rf_newpred,true=data_set[1,1])
print("The accuracy is :")
print(((accuracy[1]+accuracy[4])/(accuracy[1]+accuracy[2]+accuracy[3]+accuracy[4]))*100)

#overfitting check 
rf.pred <- predict(rf.model,data_set[indtrain,-1])
accuracy<-table(pred = rf.pred, true = data_set[indtrain,1])
rf_newpred<-predict(rf.model,data_set[1,-1])
table(pred=rf_newpred,true=data_set[1,1])
print("The accuracy is :")
print(((accuracy[1]+accuracy[4])/(accuracy[1]+accuracy[2]+accuracy[3]+accuracy[4]))*100)

#naivebayes classifier 3

nb.model <- naiveBayes(diagnosis~., data = data_set[-(ind_test2),])
nb.pred <- predict(nb.model,data_set[ind_test2,-1])
accuracy<-table(pred = nb.pred, true = data_set[ind_test2,1])
nb_newpred<-predict(nb.model,data_set[1,-1])
table(pred=nb_newpred,true=data_set[1,1])
print("The accuracy is :")
print(((accuracy[1]+accuracy[4])/(accuracy[1]+accuracy[2]+accuracy[3]+accuracy[4]))*100)

#overfitting check
nb.pred <- predict(nb.model,data_set[indtrain,-1])
accuracy<-table(pred = nb.pred, true = data_set[indtrain,1])
nb_newpred<-predict(nb.model,data_set[1,-1])
table(pred=nb_newpred,true=data_set[1,1])
print("The accuracy is :")
print(((accuracy[1]+accuracy[4])/(accuracy[1]+accuracy[2]+accuracy[3]+accuracy[4]))*100)

