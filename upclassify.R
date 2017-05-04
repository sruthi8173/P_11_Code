#Upclassify
dataset<-read.csv("new_data.csv")
library('upclass') 
install.packages("upclass")
library(upclassfit)
set.seed(60)

X <- as.matrix(dataset[, -1])
cl <- unclass(dataset[, 1])
indtrain <- sort(sample(1:569,100))
ind_test2<-sample(indtrain,50)
indtest <- setdiff(1:569, indtrain)
indtrain<-setdiff(indtrain,ind_test2)

fitup <- upclassifymodel(X[indtrain,], cl[indtrain], X[indtest,], cl[indtest],modelName = "VEV")
#upclassifymodel(testset[1:300,],testset[1:300,1],testset[301:569,-1])
#fitup1 <- upclassify(X[indtrain,], cl[indtrain], X[indtest,], cl[indtest],modelName = "VEV")

new_labels<-fitup$test$cl
new_set<-dataset
for(i in indtest)
{ 
  if(!is.na(new_labels[i]))
    new_set[i,1]=new_labels[i]
  
}
write.csv(new_set,"new_data_up.csv")
new_set<-read.csv("new_data_up.csv")
new_set<-new_set[,-1]

for( i in 1:569)
{
  if(new_set[i,1]==1)
    new_set[i,1]='M'
  else 
    new_set[i,1]='B'
}

new_set[,1]<-as.factor(new_set[,1])
new_set[-ind_test2,]
class(new_set[,1])
#classifier 1 - svm 
library("e1071")
svm.model <- svm(diagnosis~., data = new_set[(-ind_test2),])
svm.pred <- predict(svm.model,new_set[ind_test2,-1])
accuracy<-table(pred = svm.pred, true = new_set[ind_test2,1])
#new prediction
svm_newpred<-predict(svm.model,new_set[2,-1])
table(pred=svm_newpred,true=new_set[2,1])
print("The accuracy is :")
print(((accuracy[1]+accuracy[4])/(accuracy[1]+accuracy[2]+accuracy[3]+accuracy[4]))*100)

##randomforest
install.packages("randomForest")
library("randomForest")
rf.model <- randomForest(diagnosis~., data = new_set[(-ind_test2),])
rf.pred <- predict(rf.model,new_set[ind_test2,-1])
accuracy<-table(pred = rf.pred, true = new_set[ind_test2,1])
print("The accuracy is :")
print(((accuracy[1]+accuracy[4])/(accuracy[1]+accuracy[2]+accuracy[3]+accuracy[4]))*100)
#84%

#naivebayes
library('e1071')
nn.model <- naiveBayes(diagnosis~., data = new_set[(-ind_test2),])
nn.pred <- predict(nn.model,new_set[ind_test2,-1])
accuracy<-table(pred = nn.pred, true = new_set[ind_test2,1])
print("The accuracy is :")
print(((accuracy[1]+accuracy[4])/(accuracy[1]+accuracy[2]+accuracy[3]+accuracy[4]))*100)

