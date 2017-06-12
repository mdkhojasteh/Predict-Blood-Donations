#Load the csv files
train<-read.csv("/Users/mehrdadkhojasteh/Desktop/Train.csv",header=T,sep=",")
test<-read.csv("/Users/mehrdadkhojasteh/Desktop/Test.csv",header=T,sep=",")


#Write a function for normilizing the data
normalize<- function(x) {
  return ( (x-min(x)) / (max(x) - min(x) ) )}

#read and normilize train data
ntrain<- as.data.frame(lapply(train[,c(2,3,5)], normalize))

#read and normilize test data
ntest<- as.data.frame(lapply(test[,c(2,3,4)], normalize))

#creat a target file that has our training data which based on that we want to predict
train_target <- train$Made.Donation.in.March.2007

#for using knn,we have to call class package
require(class)
#show structure about our object
str(train)
sqrt(576)

#First I did a cross validation in the train data to see how accurate is my knn
#I used caret with doSnow for parallelization

library(caret)
library(doSNOW)
library(e1071)


xtrain<-train[1:499,]
xtest<-train[500:576,]
ntrain<- as.data.frame(lapply(xtrain[,c(2,3,5)], normalize))
train_target <- xtrain$Made.Donation.in.March.2007

#set sed for randomize
set.seed(2348)

#creating multifold, 10 fold 10 times
c10_folds<-createMultiFolds(train_target, k=10, times = 10)
trcrtl1<-trainControl(method = "repeatedcv",number = 10,repeats=10,index = c10_folds)

#making cluster for parallelization with socket and register it
cl<-makeCluster(6,type = "SOCK")
registerDoSNOW(cl)
set.seed(34324)

#train the data with knn
knn.5.cv<-train(x=ntrain, y=make.names(xtrain$Made.Donation.in.March.2007), method="knn", tuneLength=10, trControl=trcrtl1)
stopCluster(cl)
knn.5.cv
# I saw that with k=23 I have 0.7746041 accuracy

#knn with k=23
m1<-knn(train = ntrain,test = ntest, cl=train_target, k=23 )
m1

#make a tabel that have our result and save it as a csv file
submitdf<-data.frame(X=test$X, Made.Donation.in.March.2007=m1)
write.csv(submitdf,file = "Results.csv", row.names = FALSE)

#############################################################
