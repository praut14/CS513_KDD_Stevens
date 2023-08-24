#knowledge Discovery and Data Mining (CS 513) Homework 3: KNN classification
#Course : CS 513-A
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : HW_03_KNN

#clearing object environment
rm(list = ls())

#get working directory
getwd()

#Load the “breast-cancer-wisconsin.data.csv” from canvas into R and perform the KNN classification
dataSet1<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/breast-cancer-wisconsin.csv",na.string = "?" )
View(dataSet1)

n<-as.numeric(as.character(dataSet1$F6))
dataSet1$F6<-n

#Remove the rows with missing values 
dataSet1<-na.omit(dataSet1)

#Converting labels to factor class
dataSet1$Class<-factor(dataSet1$Class,levels = c("2","4"),labels = c("benign","malignant"))

#knn classification
#Generating  training set and testing set in the ratio 70% to 30%
size<-sample(1:nrow(dataSet1),0.7*nrow(dataSet1))
n1<-function(x){(x-min(x))/max(x)-min(x)}

#Running min-max normalization on first 4 columns since they are predictor attributes
norm<-as.data.frame(lapply(dataSet1[,c(2,3,4,5,6,7,8,9,10)],n1))

dataSet2<-dataSet1['Class']
#training set
train<-norm[size,]
train2<-dataSet2[size,]

#testing set
test<-norm[size,]
test2<-dataSet2[size,]

#Loading the package class
library(class)

#running  KNN function for k = 3
#classifier<-knn(train[, -1], test[, -1],cl=train2,k=3)
#clf <- knn(train,test,cl=train2,k=3)
clf <- knn(train,test,cl=train2,k=3)
#creating confusion matrix
library(caret)
conf_matrix<-table(clf,test2)
print(conf_matrix)
confusionMatrix(clf, test2)
#Accuracy calculation
accuracy<-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(conf_matrix)




#running  KNN function for k = 5
clf <- knn(train,test,cl=train2,k=5)
#creating confusion matrix
library(caret)
conf_matrix1<-table(clf,test2)
print(conf_matrix1)
confusionMatrix(clf, test2)
#Accuracy calculation
accuracy<-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(conf_matrix1)



#running  KNN function for k = 10
clf <- knn(train,test,cl=train2,k=10)
#creating confusion matrix
library(caret)
conf_matrix2<-table(clf,test2)
print(conf_matrix2)
confusionMatrix(clf, test2)
#Accuracy calculation
accuracy<-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(conf_matrix2)



#running  KNN function for k = 15
clf <- knn(train,test,cl=train2,k=15)
#creating confusion matrix
library(caret)
conf_matrix2<-table(clf,test2)
print(conf_matrix2)
confusionMatrix(clf, test2)
#Accuracy calculation
accuracy<-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(conf_matrix2)


#running  KNN function for k = 25
clf <- knn(train,test,cl=train2,k=25)
#creating confusion matrix
library(caret)
conf_matrix2<-table(clf,test2)
print(conf_matrix2)
confusionMatrix(clf, test2)

#Accuracy calculation
accuracy<-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(conf_matrix2)


#### END OF ASSIGNMENT ###
