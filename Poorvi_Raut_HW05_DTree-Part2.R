#knowledge Discovery and Data Mining (CS 513) Homework 5: Decision Tree USing CART Algorithm
#Course : CS 513-A
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : HW_05_DTree

#clearing object environment
rm(list = ls())
#get working directory
getwd()

#Import package rpart for CART Decision Tree Algorithm , caret package to calculate confusion matrix metrics 
library(class)
library(rpart)
library(caret)
#Load the “breast-cancer-wisconsin.data.csv” from canvas into R and perform the CART algorithm
dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/breast-cancer-wisconsin.csv",na.string = "?" )
#View Breast Cancer Dataset
View(dataSet)
#head(df, n=5)
#Summarizing each column
summary(dataSet)
#Converting the type of column F6 from character to numeric
n<-as.numeric(as.character(dataSet$F6))
summary(n,na.rm=TRUE)
#Checking the number of rows 
nrow(dataSet)
#Remove the rows with missing values
dataSet<-na.omit(dataSet)
nrow(dataSet)
#Converting the class column to factor class
dataSet$Class<-factor(dataSet$Class,levels = c("2","4"),labels = c("benign","malignant"))
is.factor(dataSet$Class)

dataSet1<-dataSet[2:11]
View(dataSet1)

#partitioning 70% of size
sample_size<-floor(0.70*nrow(dataSet1))
#Set the seed to make your partition reproducible
set.seed(123)
traindata<-sample(seq_len(nrow(dataSet1)),size = sample_size)
# 70% of data in training set 
train<-dataSet1[traindata,]

# 30% of data in testing set
test<-dataSet1[traindata,]
#Implementing CART algorithm
cart_algo<-rpart(Class ~.,data=train,method = "class")

#Predicting target class 
predict_alg<-predict(cart_algo,test,type = "class")
print(length(predict_alg))
#print(length(test$Class))

#creating confusion matrix
conf_matrix<-table(predict_alg,test$Class)
print(conf_matrix)
confusionMatrix(predict_alg,test$Class)

#Calculating Accuracy of the algorithm
accuracy<-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(conf_matrix)
#Error rate
e<- 100- accuracy(conf_matrix)
print(e)
# -----End of Assignment-----