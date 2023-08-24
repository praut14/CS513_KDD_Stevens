#knowledge Discovery and Data Mining (CS 513) Homework 6.1: Decision Tree Using C5.0 methodology
#Course : CS 513-A
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : HW_06_Question1_C5.0 methodology

#clearing object environment
rm(list = ls())
#get working directory
getwd()

#Import package C50 for C5.0 Decision Tree Algorithm , caret package to calculate confusion matrix metrics 
library(class)
library(caret)
library(C50)

#Load the “breast-cancer-wisconsin.data.csv” from canvas into R and perform the C5.0 decision tree methodology
dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/breast-cancer-wisconsin.csv",na.string = "?" )
View(dataSet)
#Summarizing each column
summary(dataSet)
#Converting the type of column F6 from character to numeric
n<-as.numeric(as.character(dataSet$F6))
summary(n,na.rm=TRUE)
#Checking the number of rows 
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

#Implementing C5.0 algorithm
C5.0_algo<-C5.0(Class~.,train[,-1])
summary(C5.0_algo)
plot(C5.0_algo)
#Predicting testing class 
predict_alg<-predict(C5.0_algo,test)
print(length(predict_alg))

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
###----- END OF ASSIGNMENT----###

