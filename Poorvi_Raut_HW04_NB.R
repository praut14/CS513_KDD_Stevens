#knowledge Discovery and Data Mining (CS 513) Homework 3: Naive Bayes classification
#Course : CS 513-A
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : HW_04_NaiveBayes

#clearing object environment
rm(list = ls())
#get working directory
getwd()
#Import package e1071 for Naive Bayes Classifier and class , caret package to calculate confusion matrix metrics 
library(e1071)
library(class)
library(caret)
#Load the “breast-cancer-wisconsin.data.csv” from canvas into R and perform the Naive Bayes Methodology
dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/breast-cancer-wisconsin.csv",na.string = "?" )
#View Breast Cancer Dataset
View(dataSet)
#remove any rows with missing values
dataSet<-na.omit(dataSet)

View(dataSet)

#Converting the type of column F6 from character to numeric
dataSet$F6<-as.numeric(dataSet$F6)
View(dataSet)

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

#Applying naive Bayes algorithm
Naive_Bayes<-naiveBayes(Class ~.,data=train)

#Predicting target class 
predict_alg<-predict(Naive_Bayes,test)

#creating confusion matrix
conf_matrix<-table(predict_naive=predict_alg,class=test$Class)
print(conf_matrix)
confusionMatrix(predict_alg,test$Class)

#Calculating Accuracy of the algorithm
accuracy<-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(conf_matrix)
#Error rate
e<- 100- accuracy(conf_matrix)
print(e)


