#knowledge Discovery and Data Mining (CS 513) Homework 7: Artificial Neural Network
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : HW_07_ANN

#clearing object environment
rm(list = ls())
#get working directory
getwd()
#Import package neuralnet for ANN 
library(neuralnet)
#Load the "wisc_bc_ContinuousVar.csv” from canvas into R and perform the ANN

dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/wisc_bc_ContinuousVar.csv",na.string = "?" )
View(dataSet)
#Summarizing each column
summary(dataSet)

table(dataSet$diagnosis)

#Factoring the dataset
dataSet<-data.frame(lapply(na.omit(dataSet),as.numeric))
#partitioning 70% of size
#sample_size<-(0.70*nrow(dataSet))
#Set the seed to make your partition reproducible
set.seed(123)
traindata<-sort(sample(nrow(dataSet),as.integer(nrow(dataSet))))
# 70% of data in training set 
train<-dataSet[traindata,]
# 30% of data in testing set
test<-dataSet[traindata,]
?neuralnet()
#Implementing Neural Network
Ann_algo<- neuralnet(diagnosis~.,train[-1], hidden=5, threshold=0.01)
#Plot the neural network
plot(Ann_algo)

#Test should have 1 input column
ann<-compute(Ann_algo,test)
 answer<-ann$net.result

ann1<-ifelse(answer<1.5,1,2)
length(ann1)
length(test$diagnosis)
table(ann1,test$diagnosis)

error<-(test$diagnosis!=ann1)
length(error)
errorRate<-sum(error)/length(error)
print(errorRate)
