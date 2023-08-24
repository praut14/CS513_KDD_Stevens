#knowledge Discovery and Data Mining (CS 513) Homework 8: Clustering
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : HW_08_Clustering

#clearing object environment
rm(list = ls())
#get working directory
getwd()

library(clue)
library(cluster)
#Load the "wisc_bc_ContinuousVar.csv” from canvas into R and perform the ANN

dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/wisc_bc_ContinuousVar.csv",na.string = "?" )
View(dataSet)
#Summarizing each column
summary(dataSet)
table(dataSet$diagnosis)
#Remove the rows with missing values
dataSet<-na.omit(dataSet)
nrow(dataSet)

dataSet<-dataSet[-1]
dataSet1<-dist(dataSet[,-1])

cluster1<-hclust(dataSet1)
plot(cluster1)
cluster2<-cutree(cluster1,2)
table(cluster2,dataSet[,1])


rm(list=ls())

dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/wisc_bc_ContinuousVar.csv",na.string = "?" )
View(dataSet)
#Summarizing each column
summary(dataSet)
table(dataSet$diagnosis)
#Remove the rows with missing values
dataSet<-na.omit(dataSet)
nrow(dataSet)
dataSet<-dataSet[-1]
kmeans_algo<-kmeans(dataSet[,-1],2,nstart = 10)
kmeans_algo$cluster
table(kmeans_algo$cluster,dataSet[,1])
