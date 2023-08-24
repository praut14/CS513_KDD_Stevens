#knowledge Discovery and Data Mining (CS 513) Final Exam Problem 3 – CART
#Course : CS 513-A
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : Final Exam Problem 3 – CART

#clearing object environment
rm(list = ls())
#get working directory
getwd()

#Import package rpart for CART Decision Tree Algorithm , caret package to calculate confusion matrix metrics 
library(class)
library(rpart)
library(caret)
library(rpart.plot)

#Load the “absenteeism_1.csv” from canvas into R and perform the C5.0 decision tree methodology
dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/absenteeism_1.csv",na.string = "?" )
#dataSet <- read.csv(file.choose())
View(dataSet)
#Summarizing each column
summary(dataSet)
#Checking the number of rows 
nrow(dataSet)
#Remove the rows with missing values
is.na(dataSet)
dataSet<-na.omit(dataSet)
nrow(dataSet)

# Check unique values in Abs_cat
unique_val<- unique(dataSet$Abs_cat)

# If there is only one unique value, remove the column
if(length(unique_val) == 1) {
  dataSet <- dataSet[, -which(names(dataSet) == "Abs_cat")]
} else {
  # Convert Abs_cat to factor variable
  dataSet$Abs_cat <- as.factor(dataSet$Abs_cat)
}

# Convert Abs_cat to factor variable
#dataSet$Abs_cat <- as.factor(dataSet$Abs_cat)


#dataSet1<-dataSet[2:11]
#View(dataSet1)
#partitioning 70% of size
sample_size<-floor(0.70*nrow(dataSet))
#Set the seed to make your partition reproducible
set.seed(123)
traindata<-sample(seq_len(nrow(dataSet)),size = sample_size)
# 70% of data in training set 
train<-dataSet[traindata,]

# 30% of data in testing set
test<-dataSet[-traindata,]

# Use the model to predict the Abs_cat values for the testing data
#predictions <- predict(model, newdata = test[-5])

#Implementing CART algorithm
cart_algo<-rpart(Abs_cat ~.,data=train,method = "class")
summary(cart_algo)
rpart.plot(cart_algo)
#Predicting target class 
predict_alg<-predict(cart_algo,test,type = "class")
print(length(predict_alg))

#creating confusion matrix
conf_matrix<-table(predict_alg,test$Abs_cat)
print(conf_matrix)
confusionMatrix(predict_alg,test$Abs_cat)

#Calculating Accuracy of the algorithm
accuracy<-function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(conf_matrix)

#Calculating Precision of the algorithm
tp <- sum(predict_alg == "Abs_High" & test$Abs_cat == "Abs_High")
print(tp)
fp <- sum(predict_alg == "Abs_High" & test$Abs_cat != "Abs_High")
print(fp)
precision <- tp / (tp + fp)
cat("Precision for Abs_cat=Abs_High:", precision, "\n")
#Error rate
e<- 100- accuracy(conf_matrix)
print(e)

