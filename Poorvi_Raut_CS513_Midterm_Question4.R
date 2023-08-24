#knowledge Discovery and Data Mining (CS 513) Midterm Question 4: Naive Bayes classification
#Course : CS 513-A
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : Midterm: NaiveBayes Algorithm
#clearing object environment
rm(list = ls())
#get working directory
getwd()
#Import package e1071 for Naive Bayes Classifier and class , caret package to calculate confusion matrix metrics 
library(e1071)
library(class)
library(caret)
#Load the “CS513_targeting_cat_full.CSV” from canvas into R and perform the Naive Bayes Methodology
mydata<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/CS513_targeting_cat_full.CSV",na.string = "?" )
View(mydata)
#Remove the missing values
mydata<-na.omit(mydata)
View(mydata)
#Converting the class column to factor class
mydata$Purchase<-factor(mydata$Purchase,levels = c("2","4"),labels = c("Yes","No"))
is.factor(dataSet$Class)
View(mydata)



#partitioning 70% of size
sample_size<-floor(0.70*nrow(mydata))

#Set the seed to make your partition reproducible
set.seed(123)
traindata<-sample(seq_len(nrow(mydata)),size = sample_size)

# 70% of data in training set 
train<-mydata[traindata,]

# 30% of data in testing set
test<-mydata[traindata,]

#Applying naive Bayes algorithm
Naive_Bayes<-naiveBayes(Purchase ~.,data=train)

#Predicting target class 
predict_alg<-predict(Naive_Bayes,test)

# Remove missing values from testing data
#testingdata1 <- testingdata[complete.cases(testingdata), ]

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
