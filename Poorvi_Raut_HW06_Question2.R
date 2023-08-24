
#Import package randomForest for Random Forest Algorithm , caret package to calculate confusion matrix metrics 
library(class)
library(caret)
library(randomForest)
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
#Implementing Random algorithm
random_algo<-randomForest(Class ~.,data=train,importance=TRUE,ntree=1000,na.action = na.omit)
importance(random_algo)
varImpPlot(random_algo)

#Predicting testing class 
predict_alg<-predict(random_algo,test,type = "class")
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
