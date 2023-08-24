#knowledge Discovery and Data Mining (CS 513) Homework 2: Exploratory Data Analysis
#Course : CS 513-A
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : Homework 2: Exploratory Data Analysis (EDA) 
#Problem 1 : Load the “breast-cancer-wisconsin.data.csv” from canvas into R and perform the EDA analysis by:

#clearing object environment
rm(list = ls())
#get working directory
getwd()
#Load the “breast-cancer-wisconsin.data.csv” from canvas into R and perform the EDA analysis
dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/breast-cancer-wisconsin.csv",na.string = "?" )
#View Breast Cancer Dataset
View(dataSet)

# I. Summarizing each column (e.g. min, max, mean )
summary(dataSet)

#II. Identifying missing values
is.na(dataSet)
print("Number of Missing Values")
print(sum(is.na(dataSet)))
View(dataSet)

#III. Replacing the missing values with the “mean” of the column.
for(i in 1:ncol(dataSet)){
  dataSet[is.na(dataSet[,i]),i]<-mean(dataSet[,i],na.rm=TRUE)
}
summary(dataSet)
#Rounding Values to first 3 decimal places
dataSet[,-1]<-round(dataSet[,-1],3)
print(dataSet)

#IV. Displaying the frequency table of “Class” vs. F6
data<-table(dataSet$Class,dataSet$F6)
ftable(data)

#V. Displaying the scatter plot of F1 to F6, one pair at a time
plot(dataSet[2:7],main="Scatter Plot of F1 to F6",ph=10,col=2)

#VI. Show histogram box plot for columns F7 to F9
boxplot(dataSet[8:10],main="Box Plot for columns F7 to F9")

#Problem 2: Delete all the objects from your R- environment. Reload the “breast-cancer-wisconsin.data.csv” from canvas into R. Remove any row with a missing value in any of the columns.

#Delete all the objects from your R- environment.
rm(list=ls())
#Reload the “breast-cancer-wisconsin.data.csv” from canvas into R. 
dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/breast-cancer-wisconsin.csv",na.string = "?" )
# Remove any row with a missing value in any of the columns.
Data_missing<-na.omit(dataSet)
View(Data_missing)
nrow(Data_missing)

###  END OF ASSIGNMENT ###
