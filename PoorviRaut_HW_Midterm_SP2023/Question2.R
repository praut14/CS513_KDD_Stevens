#knowledge Discovery and Data Mining (CS 513) Midterm Question 2:EDA Analysis
#Course : CS 513-A
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : Midterm: Exploratory Data Analysis (EDA) 

#clearing object environment
rm(list = ls())
#get working directory
getwd()
#Load the “CS513_targeting_num.csv” from canvas into R and perform the EDA analysis
dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/CS513_targeting_num.csv",na.string = "?" )
View(dataSet)
#I.	Summarizing each numerical column (e.g., min, max, mean)
summary(dataSet)
#II.	Identifying missing values
is.na(dataSet)
print("Number of Missing Values")
print(sum(is.na(dataSet)))
View(dataSet)
#III.	Replacing the numerical missing values with the “median” of the corresponding columns
median(dataSet$Income,na.rm=TRUE)
dataSet[is.na(dataSet$Income ),"Income"]<-median(dataSet$Income,na.rm=TRUE) 
View(dataSet)
#IV.	Displaying the scatter plot of “Age”, and “Income” 
pairs(dataSet[,c(2,4)])
#V.	Show the box plots for columns: “Age” and “Income”
boxplot(dataSet[,c(2,4)])
##End of Question##
