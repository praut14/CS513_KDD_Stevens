#knowledge Discovery and Data Mining (CS 513) Final Exam Problem 4 – Clustering
#Course : CS 513-A
# First Name : Poorvi
#Last Name : Raut
# ID : 20009560
# Purpose : Final Exam Problem 4 – Clustering

#clearing object environment
rm(list = ls())
#get working directory
getwd()

library(clue)
library(cluster)
#Load the "absenteeism_0.csv” from canvas into R and perform the Clustering
dataSet<-read.csv("/Users/Owner/Desktop/Spring 2023/CS 513 KDD/absenteeism_0.csv",na.string = "?" )
View(dataSet)
#Summarizing each column
summary(dataSet)
table(dataSet$Abs_cat)
#Remove the rows with missing values
sum(is.na(dataSet))
dataSet<-na.omit(dataSet)
nrow(dataSet)

abs_cat <- dataSet$Abs_cat
# Remove the "Abs_cat" column
dataSet <- subset(dataSet, select = -c(Abs_cat))

# Standardize the dataset
scaled_data <- scale(dataSet)

# Perform hierarchical clustering
hc <- hclust(dist(scaled_data), method = "ward.D2")


# Determine the optimal number of clusters
plot(hc)
rect.hclust(hc, k = 3, border = "blue")

# Perform K-means clustering
kmeans_clusters <- kmeans(scaled_data, centers = 3)
# Get new cluster labels
new_cluster_labels <- kmeans_clusters$cluster
# Add cluster labels to the dataset
dataSet$cluster <- new_cluster_labels
# View the clusters vs. "Abs_cat"
dataSet$cluster <- kmeans_clusters$cluster
head(dataSet)
length(dataSet$cluster)
length(dataSet$Abs_cat)
table(dataSet$cluster, abs_cat)

# Show the centroid of each K-means cluster
kmeans_clusters$centers



