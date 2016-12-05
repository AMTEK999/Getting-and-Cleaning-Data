##########################################################################################################
## Peer-graded Assignment: Getting and Cleaning Data Course Project
## Satya M
## Dated : December 2016
# Script file : run_analysis.R  Description:
# This script will download from the link https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# The following are the steps it performs.
###########################################################################################################
# S T E P S  for the script
# Step 1.Merges the training and the test sets to create one data set.
# Step 2.Extracts only the measurements on the mean and standard deviation for each measurement.
# Step 3.Uses descriptive activity names to name the activities in the data set
# Step 4.Appropriately labels the data set with descriptive variable names.
# Step 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##########################################################################################################

 
# Step I. Merge the training and the test sets to create one data set.

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd ("C:\\R\\Cleaning\\Assignment\\UCI HAR Dataset")

#Install Plyr Package and load
#install.packages("plyr")
library(plyr)

# Load the data from files.

#imports features.txt
features     = read.table('./features.txt' )
#imports activity_labels.txt
activityType = read.table('./activity_labels.txt')

#Load Training Data
#imports subject_train.txt
subjectTrain = read.table('./train/subject_train.txt')
#imports x_train.txt
xTrain       = read.table('./train/x_train.txt')
#imports y_train.txt
yTrain       = read.table('./train/y_train.txt')

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2] 
colnames(yTrain)        = "activityId"

# Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)

# Load the test data
#imports subject_test.txt
subjectTest = read.table('./test/subject_test.txt') 
#imports x_test.txt
xTest       = read.table('./test/x_test.txt') 
#imports y_test.txt
yTest       = read.table('./test/y_test.txt') 

# Assign column names to the test data 
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2]
colnames(yTest)       = "activityId"


# Merge the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest)


# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData)

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(finalData) 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE]


# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = join(finalData,activityType,by='activityId',match="all")
# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData) 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StandardDeviation",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","frequency",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType']

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = join(tidyData,activityType, by='activityId', match = "all")

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')
