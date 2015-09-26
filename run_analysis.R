##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Patrick Talley
## 2015-09-27

# run_analysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################

rm(list=ls())

## 1
# Read the files 
activity_labels = read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE);
features = read.table("./UCI HAR Dataset/features.txt",header=FALSE);
testDat_x = read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE);
testDat_y = read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE);
subject_test = read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE);
trainDat_x = read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE);
trainDat_y = read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE);
subject_train = read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE);

#Assign column names
colnames(activity_labels)  = c('activityId','activityType');
colnames(subject_train)  = "subjectId";
colnames(trainDat_x)        = features[,2]; 
colnames(trainDat_y)        = "activityId";
colnames(subject_test) = "subjectId";
colnames(testDat_x)       = features[,2]; 
colnames(testDat_y)       = "activityId";

#Merge training data
trainDat = cbind(trainDat_y, subject_train, trainDat_x);

#Merge test data
testDat = cbind(testDat_y, subject_test, testDat_x);

#Combine all data sets into a single data set
dat = rbind(trainDat, testDat)

#Creates a vector of column names from dat to select mean and stddev columns
colNames  = colnames(dat);

## 2
#Returns TRUE values for ID, mean and stddev columns and false for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | 
                         grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) 
                        & !grepl("mean..-",colNames) | grepl("-std..",colNames) 
                        & !grepl("-std()..-",colNames));

#Subset our dat for the columns we need
dat = dat[logicalVector==TRUE];


## 3
#Merge dat and activity_labels to include activity names
dat = merge(dat, activity_labels, by = 'activityId', all.x=TRUE);

#Refresh the col names after subset 
colNames = colnames(dat);

## 4
# Clean var names
for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(dat) = colNames;

## 5

#New table, datNoActType without activityType column
datNoActType = dat[,names(dat) != 'activityType'];

#Summarize the datNoActType table to include mean of each var for each activity and each subject
cleandat = aggregate(datNoActType[,names(datNoActType) != c('activityId','subjectId')],
                     by=list(activityId=datNoActType$activityId,subjectId = datNoActType$subjectId),
                     mean);

#Merging the tidyData with activityType to include descriptive acitvity names
cleandat = merge(cleandat,activity_labels,by='activityId',all.x=TRUE);


# Export the cleandat set 
write.table(cleandat, './tidyData.txt',row.names=TRUE,sep='\t');
