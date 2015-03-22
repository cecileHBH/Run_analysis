
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation 
##    for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

## we will need the "melt" function so we need the reshape2 library
install.packages("reshape2"); library(reshape2)

## read the files needed
training <- read.table("UCI HAR Dataset/train/X_train.txt")
training.labels <- read.table("UCI HAR Dataset/train/y_train.txt")
training.subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")

test <- read.table("UCI HAR Dataset/test/X_test.txt")
test.labels <- read.table("UCI HAR Dataset/test/y_test.txt")
test.subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")

activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt")


features<-read.table("UCI HAR Dataset/features.txt")

## merging data 
mergedData <- rbind(training,test)

## adding header
colnames(mergedData) <- c(as.character(features[,2]))

## getting the columns number for the mean and standard measurements
meanColNumber <- grep("mean()",colnames(mergedData),fixed=TRUE)
stdColNumber <- grep("std()",colnames(mergedData),fixed=TRUE)

## extracting the measurements on the mean and standard deviation
mergedData.extract <- mergedData[,c(meanColNumber, stdColNumber)]

## merging the measurements labels
mergedData.labels <- rbind(training.labels,test.labels)

## adding the activity name information to the extracted data

mergedData.labels.activity <- merge(mergedData.labels,melt(activity.labels, id.var="V1"), by.x="V1", by.y="V1", all=TRUE)
mergedData.activity <- cbind(mergedData.labels.activity[,3],mergedData.extract)
colnames(mergedData.activity)[1] <- "Activity"


## merging subjects of observations
mergedData.subjects <- rbind(training.subjects,test.subjects)
## adding subjects informations to the data
mergedData.all<-cbind(mergedData.subjects,mergedData.activity)
colnames(mergedData.all)[1] <- "Subject"

## create tidy data set with average
tidyData <- aggregate( mergedData.all[,3] ~ Subject+Activity, data = mergedData.all, FUN = "mean" )

for(i in 4:ncol(mergedData.all)){
    tidyData[,i] <- aggregate( mergedData.all[,i] ~ Subject+Activity, data = mergedData.all, FUN = "mean" )[,3]
}

colnames(tidyData)[3:ncol(tidyData)] <- colnames(mergedData.extract)
tidyData <- tidyData[order(tidyData[,1], tidyData[,2]),]

write.table(tidyData, "tidyData.txt",  row.name=FALSE)







