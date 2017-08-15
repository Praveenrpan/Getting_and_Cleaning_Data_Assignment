library (dplyr)

## Read txt documents and create training datasets
X_training <- read.table ("./UCI HAR Dataset/train/X_train.txt")
Y_training <-read.table ("./UCI HAR Dataset/train/Y_train.txt")
Sub_training <- read.table ("./UCI HAR Dataset/train/subject_train.txt")

## Read txt documents and create testing datasets
X_testing <- read.table ("./UCI HAR Dataset/test/X_test.txt")
Y_testing <-read.table ("./UCI HAR Dataset/test/Y_test.txt")
Sub_testing <- read.table ("./UCI HAR Dataset/test/subject_test.txt")

## Read variable names
namesofvariables <- read.table ("./UCI HAR Dataset/features.txt")

## Read labels for activity
activity_labels <- read.table ("./UCI HAR Dataset/activity_labels.txt")

## Merge training and test datasets to create one data set
X_merged <- rbind (X_training, X_testing)
Y_merged <- rbind (Y_training, Y_testing)
Sub_merged <- rbind (Sub_training, Sub_testing)

##Extracts only the measurements on the mean and standard deviation for each measurement.
###############

#### Mean and std in their names
meanandstdvariables <- namesofvariables [grep("mean\\(\\)| std\\(\\)", namesofvariables [,2]),]
X_merged <- X_merged[, meanandstdvariables[,1]]

####Uses descriptive activity names to name the activities in the data set
colnames(Y_merged) <- "activity"
Y_merged$activitylabel <- factor (Y_merged$activity, labels= as.character(activity_labels[,2]))
activitylabel <- Y_merged [,2]

####Appropriately labels the data set with descriptive variable names.
colnames (X_merged) <- namesofvariables [meanandstdvariables[,1],2]

####From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
colnames(Sub_merged) <- "subject"
total <- cbind (X_merged, activitylabel, Sub_merged)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarise_all(funs(mean))
write.table (total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names=FALSE, col.names=TRUE)
