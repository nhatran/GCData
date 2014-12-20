## run_analysis.R
## This is the project for the Coursera class "Getting and Cleaning Data"
## Brief description of this program:
## 1) Merge the Training and the Test sets to one data set.
## 2) Extract only the measurements on the mean and standard deviation for each measurement.
## 3) Assign descriptive activity names to the activities in the data set.  
## 4) Appropriately labels the data set with variable names
## 5) Create a new data set with the average of each variable for each activity and each subject.

library(data.table)
library(plyr)


##Reading data files into R
##activity labels
   ActivityLabels <- read.table("activity_labels.txt",header=FALSE,sep=" ",nrows=6)

##subject for train and test 
   subject.train <- read.table("subject_train.txt", sep = " ", header = FALSE)
   subject.test <- read.table("subject_test.txt", sep = " ", header = FALSE)

##train and test data set. 
   x.train <- scan("X_train.txt")
   x.test <- scan("X_test.txt")

##train and test labels
   y.train <- read.table("Y_train.txt", sep = " ", header = FALSE)
   y.test <- read.table("Y_test.txt", sep = " ", header = FALSE)

 
##Set the test data set from 1:1653267 to 2947:561
   alltest <- x.test
   dim(alltest) <- c(2947, 561)

##Set the train data set from 1:4124472 to 7352:561
   alltrain <- x.train
   dim(alltrain) <- c(7352, 561)

## Read features list
   features <- read.table("features.txt", sep = " ", header = FALSE)

##Clean the data for illegal names and remove the "BodyBody" from 516 to the end 
   cleanf <- make.names(features$V2)
   goodf <- cleanf[1:515] 

##Collect the set of features names with "mean" and "std"
   selectf <- c(grep("mean.",goodf,fixed = TRUE), grep("std.",goodf,fixed = TRUE))



##Combine train data set with subject and activity
   ext.train <- cbind(subject.train, y.train, alltrain[,c(selectf)])

##Conbine test data set with subject and activity 
   ext.test <- cbind(subject.test, y.test, alltest[,c(selectf)])
 
##Combine test and train data set
   ext.all <- rbind(ext.test, ext.train)



##Load the 60 features names to assign them to the data set 
   fnames <- 0
   for(i in 1:60)    { fnames[i] <- cleanf[selectf[i]] }

##Label the combined data set
   snames <- c("subject","activity",fnames)
   names(ext.all) <- c(snames)


## Sort the combined data set by subject and activity
   sortdata <- ext.all
   ext.all <- sortdata[with(sortdata, order(subject, activity)), ]


##Replace activity labels with descriptive names instead of just numbers 
  ext.all$activity <- as.character(ext.all$activity)

  ext.all$activity[ext.all$activity == "1"] <- "WALKING"
  ext.all$activity[ext.all$activity == "2"] <- "WALKING_UPSTAIR"
  ext.all$activity[ext.all$activity == "3"] <- "WALKING_DOWNSTAIRS"
  ext.all$activity[ext.all$activity == "4"] <- "SITTING"
  ext.all$activity[ext.all$activity == "5"] <- "STANDING"
  ext.all$activity[ext.all$activity == "6"] <- "LAYING"


##Calculate the average for each variable, activity and subject
  ave.all <- aggregate(ext.all[,3:62], by = list(ext.all$activity,ext.all$subject), FUN = "mean")

##Rename colums after aggregate.
  names(ave.all)[names(ave.all)=="Group.1"] <- "Activity"
  names(ave.all)[names(ave.all)=="Group.2"] <- "Subject"

print(ave.all[,1:4])

## write table to file.
  write.table(ave.all, file = "C:/courseraR/average_all.txt",sep = " ",row.name = FALSE)




