Project for the course "Getting and Cleaning Data" by Coursera Johns Hopkins.
Student Name: Nha Tran
============================================================================

Source of data:
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto. 
Smartlab - Non Linear Complex Systems Laboratory 
DITEN - Universit√  degli Studi di Genova, Genoa I-16145, Italy. 
activityrecognition '@' smartlab.ws 
www.smartlab.ws

The data location:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 


1) Process Details:
===================
The specific dataset used for this project includes the following files:

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'X_train.txt': Training set.

- 'y_train.txt': Training labels.

- 'X_test.txt': Test set.

- 'y_test.txt': Test labels. 

- 'subject_train.txt': Each row identifies the subject who performed the activity for each window sample.
                       Its range is from 1 to 30.
 
- 'subject_test.txt': Each row identifies the subject who performed the activity for each window sample.
                       Its range is from 1 to 30. 

Read all the above files into R for the following process:

- Merges the training and the test sets to create one data set.
- Extract only the measurements on the "mean" and standard deviation "std" for each measurement.
- Use descriptive activity names to name the activities in the data set.
- Appropriately labels the data set with descriptive variable names.
- Creates a second data set with the average of each variable for each activity and each subject.
- Write the average data set to a file using write.table()



2) Description of the analysis script:
======================================

Reading all data files into R using read.table() and scan()

Reshape the train and test data set to 561 columns of the features set.
- Set the test data set from 1:1653267 to 2947:561
- Set the train data set from 1:4124472 to 7352:561

As the features data set have bad data we need to clean it using make.names(). 
Also remove the duplicate "BodyBody" from 516 to the end.
 

Extract the set of features names with "mean" and "std" using grep().


Use cbind() to combine train and test data set with subject and activity.
 
Use rbind() to combine test and train data set.

Load the 60 features names with "mean" and "std" to label the combined data set.


Sort the combined data set by subject and activity.
Replace activity labels with descriptive names instead of just numbers. 


Use the aggregate() function to calculate the average for each variable, activity and subject


The columns which we aggregated need to be rename to the proper label "Activity" and "Subject" 

Use write.table() to save the file to disk with "row.name = FALSE"
