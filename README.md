# Data-Cleaning-Project
For the Coursera Course Data Cleaning
Package 'dplyr' is used to work with the data. 

R script called run_analysis.R that does the following.
1- downloads data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2- Merges the training and the test sets to create one data set.
3- Extracts only the measurements on the mean and standard deviation for each measurement.
4- Uses descriptive activity names to name the activities in the data set
5- Appropriately labels the data set with descriptive variable names.
6- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Uses the "group_by" function in dplyr to create 6 activities * 30 subjects = 180 groups of data where the means are calculated. 
