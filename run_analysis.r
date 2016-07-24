library(dplyr)
rm(list=ls())
filename <- "getdata_dataset.zip"

## Download and unzip the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Load the datasets
test_data <- read.table('UCI HAR Dataset/test/X_test.txt', stringsAsFactors = FALSE)
train_data <- read.table('UCI HAR Dataset/train/X_train.txt', stringsAsFactors = FALSE)
features <- read.table('UCI HAR Dataset/features.txt')
test_labels <- read.table('UCI HAR Dataset/test/y_test.txt')
train_labels <- read.table('UCI HAR Dataset/train/y_train.txt')
test_subject <- read.table('UCI HAR Dataset/test/subject_test.txt')
train_subject <- read.table('UCI HAR Dataset/train/subject_train.txt')
activity_labels <- read.table('UCI HAR Dataset/activity_labels.txt')

#merge dataset (PART 1)
all_data <-bind_rows(test_data, train_data)

# Label the data set with descriptive variable names from features
names(all_data) <- features[,2]


#get only columns with mean and STD (PART 2)
all_data2 <- all_data[,grep("mean|std",features[,2])]

#Match Activity with Test labels and training labels (PART 3)
test_activity_labeled <- left_join(test_labels, activity_labels, by = 'V1')
train_activity_labeled <- left_join(train_labels, activity_labels, by = 'V1')

#keep only labels
test_activity_labeled <- select(test_activity_labeled, V2)
train_activity_labeled <- select(train_activity_labeled, V2)

#join train & test label data,rename column to Activity, add to selected data cols w/ mean&SD
data_labels <- bind_rows(test_activity_labeled, train_activity_labeled)
names(data_labels)<-"Activity"
all_data3 <- bind_cols(all_data2, data_labels)

#renaming variables to be more descriptive
newnames = names(all_data3)
  newnames <- gsub(pattern="^t",replacement="time",x=newnames)
  newnames <- gsub(pattern="^f",replacement="freq",x=newnames)
  newnames <- gsub(pattern="-?mean[(][)]-?",replacement="Mean",x=newnames)
  newnames <- gsub(pattern="-?std[()][)]-?",replacement="Std",x=newnames)
  newnames <- gsub(pattern="-?meanFreq[()][)]-?",replacement="MeanFreq",x=newnames)
  newnames <- gsub(pattern="BodyBody",replacement="Body",x=newnames)
  names(all_data3) <- newnames

# Bind the subjects to the final data set
data_subjects <- bind_rows(test_subject, train_subject)
names(data_subjects) <- "Subject"
all_data4 <- bind_cols(all_data3, data_subjects)


# Tidy data set showing mean by Subject and Activity (PART 5)
grouped <- group_by(all_data4, Subject, Activity)
tidydata<- summarise_each(grouped, funs(mean))

# Write to the file system
write.table(tidydata, 'tidydata.txt', row.names = FALSE)