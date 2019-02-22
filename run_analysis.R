library(data.table)
library(tidyverse)

fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
# Checking if archieve already exists and unzipping
if (!file.exists('./UCI HAR Dataset.zip')){
        download.file(fileurl,'./UCI HAR Dataset.zip', method="curl")
        unzip("UCI HAR Dataset.zip")
}

# 1. Merging the training and the test sets to create one data set.

# features.txt has 561 sensor features with corresponding ID from 1 to 561.
# We create data.frame "features" with column names "featureID" and "feature"
features <- read.table("UCI HAR Dataset/features.txt"
                       , col.names = c("featureID","feature"))

# activity_labels.txt has 6 activity labels with corresponding ID from 1 to 6.
# We create data.frame "features" with column names "activityID" and "activity"
activities <- read.table("UCI HAR Dataset/activity_labels.txt"
                         , col.names = c("activityID", "activity"))

# subject_train.txt has 7352 observations containing a variable from 1 to 30. 
# The variable identifies the subject (volunteer). 
# We give the column the name "subject".
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt"
                            , col.names = "subject")

# y_train.txt has 7352 observations containing 6 activity IDs from 1 to 6
# We give the column the name "activityID".
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt"
                             , col.names = "activityID")

# X_train.txt has 7352 observations containing 561 sensor feature measurements.
# The 561 column feature names are copied from data.frame "features" by using 
# col.names = features$feature
measurement_train <- read.table("UCI HAR Dataset/train/X_train.txt"
                      , col.names = features$feature)

# subject_test.txt has 2947 observations containing a variable from 1 to 30.
# The variable identifies the subject.
# For merging we give column same name as for subject_train.
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt"
                           , col.names = "subject")

# y_test.txt has 2947 observations containing 6 activity IDs from 1 to 6.
# For merging we give column same name as for activity_train.
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt"
                            , col.names = "activityID")

# X_test.txt has 2947 observations containing 561 sensor feature measurements.
# The 561 column feature names are copied from data.frame "features" by using 
# col.names = features$feature
measurement_test <- read.table("UCI HAR Dataset/test/X_test.txt"
                     , col.names = features$feature)

# Merging the rows of train and test data
subject <- rbind(subject_train, subject_test)
activity <- rbind(activity_train, activity_test)
measurement <- rbind(measurement_train, measurement_test)

# Merging the columns of subject, activity and measurement data
merged_data <- cbind(subject, activity, measurement)
# str(merged_data) shows 10299 observations of 563 variables


# 2. Extracting only the measurements on the mean and standard deviation 
# for each measurement.

# We select columns 'subject', 'activityID' and all objects (in the 
# measurement column)that contain 'mean'or 'std' 
mean_std <- merged_data %>% select(subject, activityID
                        , contains("mean"), contains("std"))
# str(mean_std) shows 10299 observations of 88 variables


# 3. Using descriptive activity names to name the activities in the data set.

# We check that activity labels are in the 2nd column of 'activity':
activities[,2]
# [1] WALKING            WALKING_UPSTAIRS   WALKING_DOWNSTAIRS SITTING           
# [5] STANDING           LAYING   

# We replace the activityIDs in mean_std$activityID with the activity labels:
mean_std$activityID <- activities[mean_std$activityID, 2]

# We check that mean_std$activityID now contains labels:
head(mean_std$activityID)
# [1] STANDING STANDING STANDING STANDING STANDING STANDING
# Levels: LAYING SITTING STANDING WALKING WALKING_DOWNSTAIRS WALKING_UPSTAIRS

# We also change the name of column 2 from 'activityID' to 'activity'
names(mean_std)[2] = "activity"


# 4. Appropriately labeling the data set with descriptive variable names.

# gsub performs replacement of all matches respectively
names(mean_std)<-gsub("^t", "Time", names(mean_std)) # ^t = names beginning with t
names(mean_std)<-gsub("^f", "Frequency", names(mean_std))
names(mean_std)<-gsub("Mag", "Magnitude", names(mean_std))
names(mean_std)<-gsub("BodyBody", "Body", names(mean_std))
names(mean_std)<-gsub("tBody", "TimeBody", names(mean_std))
names(mean_std)<-gsub("angle", "Angle", names(mean_std))


# 5. From the data set in step 4, creating a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# mean_std contains 10299 rows. We have 30 subjects with 6 different activities so
# we have 30 x 60 = 180 different categories with 10299/180 ~ 57 values for each 
# category. E.g. subject 2 activity 3 is one category. So before taking mean we
# need to group variables by subject and activity.
TidyData <- mean_std %>%
        group_by(subject, activity) %>%
        # summarise_all() affects every variable (that we grouped)
        # funs() provides a flexible way to generate a named list of functions 
        # (e.g. mean) for input to other functions like summarise_all().
        summarise_all(funs(mean))

str(TidyData)
# Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame': 180 obs. of 88 variables:
# $ subject                     : int  1 1 1 1 1 1 2 2 2 2 ...
# $ activity                    : Factor w/ 6 levels "LAYING","SITTING",..
# $ TimeBodyAcc.mean...X        : num  0.222 0.261 0.279 0.277 0.289 ...
# $ TimeBodyAcc.mean...Y        : num  -0.04051 -0.00131 -0.01614 -0.01738 ...
# ...

write.table(TidyData, "TidyData.txt", row.name=FALSE)
