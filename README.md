# Coursera: Getting and Cleaning Data Course Project

This README file explains the analysis files for my Getting and Cleaning Data Course Project assignment:
- UCI HAR Dataset.zip
- run_analysis.R
- Codebook.md
- 

# UCI HAR Dataset.zip

The UCI HAR Dataset.zip was downloaded from following link:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The data is from an experiment where 30 volunteers performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING). The smartphone on their waist recorded pre-processed accelerometer and gyroscope signals. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The features (561 of them) are unlabeled and can be found in the x_test.txt. The six activity labels are in the y_test.txt file. The test subjects (volunteer ID) are in the subject_test.txt file.

The same holds for the training set.

# run_analysis.R

run_analysis.R that does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Codebook.md

Codebook.md contains the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
