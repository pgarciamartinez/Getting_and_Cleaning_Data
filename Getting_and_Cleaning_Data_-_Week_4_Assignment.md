Getting and Cleaning Data Course Project
----------------------------------------

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>

Here are the data for the project:

<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

You should create one R script called run\_analysis.R that does the following.

### 1. Merges the training and the test sets to create one data set.

Loading the data from the txt files downloaded:

``` r
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", 
                              sep = " ", header = FALSE)
features <- read.table("./UCI HAR Dataset/features.txt", 
                       sep = " ", header = FALSE, stringsAsFactors = FALSE)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", 
                            sep = " ", header = FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", 
                           sep = " ", header = FALSE)
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "", 
                      strip.white = TRUE, header = FALSE, stringsAsFactors = FALSE)
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", sep = "", 
                     strip.white = TRUE, header = FALSE, stringsAsFactors = FALSE)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", sep = "", 
                      strip.white = TRUE, header = FALSE)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", sep = "", 
                     strip.white = TRUE,header = FALSE)
```

Merging the training and testing datasets:

``` r
X_data <- rbind(X_train, X_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)

## Removing original data from environment for clarity:
rm(X_train, X_test, y_train, y_test, subject_train, subject_test)
```

Merging subject\_data (10299x1), X\_data (10299x561) and y\_data (10299x1) into a larger and single data frame:

``` r
data <- cbind(subject_data, X_data, y_data)

## Removing original data from environment for clarity
rm(subject_data, X_data, y_data)
```

Labelling the columns. The data frame consists of: subject\_data, X\_data & y\_data (in this order)

``` r
names(data) <- c("Subject_ID", features[, 2], "Activity_ID")

## Removing features data from environment for clarity
rm(features)
```

### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

Search for the strings 'mean' or 'std' in the column names:

``` r
choices <- c("mean", "std")
columns <- grepl(paste(choices, collapse = "|"), colnames(data))
```

Manually making that the first and last elements true to select those extra columns:

``` r
columns[1] <- TRUE
columns[length(columns)] <- TRUE

# Extracting the data in those columns only
specificData <- data[, columns]

## Removing choices and columns
rm(choices, columns)
```

### 3. Uses descriptive activity names to name the activities in the data set

``` r
table(data$Activity_ID)
```

    ## 
    ##    1    2    3    4    5    6 
    ## 1722 1544 1406 1777 1906 1944

Labelling the activity labels dataset:

``` r
names(activity_labels) <- c("Activity_ID", "Activity")
```

Storing the original order in a temporary column:

``` r
specificData$originalOrder <- 1:nrow(specificData)
```

Assigning the activity labels to the activity\_ID values using merge operation

``` r
specificData <- merge(specificData, activity_labels, by = "Activity_ID")

table(specificData$Activity_ID, specificData$Activity)
```

    ##    
    ##     LAYING SITTING STANDING WALKING WALKING_DOWNSTAIRS WALKING_UPSTAIRS
    ##   1      0       0        0    1722                  0                0
    ##   2      0       0        0       0                  0             1544
    ##   3      0       0        0       0               1406                0
    ##   4      0    1777        0       0                  0                0
    ##   5      0       0     1906       0                  0                0
    ##   6   1944       0        0       0                  0                0

With the temporary column created before, sort the new data as it was before. (The merge operation modified the order to match the activity\_ID)

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
specificData <- arrange(specificData, originalOrder)
```

### 4. Appropriately labels the data set with descriptive variable names.

``` r
columnNames <- names(specificData)
columnNames <- gsub("^t", "timeDomain_", columnNames)
columnNames <- gsub("^f", "frequencyDomain_", columnNames)
columnNames <- gsub(".Acc.", "Acceleration_", columnNames)
columnNames <- gsub(".Gyro.", "Gyroscope_", columnNames)
```

### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Labelling the columns
=====================

names(X\_train) &lt;- features\[, 2\] names(X\_test) &lt;- features\[, 2\] names(subject\_train) &lt;- "Subject\_ID" names(subject\_test) &lt;- "Subject\_ID"

body\_acc\_x\_train &lt;- read.table("./UCI HAR Dataset/train/Inertial Signals/body\_acc\_x\_train.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_acc\_y\_train &lt;- read.table("./UCI HAR Dataset/train/Inertial Signals/body\_acc\_y\_train.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_acc\_z\_train &lt;- read.table("./UCI HAR Dataset/train/Inertial Signals/body\_acc\_z\_train.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_gyro\_x\_train &lt;- read.table("./UCI HAR Dataset/train/Inertial Signals/body\_gyro\_x\_train.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_gyro\_y\_train &lt;- read.table("./UCI HAR Dataset/train/Inertial Signals/body\_gyro\_y\_train.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_gyro\_z\_train &lt;- read.table("./UCI HAR Dataset/train/Inertial Signals/body\_gyro\_z\_train.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) total\_acc\_x\_train &lt;- read.table("./UCI HAR Dataset/train/Inertial Signals/total\_acc\_x\_train.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) total\_acc\_y\_train &lt;- read.table("./UCI HAR Dataset/train/Inertial Signals/total\_acc\_y\_train.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) total\_acc\_z\_train &lt;- read.table("./UCI HAR Dataset/train/Inertial Signals/total\_acc\_z\_train.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_acc\_x\_test &lt;- read.table("./UCI HAR Dataset/test/Inertial Signals/body\_acc\_x\_test.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_acc\_y\_test &lt;- read.table("./UCI HAR Dataset/test/Inertial Signals/body\_acc\_y\_test.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_acc\_z\_test &lt;- read.table("./UCI HAR Dataset/test/Inertial Signals/body\_acc\_z\_test.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_gyro\_x\_test &lt;- read.table("./UCI HAR Dataset/test/Inertial Signals/body\_gyro\_x\_test.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_gyro\_y\_test &lt;- read.table("./UCI HAR Dataset/test/Inertial Signals/body\_gyro\_y\_test.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) body\_gyro\_z\_test &lt;- read.table("./UCI HAR Dataset/test/Inertial Signals/body\_gyro\_z\_test.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) total\_acc\_x\_test &lt;- read.table("./UCI HAR Dataset/test/Inertial Signals/total\_acc\_x\_test.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) total\_acc\_y\_test &lt;- read.table("./UCI HAR Dataset/test/Inertial Signals/total\_acc\_y\_test.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE) total\_acc\_z\_test &lt;- read.table("./UCI HAR Dataset/test/Inertial Signals/total\_acc\_z\_test.txt", sep = "", strip.white = TRUE,header = FALSE, stringsAsFactors = FALSE)

Labelling the columns
=====================

names(activity\_labels) &lt;- c("Activity\_ID", "Activity") names(features) &lt;- c("Feature\_ID", "Features") names(subject\_train) &lt;- "Subject\_ID" names(subject\_test) &lt;- "Subject\_ID" names(X\_train) &lt;- "Value" names(y\_train) &lt;- "Labels" names(X\_test) &lt;- "Value" names(y\_test) &lt;- "Labels" names(body\_acc\_x\_train) &lt;- "body\_acc\_x" names(body\_acc\_y\_train) &lt;- "body\_acc\_y" names(body\_acc\_z\_train) &lt;- "body\_acc\_z" names(body\_acc\_x\_test) &lt;- "body\_acc\_x" names(body\_acc\_y\_test) &lt;- "body\_acc\_y" names(body\_acc\_z\_test) &lt;- "body\_acc\_z" names(body\_gyro\_x\_train) &lt;- "body\_gyro\_x" names(body\_gyro\_y\_train) &lt;- "body\_gyro\_y" names(body\_gyro\_z\_train) &lt;- "body\_gyro\_z" names(body\_gyro\_x\_test) &lt;- "body\_gyro\_x" names(body\_gyro\_y\_test) &lt;- "body\_gyro\_y" names(body\_gyro\_z\_test) &lt;- "body\_gyro\_z" names(total\_acc\_x\_train) &lt;- "total\_acc\_x" names(total\_acc\_y\_train) &lt;- "total\_acc\_y" names(total\_acc\_z\_train) &lt;- "total\_acc\_z" names(total\_acc\_x\_test) &lt;- "total\_acc\_x" names(total\_acc\_y\_test) &lt;- "total\_acc\_y" names(total\_acc\_z\_test) &lt;- "total\_acc\_z"

X\_train &lt;- gsub(X\_train\[1\], " ", "")
===========================================

summary(X\_test)
================

X\_test &lt;- read.table("./UCI HAR Dataset/test/X\_test.txt", sep = "", strip.white = TRUE, header = FALSE, stringsAsFactors = TRUE) names(X\_test) &lt;- "Value" X\_test*V**a**l**u**e* &lt; −*a**s*.*n**u**m**e**r**i**c*(*a**s*.*c**h**a**r**a**c**t**e**r*(*X*<sub>*t*</sub>*e**s**t*Value))

Creating data frames
====================

training &lt;- data.frame(Value = as.numeric(X\_train))
=======================================================
