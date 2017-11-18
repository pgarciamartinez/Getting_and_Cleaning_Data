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

Showing the distribution of activities:

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

# Delete the temporary column "originalOrder"
specificData[, c("originalOrder")] <- list(NULL)
```

### 4. Appropriately labels the data set with descriptive variable names.

Making some substitutions of 'strings' into a new variable (columnNames), which is initialised to the contents of the previous labels.

``` r
columnNames <- names(specificData)

columnNames <- gsub("^t", "timeDomain_", columnNames)
columnNames <- gsub("^f", "frequencyDomain_", columnNames)
columnNames <- gsub("Acc", "Acceleration_", columnNames)
columnNames <- gsub("Gyro", "Gyroscope_", columnNames)
columnNames <- gsub("[()-]", "", columnNames) # Special characters need to be wrapped by []
columnNames <- gsub("mean", "Mean_", columnNames)
columnNames <- gsub("std", "StandardDeviation_", columnNames)
columnNames <- gsub("Freq", "Frequency_", columnNames)
columnNames <- gsub("Mag", "Magnitude_", columnNames)
```

Copying back the contents from columnNames into the labels of specificData:

``` r
names(specificData) <- columnNames
```

### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Creating the tidy dataset "tidy"

``` r
tidy <- specificData %>% 
  group_by(Activity, Subject_ID) %>%
  summarise_all(mean)
```

Exporting the result into a txt file called 'tidy.txt':

``` r
write.table(tidy, "./tidy.txt", sep="\t", row.name=FALSE)
```
