## title: "run_analysis.R"
## author: "PGM"
## date: "18/11/2017"

## Getting and Cleaning Data Course Project

rm(list=ls())

### 1. Merges the training and the test sets to create one data set.

# Loading the data from the txt files downloaded:
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

# Merging the training and testing datasets:
X_data <- rbind(X_train, X_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)

# Removing original data from environment for clarity:
rm(X_train, X_test, y_train, y_test, subject_train, subject_test)

# Merging subject_data (10299x1), X_data (10299x561) and y_data (10299x1) 
# into a larger and single data frame:
data <- cbind(subject_data, X_data, y_data)

# Removing original data from environment for clarity
rm(subject_data, X_data, y_data)

# Labelling the columns.
# The data frame consists of: subject_data, X_data & y_data (in this order)
names(data) <- c("Subject_ID", features[, 2], "Activity_ID")

## Removing features data from environment for clarity
rm(features)

### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Search for the strings 'mean' or 'std' in the column names:
choices <- c("mean", "std")
columns <- grepl(paste(choices, collapse = "|"), colnames(data))

# Manually making that the first and last elements true to select those extra columns:
columns[1] <- TRUE
columns[length(columns)] <- TRUE

# Extracting the data in those columns only
specificData <- data[, columns]

# Removing choices and columns
rm(choices, columns)

### 3. Uses descriptive activity names to name the activities in the data set

# Showing the distribution of activities:
table(data$Activity_ID)

# Labelling the activity labels dataset:
names(activity_labels) <- c("Activity_ID", "Activity")

# Storing the original order in a temporary column:
specificData$originalOrder <- 1:nrow(specificData)

# Assigning the activity labels to the activity_ID values using merge operation
specificData <- merge(specificData, activity_labels, by = "Activity_ID")

table(specificData$Activity_ID, specificData$Activity)

#With the temporary column created before, sort the new data as it was before.
# (The merge operation modified the order to match the activity_ID)
library(dplyr)

specificData <- arrange(specificData, originalOrder)

# Delete the temporary column "originalOrder"
specificData[, c("originalOrder")] <- list(NULL)

### 4. Appropriately labels the data set with descriptive variable names.

# Making some substitutions of 'strings' into a new variable (columnNames),
# which is initialised to the contents of the previous labels.
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

# Copying back the contents from columnNames into the labels of specificData:
names(specificData) <- columnNames

### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Creating the tidy dataset "tidy"
tidy <- specificData %>% 
  group_by(Activity, Subject_ID) %>%
  summarise_all(mean)

# Exporting the result into a txt file called 'tidy.txt':
write.table(tidy, "./tidy.txt", sep="\t")