# Getting-and-Cleaning-Data-Course-Project

run_analysis.R:
=========================================
## This first set of codes creates a temporary file to store the zipped file, downloads and unzips the files, reads then, and then removes the temporary file that is no longer needed.

temp <- tempfile() #Create a temporary file to store the zipped file
fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = temp, mode = "wb") #downloads it into file "temp" with wb
fileUnzipped <- unzip(temp) #unzip
testFile <- read.table("./UCI HAR Dataset/test/X_test.txt") 
trainFile <- read.table("./UCI HAR Dataset/train/X_train.txt")
unlink(temp) #remove the temporary file 


## Now work on the test data. Download the label and subject txt files. The testSubject matches with trainSubject.

testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt")
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Rename the columns of the label and subject txt files.

colnames(testLabel) <- "labels"
colnames(testSubject) <- "subject"

## Import variable names for test file.

variableNames <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
newNames <- variableNames[ ,2]
colnames(testFile) <- newNames

## Bind the test labels and test file together to create new dataframe with correct variable names.

bindedTestData <- cbind(testSubject, testLabel, testFile)

## Now work on the train data. Download the label and subject txt files. The trainSubject matches with testSubject

trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")

## Rename the columns of the label and subject txt files.
colnames(trainLabel) <- "labels"
colnames(trainSubject) <- "subject"

## Import variable names for test file. Can skip these first two codes and do colnames() since it's the same as test.
#variableNamesTrain <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
#newNames <- variableNames[ ,2]

colnames(trainFile) <- newNames

## Bind the train labels and train file together to create a new dataframe with correct variable names.

bindedTrainData <- cbind(trainSubject, trainLabel, trainFile)

## Merge the bindedTestData and bindedTrainData together
#Use 1:563 to specify we want to merge them based on their 563 common variables
#all=TRUE to merge all data from x and y sets
#no.dups prevents duplicates from being added in and doubling the variable amount
#sort=FALSE does not sort automatically so we can decide how to order it

mergedData <- merge(bindedTestData, bindedTrainData, by.x = 1:563, by.y = 1:563, all = TRUE, no.dups = TRUE, sort = FALSE)

## Order data based on subject

mergedDataSorted <- mergedData[order(mergedData$subject),]

## Add descriptive labels to mergedDataSorted$labels. 
#$labels is a numeric and needs to be changed to a factor. Assign it the six activity labels. 
#Assign these updates to mergedDataSorted$labels to update the df

as.factor(mergedDataSorted$labels)
mergedDataSorted$labels <- factor(mergedDataSorted$labels, levels = c(1, 2, 3, 4, 5, 6), labels = c("walking", "walking upstairs", "walking downstairs", "sitting", "standing", "laying"))

## Extract the mean and standard deviation for each measurement. First 2 variables are for subject/labels. Should be around 68 variables. 
#mean(): Mean values and std(): Standard deviation
#c(1:8,43:48, 83:88, 123:128, 163:168, 203:205,216:217, 229:230, 242:243, 255:256, 268:273, 347:352, 426:431, 505:506,518:519, 531:532, 544:545)] 

library(dplyr)
mergedDataExtractedTest <- select(mergedDataSorted, contains(c("subject", "labels", "mean()", "std()")))

## Label the data set with descriptive variable names.
#rename(new variable name = existing variable name)
#gsub will replace the punctuation marks: dash to period
#rename changes labels to activity. Then changes activity and subject to uppercase.

library(stringr)
mergedDataExtractedTest <- rename(mergedDataExtractedTest, Subject = subject, Activity = labels)
colnames(mergedDataExtractedTest) <- gsub("-", ".", colnames(mergedDataExtractedTest))
colnames(mergedDataExtractedTest) <- gsub("()", "", colnames(mergedDataExtractedTest), fixed = TRUE)
colnames(mergedDataExtractedTest) <- str_replace_all(colnames(mergedDataExtractedTest), "tBody", "TimeBody")
colnames(mergedDataExtractedTest) <- str_replace_all(colnames(mergedDataExtractedTest), "fBody", "FrequencyBody")
colnames(mergedDataExtractedTest) <- str_replace_all(colnames(mergedDataExtractedTest), "tGravity", "TimeGravity")
colnames(mergedDataExtractedTest) <- str_replace_all(colnames(mergedDataExtractedTest), "Gyro", "Gyroscope")
colnames(mergedDataExtractedTest) <- str_replace_all(colnames(mergedDataExtractedTest), "Acc", "Acceleration")
colnames(mergedDataExtractedTest) <- str_replace_all(colnames(mergedDataExtractedTest), "Mag", "Magnitude")
colnames(mergedDataExtractedTest)

## Create a second, independent tidy data set with the average of each variable for each activity and each subject.

library(data.table)
independentData <- copy(mergedDataExtractedTest)
independentDataAverages2 <- independentData %>% group_by(Subject, Activity) %>% summarise(across(where(is.numeric), mean))
independentDataAverages2
