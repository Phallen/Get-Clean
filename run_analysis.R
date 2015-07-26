# run_analysis.R

library(downloader)

# Reference for this database:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Note - data files are described in:
# .\UCI HAR Dataset\README.txt
# .\UCI HAR Dataset\features_info.txt

# Input data file name:
file.name <- "getdata-projectfiles-UCI HAR Dataset.zip"

# Download zipped data file, if needed
if(!file.exists(file.name)){
  print("Download file")
  URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(URL, destfile = file.name)#, method="curl")
  print("Download Complete, unzipping")
  unzip(file.name)
  print("Unzip complete")
}

# Define data directories
DIR1   <- "./UCI HAR Dataset"
SDIR1  <- "./UCI HAR Dataset/train"
SDIR2  <- "./UCI HAR Dataset/test"
SSDIR1 <- "./UCI HAR Dataset/train/Inertial Signals"
SSDIR2 <- "./UCI HAR Dataset/test/Inertial Signals"

# Load auxiliary data ---------------------------
# the "activity" file defines a textual description of each activity code
activity <- read.table(paste(DIR1, "/activity_labels.txt",sep=""),sep=" ")
names(activity) <- c("actcode", "Activity")

# the "features" file gives descriptive labels for each feature
features <- read.table(paste(DIR1, "/features.txt",sep=""),sep=" ")
names(features) <- c("featcode", "Feature")
Nfeat <- dim(features)[1]

# Select only the mean and standard deviation of features - identified by "mean" & "std" in feature name
locm <- grep("mean",features$Feature)
locs <- grep("std",features$Feature)
floc <- sort(c(locm,locs))     # locations of features that will be used
Nfeat2 <- length(floc)

# Read Training data ---------------------
# this file contains the subject ID for each observation in the training set
subject_train <- read.table(paste(SDIR1, "/subject_train.txt",sep=""),sep=" ")
names(subject_train) <- c("Subject")

# if the training data is not present in the workspace, reload
st1<-0
if (!exists("X_train")){
  
  st1 <- system.time({

    X_train <- read.fwf(paste(SDIR1, "/X_train.txt",sep=""),header=FALSE,widths=rep(16L,times=561),
                    buffersize=100, colClasses="numeric", n=7352)
  
    names(X_train) <- features$Feature  # Transfer feature labels to training data
    save(X_train, file="X_train.RData") # save to disk
    
  })
}
cat("Read time:",st1,"\n")

# Load activity codes for training data
y_train <- read.table(paste(SDIR1, "/y_train.txt",sep=""),sep=" ",header=F)
names(y_train) <- c("actcode")


# Read Test Data ------------------------
# this file contains the subject ID for each observation in the test set
subject_test <- read.table(paste(SDIR2, "/subject_test.txt",sep=""),sep=" ")
names(subject_test) <- c("Subject")

# if the training data is not present in the workspace, reload
st2<-0
if (!exists("X_test")){
  
  st2 <- system.time({
    
    X_test <- read.fwf(paste(SDIR2, "/X_test.txt",sep=""),header=FALSE,widths=rep(16L,times=561),
                        buffersize=100, colClasses="numeric", n=7352)
    
    names(X_test) <- features$Feature  # Transfer feature labels to training data
    save(X_test, file="X_test.RData")  # save to disk
    
  })
}
cat("Read time:",st2)

# Load activity codes for test data
y_test <- read.table(paste(SDIR2, "/y_test.txt",sep=""),sep=" ",header=F)
names(y_test) <- c("actcode")

# Combine -----------------------------------------

# Select Variables to be kept (mean & std)
X_train2 <- X_train[,floc]
X_test2  <- X_test[,floc]

# Add Train logical variable
X_train2$Train <- TRUE
X_test2$Train  <- FALSE

# Combine Train & Test data sets
X_data  <- rbind(X_train2, X_test2)
y_data  <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)

# Add columns with subject ID, and activity code
X <- cbind(subject, y_data, X_data)

# Include Activity description
XX <- merge(X,activity,by="actcode")

# Find means
mloc <- grep("mean",names(XX))

# Find standard deviations
sloc <- grep("std",names(XX))

# Specify columns to be used (Activity, Subject, means, standard dev, Train)
uloc<- c(2,3,mloc,sloc,82,83)

Tidyset <- XX[,uloc]

# Simplify names in Tidyset
tnames <- names(Tidyset)
tnames <- gsub("Body","B",tnames)  # abbreviate Body as B
tnames <- gsub("mean","mn",tnames) # abbreviate mean as mn
tnames <- gsub("\\()","",tnames)   # get rid of ()
names(Tidyset) <- tnames

write.table(Tidyset, file="Tidyset.txt", row.name=FALSE)

