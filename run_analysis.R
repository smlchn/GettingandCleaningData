f <- "Dataset.zip"
# Load packages
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# Working_directory
path <- getwd()
path

#Dataset_path
input_path <- file.path(path, "Dataset/UCI HAR Dataset")
list.files(input_path, recursive=TRUE)

#Read the subject files

dt_Subject_Train <- fread(file.path(input_path, "train", "subject_train.txt"))
dt_Subject_Test  <- fread(file.path(input_path, "test" , "subject_test.txt" ))

#Read Label files

dt_label_Train <- fread(file.path(input_path, "train", "Y_train.txt"))
dt_label_Test  <- fread(file.path(input_path, "test" , "Y_test.txt" ))

#Read Set files 
fileToDataTable <- function (f) {
    df <- read.table(f)
    dt <- data.table(df)
}
dt_Set_Train <- fileToDataTable(file.path(input_path, "train", "X_train.txt"))
dt_Set_Test  <- fileToDataTable(file.path(input_path, "test" , "X_test.txt" ))

#Concatenate the data tables.

dtSubject <- rbind(dt_Subject_Train, dt_Subject_Test)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dt_label_Train, dt_label_Test)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dt_Set_Train, dt_Set_Test)

#Merge columns

dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

setkey(dt, subject, activityNum)

#Read features.txt 
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

#Read measurements for the mean and standard deviation.

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
#Convert the column numbers to a vector of variable names 


dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode
# Subset variables using variable names.

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

#Read activity_labels.txt file to add descriptive names to the activities.

dtActivityNames <- fread(file.path(input_path, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

#Merge activity labels.

dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)

#Add activityName as a key.

setkey(dt, subject, activityNum, activityName)

#reshape table from short,wide to tall,narrow
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

#Merge activity name.

dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

#Create variable, activity that is equivalent to activityName as a factor class.
#Create variable, feature that is equivalent to featureName as a factor class.

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

#Seperate features from featureName 

grepthis <- function (regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))

## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

#Check to make sure all feature are accounted for by all of the factor class variables.

r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2


#Create a tidy data set

#Create a data set with the average of each variable for each activity and each subject.

setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]



#Save File
f <- file.path(path, "quiz3_proj_dataset.txt")
write.table(dtTidy, f, quote = FALSE, sep = "\t", row.names = FALSE)

