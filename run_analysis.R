getwd()
setwd('./')

# Loading activity labels
actlab <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

# Loading features
feat <- read.table("./UCI HAR Dataset/features.txt")[,2]

# Findinf features with 'mean' and 'std'
good.feat <- grepl("mean|std", feat)

# Loading X_test
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")

# Loading Y_test
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")

# Loading subject_test
subtest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
names(xtest) <- feat

# Finding required test data
xtest <- xtest[,good.feat]

# Giving activities names
ytest[,2] <- actlab[ytest[,1]]
names(ytest) <- c("Activity_Number", "Activity_Name")
names(subtest) <- "Subject"

# Binding test data
test <- cbind(subtest, ytest, xtest)

# Loading X_train
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")

# Loading Y_train
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")

# Loading subject_train
subtrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
names(xtrain) <- feat

# Finding required train data
xtrain <- xtrain[,good.feat]

# Giving activities names
ytrain[,2] <- actlab[ytrain[,1]]
names(ytrain) <- c("Activity_Number", "Activity_Name")
names(subtrain) <- "Subject"

#Binding train data
train <- cbind(subtrain, ytrain, xtrain)

# Merging test and train data
test.train <- rbind(test, train)
id <- c("Subject", "Activity_Number", "Activity_Name")
datalab <- setdiff(colnames(test.train), id)
melting <- melt(test.train, id = id, measure.vars = datalab)

# Applying mean function to data with dcast
tidy <- dcast(melting, Subject + Activity_Name ~ variable, mean)

# Witing tidy_data.txt
write.table(tidy, file = "./tidy_data.txt", row.name=F)