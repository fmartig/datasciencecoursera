###############################################################################
#Load the required packages 

library(plyr)
library(dplyr)

###############################################################################
#Download the set of files in the working directory and extract the files 

if (!file.exists("./assignment")){dir.create("./assignment")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file (fileUrl, destfile="./assignment/Dataset.zip", method="curl")
dateDownloaded<-date()

unzip("./Dataset.zip", exdir="./assignment")


###############################################################################
# Make a single training dataset: join training data, activities and subject

features<-read.table("./assignment/UCI HAR Dataset/features.txt")
features_labels<-features[,2]

traindata<- read.table ("./assignment/UCI HAR Dataset/train/X_train.txt", col.names=features_labels)
train_data<-tbl_df(traindata)
rm("traindata")

trainactivity<-read.table("./assignment/UCI HAR Dataset/train/y_train.txt", col.names=c("activity"))
train_activity <-tbl_df(trainactivity)
rm("trainactivity")

trainsubject <-read.table("./assignment/UCI HAR Dataset/train/subject_train.txt", col.names=c("subject"))
train_subject <-tbl_df(trainsubject)
rm("trainsubject")


train_set<-cbind(train_subject,train_activity,train_data)


##Make a single test set

testdata<- read.table("./assignment/UCI HAR Dataset/test/X_test.txt", col.names=features_labels)
test_data<-tbl_df(testdata)
rm("testdata")

testactivity<-read.table("./assignment/UCI HAR Dataset/test/y_test.txt", col.names=c("activity"))
test_activity<-tbl_df(testactivity)
rm("testactivity")
testsubject <-read.table("./assignment/UCI HAR Dataset/test/subject_test.txt", col.names=c("subject"))
test_subject <-tbl_df(testsubject)
rm("testsubject")

test_set<-cbind(test_subject,test_activity, test_data)


###Join the two sets "test_set" and "train_set" "vertically"

total_set<-rbind(train_set, test_set)


#############################################################################
#Extract the"mean" and standard deviation" measurments
## Remove the "meanFreq" variables

temp<- total_set[, grepl('subject|activity|mean|std', names(total_set))]
mean_std<- temp[, !grepl('Freq', names(temp))]


###############################################################################
#Change the names of the activities (ie. replace the numbers by "WALKING", "LAYING"...)

activities <-read.table("./assignment/UCI HAR Dataset/activity_labels.txt")
activities_tbl <- tbl_df (activities)
rm("activities")

final_set<- merge(activities_tbl,mean_std,by.x="V1",by.y="activity", all=TRUE)
result<-final_set %>%
                rename(activity=V2) %>%
                select(-V1)

###############################################################################
#Change the variables names: lower case, remove the () and the -, change "bodybody"

variable_names<-names(result)
variable_names<- tolower(variable_names)

brackets_removed<- gsub("\\(|\\)", "",variable_names)
brackets_minus_removed<- gsub("-", ".",brackets_removed)
body <- gsub("bodybody", "body", brackets_minus_removed)

names(result) <- body

###############################################################################
#"Create a tidy data set with the average of each variable for each activity and each subject"
##Break up the dataset into groups of rows based on the subject and the activity then use 
##summarise_each() with the mean()function to compute the mean of each variable for each group
###NB: use of chaining

tidy <-result %>%
        group_by(activity, subject) %>%
        summarise_each(funs(mean))
#View(tidy)

###############################################################################
#Create a txt file

write.table(tidy, "./assignment/finaldataset.txt", row.names=FALSE)
