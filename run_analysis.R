#1 Merges the training and the test sets to create one data set

features=read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt",
                    col.names =c("id","feature") )
  ## import and combine train data
subject_train=read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt",
                         col.names = "subject_id")
x_train=read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt",col.names = features$feature)
y_train=read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt",col.names = "Activity_id")
library(dplyr)
total_train=bind_cols(subject_train,y_train,x_train,labels="train")

  ## import and combine test data
subject_test=read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt",
                         col.names = "subject_id")
x_test=read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt",col.names = features$feature)
y_test=read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt",col.names = "Activity_id")
total_test=bind_cols(subject_test,y_test,x_test,labels="test")
  ## merge train and test sets into one data set
  total_merge=bind_rows(total_train,total_test)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
variable_meansd=grep(".*(mean|std)(\\.)(\\.).*",colnames(total_merge),value=T)
variable_meansd
meansd_merge=total_merge %>% 
   select(subject_id,Activity_id,variable_meansd,labels)

#3 Uses descriptive activity names to name the activities in the data set
activity_label=read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt",col.names = c("Activity_id","activity"))
meansd_merge=left_join(meansd_merge,activity_label,by="Activity_id")
meansd_merge=meansd_merge %>% 
  select(subject_id,activity,everything())

#4 Appropriately labels the data set with descriptive variable names. 
library(stringr)
colnames(meansd_merge)=str_replace(colnames(meansd_merge),"\\.\\.\\.","\\.")
colnames(meansd_merge)=str_replace(colnames(meansd_merge),"\\.\\.","")
colnames(meansd_merge)

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
average_merge=meansd_merge %>% 
  select(-Activity_id) %>% 
  group_by(subject_id,activity) %>% 
  summarise(across(everything(),list(mean=mean)))
write.table(average_merge,file="getdata_projectfiles_UCI HAR Dataset/summarize.txt",row.name=FALSE)
help("write.table")
