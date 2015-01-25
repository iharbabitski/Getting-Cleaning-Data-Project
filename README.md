

Quick Guide 
======================================================================================================

To run analysis on the accelerometers reads from the Samsung galaxy S smartphones you need to following steps:

1. download raw data from the following link:https://d396qusza40orc.cloudfront.net/getdata/projectfiles/UCI HAR Dataset.zip 
2. Unpack data in your working directory
3. Download R script run_analysis.R from the following repository: https://github.com/iharbabitski/Getting-Cleaning-Data-Project// check latest commits
4. Run R script to conduct the analysis


RequirenRequirements================================================================================================

To successfully run the analysis it is required to have dplyr package installed
dplyr library will be loaded during script execution


Outputs
======================================================================================================

R script produces two outputs:

1. Tidy data set pr3_table1 which contains measurements on mean and standard deviation on accelerometers reads for the training and test data sets 
and specifies descriptive activity labels.
2. Data set pr3_table2 which contains average of each variable for each activity and each subject for pr3_table1 data set.


Step by Step Analysis 
======================================================================================================

   1	Merges the training and the test sets to create one data set.
		- Program reads raw data files from the working directory and creates new variable for each file. Following files are processed:
			-activity_labels.txt
			-features.txt
			-subject_test.txt
			-X_test.txt
			-y_test.txt
			-subject_train.txt
			-X_train.txt
			-y_train.txt
		- Data sets are merged through the rbind function
		- Subject are added by using cbind

	2	Extracts only the measurements on the mean and standard deviation for each measurement. 
		- Mean and stander deviation columns are identified through the partial string match using grepl functions, unused columns dropped

	3	Uses descriptive activity names to name the activities in the data set
		- Activity labels are populated by using inner_join

	4	Appropriately labels the data set with descriptive variable names. 
		- New columns populated accordingly to the source file names Activity and Subject

	5	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
		- New data set is created by using aggregate function and grouping by both Subjects and Activity



R Script / run_analysis.R
======================================================================================================

# Warning!!!
# to run analysis it is required to have dplyr and sqldf packages installed

##################################STATUS MESSAGE###################################
print("dplyr package is required to run analysis")
print("opening lIbrary(library)

#load packges
packagesy(dplyr)

##################################STATUS MESSAGE###################################
print("opening data files from working directory...")

#features file
features<-read.table("UCI HAR Dataset/features.txt")

#activity_labels file
activity_labels<-read.table("UCI HAR Dataset/activity_labels.txt")

#open files into new variables from the test folder
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
x_test<-read.table("UCI HAR Dataset/test/x_test.txt")
y_test<-read.table("UCI HAR Dataset/test/y_test.txt")

#join activity lables with y_labels to get definitions
y_act_labl<-inner_join(y_test,activity_labels,by="V1")

#insert column names for test set
colnames(x_test)<-features[[2]]

#add labels column 
test_labels<-cbind(y_act_labl[[2]],x_test)

# add column name to label column
colnames(test_labels)[1]<-"Activity_label"

#add subject column
test_subj<-cbind(subject_test,test_labels)

# add column name to subject column
colnames(test_subj)[1]<-"Subject"


#open files into new variables from the Train folder
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")
x_train<-read.table("UCI HAR Dataset/train/x_train.txt")
y_train<-read.table("UCI HAR Dataset/train/y_train.txt")

#join activity lables with y_labels to get definitions
y_act_labl<-inner_join(y_train,activity_labels,by="V1")

#insert column names for train set
colnames(x_train)<-features[[2]]

#add labels column 
train_labels<-cbind(y_act_labl[[2]],x_train)

# add column name to label column
colnames(train_labels)[1]<-"Activity_label"

#add subject column
train_subj<-cbind(subject_train,train_labels)

# add column name to subject column
colnames(train_subj)[1]<-"Subject"

#join train and test datasets together
train_and_test<-rbind(test_subj,train_subj)


#remove columns that do not have "mean" or "std" inside the column name
##initialize new variable for final table - pr3_table1
pr3_table1<-train_and_test

##################################STATUS MESSAGE###################################
print ("producing tydy data set --> pr3_table1, and checking for NA values")

for (i in 563:3) {
  to_remove<-colnames(train_and_test)[i]
  if (grepl("mean()",colnames(train_and_test)[i],fixed=TRUE)==FALSE &
        grepl("std()",colnames(train_and_test)[i],fixed=TRUE)==FALSE)
  {pr3_table1<-pr3_table1[-i]}
  
}

#missing vlues test result:
na_test<-any(is.na(pr3_table1))
print(paste("checking table for the missing values: any(is.na(pr3_table1))=",
            na_test,sep=" "))

##################################STATUS MESSAGE###################################
print("creating tidy data set with the average of each variable for each activity and each subject")


#identify unique subjects (there are 30 subjects by definition) and unique activities
unique_subjects<-c(1:30)
unique_activities<-activity_labels[[2]]


#initialize variable for the final table -- rslt_tbl
rslt_tbl<-data.frame(matrix(ncol = 68, nrow = 1))
names(rslt_tbl)<-colnames(pr3_table1)


# creates a second, independent tidy data set with the average of each variable 
## for each activity and each subject

#concatenate two first columns of the table
tmp1<-do.call(paste0, pr3_table1[c(1,2)])
#add new column into temporary table
tmp2<-cbind(tmp1,pr3_table1)
#calculate mean ffor each activity and each subject
tmp3<-aggregate(tmp2[,4:69],by=c(list(tmp2$Subject),list(tmp2$Activity_label)), mean)
# change first two column names
names(tmp3)[1]="Subject"
names(tmp3)[2]="Activity"
pr3_table2<-tmp3

##################################STATUS MESSAGE###################################
print("analysis completed!")

# write final result into the working directory
##################################STATUS MESSAGE###################################
print("writing final table into working directory")
write.table(pr3_table2,file="pr3_table2.txt",row.name=FALSE)


# Final results
##################################STATUS MESSAGE###################################
print("tidy data set 1 --> pr3_table1")
print("tidy data set 2 --> pr3_table2 -- saved in your working directory")
