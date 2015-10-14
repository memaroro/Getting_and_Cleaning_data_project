
###############################  STEP 1  ##########################################
#########  Merges the training and the test sets to create one data set############

# Y_test file values indicate what activity their corresponding X_test files are 
# measurements of, and the subject_test file values indicate which subject they
# belong to. 

data_obs_test <- read.table("./UCI HAR Dataset./test./X_test.txt")
dim(data_obs_test)      ## dim--> 2947rows  561column ##

data_label_test <- read.table("./UCI HAR Dataset./test./y_test.txt")
dim(data_label_test)      ## dim--> 2947rows  1column  ## 
           
data_subject_test <- read.table("./UCI HAR Dataset./test./subject_test.txt")
dim(data_subject_test)      ## dim--> 2947rows  1column  ##                            

data_obs_train <- read.table("./UCI HAR Dataset./train./X_train.txt")
dim(data_obs_train)      ## dim--> 7352rows  561column ##

data_label_train <- read.table("./UCI HAR Dataset./train./y_train.txt")
dim(data_label_train)      ## dim--> 7352rows  1column ##

data_subject_train <- read.table("./UCI HAR Dataset./train./subject_train.txt")
dim(data_subject_train)      ## dim--> 7352rows  1column ##

df_obs <- rbind(data_obs_test, data_obs_train)
dim(df_obs) ## dim --> 10299rows 561columns ##

df_label <- rbind(data_label_test, data_label_train)
dim(df_label) ## dim --> 10299rows 1column ##

df_subject <- rbind(data_subject_test, data_subject_train)
dim(df_subject) ## dim --> 10299rows 1column ##

###############################  STEP 2  #########################################
######### Extracts only the measurements on the mean and standard deviation ######
######### for each measurement ###################################################

features <- read.table("./UCI HAR Dataset./features.txt")
dim(features)  ## dim --> 561rows 2columns ##

# Limit to columns with feature names matching mean() or std():
meanStd <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStd) # 66 

# subset the desired columns
df_obs <- df_obs[, meanStd]
dim(df_obs) # dim --> 10299rows 66columns ##

# Read features and make the feature names better suited for R 
# with some substitutions
names(df_obs) <- gsub("\\(\\)", "", features[meanStd, 2]) # remove "()"
names(df_obs) <- gsub("mean", "Mean", names(df_obs)) # capitalize M
names(df_obs) <- gsub("std", "Std", names(df_obs)) # capitalize S
names(df_obs) <- gsub("-", "", names(df_obs)) # remove "-" in column names 

###############################  STEP 3  #########################################
####### Uses descriptive activity names to name the activities to data set ######

activities <- read.table("./UCI HAR Dataset./activity_labels.txt")

# update values with correct activity names
df_label[, 1] <- activities[df_label[, 1], 2]

# correct column name
names(df_label) <- "activity"

###############################  STEP 4  #########################################
####### Appropriately label the data set with descriptive variable names #########


# correct column name
names(df_subject) <- "subject"

# bind all the data in a single data set
all_data <- cbind(df_obs, df_label, df_subject)
dim(all_data) # dim --> 10299rows 68columns ##

###############################  STEP 5  #########################################
#################### Create a second, independent tidy data set ##################
###### with the average of each variable for each activity and each subject ######   

# 66 <- 68 columns but last two (activity & subject)
tidy_data <- ddply(all_data, .(subject, activity), function(x) colMeans(x[, 1:66]))
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)
