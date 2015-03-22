run_analysis <- function() {
        ## Returns a tidy data set in which the training data set and the test data set are merged.
        
        features <- read.table("UCI HAR Dataset/features.txt")
        ## print(class(features)) ## shows that features is a data.frame
       
        col_names <- features$V2
       
        ## To convert from factors into strings:
        col_names2 <- as.character(col_names)
       
        
        ## Now, we need to create a factor that selects only the variables with mean() or std() in their names.
       
       
        my_factor <- c()
        #my_factor <- lapply(col_name2, grepl("mean(),") | grepl("std(),"))
        for(title in col_names2) {
                my_factor <- cbind(my_factor, grepl("mean\\(\\)", title) | grepl("std()", title))
        }
       
        
        training <- read.table("UCI HAR Dataset/train/X_train.txt")
       
       
        names(training) <- col_names2
        
        training_activities <- read.table("UCI HAR Dataset/train/y_train.txt")
        names(training_activities) <- c("Activity")
      
        ## now, to add the training activities to training
        training3 <- cbind(training, training_activities)
        
        
        subjects_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
        names(subjects_train) <- c("Subject")
        training2 <- cbind(training3, subjects_train)
        
        test <- read.table("UCI HAR Dataset/test/X_test.txt")
        ## print(head(test, 3))
        names(test) <- col_names2
        
        test_activities <- read.table("UCI HAR Dataset/test/y_test.txt")
        names(test_activities) <- c("Activity")
        ## print(head(test_activities))
        
        ## now, to add the test activities to test
        test3 <- cbind(test, test_activities)
        ## print(head(test2, 2))
        
        subjects_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
        names(subjects_test) <- c("Subject")
        test2 <- cbind(test3, subjects_test)
         
       
        merged_set <- rbind(training2, test2)
        ## print(head(merged_set, 2))
        
        ## print(nrow(training))
        ## print(nrow(test))
        ## print(nrow(merged_set)) ## shows that merged_set has 10299 rows, as expected from merging the 2 sets
         
        # print(nrow(features)) ## shows that features has 561 variables, which are the cols in merged_set
        
        ## print(head(merged_set[,1:5], 2))
       
        ## Now, time to filter out the unwanted col's using my_factor:
        filtered_set <- merged_set[, my_factor]
        #print(tail(filtered_set))
        
        #Adding descriptive activity names to name activities in data set:
        filtered_set$ActivityF <- as.factor(filtered_set$Activity)
        levels(filtered_set$ActivityF) <- c("WALKING", "WALKING-UPSTAIRS", "WALKING-DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
        # print(tail(filtered_set$ActivityF))
        
        #return(filtered_set)
        
        ## Step 5: Creating a new data set with ave of ea variable for ea. activity and ea. subject
        library(dplyr)
        summary_data <- group_by(filtered_set, ActivityF, Subject)
        #print(head(summary_data))
        summary_data_frame <- summarise_each(summary_data, c("mean"))
        #print(head(summary_data_frame))
        write.table(summary_data_frame, file = "summary_data_frame.txt", row.names=FALSE)
}