run_analysis <- function(){

        ## Setup workspace
        getwd()
        dir <- "C:/Users/userName/Documents/R/GettingAndCleaningData_Project"
        if(!dir.exists(dir)){
                dir.create(dir)}
        setwd(dir)
        dataLoc <- paste(dir,"/UCI HAR Dataset", sep="")
        
        ##       ....
        
                # Read in the datasets
                train_set <- read.table(paste(dataLoc,"/train/X_train.txt", sep=""), header = FALSE)        
                test_set <- read.table(paste(dataLoc,"/test/X_test.txt", sep = ""), header = FALSE)

        
        ##      STEP 1 -- MERGE DATASETS
        ##      DEVELOP MASTER DATASET BY ROW BINDING DATASETS
                test_set$status <- "test"
                train_set$status <- "train"
                combinedData <- rbind(train_set,test_set)
        
        ##      STEP 2 -- EXTRACT MEASUREMENTS WITH MEAN AND STANDARD DEVIATION, ONLY
                fts <- read.table(paste(dataLoc,"/features.txt", sep=""), header = FALSE)
                colnames(combinedData) <- as.character(fts[,2])
                mean_std_only <- combinedData[,grepl("mean|std",names(combinedData))]
                
                
        ##      STEP 3 -- NAME ACTIVITIES
                train_label <- read.table(paste(dataLoc,"/train/y_train.txt", sep=""), header = FALSE)
                test_label <- read.table(paste(dataLoc,"/test/y_test.txt",sep = ""), header = FALSE)
                
                activity_labels <- read.table(paste(dataLoc,"./activity_labels.txt", sep = ""), header = FALSE)
                combinedLabels <- rbind(train_label,test_label)
                
                dataLabels <- merge(combinedLabels,activity_labels,by="V1")
                
        ##      STEP 4 -- LABEL DATA
                train_subjects <- read.table(paste(dataLoc,"/train/subject_train.txt", sep = ""), header = FALSE)
                test_subjects <- read.table(paste(dataLoc,"/test/subject_test.txt", sep = ""), header = FALSE)
                subjects <- rbind(train_subjects,test_subjects)
                
                masterData <- data.frame(subjects, dataLabels[,2],mean_std_only)
                colnames(masterData) <- c("subjects","activity",names(mean_std_only))
                
        ##      STEP 5 -- SUMMARIZE
                tidyData <- tbl_df(masterData)
                dataResults <- tidyData %>% group_by(subjects,activity) %>% summarize_all(funs(mean))
                write.table(dataResults, file = paste(dataLoc,"/tidyDataResults.txt",sep = ""), row.names = FALSE, col.names = TRUE)
                
}
