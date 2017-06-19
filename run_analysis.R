# Run Analysis for Data Cleaning Final Project

run_analysis <- function() {
  
  library(dplyr) # Used for getting tidy data mean
  
  # Load and merge data sets
  trainingX  <- read.table("datacleaning4data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
  testX      <- read.table("datacleaning4data/UCI HAR Dataset/test/X_test.txt", header = FALSE)

  trainingY  <- read.table("datacleaning4data/UCI HAR Dataset/train/Y_train.txt", header = FALSE)
  testY      <- read.table("datacleaning4data/UCI HAR Dataset/test/Y_test.txt", header = FALSE)
  
  trainingSubject <- read.table("datacleaning4data/UCI HAR Dataset/train/subject_train.txt", header = FALSE) 
  testSubject     <- read.table("datacleaning4data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)

  namesX     <- read.table("datacleaning4data/UCI HAR Dataset/features.txt", header = FALSE) 
  namesY     <- read.table("datacleaning4data/UCI HAR Dataset/activity_labels.txt", header = FALSE)

  # Merge subject data, then applies proper labels from the activity labels to tidy up
  SubjectData <- rbind(trainingSubject, testSubject)
  SubjectData <- as.factor(SubjectData[,1])
  SubjectData <- data.frame(SubjectData)
  names(SubjectData) <- c("Subject")
  
  YData <- rbind(trainingY, testY)
  YData <- namesY[match(YData[,1], namesY[,1]),2] # Use appropriate Activity names, as per 3
  YData <- data.frame(YData)
  names(YData) <- c("Activity") # Labelled, as per 4
  
  # Merge X data, apply proper column names from the features file
  XData <- data.frame(rbind(trainingX, testX))
  names(XData) <- paste(namesX[,1], namesX[,2]) # Dealing with columns having same names issue
  
  # Merge all datasets together (For 1)
  data <- cbind(SubjectData, YData, XData)
  
  # Extract measurements on the mean and stdev 
  
  # Means and sds for each column, before doing by activity (For 2)
  untidy_data_means <- apply(data, 2, mean, na.rm = TRUE)
  untidy_data_sdevs <- apply(data, 2, sd, na.rm = TRUE)
  
  # Extract means, grouped by Activity and Subject (For 5)
  tidy_data_means = data %>% group_by(Activity, Subject) %>% summarise_all(mean)
  write.table(tidy_data_means, "tidy_data_means.txt", row.names = FALSE)
}