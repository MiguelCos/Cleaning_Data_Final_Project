##### Getting and Cleaning Data ####
##### Main Script ####

#### Downloading data and creating data.frames in R to work with ####

data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if(!dir.exists("./data")){dir.create("./data")}

download.file(url = data_url,
              destfile = "./data/Data.zip")

if(!dir.exists("./data/unziped.files")){dir.create("./data/unziped.files")}

unzip(zipfile = "./data/Data.zip",
      exdir = "./data/unziped.files")

testlab <- read.table(file = "./data/unziped.files/UCI HAR Dataset/test/y_test.txt")

test_data <- read.table(file = "./data/unziped.files/UCI HAR Dataset/test/X_test.txt")

test_subject <- read.table(file = "./data/unziped.files/UCI HAR Dataset/test/subject_test.txt")

trainlab <- read.table(file = "./data/unziped.files/UCI HAR Dataset/train/y_train.txt")

train_data <- read.table(file = "./data/unziped.files/UCI HAR Dataset/train/X_train.txt")

train_subject <- read.table(file = "./data/unziped.files/UCI HAR Dataset/train/subject_train.txt")
  
variables <- read.table(file = "./data/unziped.files/UCI HAR Dataset/features.txt")

factor_train <- as.factor(sample(x = "train", replace = TRUE,
                                size = 7352))
factor_test <- as.factor(sample(x = "test", replace = TRUE,
                               size = 2947))



library(dplyr)
library(plyr)

test_tbl <- tbl_df(test_data)

  ## Adding descriptive factors of activity for both datasets (Adding a new variable) ####

testlab_tbl <- tbl_df(testlab) 

  vectortestlab <- as.character(testlab_tbl$V1)
  vectortestlab <- recode(vectortestlab, "1" = "WALKING", "2" = "WALKING_UPSTAIRS",
                          "3" = "WALKING_DOWNSTAIR", "4" = "SITTING",
                          "5" = "STANDING", "6" = "LAYING")
  
  testlab_tbl <- mutate(testlab_tbl, Activity_Label = as.factor(vectortestlab))
                  
test_tbl <- mutate(test_tbl, Activity_Lab = testlab_tbl$Activity_Label,
                   Data_Type = factor_test, Subject = test_subject$V1) 
          ## test dataset added with one variable Activity_Lab 
          ## which is a factor for each type of activity measured

    
    
train_tbl <- tbl_df(train_data)
       
trainlab_tbl <- tbl_df(trainlab)

  vectortrainlab <- as.character(trainlab_tbl$V1)
  vectortrainlab <- recode(vectortrainlab, "1" = "WALKING", "2" = "WALKING_UPSTAIRS",
                           "3" = "WALKING_DOWNSTAIR", "4" = "SITTING",
                           "5" = "STANDING", "6" = "LAYING")
  
  trainlab_tbl <- mutate(trainlab_tbl, Activity_Label = as.factor(vectortrainlab))
  
train_tbl <- mutate(train_tbl, Activity_Lab = trainlab_tbl$Activity_Label,
                    Data_Type = factor_train, Subject = train_subject$V1)
        ## train dataset added with one variable Activity_Lab 
        ## which is a factor for each type of activity measured


### Merge the two datasets into one ####

whole_data <- rbind(test_tbl, train_tbl)

colnames(whole_data) <- c(as.character(variables$V2), "Activity_Lab", "Data_Type",
                          "Subject")

### Select variables of mean and standar deviation ####

charvar <- c(as.character(variables$V2), "Activity_Lab", "Data_Type", "Subject")

wmean <- charvar[grep("mean()", charvar)]
wstd <- charvar[grep("std()", charvar)]

charvarsel <- c(wmean,wstd)

Var_Means_Std <- whole_data[, which (colnames(whole_data) %in% charvarsel)]
                              
Var_Means_Std <- mutate(Var_Means_Std, Activity_Lab = whole_data$Activity_Lab,
                        Data_Type = whole_data$Data_Type, Subject = whole_data$Subject)

### Creating the second tidy dataset ####

Snd_Tidy_Set <- aggregate(. ~Subject + Activity_Lab, Var_Means_Std, mean)
Snd_Tidy_Set <- Snd_Tidy_Set[order(Snd_Tidy_Set$Subject, Snd_Tidy_Set$Activity_Lab),]

### Saving New Dataset ####

write.csv(Snd_Tidy_Set, "Summary_Tidy_Data.csv", row.names = FALSE)
