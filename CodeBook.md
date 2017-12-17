##### Getting and Cleaning Data ####
##### Main Script ####

### Downloading data and creating data.frames in R to work with ####


data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if(!dir.exists("./data")){dir.create("./data")} # Create a data directory

download.file(url = data_url,
              destfile = "./data/Data.zip") # Download the .zip with the whole data #

if(!dir.exists("./data/unziped.files"))
  {dir.create("./data/unziped.files")} # Create a directory to open the unziped files #

unzip(zipfile = "./data/Data.zip",
      exdir = "./data/unziped.files") # Unzip the downloaded files

testlab <- read.table(file = 
        "./data/unziped.files/UCI HAR Dataset/test/y_test.txt") # Create the dataframe                                                                    # of the 'test' labels
                                                                # for the type of activity
 
test_data <- read.table(file = 
      "./data/unziped.files/UCI HAR Dataset/test/X_test.txt") # Create the dataframe                                                                     # of the 'test' data

test_subject <- read.table(file =                             # Create the dataframe  
"./data/unziped.files/UCI HAR Dataset/test/subject_test.txt") # of the 'test' subject
                                                              # indicate the subject
                                                              # responsible for each
                                                              # measure.

trainlab <- read.table(file =                             
"./data/unziped.files/UCI HAR Dataset/train/y_train.txt") # Create the dataframe                                                                    # of the 'train' labels
                                                          # for the type of activity

train_data <- read.table(file = 
"./data/unziped.files/UCI HAR Dataset/train/X_train.txt") # Create the dataframe                                                                     # of the 'train' data

train_subject <- read.table(file = "./data/unziped.files/UCI HAR Dataset/train/subject_train.txt")
  
variables <- read.table(file = 
"./data/unziped.files/UCI HAR Dataset/features.txt") # Create the dataframe  
                                                     # of the 'test' subject
                                                     # indicate the subject
                                                     # responsible for each
                                                     # measure.

       ## Create a factor variable that identifies each dataset after merging as
       ## 'train' or 'test'

factor_train <- as.factor(sample(x = "train", replace = TRUE, 
                                size = 7352)) 
                                
factor_test <- as.factor(sample(x = "test", replace = TRUE,
                               size = 2947))

#### Reordering, selecting and mergin the data ####

library(dplyr)
library(plyr)

test_tbl <- tbl_df(test_data) # Transforming test data in tbl form

  ## Adding descriptive factors of activity for both datasets (Adding a new variable) ####

testlab_tbl <- tbl_df(testlab) # Transforming test labels data in tbl form

  vectortestlab <- as.character(testlab_tbl$V1) # Creating vector of descriptive names
                                                # for activity 
  vectortestlab <- recode(vectortestlab, 
                          "1" = "WALKING", "2" = "WALKING_UPSTAIRS",
                          "3" = "WALKING_DOWNSTAIR", "4" = "SITTING",
                          "5" = "STANDING", "6" = "LAYING") # Recoding the vector
                                                            # change numbers to descriptive
                                                            # string.
  
  testlab_tbl <- mutate(testlab_tbl, 
                  Activity_Label = as.factor(vectortestlab)) # Add the new recoded variable 
                                                             # to the testlab dataset.
                  
test_tbl <- mutate(test_tbl, Activity_Lab = testlab_tbl$Activity_Label,
                   Data_Type = factor_test, Subject = test_subject$V1) # Add the new recoded variable 
                                                                        # to the whole test dataset.
          
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

charvar <- c(as.character(variables$V2), 
            "Activity_Lab", "Data_Type", "Subject") # A vector of characters with the descriptive  
                                                    # names of the variables (All the dataset)

wmean <- charvar[grep("mean()", charvar)] # Selecting the names with the token 'mean()' in it

wstd <- charvar[grep("std()", charvar)] # Selecting the names with the token 'std()' in it

charvarsel <- c(wmean,wstd) # Mergin of character vector with the names of the variables
                            # with the token 'mean()' and variables with the token 'std()'
                            # in their names

Var_Means_Std <- whole_data[, 
                 which (colnames(whole_data) %in% charvarsel)] # Create a new dataset by selecting
                                                              # only the columns which names match
                                                              # with the names in the vector 
                                                              # charvarsel
                              
Var_Means_Std <- mutate(Var_Means_Std, 
                  Activity_Lab = whole_data$Activity_Lab,
                  Data_Type = whole_data$Data_Type, 
                  Subject = whole_data$Subject) # Adding three variables to the data set 
                                                # Activity lab = descriptive factor of activity
                                                # Data Type = is it from 'test' or from 'train'
                                                # Subject = Variable to identify the subject
                                                #           who performed the activity measured.
                                                

### Creating the second tidy dataset ####

Snd_Tidy_Set <- aggregate(. ~Subject + Activity_Lab, Var_Means_Std, mean)
Snd_Tidy_Set <- Snd_Tidy_Set[order(Snd_Tidy_Set$Subject, Snd_Tidy_Set$Activity_Lab),]

### Saving New Dataset ####

write.csv(Snd_Tidy_Set, "Summary_Tidy_Data.csv", row.names = FALSE)
