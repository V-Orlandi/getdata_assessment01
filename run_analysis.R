run_analysis <- function() {
  #get files location
  #test_files <- paste(getwd(),"/test/",list.files(paste(getwd(),"/test", sep="")), sep="")
  #train_files <- paste(getwd(),"/train/",list.files(paste(getwd(),"/train", sep="")), sep="")
  
  #labels 
  labels <- read.table("features.txt", header=FALSE, colClasses="character")
  act <- read.table("activity_labels.txt", header=FALSE, colClasses=c("integer","character"))
  
  #'test' tables
  test_sub <- read.table("test/subject_test.txt", header=FALSE, colClasses="numeric")
  test_act <- read.table("test/y_test.txt", header=FALSE, colClasses="numeric")
  test <- read.table("test/X_test.txt", header=FALSE, colClasses="numeric", col.names=labels[,2])
  
  #'train' tables
  train <- read.table("train/X_train.txt", header=FALSE, colClasses="numeric", col.names=labels[,2])
  train_sub <- read.table("train/subject_train.txt", header=FALSE, colClasses="numeric")
  train_act <- read.table("train/y_train.txt", header=FALSE, colClasses="numeric")
  
  #Thanks Dung Minh Tran @ Coursera forum for regex
  #Get position of mean's and std's
  toMatch <- c(".*mean\\(\\).*-X$", ".*std\\(\\).*-X$")
  mean_std <- grepl(paste(toMatch, collapse="|"), labels[,2])
  
  #subset to valid columns
  test <- test[,mean_std]
  train <- train[,mean_std]
  
  #add Subject Column
  test$Subject <- test_sub[,1]
  train$Subject <- train_sub[,1]
  
  #get activity names
  test_act <- merge(test_act, act, by = intersect(names(test_act), names(act)))
  train_act <- merge(train_act, act, by = intersect(names(train_act), names(act)))
  
  #add Activity Column
  test$Activity <- test_act[,2]
  train$Activity <- train_act[,2]
  
  #Create a data set with all values
  data_set <- merge(test, train, by = intersect(names(test), names(train)), all=TRUE)
  
  tidy <- aggregate(data_set,list(Subject = data_set$Subject, Activity = data_set$Activity),mean)
  
}