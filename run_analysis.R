library(swirl)
ls()
rm(list=ls())

# Library Needed
library(dplyr)
library(stringr)
# Read factors
setwd("C:/Users/mrsca/OneDrive - Deadlock Solutions, SRL/Entrenamientos/Coursera/R/Clean_Data")

features <- read.csv("./data/UCI HAR Dataset/features.txt", sep = " ", header = FALSE) 

activitylabels <- read.csv("./data/UCI HAR Dataset/activity_labels.txt", sep = " ", header = FALSE) 

# Test Data load 

path = "./data/UCI HAR Dataset/test/"
files <- list.files(path)
files <- files[grepl(".txt",files)]
filespath <- paste0(path,files)

pathis = "./data/UCI HAR Dataset/test/Inertial Signals/"
filesis <- list.files(pathis)
filesis <- filesis[grepl(".txt",filesis)]

filesispath <- paste0(pathis,filesis)
test.list <- c(filespath,filesispath)

testdf <- lapply(test.list, function(x){ read.csv(x, header=FALSE, stringsAsFactors = FALSE)})


testdfDT <- cbind.data.frame(testdf)
names(testdfDT) <- tolower(gsub("_test.txt","",c(files,filesis)))

# Carga masiva de Trainning

path = "./data/UCI HAR Dataset/train/"
files <- list.files(path)
files <- files[grepl(".txt",files)]
filespath <- paste0(path,files)

pathis = "./data/UCI HAR Dataset/train/Inertial Signals/"
filesis <- list.files(pathis)
filesis <- filesis[grepl(".txt",filesis)]

filesispath <- paste0(pathis,filesis)
train.list <- c(filespath,filesispath)

traindf <- lapply(train.list, function(x){ read.csv(x, header=FALSE, stringsAsFactors = FALSE)})


traindfDT <- cbind.data.frame(traindf)
names(traindfDT) <- tolower(gsub("_train.txt","",c(files,filesis)))

# Merges the training and the test sets to create one data set.

test <- mutate(testdfDT, setstype = "test")
training <- mutate(traindfDT, setstype = "train")

ds <- bind_rows(test,training)

# Uses descriptive activity names to name the activities in the data set

dsfactor <- merge( x=ds, y=activitylabels, by.x = "y", by.y = "V1")

#rename variables

names(dsfactor) <- gsub("V2","activity",names(dsfactor))
names(dsfactor) <- gsub("^y$","activityid",names(dsfactor))
names(dsfactor) <- gsub("^x$","sets",names(dsfactor))


# Extracts only the measurements on the mean and standard deviation for each measurement. 


dsfactordf <- data.frame(dsfactor)


calmean <- function(x){
  mesvar <- gsub("[,|\n]", "", x)
  mesvar <- strsplit(mesvar," ")
  mesvarnum <- sapply(mesvar, as.numeric)
  sapply(mesvarnum, function(x){ mean(x, na.rm = TRUE)} )
}

setsmean <- calmean(dsfactordf$sets)
body_acc_xmean <- calmean(dsfactordf$body_acc_x)
body_acc_ymean <- calmean(dsfactordf$body_acc_y)
body_acc_zmean <- calmean(dsfactordf$body_acc_z)
body_gyro_xmean <- calmean(dsfactordf$body_gyro_x)
body_gyro_ymean <- calmean(dsfactordf$body_gyro_y)
body_gyro_zmean <- calmean(dsfactordf$body_gyro_z)
total_acc_xmean <- calmean(dsfactordf$total_acc_x)
total_acc_ymean <- calmean(dsfactordf$total_acc_y)
total_acc_zmean <- calmean(dsfactordf$total_acc_z)


dsmeans <- data.frame(cbind(
  dsfactordf$subject
  ,dsfactordf$setstype
  ,dsfactordf$activity
  ,setsmean
  ,body_acc_xmean
  ,body_acc_ymean
  ,body_acc_zmean
  ,body_gyro_xmean
  ,body_gyro_ymean
  ,body_gyro_zmean
  ,total_acc_xmean
  ,total_acc_ymean
  ,total_acc_zmean
))

nombresmean <- c( 
  "subject"
  ,"setstype"
  ,"activity"
  ,"setsmean" 
  ,"body_acc_xmean" 
  ,"body_acc_ymean" 
  ,"body_acc_zmean"
  ,"body_gyro_xmean"
  ,"body_gyro_ymean"
  ,"body_gyro_zmean"
  ,"total_acc_xmean"
  ,"total_acc_ymean"
  ,"total_acc_zmean"
)
names(dsmeans) <- nombresmean

# std

calstd <- function(x){
  mesvar <- gsub("[,|\n]", "", x)
  mesvar <- strsplit(mesvar," ")
  mesvarnum <- sapply(mesvar, as.numeric)
  sapply(mesvarnum, function(x){ sd(x, na.rm = TRUE)} )
}

setssd <- calstd(dsfactordf$sets)
body_acc_xsd <- calstd(dsfactordf$body_acc_x)
body_acc_ysd <- calstd(dsfactordf$body_acc_y)
body_acc_zsd <- calstd(dsfactordf$body_acc_z)
body_gyro_xsd <- calstd(dsfactordf$body_gyro_x)
body_gyro_ysd <- calstd(dsfactordf$body_gyro_y)
body_gyro_zsd <- calstd(dsfactordf$body_gyro_z)
total_acc_xsd <- calstd(dsfactordf$total_acc_x)
total_acc_ysd <- calstd(dsfactordf$total_acc_y)
total_acc_zsd <- calstd(dsfactordf$total_acc_z)


dssd <- data.frame(cbind(
  setssd
  ,body_acc_xsd
  ,body_acc_ysd
  ,body_acc_zsd
  ,body_gyro_xsd
  ,body_gyro_ysd
  ,body_gyro_zsd
  ,total_acc_xsd
  ,total_acc_ysd
  ,total_acc_zsd
))

# Appropriately labels the data set with descriptive variable names. 

nombressd <- c( "setssd" ,"body_acc_xsd" ,"body_acc_ysd" ,"body_acc_zsd"
                ,"body_gyro_xsd","body_gyro_ysd","body_gyro_zsd","total_acc_xsd","total_acc_ysd","total_acc_zsd"
)
names(dssd) <- nombressd

dscompleate <- data.frame(cbind(dsmeans,dssd))
names(dscompleate)

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
dsgroup <- group_by(dscompleate, subject, setstype, activity ) 
dssummarize <- summarize(dsgroup,
                         avg_setsmean = mean(as.numeric(setsmean))
                         ,avg_body_acc_xmean  = mean(as.numeric(body_acc_xmean  ))
                         ,avg_body_acc_ymean	= mean(as.numeric(body_acc_ymean	))
                         ,avg_body_acc_zmean	= mean(as.numeric(body_acc_zmean	))
                         ,avg_body_gyro_xmean	= mean(as.numeric(body_gyro_xmean))	
                         ,avg_body_gyro_ymean	= mean(as.numeric(body_gyro_ymean))	
                         ,avg_body_gyro_zmean	= mean(as.numeric(body_gyro_zmean))	
                         ,avg_total_acc_xmean	= mean(as.numeric(total_acc_xmean))	
                         ,avg_total_acc_ymean	= mean(as.numeric(total_acc_ymean))	
                         ,avg_total_acc_zmean	= mean(as.numeric(total_acc_zmean))	
                         ,avg_setssd			= mean(as.numeric(setssd			))
                         ,avg_body_acc_xsd	= mean(as.numeric(body_acc_xsd	  ))
                         ,avg_body_acc_ysd	= mean(as.numeric(body_acc_ysd	  ))
                         ,avg_body_acc_zsd	= mean(as.numeric(body_acc_zsd	  ))
                         ,avg_body_gyro_xsd	= mean(as.numeric(body_gyro_xsd	  ))
                         ,avg_body_gyro_ysd	= mean(as.numeric(body_gyro_ysd	  ))
                         ,avg_body_gyro_zsd	= mean(as.numeric(body_gyro_zsd	  ))
                         ,avg_total_acc_xsd	= mean(as.numeric(total_acc_xsd	  ))
                         ,avg_total_acc_ysd	= mean(as.numeric(total_acc_ysd	  ))
                         ,avg_total_acc_zsd	= mean(as.numeric(total_acc_zsd	  )))

View(dscompleate)
View(dssummarize)
