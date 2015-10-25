### SECTION 0: Load in .zip and list all files of interests
library(knitr)
opts_knit$set(root.dir = '/Users/easterEgg')

library(downloader)
url = "https://s3.amazonaws.com/tripdata/201307-citibike-tripdata.zip"
filename = "dat.zip"
if(!file.exists(filename)){ download.file(url, filename, method='curl')}

# Only the following listed files are used in the current analysis:
datUnzip = unzip("dat.zip")
files = list(datUnzip)[[1]][c(1,2,14:16,26:28)]  


### SECTION 1: Row bind the training and the test sets to create one data set, *combDat*
testdat = read.table(files[4])  # from "X_test.txt", dimension: 2947x561 
traindat = read.table(files[7]) # from "X_train.txt", dimension: 7352x561 
combDat = rbind(testdat, traindat)


### SECTION 2: Extract only the measurement data about *mean* and *standard deviation* in *combDat* 
featuresColNames = read.table(files[2]) # from "features.txt", 561x2, 561 features

meanIdx = grep(".-mean()",featuresColNames$V2)
meanFreqIdx = grep(".-meanFreq()",featuresColNames$V2)
meanOnlyIdx = meanIdx[! meanIdx %in% meanFreqIdx]
meanColNames = featuresColNames$V2[meanOnlyIdx]

combDatSub_means = combDat[,meanOnlyIdx] # 10299x33, 33 mean features
names(combDatSub_means) = meanColNames

stdIdx = grep(".-std()",featuresColNames$V2)
stdColNames = featuresColNames$V2[stdIdx]

combDatSub_std = combDat[,stdIdx]  # 10299x33, 33 std features
names(combDatSub_std) = stdColNames

meanstdData = cbind(combDatSub_means, combDatSub_std) # 10299x33, 66 features

# The data frame, *meanstdData*, contains 10,299 train and test combined observations, with 66 named columns on *mean* and *std*.


### SECTION 3: Create the data frame, *meanstdData_subj_actiY*, by appeneding meanstdData to two columns: *Subject* and *Activity*
subject_test = read.table(files[3]) # from "subject_test.txt", 2947x1
subject_train = read.table(files[6]) # from "subject_train.txt", 7352x1

y_test = read.table(files[5]) # from "y_test.txt", 2947x1
y_train = read.table(files[8]) # from "y_train.txt", 7352x1

subject_testTrain = rbind(subject_test, subject_train) # 10299x1
names(subject_testTrain) = "Subject" 
y_testTrain = rbind(y_test, y_train) # 10299x1
names(y_testTrain) = "Activity"  

meanstdData_subj_actiY = cbind(meanstdData, subject_testTrain, y_testTrain) # 10299x68


### SECTION 4: In *meanstdData_subj_actiY*, modify subject indices and substitute numerical activity identifiers into meaningful descriptive strings 
library(dplyr)
meanstdData_subj_actiY_after10 = meanstdData_subj_actiY %>% filter(Subject >= 10)
meanstdData_subj_actiY_before10 = meanstdData_subj_actiY %>% filter(Subject < 10) %>%  mutate(Subject = paste0(0,Subject)) 
meanstdData_subj_actiY = rbind(meanstdData_subj_actiY_before10,meanstdData_subj_actiY_after10)

actiLabels = read.table(files[1]) # from "activity_labels.txt"

meanstdData_subj_actiY$Activity = as.factor(meanstdData_subj_actiY$Activity) 
levels(meanstdData_subj_actiY$Activity) <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING" )


### SECTION 5: Create a new column: *Subj_Acti*
meanstdData_subj_actiY = meanstdData_subj_actiY %>% mutate(Subj_Acti = paste0(Subject, sep="_", Activity))


### SECTION 6: Create a tidy data set with the average of each features by each subject and each activity
avgsBy_SubjActi = meanstdData_subj_actiY %>% select(c(1:66,69)) %>% group_by(Subj_Acti) %>% 
                                             summarise_each(funs(mean)) %>% arrange(Subj_Acti)


### SECTION 7: Output the data frame, *avgsBy_SubjActi*, as the tidy data set in txt format in the current working directory
write.table(avgsBy_SubjActi, "avgsBy_SubjActi.txt", row.name=FALSE) 


