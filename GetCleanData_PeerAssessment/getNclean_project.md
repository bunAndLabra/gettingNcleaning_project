# CODEBOOK: Averaged Summary Statistics of Human Activity Using Smartphones Dataset
DorC  
October 23, 2015  

<br><br>

# I. Data Background and Raw Data Retriveal 
Measurements of 561 time and frequency domain features are taken from 30 volunteers (subjects) while they are performing six activities labeled as: *WALKING*, *WALKING_UPSTAIRS*, *WALKING_DOWNSTAIRS*, *SITTING*, *STANDING*, and *LAYING*, with a smartphone (Samsung Galaxy S II) on the waist. For technical details on mechanical sensoring, noise filtering and data gathering criteria, please consult the original report listed under **License**.

The zipped data set is downloadable [here][data]. More details are available at the [UCI Machine Learning Respository][UCI ML Respos].

<br><br>

#####License:
Use of this dataset in publications must be acknowledged by referencing the following publication:

Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed as is and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

<br><br>

# II. Data Usage and Variable Descriptions
Only a subset of measurements and files in the raw zipped data file are considered. The goal is to subset and process the measurements on the 66 features on mean and standard deviation (std) for each subject and each activity to obtain the corresponding 66 averages. 

The list of 66 vaiables on mean and std can be found in section 2.4 & 2.6 below. Briefly, the variables represent the mean and std of linear acceleration and angular velocity in all three axes: X, Y and Z sampled by accelerometer and gyroscope (inside the smartphone) at a constant rate of 50 Hz, in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window).

In addition, three variables: *Subject*, *Activity* and *Subj_Acti* are added during data processing (Section 3 and 5) in order to create the final tidy data set with 66 averaged features. 

<br><br>

# III. Data Processing and Creation of the Tidy Dataset
<br><br>

### SECTION 0: Load in .zip and list all files of interests




```r
## important note: This R-chunck is set to be "eval=FALSE". Please delete "eval=FALSE" if there is no file: "dat.zip", in the current working directory!
library(downloader)
url = "https://s3.amazonaws.com/tripdata/201307-citibike-tripdata.zip"
filename = "dat.zip"
if(!file.exists(filename)){ download.file(url, filename, method='curl')}
```

Only the following listed files are used in the current analysis:


```r
datUnzip = unzip("dat.zip")
files = list(datUnzip)[[1]][c(1,2,14:16,26:28)]  
files
```

```
## [1] "./UCI HAR Dataset/activity_labels.txt"    
## [2] "./UCI HAR Dataset/features.txt"           
## [3] "./UCI HAR Dataset/test/subject_test.txt"  
## [4] "./UCI HAR Dataset/test/X_test.txt"        
## [5] "./UCI HAR Dataset/test/y_test.txt"        
## [6] "./UCI HAR Dataset/train/subject_train.txt"
## [7] "./UCI HAR Dataset/train/X_train.txt"      
## [8] "./UCI HAR Dataset/train/y_train.txt"
```

<br><br>

### SECTION 1: Row bind the training and the test sets to create one data set, *combDat*

```r
testdat = read.table(files[4])  # from "X_test.txt", dimension: 2947x561 
traindat = read.table(files[7]) # from "X_train.txt", dimension: 7352x561 
combDat = rbind(testdat, traindat)
dim(combDat) 
```

```
## [1] 10299   561
```

<br><br>

### SECTION 2: Extract only the measurement data about *mean* and *standard deviation* in *combDat* 

#### 2.1 Get the features file 

```r
featuresColNames = read.table(files[2]) # from "features.txt", 561x2, 561 features
head(featuresColNames,1)
```

```
##   V1                V2
## 1  1 tBodyAcc-mean()-X
```

#### 2.3 Get the feature/ column indices and names for measurements about *mean* 

```r
meanIdx = grep(".-mean()",featuresColNames$V2)
meanFreqIdx = grep(".-meanFreq()",featuresColNames$V2)
meanOnlyIdx = meanIdx[! meanIdx %in% meanFreqIdx]
meanColNames = featuresColNames$V2[meanOnlyIdx]
```

#### 2.4 Extract all columns of combDat corresponding to measurements about *mean* 

```r
combDatSub_means = combDat[,meanOnlyIdx] # 10299x33, 33 mean features
names(combDatSub_means) = meanColNames
head(combDatSub_means,1) 
```

```
##   tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z
## 1         0.2571778       -0.02328523       -0.01465376
##   tGravityAcc-mean()-X tGravityAcc-mean()-Y tGravityAcc-mean()-Z
## 1            0.9364893           -0.2827192            0.1152882
##   tBodyAccJerk-mean()-X tBodyAccJerk-mean()-Y tBodyAccJerk-mean()-Z
## 1            0.07204601             0.0457544            -0.1060427
##   tBodyGyro-mean()-X tBodyGyro-mean()-Y tBodyGyro-mean()-Z
## 1          0.1199762        -0.09179234          0.1896285
##   tBodyGyroJerk-mean()-X tBodyGyroJerk-mean()-Y tBodyGyroJerk-mean()-Z
## 1             -0.2048962             -0.1744877            -0.09338934
##   tBodyAccMag-mean() tGravityAccMag-mean() tBodyAccJerkMag-mean()
## 1         -0.8669294            -0.8669294             -0.9297665
##   tBodyGyroMag-mean() tBodyGyroJerkMag-mean() fBodyAcc-mean()-X
## 1          -0.7955439              -0.9251949        -0.9185097
##   fBodyAcc-mean()-Y fBodyAcc-mean()-Z fBodyAccJerk-mean()-X
## 1        -0.9182132        -0.7890915            -0.8996332
##   fBodyAccJerk-mean()-Y fBodyAccJerk-mean()-Z fBodyGyro-mean()-X
## 1             -0.937485            -0.9235514         -0.8235579
##   fBodyGyro-mean()-Y fBodyGyro-mean()-Z fBodyAccMag-mean()
## 1          -0.807916         -0.9179126         -0.7909464
##   fBodyBodyAccJerkMag-mean() fBodyBodyGyroMag-mean()
## 1                 -0.8950612                -0.77061
##   fBodyBodyGyroJerkMag-mean()
## 1                  -0.8901655
```
The data frame, *combDatSub_means*, contains 10,299 train and test combined observations, with 33 named columns on *mean*.

#### 2.5 Get the feature/ column indices and names for measurements about *std*

```r
stdIdx = grep(".-std()",featuresColNames$V2)
stdColNames = featuresColNames$V2[stdIdx]
```

#### 2.6 Extract all columns of *combDat* corresponding to measurements about *std*

```r
combDatSub_std = combDat[,stdIdx]  # 10299x33, 33 std features
names(combDatSub_std) = stdColNames
head(combDatSub_std,1) 
```

```
##   tBodyAcc-std()-X tBodyAcc-std()-Y tBodyAcc-std()-Z tGravityAcc-std()-X
## 1        -0.938404       -0.9200908       -0.6676833          -0.9254273
##   tGravityAcc-std()-Y tGravityAcc-std()-Z tBodyAccJerk-std()-X
## 1          -0.9370141          -0.5642884           -0.9066828
##   tBodyAccJerk-std()-Y tBodyAccJerk-std()-Z tBodyGyro-std()-X
## 1           -0.9380164           -0.9359358        -0.8830891
##   tBodyGyro-std()-Y tBodyGyro-std()-Z tBodyGyroJerk-std()-X
## 1        -0.8161636        -0.9408812            -0.9012242
##   tBodyGyroJerk-std()-Y tBodyGyroJerk-std()-Z tBodyAccMag-std()
## 1            -0.9108601            -0.9392504        -0.7051911
##   tGravityAccMag-std() tBodyAccJerkMag-std() tBodyGyroMag-std()
## 1           -0.7051911            -0.8959942         -0.7620732
##   tBodyGyroJerkMag-std() fBodyAcc-std()-X fBodyAcc-std()-Y
## 1             -0.8943436       -0.9482903       -0.9251369
##   fBodyAcc-std()-Z fBodyAccJerk-std()-X fBodyAccJerk-std()-Y
## 1       -0.6363167           -0.9244291           -0.9432104
##   fBodyAccJerk-std()-Z fBodyGyro-std()-X fBodyGyro-std()-Y
## 1           -0.9478915        -0.9032627         -0.822677
##   fBodyGyro-std()-Z fBodyAccMag-std() fBodyBodyAccJerkMag-std()
## 1        -0.9561651         -0.711074                -0.8963596
##   fBodyBodyGyroMag-std() fBodyBodyGyroJerkMag-std()
## 1             -0.7971128                 -0.9073076
```

The data frame, *combDatSub_std*, contains 10,299 train and test combined observations, with 33 named columns on *std*.

#### 2.7 Combine the two data frames into one: *meanstdData*

```r
meanstdData = cbind(combDatSub_means, combDatSub_std) # 10299x33, 66 features
```

The data frame, *meanstdData*, contains 10,299 train and test combined observations, with 66 named columns on *mean* and *std*.

<br><br>

### SECTION 3: Create the data frame, *meanstdData_subj_actiY*, by appeneding meanstdData to two columns: *Subject* and *Activity*

#### 3.1 Get the numeric subject labels

```r
subject_test = read.table(files[3]) # from "subject_test.txt", 2947x1
subject_train = read.table(files[6]) # from "subject_train.txt", 7352x1
```

#### 3.2 Get the numeric activity labels 

```r
y_test = read.table(files[5]) # from "y_test.txt", 2947x1
y_train = read.table(files[8]) # from "y_train.txt", 7352x1
```

#### 3.3 Row bind *subject_test* & *train* and *y_test* & *train*, respectively, and name the corresponding two columns: *Subject* and *Activity*.

```r
subject_testTrain = rbind(subject_test, subject_train) # 10299x1
names(subject_testTrain) = "Subject" 
y_testTrain = rbind(y_test, y_train) # 10299x1
names(y_testTrain) = "Activity"      
```

#### 3.4 Column bind the data frame *meanstdData*, and the two columns: *Subject* and *Activity*

```r
meanstdData_subj_actiY = cbind(meanstdData, subject_testTrain, y_testTrain) # 10299x68
```

The following subsetting of the whole data frame, *meanstdData_subj_actiY*, is for demonstration purposes.
As expected, the first column is the first measure of *mean*, the 34th columns is the first measure of *std*, and the final two columns are the newly appended *Subject* and *Activity*:

```r
head(meanstdData_subj_actiY[,c(1,34,67,68)],3)  
```

```
##   tBodyAcc-mean()-X tBodyAcc-std()-X Subject Activity
## 1         0.2571778       -0.9384040       2        5
## 2         0.2860267       -0.9754147       2        5
## 3         0.2754848       -0.9938190       2        5
```

<br><br>

### SECTION 4: In *meanstdData_subj_actiY*, modify subject indices and substitute numerical activity identifiers into meaningful descriptive strings 

#### 4.1 Append a leading zero to subjects indexed 1-9 in the column *Subject*, for more optimal display in section6

Different numbers of observations are tabulated for all 30 subjects:

```r
table(meanstdData_subj_actiY$Subject) 
```

```
## 
##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18 
## 347 302 341 317 302 325 308 281 288 294 316 320 327 323 328 366 368 364 
##  19  20  21  22  23  24  25  26  27  28  29  30 
## 360 354 408 321 372 381 409 392 376 382 344 383
```

Convert 1-9 to 01-09:

```r
library(dplyr)
meanstdData_subj_actiY_after10 = meanstdData_subj_actiY %>% filter(Subject >= 10)
meanstdData_subj_actiY_before10 = meanstdData_subj_actiY %>% filter(Subject < 10) %>%  mutate(Subject = paste0(0,Subject)) 
meanstdData_subj_actiY = rbind(meanstdData_subj_actiY_before10,meanstdData_subj_actiY_after10)
```

Leading zero are appended successfully:

```r
table(meanstdData_subj_actiY$Subject) 
```

```
## 
##  01  02  03  04  05  06  07  08  09  10  11  12  13  14  15  16  17  18 
## 347 302 341 317 302 325 308 281 288 294 316 320 327 323 328 366 368 364 
##  19  20  21  22  23  24  25  26  27  28  29  30 
## 360 354 408 321 372 381 409 392 376 382 344 383
```

#### 4.2 Get meaningful activity labels 

```r
actiLabels = read.table(files[1]) # from "activity_labels.txt"
```

There are six activity labels to match the six numerical values in the column *Activity*

```r
actiLabels[,2] 
```

```
## [1] WALKING            WALKING_UPSTAIRS   WALKING_DOWNSTAIRS
## [4] SITTING            STANDING           LAYING            
## 6 Levels: LAYING SITTING STANDING WALKING ... WALKING_UPSTAIRS
```

```r
str(unique(meanstdData_subj_actiY$Activity))
```

```
##  int [1:6] 5 4 6 1 3 2
```

#### 4.3 Substitute numerical identifier by meaningful strings in the column *Activity*

Convert from class integer to factor for string assignment:

```r
meanstdData_subj_actiY$Activity = as.factor(meanstdData_subj_actiY$Activity) 
levels(meanstdData_subj_actiY$Activity) <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING" )
```

Activity labels are assigned successfully:

```r
table(meanstdData_subj_actiY$Activity)  
```

```
## 
##            WALKING   WALKING_UPSTAIRS WALKING_DOWNSTAIRS 
##               1722               1544               1406 
##            SITTING           STANDING             LAYING 
##               1777               1906               1944
```

The columns: *Subject* and *Activity* are successfully modified in *meanstdData_subj_actiY*:

```r
head(meanstdData_subj_actiY[,c(1,34,67,68)],3)  
```

```
##   tBodyAcc-mean()-X tBodyAcc-std()-X Subject Activity
## 1         0.2571778       -0.9384040      02 STANDING
## 2         0.2860267       -0.9754147      02 STANDING
## 3         0.2754848       -0.9938190      02 STANDING
```

<br><br>

### SECTION 5: Create a new column: *Subj_Acti*

```r
meanstdData_subj_actiY = meanstdData_subj_actiY %>% mutate(Subj_Acti = paste0(Subject, sep="_", Activity))
```

Note, the final column is now *Subj_Acti*:

```r
meanstdData_subj_actiY[1,c(1,34,67:69)]  
```

```
##   tBodyAcc-mean()-X tBodyAcc-std()-X Subject Activity   Subj_Acti
## 1         0.2571778        -0.938404      02 STANDING 02_STANDING
```

There are 180 unique *Subj_Acti* combinations:


```r
str(unique(meanstdData_subj_actiY$Subj_Acti)) 
```

```
##  chr [1:180] "02_STANDING" "02_SITTING" "02_LAYING" ...
```

<br><br>

### SECTION 6: Create a tidy data set with the average of each features by each subject and each activity


```r
avgsBy_SubjActi = meanstdData_subj_actiY %>% select(c(1:66,69)) %>% group_by(Subj_Acti) %>% 
                                             summarise_each(funs(mean)) %>% arrange(Subj_Acti)
```

Note, *Subj_Acti* is at the first column:

```r
options(dplyr.print_max = Inf) # set dplyr to show all rows instead of only top 10
head(avgsBy_SubjActi[,c(1,2,35)],7) 
```

```
## Source: local data frame [7 x 3]
## 
##               Subj_Acti tBodyAcc-mean()-X tBodyAcc-std()-X
##                   (chr)             (dbl)            (dbl)
## 1             01_LAYING         0.2215982      -0.92805647
## 2            01_SITTING         0.2612376      -0.97722901
## 3           01_STANDING         0.2789176      -0.99575990
## 4            01_WALKING         0.2773308      -0.28374026
## 5 01_WALKING_DOWNSTAIRS         0.2891883       0.03003534
## 6   01_WALKING_UPSTAIRS         0.2554617      -0.35470803
## 7             02_LAYING         0.2813734      -0.97405946
```

The data frame, *avgsBy_SubjActi*, contains 180 unique *Subj_Acti* combinations and their corresponding averaged 66 features:

```r
dim(avgsBy_SubjActi) 
```

```
## [1] 180  67
```

<br><br>

### SECTION 7: Output the data frame, *avgsBy_SubjActi*, as the tidy data set in txt format in the current working directory

```r
write.table(avgsBy_SubjActi, "avgsBy_SubjActi.txt", row.name=FALSE) 
```

[data]:https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
[UCI ML Respos]:http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

