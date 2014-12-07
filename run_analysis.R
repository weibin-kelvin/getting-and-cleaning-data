# 1. Merge the training and the test sets to create one data set.

features     = read.table('C:/Users/weibinkelvin/Downloads/UCI HAR Dataset/features.txt',header=FALSE);
activityType = read.table('C:/Users/weibinkelvin/Downloads/UCI HAR Dataset/activity_labels.txt',header=FALSE); 
subjectTrain = read.table('C:/Users/weibinkelvin/Downloads/UCI HAR Dataset/train/subject_train.txt',header=FALSE); 
xTrain       = read.table('C:/Users/weibinkelvin/Downloads/UCI HAR Dataset/train/x_train.txt',header=FALSE); 
yTrain       = read.table('C:/Users/weibinkelvin/Downloads/UCI HAR Dataset/train/y_train.txt',header=FALSE); 
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";
trainingData = cbind(yTrain,subjectTrain,xTrain);
subjectTest = read.table('C:/Users/weibinkelvin/Downloads/UCI HAR Dataset/test/subject_test.txt',header=FALSE); 
xTest       = read.table('C:/Users/weibinkelvin/Downloads/UCI HAR Dataset/test/x_test.txt',header=FALSE); 
yTest       = read.table('C:/Users/weibinkelvin/Downloads/UCI HAR Dataset/test/y_test.txt',header=FALSE); 
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";
testData = cbind(yTest,subjectTest,xTest);
combineData = rbind(trainingData,testData);
colNames  = colnames(combineData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
combineData = combineData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

combineData = merge(combineData,activityType,by='activityId',all.x=TRUE);
colNames  = colnames(combineData); 

# 4. Appropriately label the data set with descriptive activity names. 

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(combineData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

combineDataNoActivityType  = combineData[,names(combineData) != 'activityType'];
tidyData    = aggregate(combineDataNoActivityType[,names(combineDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=combineDataNoActivityType$activityId,subjectId = combineDataNoActivityType$subjectId),mean);
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');

