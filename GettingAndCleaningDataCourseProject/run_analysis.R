#It is necesary to check the working directory is OK.
#Data table, dplyr and reshape2 libraries are needed for running the code
if (!require("data.table")){
  install.packages("data.table")
  library(data.table)
}

if (!require("dplyr")){
  install.packages("dplyr")
  library(dplyr)
}

if (!require("reshape2")){
  install.packages("reshape2")
  library(reshape2)
}

####LOAD DATA AND TIDY DATA SETS##########

  #Loading Features and labels of activity (column names)
  features=fread("features.txt")
  activity_labels=fread("activity_labels.txt")
  
  #Loading Training sets
  trainSet=fread("./train/X_train.txt")
  trainLabels=fread("./train/y_train.txt")
  trainSubject=fread("./train/subject_train.txt")
  
  #Loading Test sets
  testSet=fread("./test/X_test.txt")
  testLabels=fread("./test/y_test.txt")
  testSubject=fread("./test/subject_test.txt")

#MUTATE subjects and activity labels into trainSet and testSet
trainLabels=merge(trainLabels,activity_labels,by="V1",sort=F)
trainSet=cbind("subject"=trainSubject$V1,"activity"=trainLabels$V2,trainSet)

testLabels=merge(testLabels,activity_labels,by="V1",sort=F)
testSet=cbind("subject"=testSubject$V1,"activity"=testLabels$V2,testSet)

####Merging the training and the test sets to create one data set
DataSet=as.data.frame(rbind(trainSet,testSet))
  

#RENAME the variables on DataSet with the features (variable "V2" of features)
#data frame)
names(DataSet)=c("subject","activity",features$V2)


#This vector contains the columns of DataSet with mean and standard deviation measurements
#Subjects and activities are incluided as well (columns 1 and 2)
vectorMeanSTD=combine(1,2,(grep("mean\\(\\)|std\\(\\)",names(DataSet))))

####FINAL TIDY DATASET (STEP 4 OF COURSE PROJECT)####        
DataSet=DataSet[,vectorMeanSTD]


#Creating a new independiente data set (DataSet2) from DataSet with the average of each
#variable for each activity and each subject.

DataSet2=DataSet
DataSet2=melt(DataSet2,id.vars=names(DataSet2)[1:2])
DataSet2=dcast(DataSet2,subject+activity~variable,mean)
write.table(DataSet2,file="./tidyData.txt",row.names=FALSE)

#With group-by:
byact<- group_by(DataSet, subject, activity) 
means<-summarize_each(byact, funs(mean(., na.rm=TRUE)))
