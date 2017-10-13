#Loading
training=read.csv(file="./train.csv",na.strings=c("NA","#DIV/0!",""))
testing=read.csv(file="./test.csv",na.strings=c("NA","#DIV/0!",""))

#Cleaning
NAs=apply(training,2,function(x)sum(is.na(x))/length(x))#NAs percent by column
#blanks=colSums(training=="")/dim(training)[1]#Blanks percent by column
#blanks=blanks[NAs<0.5]

training=training[,NAs<0.5]
#training=training[,blanks<0.5]

testing=testing[,NAs<0.5]
#testing=testing[,blanks<0.5]

#Splitting
inTrain=createDataPartition(y=training$classe,p=0.6,list=F)
myTraining=training[inTrain,]
myTesting=training[-inTrain,]

#Preparing myTraining
myTraining=myTraining[,8:dim(myTraining)[2]]
nzv=nearZeroVar(myTraining, saveMetrics = TRUE)
  
#ALGORITHM
##Decision Trees
TreeModel=train(classe ~ .,data=myTraining,method="rpart")
predictions1=predict(TreeModel,myTesting)
confusionMatrix(predictions1,myTesting$classe)

##Random Forests
rfModel=randomForest(classe ~ .,data=myTraining)
predictions2=predict(rfModel,myTesting)
confusionMatrix(predictions2,myTesting$classe)

predictions3=predict(TreeModel,testing)
predictions4=predict(rfModel,testing)

predictions3
predictions4
