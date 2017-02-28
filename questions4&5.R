#####################################Question 4
library(ElemStatLearn); data(SAheart); library(caret)

set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
trainSA$chd=as.factor(trainSA$chd)
testSA$chd=as.factor(testSA$chd)

model=train(chd ~ age + alcohol + obesity + tobacco + typea + ldl ,data=trainSA,
            method="glm",family="binomial")

testPredictions=predict(model,testSA)
trainPredictions=predict(model,trainSA)

###Missclasification rate in R
mean(as.numeric(trainPredictions) != as.numeric(trainSA$chd))
mean(as.numeric(testPredictions) != as.numeric(testSA$chd))


#############################Question 5
library(ElemStatLearn); library(caret); data(vowel.train); data(vowel.test)
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)

set.seed=33833
library(randomForest)
model=randomForest(y ~ .,data=vowel.train)
imp=varImp(model,scale=F)
