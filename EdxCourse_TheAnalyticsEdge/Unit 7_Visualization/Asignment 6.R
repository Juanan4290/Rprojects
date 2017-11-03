setwd("G:/DOCUMENTOS/Edx_Analytics Edge/Unit 6_Clustering")

###---Document Cluster with Daily Kos---###
rm(list=ls())
#Problem 1.1 & 1.2 & 1.3
dailykos = read.csv("data/dailykos.csv")
disKos = dist(dailykos, method = "euclidean")
clustKos = hclust(disKos, method = "ward.D")

plot(clustKos)

#Problem 1.4
clusters = cutree(clustKos,k=7)
cluster1 = subset(dailykos,clusters==1)
cluster2 = subset(dailykos,clusters==2)
cluster3 = subset(dailykos,clusters==3)
cluster4 = subset(dailykos,clusters==4)
cluster5 = subset(dailykos,clusters==5)
cluster6 = subset(dailykos,clusters==6)
cluster7 = subset(dailykos,clusters==7)

HierCluster = split(dailykos, clusters) #easy way to do the same

#Problem 1.5
tail(sort(colMeans(cluster1)))

#Problem 1.6
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))


#Problem 2.1
set.seed(1000)
KMC = kmeans(dailykos, centers = 7)
table(KMC$cluster)

#Problem 2.2
tail(sort(colMeans(subset(dailykos,KMC$cluster==1))))
tail(sort(colMeans(subset(dailykos,KMC$cluster==2))))
tail(sort(colMeans(subset(dailykos,KMC$cluster==3))))
tail(sort(colMeans(subset(dailykos,KMC$cluster==4))))
tail(sort(colMeans(subset(dailykos,KMC$cluster==5))))
tail(sort(colMeans(subset(dailykos,KMC$cluster==6))))
tail(sort(colMeans(subset(dailykos,KMC$cluster==7))))

#Problem 2.3 & 2.4
table(KMC$cluster,clusters)
    


###---Market Segmentation for Airlines---###
rm(list=ls())
#Problem 1.1 & 1.2
airlines = read.csv("data/AirlinesCluster.csv")
summary(airlines)

#Problem 1.3
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)


#Problem 2.1
distAirlines = dist(airlinesNorm,method="euclidean")
clusterAirlines = hclust(distAirlines, method = "ward.D")
plot(clusterAirlines)

#Problem 2.2 
clustVector = cutree(clusterAirlines,k=5)
airlines1 = subset(airlines,clustVector == "1")

#Problem 2.3
tapply(airlines$Balance, clustVector, mean)
tapply(airlines$QualMiles, clustVector, mean)
tapply(airlines$BonusMiles, clustVector, mean)
tapply(airlines$BonusTrans, clustVector, mean)
tapply(airlines$FlightMiles, clustVector, mean)
tapply(airlines$FlightTrans, clustVector, mean)
tapply(airlines$DaysSinceEnroll, clustVector, mean)


#Problem 3.1
set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
table(KMC$cluster)

#Problem 3.2
tapply(airlines$Balance, KMC$cluster, mean)
tapply(airlines$QualMiles, KMC$cluster, mean)
tapply(airlines$BonusMiles, KMC$cluster, mean)
tapply(airlines$BonusTrans, KMC$cluster, mean)
tapply(airlines$FlightMiles, KMC$cluster, mean)
tapply(airlines$FlightTrans, KMC$cluster, mean)
tapply(airlines$DaysSinceEnroll, KMC$cluster, mean)


###---Predicting Stock Returns with Cluster-Then-Predict---###
rm(list=ls())
#Problem 1.1
stocks = read.csv("data/StocksCluster.csv")

#Problem 1.2
table(stocks$PositiveDec)
6324/(6324+5256)

#Problem 1.3
sort(cor(stocks))

#Problem 1.4
lapply(stocks,mean,na.rm=TRUE)


#Problem 2.1
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

stocksModel = glm(PositiveDec~.,data=stocksTrain,family=binomial)
PositiveDecPredict = predict(stocksModel,type="response")

table(stocksTrain$PositiveDec,PositiveDecPredict>=0.5)
(990+3640)/8106

#Problem 2.2
PositiveDecPredictTest = predict(stocksModel,newdata=stocksTest,type="response")
table(stocksTest$PositiveDec,PositiveDecPredictTest>=0.5)
(417+1553)/3474

#Problem 2.3
table(stocksTest$PositiveDec)
1897/3474

#Resumen resultados
aqTrainLog = 0.5711818
aqTestLog = 0.5670697
aqTestBaseline = 0.5460564

#Problem 3.1
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#Problem 3.2
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

#Problem 3.3 & 3.4
set.seed(144)
km = kmeans(normTrain,centers=3)
table(km$cluster)

#Problem 3.5
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)


#Problem 4.1
stocksTrain1 = subset(stocksTrain, clusterTrain==1)
stocksTrain2 = subset(stocksTrain, clusterTrain==2)
stocksTrain3 = subset(stocksTrain, clusterTrain==3)

stocksTest1 = subset(stocksTest, clusterTest==1)
stocksTest2 = subset(stocksTest, clusterTest==2)
stocksTest3 = subset(stocksTest, clusterTest==3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

#Problem 4.2
StocksModel1 = glm(PositiveDec~.,data=stocksTrain1,family=binomial)
StocksModel2 = glm(PositiveDec~.,data=stocksTrain2,family=binomial)
StocksModel3 = glm(PositiveDec~.,data=stocksTrain3,family=binomial)

summary(StocksModel1)
StocksModel2
StocksModel3

#Problem 4.3
stocksTest1Predict = predict(StocksModel1,newdata=stocksTest1,type="response")
stocksTest2Predict = predict(StocksModel2,newdata=stocksTest2,type="response")
stocksTest3Predict = predict(StocksModel3,newdata=stocksTest3,type="response")

table(stocksTest1$PositiveDec,stocksTest1Predict>=0.5)
(30+774)/(30+774+471+23)
table(stocksTest2$PositiveDec,stocksTest2Predict>=0.5)
(388+757)/(388+757+626+309)
table(stocksTest3$PositiveDec,stocksTest3Predict>=0.5)
(49+13)/(49+13+13+21)

#Problem 4.4
AllPredictions = c(stocksTest1Predict, stocksTest2Predict, stocksTest3Predict)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes,AllPredictions>0.5)
(467+1544)/(467+1544+1110+353)
