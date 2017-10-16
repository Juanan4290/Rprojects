##Assignment 2: climate change
climate = read.csv ("G:/DOCUMENTOS/Edx_Analytics Edge/Unit 2/climate_change.csv")
climate_train = subset(climate, climate$Year <= 2006)
climate_test = subset(climate, climate$Year > 2006)

model1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_train) 
summary(model1) 

cor(climate_train$N2O,climate_train)
cor(climate_train$CFC.11,climate_train)

model2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data = climate_train)
summary(model2)

model3 = step(model1)
summary(model3)

predictions = predict(model3, climate_test)

SSE = sum((predictions - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST
R2

##Assignmetn 2: Reading Test Scores
pisaTrain = read.csv ("G:/DOCUMENTOS/Edx_Analytics Edge/Unit 2/pisa2009train.csv")
pisaTest = read.csv ("G:/DOCUMENTOS/Edx_Analytics Edge/Unit 2/pisa2009test.csv")

nrow(pisaTrain)
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

summary(pisaTrain)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

trainPredictions = predict(lmScore, pisaTrain)
SSE = sum((trainPredictions - pisaTrain$readingScore)^2) #also: SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)

SSE = sum((predTest - pisaTest$readingScore)^2)
RMSE = sqrt(SSE/nrow(pisaTest))
SSE
RMSE

mean(pisaTrain$readingScore)
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST
R2 = 1 - SSE/SST
R2


##Assignment 2: Detecting Flu Epidemics
fluTrain = read.csv("G:/DOCUMENTOS/Edx_Analytics Edge/Unit 2/FluTrain.csv")
fluTrain$Week[which.max(fluTrain$ILI)]
fluTrain$Week[which.max(fluTrain$Queries)]

hist(fluTrain$ILI, breaks = 20)
plot(fluTrain$Queries, log(fluTrain$ILI))

FluTrend1 = lm(log(ILI) ~ Queries, data = fluTrain)
summary(FluTrend1)
cor(log(fluTrain$ILI), fluTrain$Queries)

fluTest = read.csv("G:/DOCUMENTOS/Edx_Analytics Edge/Unit 2/FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata = fluTest))
PredTest1[which(fluTest$Week == "2012-03-11 - 2012-03-17")]

(fluTest$ILI[11] - PredTest1[11]) / fluTest$ILI[11] 
SSE = sum((PredTest1 - fluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(fluTest))
RMSE


library(zoo)
ILILag2 = lag(zoo(fluTrain$ILI), -2, na.pad=TRUE)
fluTrain$ILILag2 = coredata(ILILag2)
plot(log(fluTrain$ILILag2), log(fluTrain$ILI))
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = fluTrain)
summary(FluTrend2)

ILILag2 = lag(zoo(fluTest$ILI), -2, na.pad=TRUE)
fluTest$ILILag2 = coredata(ILILag2)
fluTest$ILILag2[1] = fluTrain$ILI[416]
fluTest$ILILag2[2] = fluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata = fluTest))
SSE = sum((PredTest2 - fluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(fluTest))
RMSE

