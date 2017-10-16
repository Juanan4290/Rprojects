setwd("G:/DOCUMENTOS/Edx_Analytics Edge/Unit 3_Logistic Regression/R")
parole = read.csv('parole.csv')

#Problem 1.1 & 1.2
table(parole$violator)


#Problem 2.1
cols = c(1,2,4,7,8,9)
parole[cols] = lapply(parole[cols], factor)
str(parole)

#Problem 2.2
summary(parole)
table(parole$male)


#Problem 3.1 & 3.2
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)


#Problem 4.1 & 4.2
mod1 = glm(violator~.,data=train,family=binomial)
summary(mod1)

#Problem 4.3
exp(sum(mod1$coefficients*c(1,1,0,50,0,0,0,3,12,0,1,0,0))) #odds
1/(1+exp(-(sum(mod1$coefficients*c(1,1,0,50,0,0,0,3,12,0,1,0,0))))) #prob


#Problem 5.1
pred = predict(mod1,newdata=test,type='response')
max(pred)

#Problem 5.2 & 5.4 & 5.5
table(test$violator,pred>=0.1)
(12)/(11+12)#sensitivity
167/(167+12)#specificity
(167+12)/(167+12+11+12)#Accuracy

#Problem 5.3
table(test$violator)
179/(179+23)

#Problem 5.6 & 5.7
library(ROCR)
ROCRpred = prediction(pred, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.25), text.adj=c(-0.2,1.7))
    








