loans = read.csv('loans.csv')

#Problem 1.1 & 1.2
str(loans)
summary(loans)

#Problem 1.3
table(subset(loans,complete.cases(loans)==FALSE)$not.fully.paid)

#Problem 1.4
library('mice')
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed


#Problem 2.1
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)

train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

mod1=glm(not.fully.paid~.,data=train,family=binomial)
summary(mod1)

#Problem 2.2
mod1$coefficients['fico']*(700-710)
exp(mod1$coefficients['fico']*(700-710))

#Problem 2.3
predicted.risk = predict(mod1,newdata=test,type='response')
test$predicted.risk = predicted.risk
table(test$not.fully.paid,test$predicted.risk>=0.5)
(2401+3)/(2401+3+12+457)#accuracy
table(test$not.fully.paid)
2413/(2413+460)#baseline accuracy

#Problem 2.4
library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)


#Problem 3.1 & 3.2
mod2=glm(not.fully.paid~int.rate,data=train,family=binomial)
summary(mod2)
pred2=predict(mod2,newdata=test,type='response')

max(pred2)
table(pred2>=0.5)

#Problem 3.3
ROCRpred = prediction(pred2, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)


#Problem 4.1
10*exp(0.06*3)


#Problem 5.1
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)*10
