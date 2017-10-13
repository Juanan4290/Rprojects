#Inputs
n=40                            #Sample Size
B=1000                         #Number of simulations
lambda=0.2                      #Lambda for exponential distributions

ExpMean=1/0.2                   #Theorical mean and sd of the exponential distribution
ExpVariance=(1/lambda)^2/n      #Theorical variance of the exponential distribution

set.seed=1
SampleMean=NULL
SampleVar=NULL

#Simulations
for (i in 1:B) {
        sample=rexp(n,lambda)
        SampleMean=c(SampleMean,mean(sample))
} 

#Results
ErrorMean=mean(SampleMean)-ExpMean
ErrorVar=var(SampleMean)-ExpVariance

data.frame(mean(SampleMean),ExpMean,ErrorMean)

hist(rexp(1000,0.2))
hist(SampleMean,40)
