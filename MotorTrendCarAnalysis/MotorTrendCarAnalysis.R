library(datasets); data(mtcars); library(ggplot2)
mtcars$am=as.factor(mtcars$am)

str(mtcars)
head(mtcars)
summary(mtcars)

g=ggplot(mtcars, aes(x=am,y=mpg,fill=am)) +
        geom_boxplot(varwidth=T,width=0.5)+
        geom_jitter(shape=16,position=position_jitter(0.1),alpha=0.5)+
        labs(x="Transmission",y="Miles per galon")+
        scale_x_discrete(labels=c("Manual","Automatic"))
g        

t.test(mpg ~ am,data=mtcars)

fit=lm(mpg ~ am,data=mtcars)
summary(lm(mpg ~ am,data=mtcars))

unique(ave(mtcars$mpg,mtcars$am))

plot(lm(mpg ~ .,data=mtcars))

mtcars$am=as.numeric(mtcars$am)
fit1=lm(mpg ~ am,data=mtcars)
fit2=lm(mpg ~ am + wt + cyl, data=mtcars)
fit3=lm(mpg ~ am + wt + cyl + hp, data=mtcars)


anova(fit1,fit2,fit3)
data(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))


full.model <- lm(mpg ~ ., data = mtcars)
best.model <- step(full.model, direction = "backward")
summary(best.model)