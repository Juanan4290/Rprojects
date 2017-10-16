setwd("G:/DOCUMENTOS/Edx_Analytics Edge/Unit 3_Logistic Regression/R")
songs = read.csv('songs.csv')


#Problem 1.1
table(songs$year)

#Problem 1.2
nrow(subset(songs,artistname=='Michael Jackson'))

#Problem 1.3
subset(songs,artistname=='Michael Jackson'&Top10==1)$songtitle

#Problem 1.4
table(as.factor(songs$timesignature))

#Problem 1.5
songs$songtitle[which.max(songs$tempo)]


#Problem 2.1
songsTrain = subset(songs,year<=2009)
songsTest = subset(songs,year==2010)

#Problem 2.2 & 2.3 & 2.4 & 2.5
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain = songsTrain[ , !(names(songsTrain) %in% nonvars) ]
songsTest = songsTest[ , !(names(songsTest) %in% nonvars) ]

songsLog1 = glm(Top10 ~ ., data=songsTrain, family=binomial)
summary(songsLog1)


#Problem 3.1
cor(songsTrain$loudness,songsTrain$energy)

#Problem 3.2
songsLog2 = glm(Top10 ~ . - loudness, data=songsTrain, family=binomial)
summary(songsLog2)

#Problem 3.3
songsLog3 = glm(Top10 ~ . - energy, data=songsTrain, family=binomial)
summary(songsLog3)


#Problem 4.1 & 4.3 & 4.5
pred3 = predict(songsLog3,newdata=songsTest,type='response')
table(songsTest$Top10,pred3>=0.45)
(309+19)/(309+19+5+40)

#Problem 4.2
table(songsTest$Top10)
314/(314+59)

#Problem 4.4 & 4.5
19/(40+19)  #sensitivity
309/(309+5) #Specificity






