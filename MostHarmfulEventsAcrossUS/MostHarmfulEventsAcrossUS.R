#Reproducible Research Data: COURSE PROJECT 1

##Packages installation
###data.table
if (!require(data.table)){install.packages("data.table")}
if (!require(plyr)){install.packages("plyr")}
if (!require(dplyr)){install.packages("dplry")}
if (!require(ggplot2)){install.packages("ggplot2")}

library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)


##Loading and preprocessing the data
StormData=fread("repdata%2Fdata%2FStormData.csv")
StormData=as.data.frame(StormData)

##Subsetting 5 most harmful events for population health
StormHealth=select(StormData,c(EVTYPE,FATALITIES,INJURIES))
StormHealth$EVTYPE=as.factor(StormHealth$EVTYPE)
        
        ###Grouping for types of events
        StormHealth5=group_by(StormHealth,EVTYPE)
        StormHealth5=summarize(StormHealth5,FATALITIES=sum(FATALITIES),
                               INJURIES=sum(INJURIES),TOTAL=sum(c(FATALITIES,INJURIES)))
        
        ###Ordering according to number of total fatalities and injuries
        StormHealth5=StormHealth5[with(StormHealth5,order(-TOTAL)),][1:5,]
        MostHarmful=as.character(StormHealth5$EVTYPE[1])
        
                ###Plotting...
                data=as.data.frame(t(StormHealth5[,2:4]))
                names(data)=StormHealth5$EVTYPE
                
                png("5mostHarmfulEvents.png")
                barplot(as.matrix(data),beside=T,main="5 most harmful events in United States",
                     ylab="people",col=c("yellow","blue","red"),cex.axis=0.8,cex.names=0.8,cex.lab=1.1)
                
                legend("topright", c("Fatalities","Injuries","Total"),fill=c("yellow","blue","red"))
                
                dev.off()
        
        
##Subsetting 5 types of events that have the greatest economic consequences
StormEc=select(StormData,c(EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP))        
StormEc$EVTYPE=as.factor(StormEc$EVTYPE)
StormEc$PROPDMGEXP=as.factor(StormEc$PROPDMGEXP)
StormEc$CROPDMGEXP=as.factor(StormEc$CROPDMGEXP)
        
        ###Replacing exponent code for exponent number
        StormEc$PROPDMGEXP=mapvalues(StormEc$PROPDMGEXP,
                         c("K","M","","B","m","+","0","5","6","?","4","2","3","h","7","H","-","1","8"), 
                         c(1e3,1e6,1,1e9,1e6,1,1,1e5,1e6,1,1e4,1e2,1e3,1,1e7,1e2,1,10,1e8))
        
        StormEc$CROPDMGEXP=mapvalues(StormEc$CROPDMGEXP,
                         c("","M","K","m","B","?","0","k","2"),
                         c( 1,1e6,1e3,1e6,1e9,1,1,1e3,1e2))
        
        ###Transforming PROPDMG and CROPDMG in the correct number with the exponent
        StormEc$PROPDMGEXP=as.numeric(levels(StormEc$PROPDMGEXP))[StormEc$PROPDMGEXP] #transform a factor to numeric
        StormEc$PROPDMG=StormEc$PROPDMG*StormEc$PROPDMGEXP
        
        StormEc$CROPDMGEXP=as.numeric(levels(StormEc$CROPDMGEXP))[StormEc$CROPDMGEXP] #transform a factor to numeric
        StormEc$CROPDMG=StormEc$CROPDMG*StormEc$CROPDMGEXP
       
        ##Subsetting 5 most harmful events for economic consequences
        StormEc=select(StormEc,c(EVTYPE,PROPDMG,CROPDMG))

        ###Grouping for types of events
        StormEc5=group_by(StormEc,EVTYPE)
        StormEc5=summarize(StormEc5,property=sum(PROPDMG),crop=sum(CROPDMG),
                           total=sum(c(PROPDMG,CROPDMG)))
        
        ###Ordering according to economic consequences
        StormEc5=StormEc5[with(StormEc5,order(-total)),][1:5,]
        MostHarmfulEc=as.character(StormEc5$EVTYPE[1])
        
                ###Plotting...
                dataEc=as.data.frame(t(StormEc5[,2:4]))
                names(dataEc)=StormEc5$EVTYPE
                
                png("5mostEconomicEvents.png")
                barplot(as.matrix(dataEc),beside=T,main="top 5 events that have the greatest economic consequences in United States",
                        ylab="dollars",col=c("yellow","blue","red"),cex.axis=0.8,cex.names=0.8,cex.lab=1.1)
                
                legend("topright", c("Property","Crop","Total"),fill=c("yellow","blue","red"))
                
                dev.off()
        