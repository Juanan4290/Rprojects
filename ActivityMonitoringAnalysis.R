#Reproducible Research Data: COURSE PROJECT 1

##Packages installation
        ###data.table
if (!require(data.table)){
        install.packages("data.table")
        library(data.table)
}

        ###dplyr
if (!require(dplyr)){
        install.packages("dplry")
        library(dplry)
}

##Loading and preprocessing the data
activity=fread("activity.csv")
activity$date=as.Date(activity$date)
head(activity)

##What is mean total number of steps taken per day?
stepsDay=group_by(activity,date)
stepsDay=summarize(stepsDay,totalSteps=sum(steps))

        ###Histogram of the total number of steps taken each day
        hist(stepsDay$totalSteps,
             xlab="Steps",main="Total number of steps taken each day")
        
        ###Mean and median number of steps taken each day
        mean(stepsDay$totalSteps,na.rm=T) #Mean
        median(stepsDay$totalSteps,na.rm=T) #Median

##What is the average daily activity pattern?
intervalDay=group_by(activity,interval)
intervalDay=summarize(intervalDay,stepsInterval=mean(steps,na.rm=T))
        ###Time series plot of the 5-minute interval (x-axis) and 
          #the average number of steps taken, averaged across all days (y-axis)
        plot(intervalDay$interval,intervalDay$stepsInterval,type="l",
             main="Time series plot of the 5 minute interval",
             xlab="Interval",ylab="Number of steps")
        
        ###5-minute interval that contains the maximum number of steps
        intervalDay$interval[intervalDay$stepsInterval==max(intervalDay$stepsInterval)]

##Imputing missing values
        ###Calculate and report the total number of missing values in the dataset
        nrow(activity)-sum(complete.cases(activity))
        
        ###Create a new dataset that is equal to the original dataset but with the missing data filled in.
        activityComplete=activity #New dataset
        for (i in 1:nrow(activity)){
                if (is.na(activity$steps[i]==TRUE)){
                        interval=as.numeric(activityComplete$interval[i])
                        intervalMean=as.numeric(intervalDay[intervalDay$interval==interval,2])
                        activityComplete$steps[i]=intervalMean #We consider the mean for 5-minute interval
                }
        }
        
        ###Make a histogram of the total number of steps taken each
        stepsDay2=group_by(activityComplete,date)
        stepsDay2=summarize(stepsDay2,totalSteps=sum(steps))
        hist(stepsDay2$totalSteps,
             xlab="Steps",main="Total number of steps taken each day imputing missing values")
        
        ###Mean and median number of steps taken each day imputing missing values
        mean(stepsDay2$totalSteps) #Mean
        median(stepsDay2$totalSteps) #Median
        
##Are there differences in activity patterns between weekdays and weekends?
        ###Creating a new factor variable in the dataset with two levels - 
        ###"weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
        Sys.setlocale("LC_TIME", "English") #Weekdays in English
        
        activityComplete=mutate(activityComplete,weekday=as.factor(weekdays(activityComplete$date)))
        levels(activityComplete$weekday)=c(rep("weekday",2),rep("weekend",2),rep("weekday",3))                                        
        
        ###Make a panel plot containing a time series plot of the 5-minute interval 
        ###and the average number of steps taken, averaged across all weekday days or weekend days
        ###Subseting weekday
        weekday=subset(activityComplete,weekday=="weekday") 
        intervalWeekday=group_by(weekday,interval)
        intervalWeekday=summarize(intervalWeekday,stepsInterval=mean(steps,na.rm=T))
        
        ###Subseting weekend
        weekend=subset(activityComplete,weekday=="weekend")
        intervalWeekend=group_by(weekend,interval)
        intervalWeekend=summarize(intervalWeekend,stepsInterval=mean(steps,na.rm=T))
        
                ###Ploting...
                par(mfcol=c(2,1))
                windows.options(width=8, height=12)
                
                ###top plot
                plot(intervalWeekend$interval,intervalWeekend$stepsInterval,
                     main="Weekend",xlab="Interval",ylab="Number of steps",
                     type="l",col="blue")
                
                ###bottom plot
                plot(intervalWeekday$interval,intervalWeekday$stepsInterval,
                     main="Weekday",xlab="Interval",ylab="Number of steps",
                     type="l",col="red")