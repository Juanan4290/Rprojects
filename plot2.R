##Installing packages needed if they are required
if (!require(data.table)){
  install.packages("data.table")
  library(data.table)
}

if (!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

##Day of week in English
Sys.setlocale("LC_TIME", "English")

##Reading dataset and filtering 1st and 2nd of February (2007)
epc=fread("household_power_consumption.txt")
    ##Transform date variable in date format
    epc$Date=as.Date(epc$Date,"%d/%m/%Y")

epc=filter(epc,Date=="2007-02-01"|Date=="2007-02-02")

    #Transform time varible in time format and merging date and time
    epc$Date=as.POSIXct(paste(epc$Date, epc$Time), format="%Y-%m-%d %H:%M:%S")
    epc=epc[, !(colnames(epc) %in% "Time")]
    
    #Transform rest of variables in numeric format
    for (i in 2:8){
      epc[[i]]=as.numeric(epc[[i]])
    }

##Ploting...
plot(epc$Date,epc$Global_active_power,main=NULL,
    ylab="Global Active Power (kilowatts)",xlab="",
    type="l", xaxt = "n")
    
axis.POSIXct(side = 1, epc$Date)
dev.copy(png,file="plot2.png",width=480,height=480)
dev.off()