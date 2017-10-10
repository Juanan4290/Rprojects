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
par(mfcol=c(2,2))    

  #plot topleft
  plot(epc$Date,epc$Global_active_power,main=NULL,
       ylab="Global Active Power (kilowatts)",xlab="",
       type="l", xaxt = "n")
  
  axis.POSIXct(side = 1, epc$Date)
  
  #plot bottomleft
  plot(epc$Date,epc$Sub_metering_1,main=NULL,
       ylab="Energy sub metering",xlab="",
       type="l", xaxt = "n")
  lines(epc$Date,epc$Sub_metering_2,col="red")
  lines(epc$Date,epc$Sub_metering_3,col="blue")
  legend("topright",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1))
  
  axis.POSIXct(side = 1, epc$Date)
  
  #plot topright
  plot(epc$Date,epc$Voltage,main=NULL,
       ylab="Voltage",xlab="datetime",
       type="l", xaxt = "n")
  
  axis.POSIXct(side = 1, epc$Date)
  
  #plot bottomright
  plot(epc$Date,epc$Global_reactive_power,main=NULL,
       ylab="Global_reactive_power",xlab="datetime",
       type="l", xaxt = "n")
  
  axis.POSIXct(side = 1, epc$Date)
  
  
dev.copy(png,file="plot4.png",width=480,height=480)
dev.off()
