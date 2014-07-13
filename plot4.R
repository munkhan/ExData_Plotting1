plot4<-function(dat_file){
        
        if(!file.exists(dat_file))
                stop("Input file not found ..please check file path")
        #vector for funtion use
        date_time<-double()
        Global_active_power<-double()
        Global_reactive_power<-double()
        Voltage<-double()
        #Global_intensity<-double()
        Sub_metering_1<-double()
        Sub_metering_2<-double()
        Sub_metering_3<-double()
        
        #read file one line at a time
        content  <- file(dat_file, open = "r") 
        on.exit(close(content))
        line <- readLines(content, n = 1)
        unlist_data <- unlist((strsplit(line, ";"))) 
        
        i<-1
        print("loadig data.....this can take a few minutes")
        #load data if it is witn in date range
        while (length(line <- readLines(content, n = 1)) > 0) {   
                unlist_data <- unlist((strsplit(line, ";")))   
                date_val<-as.Date(unlist_data[1],"%d/%m/%Y")
                
                if(date_val >= "2007-2-1" && date_val < "2007-2-3" && unlist_data[7]!="?" && unlist_data[8]!="?" && unlist_data[9]!="?"){
                        #date_time[i]<-paste(date_val,unlist_data[2])
                        date_time[i]=weekdays(date_val)
                        Global_active_power[i]<-as.double(unlist_data[3])
                        
                        Global_reactive_power[i]<-as.double(unlist_data[4])
                        Voltage[i]<-as.double(unlist_data[5])
                        #Global_intensity[i]<-unlist_data[6]war
                        Sub_metering_1[i]<-as.double(unlist_data[7])
                        Sub_metering_2[i]<-as.double(unlist_data[8])
                        Sub_metering_3[i]<-as.double(unlist_data[9])
                        i<-i+1
                }
                #break if it is outside date range
                if(date_val >  "2007-2-2")
                        break;
                
        }
        
       
        plot4_data_frame<-data.frame(date_time,
                                     Global_active_power,
                                     Global_reactive_power,
                                     Voltage,
                                     #Global_intensity,
                                     Sub_metering_1,
                                     Sub_metering_2,
                                     Sub_metering_3
        )
        #png device for saving graph
        png(filename = "figure/plot4.png", width = 480, height = 480,units = "px")
        #graph setting
        par(mar=c(6,4,2,1))
        
        par(mfcol=c(2,2))
        #graph1 Global Active Power
        #par(mar=c(2,2,2,2))
        plot(plot4_data_frame$Global_active_power,type="n",axes=FALSE,ann=FALSE)
        #m<-barplot(plot2_data_frame$Global_active_power)
        lines(plot4_data_frame$Global_active_power)
        box()
        axis(1,at=c(1,2880/2,2880),lab=c("Thu","Fri","Sat"))
        axis(2,at=c(0,2,4,6),lab=c("0","2","4","6"))
        title(ylab="Global Active Power (kilowatts)")
        
        
        
        #grap2
        
        
        
        plot(plot4_data_frame$Sub_metering_1,type="n",axes=FALSE,ann=FALSE)
        lines(plot4_data_frame$Sub_metering_1,type="l")
        lines(plot4_data_frame$Sub_metering_2,type="l",col="Red")
        lines(plot4_data_frame$Sub_metering_3,type="l",col="Blue")
        box()
        axis(1,at=c(1,2880/2,2880),lab=c("Thu","Fri","Sat"))
        axis(2,at=c(0,10,20,30),lab=c("0","10","20","30"))
        legend("topright",col=c("Black","Red","Blue"),lty=c(1,1,1),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),cex=.6)
        title("", ylab="Energy sub metering")
        
        
        #graph 3  Voltage /Day
        #par(mar=c(2,6,4,1))
        
        plot(plot4_data_frame$Voltage,type="n",axes=FALSE,ann=FALSE)
        lines(plot4_data_frame$Voltage,type="l")
       
        box( col = 'black')
        axis(1,at=c(1,2880/2,2880),lab=c("Thu","Fri","Sat"))
        axis(2,at=c(234,238,242,246),lab=c("234","238","242","246"))
        title(ylab="Voltage",xlab="datetime")
        
        
        
        #graph4
        #par(mar=c(2,6,1,1))
        
        plot(plot4_data_frame$Global_reactive_power,type="n",axes=FALSE,ann=FALSE)
        lines(plot4_data_frame$Global_reactive_power,type="l")
        
        box( col = 'black')
        axis(1,at=c(1,2880/2,2880),lab=c("Thu","Fri","Sat"))
        axis(2,at=c(0.0,0.1,0.2,0.3,0.4,0.5),lab=c("0.0","0.1","0.2","0.3","0.4","0.5"))
        
        title("", ylab="Global_reactive_power",xlab="datetime")
        
        dev.off()
        
        print("plot4.png saved in figure sub directory")
}