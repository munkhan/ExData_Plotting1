plot3<-function(dat_file){
        
        if(!file.exists(dat_file))
                stop("Input file not found ..please check file path")
        #vector for function use
        date_time<-double()
        
        Sub_metering_1<-double()
        Sub_metering_2<-double()
        Sub_metering_3<-double()
        
        #open file and read one line at a time
        content  <- file(dat_file, open = "r") 
        on.exit(close(content))
        line <- readLines(content, n = 1)
        unlist_data <- unlist((strsplit(line, ";"))) 
        
        i<-1
        print("loadig data.....this can take a few minutes")
        while (length(line <- readLines(content, n = 1)) > 0) {   
                unlist_data <- unlist((strsplit(line, ";")))   
                date_val<-as.Date(unlist_data[1],"%d/%m/%Y")
                #read data if it is within the range
                if(date_val >= "2007-2-1" && date_val < "2007-2-3" && unlist_data[7]!="?" && unlist_data[8]!="?" && unlist_data[9]!="?"){
                        #date_time[i]<-paste(date_val,unlist_data[2])
                        date_time[i]=weekdays(date_val)
                        
                        Sub_metering_1[i]<-as.double(unlist_data[7])
                        Sub_metering_2[i]<-as.double(unlist_data[8])
                        Sub_metering_3[i]<-as.double(unlist_data[9])
                        i<-i+1
                }
                #break while loop is date > max(date_range)
                if(date_val >  "2007-2-2")
                        break;
                
        }
        
        plot3_data_frame<-data.frame(date_time,
                                         #Global_active_power,
                                         #Global_reactive_power,
                                         #Voltage,
                                         #Global_intensity,
                                         Sub_metering_1,
                                         Sub_metering_2,
                                         Sub_metering_3
        )
        #png device for saving file
        png(filename = "figure/plot3.png", width = 480, height = 480,units = "px")
        par(mar=c(6,4,2,1))
        #Chart and custom setting
        plot(plot3_data_frame$Sub_metering_1,type="n",axes=FALSE,ann=FALSE)
        lines(plot3_data_frame$Sub_metering_1,type="l")
        lines(plot3_data_frame$Sub_metering_2,type="l",col="Red")
        lines(plot3_data_frame$Sub_metering_3,type="l",col="Blue")
        box( col = 'black')
        axis(1,at=c(1,2880/2,2880),lab=c("Thu","Fri","Sat"))
        axis(2,at=c(0,10,20,30),lab=c("0","10","20","30"))
        legend("topright",col=c("Black","Red","Blue"),lty=c(1,1,1),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        title("", ylab="Energy sub metering")
        
        dev.off()
        
        print("plot3.png saved in figure sub directory")
        
}