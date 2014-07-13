plot2<-function(dat_file){
        
        if(!file.exists(dat_file))
                stop("Input file not found ..please check file path")
        
        #vectors for function use
        date_time<-double()
        
        Global_active_power<-double()
        Global_reactive_power<-double()
        Voltage<-double()
        Global_intensity<-double()
        Sub_metering_1<-double()
        Sub_metering_2<-double()
        Sub_metering_3<-double()
        
        #open file for reading ...close on.exit
        content  <- file(dat_file, open = "r") 
        on.exit(close(content))
        line <- readLines(content, n = 1)
        unlist_data <- unlist((strsplit(line, ";"))) 
        
        i<-1
        print("loadig data.....this can take a few minutes")
        #read one line at a time if the date range is good store in vector
        while (length(line <- readLines(content, n = 1)) > 0) {   
                unlist_data <- unlist((strsplit(line, ";")))   
                date_val<-as.Date(unlist_data[1],"%d/%m/%Y")
                
                if(date_val >= "2007-2-1" && date_val < "2007-2-3" && unlist_data[2]!="?"){
                        #date_time[i]<-paste(weekdays(date_val),unlist_data[2])
                        date_time[i]=weekdays(date_val)
                        
                        Global_active_power[i]<-as.numeric(unlist_data[3])
                       
                        i<-i+1
                }
                #Exit if the date > max date of interest
                if(date_val >  "2007-2-2")
                        break;
                
        }
        
        plot2_data_frame<-data.frame(date_time,
                                         Global_active_power
                                       
        )
        #Png device
        png(filename = "figure/plot2.png", width = 480, height = 480,units = "px")
        #plot(plot2_data_frame$Global_active_power,plot2_data_frame$date_time,type="n",axes=FALSE,ann=FALSE)
        #margin setup
        par(mar=c(4,4,2,1))
        plot(plot2_data_frame$Global_active_power,type="n",axes=FALSE,ann=FALSE)
        #m<-barplot(plot2_data_frame$Global_active_power)
        lines(plot2_data_frame$Global_active_power)
        box()
        axis(1,at=c(1,2880/2,2880),lab=c("Thu","Fri","Sat"))
        axis(2,at=c(0,2,4,6),lab=c("0","2","4","6"))
        title(ylab="Global Active Power (kilowatts)")
        #save png and close dvice
        dev.off()
       
       print("plot2.png saved in figure sub directory")
}