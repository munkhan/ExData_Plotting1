plot1<-function(dat_file){
        
        if(!file.exists(dat_file))
                stop("Input file not found ..please check file path")
        # vectors for storing data 
        date_time<-double()
        Global_active_power<-double()
        Global_reactive_power<-double()
        Voltage<-double()
        Global_intensity<-double()
        Sub_metering_1<-double()
        Sub_metering_2<-double()
        Sub_metering_3<-double()
       
        #open the file for reading
        content  <- file(dat_file, open = "r") 
        #close resource 
        on.exit(close(content))
        line <- readLines(content, n = 1)
        #Skip the header
        unlist_data <- unlist((strsplit(line, ";"))) 
       
        i<-1
        print("loadig data.....this can take a few minutes")
        while (length(line <- readLines(content, n = 1)) > 0) {   
                unlist_data <- unlist((strsplit(line, ";")))   
                date_val<-as.Date(unlist_data[1],"%d/%m/%Y")
                #check if date range is within our workset
                if(date_val >= "2007-2-1" && date_val < "2007-2-3"){
                        date_time[i]<-paste(date_val,unlist_data[2])
                        
                        Global_active_power[i]<-as.numeric(unlist_data[3])
                        
                        Global_reactive_power[i]<-unlist_data[4]
                        Voltage[i]<-unlist_data[5]
                        Global_intensity[i]<-unlist_data[6]
                        Sub_metering_1[i]<-unlist_data[7]
                        Sub_metering_2[i]<-unlist_data[8]
                        Sub_metering_3[i]<-unlist_data[9]
                        i<-i+1
                }
                #If the line is beyond our end date break while loop
                if(date_val >  "2007-2-2")
                        break;
                
        }
        #Create a Data frame                
        plot1_data_frame<-data.frame(date_time,
                                         Global_active_power,
                                         Global_reactive_power,
                                         Voltage,
                                         Global_intensity,
                                         Sub_metering_1,
                                         Sub_metering_2,
                                         Sub_metering_3
                                         )
        #Png device for writing
        png(filename = "figure/plot1.png", width = 480, height = 480,units = "px")
        par(mar=c(6,4,2,1))
        #graph
        hist(plot1_data_frame$Global_active_power,col="Red",main="Global Active Power",xlab="Global Active Power (kilowatts)")
        #close device
        dev.off()
        
        print("plot1.png saved in figure sub directory")
}