## Function to make "plot3" as in the assignment
## Default parameters are set such that user do not need to change them
## Function will try to check if local txt file specified in "datafile" exists
## If not, it will download it and unzip it

## It reads in the full text file and then subsets to data covering 1-2/2/2007
## Then it merges time and date to one column and converts it to internal R time format
## Finally it plots all requested plots in one picture and saves then in the file

## Note that in this case the output file is opened before the plotting: copying
## of plot created in "screen" graphic device to the png causes distortions of the legend 
## (the fonts are not scaled down, so they are truncated)

## example:
## > source("plot4.R")
## > plot4()


plot4<- function(useLocal=TRUE, fileUrl="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
         zipfile="exdata-data-household_power_consumption.zip", datafile="household_power_consumption.txt"){
      ## checking if local input exists and download it if not
      download<-FALSE #download only if requested, or input file missing
      if(useLocal){
            message("Checking if local input file exists...")
            if(file.exists(datafile)) inFileName<-datafile
            else{
                  download<-TRUE
                  warning("Local file not found! Downloading it!")
            } 
      }else download<-TRUE 
      
      if(download){
            ##DOWNLOADING TO TMP
            #make tempdir (if not existing)
            
            tmpdir<-tempdir()
            if(!file.exists(tmpdir)) dir.create(tmpdir,recursive=TRUE)
            destZipFile<-paste0(tmpdir,"/",zipfile)
            download.file(fileUrl, destfile=destZipFile, method = "curl")
            
            ## UNZIPPING
            #unzip to tempdir
            unzip(destZipFile,exdir=tmpdir)
            inFileName<-paste0(tmpdir,"/",datafile) #full path to unzipped file
            #check that input file exists and exit function if not
            if(!file.exists(inFileName)){
                  err_msg<-paste("The file",inFileName,"does not exists. Please check the input URL and the datafile name")
                  stop(err_msg)
            }
      }
      msg<-paste("The file",inFileName,"will be used as input")
      message(msg)
      
      ## LOADING, CLEANING AND PREPARING DATA
      # load the full file in correct format (separators are ";" and missing values are "?")
      all<-read.table(inFileName,header=TRUE, sep=";", na.strings = "?")
      # select only the two days
      data<-all[(all$Date=="1/2/2007" | all$Date=="2/2/2007"),]
      # merge date and time in the time column
      data[,2]<-paste(data[,1],data[,2])
      #convert to internal R time format 
      data[,2]<-as.POSIXct(strptime(data[,2], "%d/%m/%Y %T"))
      
      ## PLOTTING
      png(file="plot4.png",width=480,height=480) #open the output file
      par(mfrow=c(2,2)) # split the canvas to 2x2 parts
      plot(data[,2],data[,3],type="l",xlab="",ylab="Global Active Power")
      plot(data[,2],data[,5],type="l",xlab="datetime",ylab="Voltage")
      plot(data[,2],data[,7],type="l",xlab="",ylab="Energy sub metering")
      lines(data[,2],data[,8],col="red") # adding another line
      lines(data[,2],data[,9],col="blue") #yet another line
      #legend - if lwd is specified, it uses lines as indicators
      #bty="n" turns off the border around legend
      legend("topright",lwd=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), bty="n")
      plot(data[,2],data[,4],type="l",xlab="datetime",ylab="Global_reactive_power")
      
      #close the file
      dev.off()
}