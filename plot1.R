## Function to make "plot1" as in the assignment
## Default parameters are set such that user do not need to change them
## Function will try to check if local txt file specified in "datafile" exists
## If not, it will download it and unzip it

## It reads in the full text file and then subsets to data covering 1-2/2/2007
## Finally it creates plot and saves it


plot1<- function(useLocal=TRUE, fileUrl="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
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
      ## LOADING AND CLEANING DATA
      # load the full file in correct format (separators are ";" and missing values are "?")
      all<-read.table(inFileName,header=TRUE, sep=";", na.strings = "?")
      # select only the two days
      data<-all[(all$Date=="1/2/2007" | all$Date=="2/2/2007"),]
      
      ## PLOTTING
      # make histogram with requested title, labels and color
      hist(data$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)", col="red")
      # copy output to png file with size 480x480 pixels
      dev.copy(png,width=480,height=480,file="plot1.png")
      #close the file
      dev.off()
}