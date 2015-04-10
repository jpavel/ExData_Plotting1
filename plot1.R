plot1<- function(zipfile="exdata-data-household_power_consumption.zip", datafile="household_power_consumption.txt"){
      ## UNZIPPING
      #make tempdir (if not existing)
      tmpdir<-tempdir()
      if(!file.exists(tmpdir) dir.create(tmpdir,recursive=TRUE)
      #unzip to tempdir
      unzip(zipfile,exdir=tmpdir)
      inFileName<-paste0(tmpdir,"/",datafile) #full path to unzipped file
      
      ## LOADING AND CLEANING DATA
      # load the full file in correct format (separators are ";" and missing values are "?")
      all<-read.table(datafile,header=TRUE, sep=";", na.strings = "?")
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