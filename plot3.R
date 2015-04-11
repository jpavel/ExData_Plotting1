## Function to make "plot2" as in the assignment
## Default parameters are set such that user do not need to change them
## Function will try to check if local txt file specified in "datafile" exists
## If not, it will download it and unzip it

## It reads in the full text file and then subsets to data covering 1-2/2/2007
## Then it merges time and date to one column and converts it to internal R time format
## Finally it plots global consumption as a function of time and saves it to the file

## example:
## > source("plot2.R")
## > plot2()


plot3<- function(data){
     
      # merge date and time in the time column
      data[,2]<-paste(data[,1],data[,2])
      #convert to internal R time format 
      data[,2]<-as.POSIXct(strptime(data[,2], "%d/%m/%Y %T"))
      
      ## PLOTTING
      plot(data[,2],data[,7],type="l",xlab="",ylab="Energy sub metering")
      lines(data[,2],data[,8],col="red")
      lines(data[,2],data[,9],col="blue")
      legend("topright",lwd=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
}