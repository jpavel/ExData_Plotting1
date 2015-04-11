plot2<- function(data){
      
      # merge date and time in the time column
      data[,2]<-paste(data[,1],data[,2])
      #convert to internal R time format
      data[,2]<-as.POSIXct(strptime(data[,2], "%d/%m/%Y %T"))
      data[,2]
}