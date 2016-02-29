plot4 <- function () {
  
  ## Read in data
  filename <- "household_power_consumption.txt"
  
  header <- read.table(filename,nrow=1,sep=";")
  datebeg <- as.Date("2007-02-01")
  dateend <- as.Date("2007-02-02")
  
  dat <- NULL # initialize a data frame
  startRead = F
  
  for (i in 1:3000000) {
    linedat <- read.table(filename,header=F,skip=i,nrow=1,sep=";")
    date <- as.Date(linedat[[1]], format="%d/%m/%Y")
    if (date >= datebeg && date <= dateend) {
        startRead = T
        # add a new row to the data frame
        dat <- rbind(dat,linedat)
    }
    else if (startRead) break;
  }
  
  ## Create plot
  png("plot4.png", width=480, height=480)
  
  par(mfrow = c(2,2))
  
  # Plot 1
  ylabel <- "Global Active Power"
  active_power <- as.numeric(dat[[3]])
  ldat <- length(active_power)
  plot(active_power,type="l",xlab="",ylab=ylabel,xaxt="n")
  axis(1,at=c(0,ldat/2,ldat),labels=c("Thu","Fri","Sat"))
  
  # Plot 2
  xlabel <- "datetime"
  ylabel <- "Voltage"
  voltage <- as.numeric(dat[[5]])
  plot(voltage,type="l",xlab=xlabel,ylab=ylabel,xaxt="n")
  axis(1,at=c(0,ldat/2,ldat),labels=c("Thu","Fri","Sat"))
  
  # Plot 3
  ylabel <- "Energy sub metering"
  submet1 <- as.numeric(dat[[7]])
  submet2 <- as.numeric(dat[[8]])
  submet3 <- as.numeric(dat[[9]])
  plot(submet1,type="l",xlab="",ylab=ylabel,xaxt="n",col="black")
  lines(submet2,col="red")
  lines(submet3,col="blue")
  
  axis(1,at=c(0,ldat/2,ldat),labels=c("Thu","Fri","Sat"))
  legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),bty="n")
  
  # Plot 4
  ylabel <- "Global_reactive_power"
  voltage <- as.numeric(dat[[4]])
  plot(voltage,type="l",xlab=xlabel,ylab=ylabel,xaxt="n")
  axis(1,at=c(0,ldat/2,ldat),labels=c("Thu","Fri","Sat"))
  
  # Finish
  dev.off()
}
