plot2 <- function () {
  
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
  active_power <- as.numeric(dat[[3]])
  ldat <- length(active_power)
  png("plot2.png", width=480, height=480)
  ylabel <- "Global Active Power (kilowatts)"
  plot(active_power,type="l",xlab="",ylab=ylabel,xaxt="n")
  axis(1,at=c(0,ldat/2,ldat),labels=c("Thu","Fri","Sat"))
  dev.off()
}
