## Main function for Course Project 1
## In R console run exdata013.run() if the data for the project locates in 
## working directory. Or run exdata013.run(offline = FALSE) with downloading 
## data from internet. As result, function will create plot 2 and save it in
## PNG file.
exdata013.run <- function(offline = TRUE) {
  f <- "exdata-data-household_power_consumption.zip"
  if (!offline) exdata013.download(f)
  if (!file.exists("household_power_consumption.txt")) unzip(f, overwrite = TRUE)
  x <- exdata013.read()
  exdata013.plot2(x)
}

## Download row data from internet in zip file
## f - target file name
exdata013.download <- function(f) {
  u <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  download.file(u, f)
}

## Reads data from smartmeter
exdata013.read <- function() {
  x <- read.table("./household_power_consumption.txt", header = TRUE, sep = ";")
  y <- x[x$Date == "1/2/2007" | x$Date == "2/2/2007",]
}

## Create and save plot2
exdata013.plot2 <- function(x) {
  dt <- strptime(paste(x$Date, x$Time, sep = " "), "%d/%m/%Y %H:%M:%S")
  gap <- as.numeric(levels(x$Global_active_power))[x$Global_active_power]
  png("plot2.png", width=480, height=480, units="px")
  par(ps=12)
  par(mfcol=c(1,1))
  plot(dt, gap, type = "o", pch=".", xlab = "", ylab = "Global Active Power (kilowatts)")
  dev.off()
}