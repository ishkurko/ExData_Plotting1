## Main function for Course Project 1
## In R console run exdata013.run() if the data for the project locates in 
## working directory. Or run exdata013.run(offline = FALSE) with downloading 
## data from internet. As result, function will create plot 3 and save it in
## PNG file.
exdata013.run <- function(offline = TRUE) {
  f <- "exdata-data-household_power_consumption.zip"
  if (!offline) exdata013.download(f)
  if (!file.exists("household_power_consumption.txt")) unzip(f, overwrite = TRUE)
  x <- exdata013.read()
  exdata013.plot3(x)
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

## Create and save plot3
exdata013.plot3 <- function(x) {
  dt <- strptime(paste(x$Date, x$Time, sep = " "), "%d/%m/%Y %H:%M:%S")
  sm1 <- as.numeric(levels(x$Sub_metering_1))[x$Sub_metering_1]
  sm2 <- as.numeric(levels(x$Sub_metering_2))[x$Sub_metering_2]
  sm3 <- as.numeric(x$Sub_metering_3)
  png("plot3.png", width=480, height=480, units="px")
  par(ps=12)
  par(mfcol=c(1,1))
  plot(dt, sm1, type = "o", pch=".", col = "black", xlab = "", ylab = "Energy sub metering")
  lines(dt, sm2, type = "o", pch=".", col = "red")
  lines(dt, sm3, type = "o", pch=".", col = "blue")
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1, 1, 1), lwd=c(2.5, 2.5, 2.5),
         col = c("black", "red", "blue"))
  dev.off()
}