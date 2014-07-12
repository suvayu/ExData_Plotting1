## plot 1 assignment

## read dataset from provided file, return data for only 2 days in
## February, 2007.
dst.read <- function(datafile = "household_power_consumption.txt") {
  ## expected types for better performance
  types <- character(length = 9)
  for (i in 1:2) {
    types[i] = "character"
  }
  for (i in 3:9) {
    types[i] = "numeric"
  }

  ## read full dataset
  dst <- read.csv(datafile, sep = ";", colClasses = types, nrow = 2075260, na.strings = "?")
  dst$Time <- as.POSIXct(paste(dst$Date, dst$Time, sep = " "), format = "%d/%m/%Y %H:%M:%S")
  dst$Date <- as.Date(dst$Date, format = "%d/%m/%Y")

  ## subset data to 2007-02-01 - 2007-02-02
  dst <- dst[dst$Date >= as.Date("2007-02-01") & dst$Date <= as.Date("2007-02-02"),]

  ## return dataset
  return(dst)
}


plot4 <- function(dst = NULL, filename = "plot4.png") {
  ## read default dataset if none is passed
  if (!is.data.frame(dst)) dst <- dst.read()

  ## set png output device
  png(file = filename, width = 480, height = 480)
  message("Printing to ", filename)

  ## partition frame
  par(mfrow = c(2, 2))

  ## create time series & plot: 1440 = 24*60, one reading per minute

  ## plot 1, 1
  dst.ts.apow <- ts(dst$Global_active_power, frequency = 1440, start = 0)
  plot(dst$Time, dst.ts.apow, type = "l", ylab = "Global Active Power", xlab = "")

  ## plot 1, 2
  dst.ts.volt <- ts(dst$Voltage, frequency = 1440, start = 0)
  plot(dst$Time, dst.ts.volt, type = "l", ylab = "Voltage", xlab = "")

  ## plot 2, 1
  dst.ts.sub1 <- ts(dst$Sub_metering_1, frequency = 1440, start = 0)
  dst.ts.sub2 <- ts(dst$Sub_metering_2, frequency = 1440, start = 0)
  dst.ts.sub3 <- ts(dst$Sub_metering_3, frequency = 1440, start = 0)
  plot(dst$Time, dst.ts.sub1, ylim = c(0,38), type = "l", ylab = "Energy sub metering", xlab = "")
  par(new = TRUE)
  plot(dst$Time, dst.ts.sub2, ylim = c(0,38), type = "l", col = "red", axes = FALSE, ylab = "", xlab = "")
  par(new = TRUE)
  plot(dst$Time, dst.ts.sub3, ylim = c(0,38), type = "l", col = "blue", axes = FALSE, ylab = "", xlab = "")
  legend("topright", lty = c(1,1,1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

  ## plot 2, 2
  dst.ts.rpow <- ts(dst$Global_reactive_power, frequency = 1440, start = 0)
  plot(dst$Time, dst.ts.rpow, type = "l", ylab = "Global Reactive Power", xlab = "")
  dev.off()
}
