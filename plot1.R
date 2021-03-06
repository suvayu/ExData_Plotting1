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


plot1 <- function(dst = NULL, filename = "plot1.png") {
  ## read default dataset if none is passed
  if (!is.data.frame(dst)) dst <- dst.read()

  ## set png output device
  png(file = filename, width = 480, height = 480)
  message("Printing to ", filename)
  hist(dst$Global_active_power, breaks = 12, col = "red",
       main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
  dev.off()
}
