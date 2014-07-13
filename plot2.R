
plot2 <- function()
{
	unzip("./exdata-data-household_power_consumption.zip", exdir="./")

	library(sqldf)

	hpcfile <- "household_power_consumption.txt" 

# setAs("character","hpcDate", function(from) as.Date(from, format="%d/%m/%Y") )

	hpc <- read.csv.sql(hpcfile, sql = 'select * from file where Date = "1/2/2007" or Date = "2/2/2007"', sep=";", header=T, colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

	hpc$ds <- strptime(paste(hpc$Date,hpc$Time),"%d/%m/%Y %H:%M:%S")
    day1 <- as.POSIXct(hpc[1,10] )
	hpc$sec <-  sapply(as.POSIXct(hpc$ds), function(ds) as.integer(ds) - as.integer(day1))

#	daySplit <- split(hpc, weekdays(as.POSIXct(hpc[, 10]),T) )

	days <- c(weekdays(as.POSIXct(hpc[1,10]),T), weekdays(as.POSIXct(hpc[nrow(hpc),10]),T), weekdays(as.POSIXct(hpc[nrow(hpc), 10] + 60),T))

#	Open the PNG graphic device

	png("plot2.png", width = 480, height = 480, units = "px")
# Plot a line graph

	plot.window(xlim = c( 0, nrow(hpc)), ylim = c(0,8))
	plot(hpc$sec,hpc$Global_active_power,xaxt = "n", 
		 xlab = "", ylab = "Global Active Power (kilowatts)", type = "l")

#	axis(labels = c( "Thu", "Fri", "Sat"), side = 1, at= c(0,2880,3))
	axis(1,at=c(min(hpc$sec),median(hpc$sec),max(hpc$sec)),label=days)
	dev.off()
}

plot2()