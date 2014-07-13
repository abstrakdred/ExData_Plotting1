plot4 <- function()
{
	
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

	png("plot4.png", width = 480, height = 480, units = "px")
	
	par(mfrow = c(2,2))
	
	# Plot a line graph for Global Active Power
	plot.window(xlim = c( 0, nrow(hpc)), ylim = c(0,8))
	plot(hpc$sec,hpc$Global_active_power,xaxt = "n", 
		 xlab = "", ylab = "Global Active Power (kilowatts)", type = "l")
	axis(1,at=c(min(hpc$sec),median(hpc$sec),max(hpc$sec)),label=days)
	
	# Plot a line graph for Voltage
	plot.window(xlim = c( 0, nrow(hpc)), ylim = c(min(hpc$Voltage),max(hpc$Voltage)))
	plot(hpc$sec,hpc$Voltage,xaxt = "n", xlab = "datetime", ylab = "Voltage", type = "l")
	axis(1,at=c(min(hpc$sec),median(hpc$sec),max(hpc$sec)),label=days)
	
	# Plot three line graphs for Sub_metering_1, Sub_metering_2, Sub_metering_3
	plot.window(xlim = c( 0, nrow(hpc)), ylim = c(0,40))
	plot(hpc$sec,hpc$Sub_metering_1,xaxt = "n", xlab = "", ylab = "Energy sub metering", type = "l")
	lines(hpc$sec,hpc$Sub_metering_2, type = "l",col="red")
    lines(hpc$sec,hpc$Sub_metering_3, type = "l",col="blue")
    legend("topright", lty = 1,
    		legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
    		col=c("black","red","blue"))
    axis(1,at=c(min(hpc$sec),median(hpc$sec),max(hpc$sec)),label=days)
	
	# Plot a line graph for Global_reactive_power
	plot.window(xlim = c( 0, nrow(hpc)), 
				ylim = c(min(hpc$Global_reactive_power),max(hpc$Global_reactive_power)))
	plot(hpc$sec,hpc$Global_reactive_power,xaxt = "n", 
		xlab = "datetime", ylab = "Global_reactive_power", type = "l")
	axis(1,at=c(min(hpc$sec),median(hpc$sec),max(hpc$sec)),label=days)
	
	dev.off()
}

plot4()

