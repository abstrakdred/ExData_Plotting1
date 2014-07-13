
plot1 <- function()
{
	hpcfile <- "household_power_consumption.txt"
	
	hpc <- read.csv.sql(hpc_txt_file, 
				 		sql = 'select * from file where Date = "1/2/2007" or Date = "2/2/2007"',
				 		sep=";", header=T, 
				 		colClasses = c("character","character","numeric","numeric","numeric","numeric",
				 					   "numeric","numeric","numeric")
					   )

#	Open the PNG graphic defaults with defaults width = height = 480 and units being pixels

	print("Plotting data")
	png("plot1.png", width = 480, height = 480, units = "px")
	hist(hpc$Global_active_power,breaks=12, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
	dev.off()
}


library(sqldf)
plot1()