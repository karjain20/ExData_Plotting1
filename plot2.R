plot_global_active_power_time_series <- function(file_path) {
  # Read the dataset, filtering for the specific dates
  data <- transform(
    subset(read.table(file_path, header = TRUE, sep = ";"), Date %in% c("1/2/2007", "2/2/2007"))
  )
  
  # Check and convert 'Global_active_power' column to numeric
  if (class(data$Global_active_power) != "numeric") {
    data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
  }
  
  # Combine 'Date' and 'Time' into POSIXct datetime
  data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format = "%d/%m/%Y %H:%M:%S")
  
  # Create and save the plot as a PNG file
  png("plot2.png", width = 480, height = 480)
  plot(data$DateTime, data$Global_active_power, 
       type = "l", 
       xlab = "", 
       ylab = "Global Active Power (kilowatts)", 
       xaxt = "n")
  
  # Add custom x-axis labels
  axis.POSIXct(1, at = seq(min(data$DateTime), max(data$DateTime) + 86400, by = "days"), format = "%a")
  
  #Closing the png device
  dev.off()
  
}

# Call the function with the appropriate file path
plot_global_active_power_time_series("household_power_consumption.txt")