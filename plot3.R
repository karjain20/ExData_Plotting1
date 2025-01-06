plot_energy_sub_metering <- function(file_path) {
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
  png("plot3.png", width = 480, height = 480)
  plot(data$DateTime, data$Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub metering", xaxt = "n")
  lines(data$DateTime, data$Sub_metering_2, col = "red")
  lines(data$DateTime, data$Sub_metering_3, col = "blue")
  
  # Add custom x-axis labels
  axis.POSIXct(1, at = seq(min(data$DateTime), max(data$DateTime) + 86400, by = "days"), format = "%a")
  
  # Add legend
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = 1)
  
  #Closing the png device
  dev.off()
}

# Call the function with the appropriate file path
plot_energy_sub_metering("household_power_consumption.txt")