plot_multi_panel <- function(file_path) {
  # Read the dataset, filtering for the specific dates
  data <- transform(
    subset(read.table(file_path, header = TRUE, sep = ";"), Date %in% c("1/2/2007", "2/2/2007"))
  )
  
  # Convert 'Global_active_power' to numeric
  data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
  
  # Convert 'Voltage' to numeric
  data$Voltage <- as.numeric(as.character(data$Voltage))
  
  # Convert 'Global_reactive_power' to numeric
  data$Global_reactive_power <- as.numeric(as.character(data$Global_reactive_power))
  
  # Combine 'Date' and 'Time' into POSIXct datetime
  data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format = "%d/%m/%Y %H:%M:%S")
  
  # Create and save the plot as a PNG file
  png("plot4.png", width = 480, height = 480)
  
  # Set up a 2x2 plotting layout
  par(mfrow = c(2, 2))
  
  # Plot 1: Global Active Power
  plot(data$DateTime, data$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power", xaxt = "n")
  axis.POSIXct(1, at = seq(min(data$DateTime), max(data$DateTime) + 86400, by = "days"), format = "%a")
  
  # Plot 2: Voltage
  plot(data$DateTime, data$Voltage, type = "l", xlab = "datetime", ylab = "Voltage", xaxt = "n")
  axis.POSIXct(1, at = seq(min(data$DateTime), max(data$DateTime) + 86400, by = "days"), format = "%a")
  
  # Plot 3: Energy sub metering
  plot(data$DateTime, data$Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub metering", xaxt = "n")
  lines(data$DateTime, data$Sub_metering_2, col = "red")
  lines(data$DateTime, data$Sub_metering_3, col = "blue")
  axis.POSIXct(1, at = seq(min(data$DateTime), max(data$DateTime) + 86400, by = "days"), format = "%a")
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = 1, bty = "n")
  
  # Plot 4: Global Reactive Power
  plot(data$DateTime, data$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power", xaxt = "n")
  axis.POSIXct(1, at = seq(min(data$DateTime), max(data$DateTime) + 86400, by = "days"), format = "%a")
  
  #Closing the png device
  dev.off()
}

# Call the function with the appropriate file path
plot_multi_panel("household_power_consumption.txt")