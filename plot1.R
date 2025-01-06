# Function to read, clean, and plot the Global Active Power histogram
plot_global_active_power <- function(file_path) {
  # Read the dataset, filtering for the specific dates
  data <- transform(
    subset(read.table(file_path, header = TRUE, sep = ";"), Date %in% c("1/2/2007", "2/2/2007")), 
    Date = as.Date(Date, "%d/%m/%Y")
  )
  
  # Confirm that the dates are correctly filtered
  print(unique(data$Date))  # Should return "2007-02-01" and "2007-02-02"
  
  # Check and convert 'Global_active_power' column to numeric
  if (class(data$Global_active_power) != "numeric") {
    data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
  }
  
  # Create and save the histogram as a PNG file
  png("plot1.png", width = 480, height = 480)
  hist(data$Global_active_power, col = "red", main = "Global Active Power", 
       xlab = "Global Active Power (kilowatts)")
  
  #Closing the png device
  dev.off()
}

# Call the function with the appropriate file path
plot_global_active_power("household_power_consumption.txt")
