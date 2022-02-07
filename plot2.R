plot2 <-function(file_dir="C:/Users/Susanne Haas/Documents/Coursera/Explanatory_data_analysis/Course_Project_1/exdata_data_household_power_consumption", out_dir="C:/Users/Susanne Haas/Documents/Coursera/Explanatory_data_analysis/Course_Project_1"){
  
  ## Read the data
 
  setwd(file_dir)
  hh_power <- read.table(file = "household_power_consumption.txt", header = TRUE, sep=";") 
  
  ## Convert Date and Time variables to Date/Time classes
  hh_power$Date <- as.Date(hh_power$Date, format="%d/%m/%Y")
  datetime <- paste(hh_power$Time, hh_power$Date)
  hh_power$Date_time <- strptime(datetime, "%H:%M:%S %Y-%m-%d")
  
  ## Subset 1-Feb-2007 to 2-Feb-2007 and add weekday
  hh_power_subset<-subset(hh_power, Date=="2007-02-01"|Date=="2007-02-02")
  hh_power_subset$Weekday <-weekdays(hh_power_subset$Date)
  
  ## Create Plot Global Active Power
  hh_power_subset$Global_active_power<-as.numeric(hh_power_subset$Global_active_power)
  
  setwd(out_dir)
  png(file="plot2.png", width=480, height=480)
  plot(hh_power_subset$Date_time,hh_power_subset$Global_active_power, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  dev.off()
}