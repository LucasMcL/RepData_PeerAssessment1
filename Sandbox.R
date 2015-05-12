## This script will not be included in the final analysis

library(ggplot2)
library(plyr)

# Unzip and read in the data
unzip("activity.zip")
activity <- read.csv("activity.csv")

# Create a new column containing dates as POSIXct format
activity$date.POS <- as.POSIXct(activity$date, format = "%Y-%m-%d")

# Calculate total steps per day
total.steps <- tapply(activity$steps, activity$date, sum)
qplot(total.steps, binwidth = 2500,
      xlab = "Total Steps Per Day",
      ylab = "Frequency",
      xlim = c(0, round_any(max(total.steps, na.rm = TRUE), 2500, ceiling)),
      main = "Histogram of Total Steps Per Day")
mean(total.steps, na.rm = TRUE); median(total.steps, na.rm = TRUE)

