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
total.steps <- as.data.frame(total.steps)
total.steps$date <- as.POSIXct(rownames(total.steps), format = "%Y-%m-%d")
g <- ggplot(total.steps, aes(x = total.steps))
g + geom_histogram(binwidth = 2500) +
  xlab("Total Steps Per Day") +
  ylab("Frequency") + 
  xlim(0, round_any(max(total.steps$total.steps, na.rm = TRUE), 2500, ceiling)) +
  ggtitle("Histogram of Total Steps Per Day")

# Use inline calculations when reporting these
mean(total.steps, na.rm = TRUE); median(total.steps, na.rm = TRUE)

# Create a time series plot
activity$interval.cat <- as.factor(activity$interval)
interval.totals <- tapply(activity$steps, activity$interval.cat, sum, na.rm = TRUE)
interval.avg <- data.frame(interval = as.numeric(names(interval.totals)),
                           avg.steps = as.numeric(interval.totals)/length(unique(activity$date)))
g <- ggplot(interval.avg, aes(interval, avg.steps))
g + geom_line() + xlab("Minutes") + ylab("Average Steps") +
  ggtitle("Average Steps in Each Five-Minute Interval of a Day")

# Time interval with greatest number of average steps
max.index <- which.max(interval.avg$avg.steps)
interval.avg$interval[max.index]

