## This script will not be included in the final analysis

library(ggplot2)
library(plyr)
library(dplyr)

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
mean <- floor(mean(total.steps$total.steps, na.rm = TRUE))
median <- median(total.steps$total.steps, na.rm = TRUE)

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

# Total number of missing values in original dataset
na.total <- sum(is.na(activity$steps))

# Add column containing interval averages to activity dataset.
activity <- join(activity, interval.avg, by = "interval")
activity.adj <- activity # Preserve dataset; create activity.adj

# Loop through adjusted activity dataset and replace NAs with interval average
for (i in 1:nrow(activity.adj)){
  if(is.na(activity.adj$steps[i]) == TRUE){
    activity.adj$steps[i] <- activity.adj$avg.steps[i]
  }
}
# Identical to original, but with NAs removed and date as both factor and POSIXct
activity.adj <- activity.adj[, 1:5]

# Histogram of adjusted dataset
total.steps <- tapply(activity.adj$steps, activity.adj$date, sum)
total.steps <- as.data.frame(total.steps)
total.steps$date <- as.POSIXct(rownames(total.steps), format = "%Y-%m-%d")
g <- ggplot(total.steps, aes(x = total.steps))
g + geom_histogram(binwidth = 2500) +
  xlab("Total Steps Per Day") +
  ylab("Frequency") + 
  xlim(0, round_any(max(total.steps$total.steps, na.rm = TRUE), 2500, ceiling)) +
  ggtitle("Histogram of Total Steps Per Day (Adjusted)")

mean(total.steps$total.steps, na.rm = TRUE)
median(total.steps$total.steps, na.rm = TRUE)

# Create new factor variable identifying day of the week, then weekday/weekend
activity.adj$day.of.week <- weekdays(activity.adj$date.POS)
activity.adj$weekday <- rep("Weekday", times = nrow(activity.adj))
for (i in 1:nrow(activity.adj)){
  if(activity.adj$day.of.week[i] == "Saturday" | activity.adj$day.of.week[i] == "Sunday"){
    activity.adj$weekday[i] <- "Weekend"
  }
}
activity.adj$weekday <- as.factor(activity.adj$weekday)

# Split into 2 dataframes according to the weekday variable; calculate averages
weekday <- activity.adj[activity.adj$weekday == "Weekday", ]
weekend <- activity.adj[activity.adj$weekday == "Weekend", ]
weekday.interval.avg <- tapply(weekday$steps, weekday$interval.cat, sum)/
  length(unique(activity.adj$date))
weekend.interval.avg <- tapply(weekend$steps, weekend$interval.cat, sum)/
  length(unique(activity.adj$date))

# Put into temporary data frame to make plot
df <- data.frame(avg.steps = c(unname(weekday.interval.avg), unname(weekend.interval.avg)),
                 interval = as.numeric(rep(names(weekday.interval.avg), times = 2)),
                 weekday = factor(rep(c("Weekday", "Weekend"), each = length(weekday.interval.avg))))

g <- ggplot(df, aes(interval, avg.steps))
g + geom_line() + facet_grid(weekday ~ .) + xlab("Minutes") + ylab("Average Steps") +
  ggtitle("Average Steps in Each Five-Minute Interval of a Day (Adjusted)")
