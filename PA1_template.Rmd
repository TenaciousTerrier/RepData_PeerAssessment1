---
title: "Peer Assessment 1"
author: "Thomas M Hepner"
date: "Thursday, May 14, 2015"
output: html_document
---

### Loading and Preprocessing Data
```{r, echo = TRUE}
setwd("C:/Users/thep3_000/Desktop/Coursera/Reproducible Research")
data = read.csv("activity.csv")
```

### What is the mean total number of steps taken per day?
```
1. Total Number of Steps Taken per Day
```
```{r, echo = TRUE}
newdata = aggregate(data$steps, by = list(data$date), FUN = sum, na.rm = TRUE)
colnames(newdata) = c("date", "steps")
print(newdata)
```

```
2. Plot Histogram of Data
```
```{r, echo = TRUE}
hist(newdata$steps, xlab = "", main = "Steps per Day", col = "red", breaks = 20)
```

```
3. Mean and Median of Steps Taken per Day
```
```{r, echo = TRUE}
summary(newdata$steps)
```

### What is the daily average activity pattern?
```
1. Make a time series plot
```
```{r, echo = TRUE}
dailydata = aggregate(data$steps, by = list(data$interval), FUN = mean, na.rm = TRUE)
colnames(dailydata) = c("Interval", "Meansteps")
plot(Meansteps ~ Interval, data = dailydata, type = "l", xlim = c(0, 2400), ylab = "Average Steps", main = "Average Daily Steps by 5-Minute Time Interval")
```

```
2. Which 5-minute interval on average contains the most number of steps?
```
```{r, echo = TRUE}
dailydata[which(dailydata$Meansteps == max(dailydata$Meansteps)), ]
```

### Imputing Missing Values
```
1. Total number of missing values in the dataset
```
```{r, echo = TRUE}
length(data$steps[which(is.na(data$steps) == TRUE)])
```

```
2-3. Create a new dataset equal to the original with missing data filled in. 
```
```{r, echo = TRUE}
impdata = data

# Match missing data to daily average for 5-minute interval
for(i in 1:dim(impdata)[1]) {
  if(is.na(impdata$steps[i])) {
    impdata$steps[i] = dailydata[which(dailydata$Interval == impdata$interval[i]),2]
  }
}

# Check no missing data
length(impdata$steps[which(is.na(impdata$steps) == TRUE)])

# No data missing
```




```
4. (part 1) Make a histogram of the total number of steps taken each day 

```
```{r, echo = TRUE}
# Aggregate data
dailydataimp = aggregate(impdata$steps, by = list(impdata$date), FUN = sum, na.rm = TRUE)
colnames(dailydataimp) = c("date", "steps")

# Create histogram
hist(dailydataimp$steps, xlab = "", main = "Steps per Day", col = "red", breaks = 20)
```

```
4. (part 2) Calculate and report the mean and median total number of steps taken per day
```
```{r, echo = TRUE}
summary(dailydataimp$steps)
```

```
4. (part 3) Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```
```
The mean and median estimates are higher than they were in the first part of the assignment, but the total daily number of steps is the exact same. 
```

### Are there any differences in activity between weekdays and weekends?
```
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```
```{r, echo = TRUE}
impdata2 = impdata
impdata2$weekday = weekdays(as.Date(impdata$date))
impdata2$weekday = ifelse((impdata2$weekday == "Saturday" | impdata2$weekday == "Sunday"), "weekend", "weekday")
```

```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```
```{r, echo = TRUE}
impdata3 = aggregate(impdata2$steps, by = list(impdata2$interval, impdata2$weekday), FUN = mean, na.rm = TRUE)
colnames(impdata3) = c("Interval", "WeekDay", "Meansteps")

library(lattice)
xyplot(impdata3$Meansteps ~ impdata3$Interval | impdata3$WeekDay, type = "l", ylab = "Number of Steps", xlab = "Interval", )
```

