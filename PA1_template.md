---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Load the data

```r
if (!file.exists("activity.csv")) {
        unzip("activity.zip")
}
activity <- read.csv("activity.csv", header = TRUE)
```

## What is mean total number of steps taken per day?
1. Make histogram

```r
totalSteps <- aggregate(steps ~ date, activity, FUN = sum)
hist(totalSteps$steps,
     main = "Total Steps per Day",
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

2. Calculate **mean** and **median** 

```r
options(scipen = 999)
meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
medSteps <- median(totalSteps$steps, na.rm = TRUE)
```
- The mean number of steps is **10766.1886792**  
- The median number of steps is **10765**

## What is the average daily activity pattern?
1. Make time series plot of 5-minute interval and the average number of steps taken, averaged across all days 

```r
meanStepByInt <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(meanStepByInt ~ unique(activity$interval), type = "l", xlab = "5-minute interval", ylab = "Average Numbor of Steps")
```

![](PA1_template_files/figure-html/time-1.png)<!-- -->

2. Which 5-minute interval, on average across all days in the dataset, contains the maximum number of steps?

```r
max <- meanStepByInt[which.max(meanStepByInt)]
print(c("5-minute Interval containing max number of steps", max))
```

```
##                                                    
## "5-minute Interval containing max number of steps" 
##                                                835 
##                                 "206.169811320755"
```

## Imputing missing values
1. Calculate total number of missing values in data set

```r
totalNA <- sum(is.na(activity$steps))
```
The total number of missing values is **2304**  
2 and 3. Devise strategy for filling in missing values and create dataset with missing data filled in

```r
#Strategy: use mean of 5 minute interval
activity2 <- activity
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                activity2$steps[i] <- meanStepByInt[[as.character(activity[i, "interval"])]]
        }
}
```
4. Make a histogram and calculate/report new mean and median

```r
totalSteps2 <- aggregate(steps ~ date, activity2, FUN = sum)
hist(totalSteps2$steps, 
     xlab = "Number of Steps", 
     main = "Total Steps per Day")
```

![](PA1_template_files/figure-html/hist2-1.png)<!-- -->

```r
meanSteps2 <- round(mean(totalSteps2$steps), 2)
medSteps2 <- round(median(totalSteps2$steps), 2)
```
The new mean number of steps is **10766.19**  
The new median number of steps is **10766.19**   
Looking at the new values, we can see that the mean value did not change.  However, the median value changed from **10765** to **10766.19**  

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor with two levels -- "weekday" and "weekend"

```r
Day <- function(date) {
        day <- weekdays(date)
        if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
                return("weekday")
        else
                return("weekend")
}
activity2$date <- as.Date(activity2$date)
activity2$day <- sapply(activity2$date, FUN = Day)
```
2. Make panel plot containing time series plot of 5-minute interval and average number of steps taken averaged aross weekday days and weekend days   

```r
avgStepDate <- aggregate(steps ~ interval + day, activity2, mean)

library(lattice)
xyplot(steps ~ interval | day, data = avgStepDate, type = "l", layout = c(1,2), xlab = "Interval", ylab = "Number of Steps")
```

![](PA1_template_files/figure-html/panel-1.png)<!-- -->
