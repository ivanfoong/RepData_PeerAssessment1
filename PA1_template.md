# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(ggplot2)
library(gridExtra, quietly = TRUE)
dat <- read.csv("activity.csv")
completeDat <- dat[complete.cases(dat), ]

theme_set(theme_bw(base_size = 10))
```



## What is mean total number of steps taken per day?

### 1. Make a histogram of the total number of steps taken each day

```r
totalStepsDat <- aggregate(steps ~ date, completeDat, sum)
m <- ggplot(totalStepsDat, aes(x = steps)) + labs(title = "Frequency of Total Steps taken per day", 
    x = "Total Steps", y = "Frequency") + geom_histogram(binwidth = 1000, colour = "blue", 
    fill = "blue")
m
```

![plot of chunk totalSteps setcache](figure/totalSteps_setcache.png) 


### 2. Calculate the mean and median for total number of steps taken per day

```r
meanStepsDat <- aggregate(steps ~ date, completeDat, mean)
meanStepsDat$date <- as.Date(meanStepsDat$date)
m <- ggplot(meanStepsDat, aes(date, steps)) + labs(title = "Mean of Total Steps taken per day", 
    x = "Total Steps", y = "Mean") + geom_bar(colour = "blue", fill = "blue", 
    width = 0.7, stat = "identity")
m
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-11.png) 

```r

medianStepsDat <- aggregate(steps ~ date, completeDat, median)
medianStepsDat$date <- as.Date(medianStepsDat$date)
m <- ggplot(medianStepsDat, aes(date, steps)) + labs(title = "Median of Total Steps taken per day", 
    x = "Total Steps", y = "Median") + geom_bar(colour = "blue", fill = "blue", 
    width = 0.7, stat = "identity")
m
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-12.png) 


#### * If single value of mean and median for all the days are expected for the result instead:

```r
mean(totalStepsDat$steps)
```

```
## [1] 10766
```

```r
median(totalStepsDat$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averageStepsByIntervalDat <- aggregate(steps ~ interval, completeDat, mean)
m <- ggplot(averageStepsByIntervalDat, aes(interval, steps)) + geom_line(colour = "blue") + 
    labs(title = "Average number of steps per interval", x = "Interval", y = "Average number of Steps")
m
```

![plot of chunk averageStepsByIntervalDat setcache](figure/averageStepsByIntervalDat_setcache.png) 


### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averageStepsByIntervalDat[order(averageStepsByIntervalDat$steps, decreasing = TRUE), 
    ][1, ]
```

```
##     interval steps
## 104      835 206.2
```


## Imputing missing values
### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(dat[!complete.cases(dat), ])
```

```
## [1] 2304
```


### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
fillMissingStepsFunction <- function(x, averageStepsByIntervalDat) {
    steps <- x[1]
    date <- x[2]
    interval <- as.integer(x[3])  # for some reason interval values gets converted to character type, so had to cast it as integer
    if (is.na(steps)) {
        x[1] <- averageStepsByIntervalDat[averageStepsByIntervalDat$interval == 
            interval, 2]
    }
}
```


### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
filledDat <- dat
filledDat[is.na(filledDat$steps), 1] <- apply(dat[is.na(filledDat$steps), ], 
    1, fillMissingStepsFunction, averageStepsByIntervalDat)
head(filledDat)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```


### 4a. Make a histogram of the total number of steps taken each day

```r
totalStepsDat <- aggregate(steps ~ date, completeDat, sum)
m <- ggplot(totalStepsDat, aes(x = steps)) + labs(title = "Frequency of Total Steps taken per day (without missing values)", 
    x = "Total Steps", y = "Frequency") + geom_histogram(binwidth = 1000, colour = "blue", 
    fill = "blue")

totalStepsDat2 <- aggregate(steps ~ date, filledDat, sum)
m2 <- ggplot(totalStepsDat2, aes(x = steps)) + labs(title = "Frequency of Total Steps taken per day (with missing values filled with average steps)", 
    x = "Total Steps", y = "Frequency") + geom_histogram(binwidth = 1000, colour = "blue", 
    fill = "blue")

multiplot(m, m2, cols = 1)
```

```
## Error: could not find function "multiplot"
```


### 4b. Calculate and report the mean and median total number of steps taken per day.

```r
meanStepsDat <- aggregate(steps ~ date, completeDat, mean)
meanStepsDat$date <- as.Date(meanStepsDat$date)

medianStepsDat <- aggregate(steps ~ date, completeDat, median)
medianStepsDat$date <- as.Date(medianStepsDat$date)

meanStepsDat2 <- aggregate(steps ~ date, filledDat, mean)
meanStepsDat2$date <- as.Date(meanStepsDat2$date)

medianStepsDat2 <- aggregate(steps ~ date, filledDat, median)
medianStepsDat2$date <- as.Date(medianStepsDat2$date)

names(meanStepsDat)[2] = "mean1"
names(meanStepsDat2)[2] = "mean2"

mergedMeanStepsDat <- merge(meanStepsDat, meanStepsDat2, all = TRUE)
mergedMeanStepsDat$meanDiff <- mergedMeanStepsDat$mean2 - mergedMeanStepsDat$mean1

m <- ggplot(mergedMeanStepsDat, aes(date), na.rm = TRUE) + labs(title = "Mean of Total Steps/Day", 
    x = "Date", y = "Mean (NA values)") + geom_line(aes(y = mean1), colour = "blue")

m2 <- ggplot(mergedMeanStepsDat, aes(date), na.rm = TRUE) + labs(title = "Mean of Total Steps/Day", 
    x = "Date", y = "Mean (NA values as Average)") + geom_line(aes(y = mean2), 
    colour = "blue")

grid.arrange(m, m2, ncol = 1)
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-61.png) 

```r

names(medianStepsDat)[2] = "median1"
names(medianStepsDat2)[2] = "median2"

mergedMedianStepsDat <- merge(medianStepsDat, medianStepsDat2, all = TRUE)
mergedMedianStepsDat$medianDiff <- mergedMedianStepsDat$median2 - mergedMedianStepsDat$median1

m <- ggplot(mergedMedianStepsDat, aes(date), na.rm = TRUE) + labs(title = "Median of Total Steps/Day", 
    x = "Date", y = "Median (NA values)") + geom_line(aes(y = median1), colour = "blue")

m2 <- ggplot(mergedMedianStepsDat, aes(date), na.rm = TRUE) + labs(title = "Median of Total Steps/Day", 
    x = "Date", y = "Median (NA values as Average)") + geom_line(aes(y = median2), 
    colour = "blue")

grid.arrange(m, m2, ncol = 1)
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-62.png) 


### 4c. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

There is impact on both mean and median results. It has minor impact on mean results as it fills in the average of all available days for days that only NA values are available, which potentially allows those days to be included in other calculations, but it greatly affects the median results as it's fills with average value for a particular day, which median value will be calculated from the average value, instead of including multiple 0, similar to other steps/days as provided by the original dataset. (Median for non-NA days are typically 0 due to multiple 0 steps entries, whereas NA days's steps are filled using the average which is not 0 and typically > 30 steps per day)

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekDayEndDat <- dat
weekDayEndDat$weekDayEnd <- as.POSIXlt(weekDayEndDat$date)$wday

assignWeekDayEnd <- function(x) {
    weekDayEndValue = ""
    if (as.POSIXlt(x[2])$wday <= 5) {
        weekDayEndValue = "weekday"
    } else {
        weekDayEndValue = "weekend"
    }
    weekDayEndValue
}
weekDayEndDat$weekDayEnd <- apply(weekDayEndDat, 1, assignWeekDayEnd)
```


### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
m <- ggplot(weekDayEndDat[complete.cases(weekDayEndDat), ], aes(interval, steps), 
    group = weekDayEnd, na.rm = TRUE) + labs(title = "Average number of steps per interval", 
    x = "Interval", y = "Average number of Steps") + geom_line(colour = "blue") + 
    facet_grid(weekDayEnd ~ .)
m
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

