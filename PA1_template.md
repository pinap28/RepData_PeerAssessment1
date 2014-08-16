# Reproducible Research: Peer Assessment 1

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:
* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

We read the decompressed .csv dataset file from the working directory.

```r
data <- read.csv("activity.csv")
```

We convert the date field from a factor class to date class.

```r
data$date <- as.Date(data$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

**Histogram** of the total number of steps taken each day.

```r
totalStepDay <- tapply(data$steps, data$date, sum)
hist(totalStepDay, xlab = "Total Steps per Day", main = "Histogram of Total Steps per Day")
```

![plot of chunk histStepsDay](figure/histStepsDay.png) 

**Mean** and **median** total number of steps taken per day.

```r
mean(totalStepDay, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(totalStepDay, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
meanStepInterval <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
plot(meanStepInterval, type = "l", main = "Time series plot", xlab = "number 5-minute interval", ylab = "Average number of steps")
```

![plot of chunk timeSeriesPlot](figure/timeSeriesPlot.png) 

Interval with **maximum average number of steps**.

```r
max <- which (meanStepInterval==max(meanStepInterval))
h <- (max * 5) %/% 60
m <- (max * 5) %% 60 -5
dayTime <- paste(as.character(h), as.character(m), sep = ":")
```

**Interval number.**

```r
max[[1]]
```

```
## [1] 104
```

**Day time.**

```r
dayTime
```

```
## [1] "8:35"
```

## Imputing missing values

Total **number of missing values** in the dataset.

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

We are going to use the **mean of the 5-minute interval** over the two months to fill the missing values. 

**New dataset** with the missing data filled in.

```r
index <- is.na(data$steps)
intervalTime <- as.integer(names(meanStepInterval))
meanTime <- cbind(meanStepInterval, intervalTime)
newData <- data
newData[,1] <- replace(data$steps, index, meanTime[match(data[index,3], meanTime[,2]), 1])
```

We identify the `NA` values and the interval times. We construct a new array with the interval mean and the corresponding interval time. Then we replace the `NA` values by the corresponding mean value. 

We store the dataset in **newData**.

**Histogram** of the total number of steps taken each day.

```r
newTotalStepDay <- tapply(newData$steps, newData$date, sum)
hist(newTotalStepDay, xlab = "Total Steps per Day", main = "New Histogram of Total Steps per Day")
```

![plot of chunk newHistStepsDay](figure/newHistStepsDay.png) 

New **Mean** and **median** total number of steps taken per day.

```r
mean(newTotalStepDay)
```

```
## [1] 10766
```

```r
median(newTotalStepDay)
```

```
## [1] 10766
```

The new **mean** is the same than before filling in the `NA` values.

The new **median** is a little bit larger than before.

Now the **mean** and the **median** have the same value.

Imputing missing data on the estimates of the total daily number of steps **increases** the frequency of the data. We have more values.

## Are there differences in activity patterns between weekdays and weekends?

New **factor variable** in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
newData$weekday <- as.factor(ifelse(weekdays(newData$date) %in% c("Saturday","Sunday"), "weekend", "weekday")) 
```

**Plot containing a time series plot** of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
dataMean <- aggregate(steps ~ weekday + interval, data = newData, FUN = mean)
xyplot(steps ~ interval | weekday, data = dataMean, type = "l", xlab = "Interval", ylab = "Number of steps", layout = c(1,2)) 
```

![plot of chunk weekdaysPlot](figure/weekdaysPlot.png) 
