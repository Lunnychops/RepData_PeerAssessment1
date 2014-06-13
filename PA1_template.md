
# Reproducible Research: Peer Assessment 1
This assignment is part of the Coursera course Reproducible Research.
Activity monitoring data will be used here as an accessory to demonstrate our
capacity to make a literate statistical program.
You can find more information about it in the `README.md` document.

## Loading and preprocessing the data
The data are already in the working directory in the `activity.csv` file.
Thus, there is no need to download the file from the
[url provided](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

Set your working directory to the one containing the file!


```r
dataset <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```





## What is mean total number of steps taken per day?

```r
library(plyr)
dailySteps <- ddply(dataset, .(date), summarize, observations = sum(!is.na(steps)), 
    steps = sum(steps))
```


```r
head(dailySteps)
```

```
##         date observations steps
## 1 2012-10-01            0    NA
## 2 2012-10-02          288   126
## 3 2012-10-03          288 11352
## 4 2012-10-04          288 12116
## 5 2012-10-05          288 13294
## 6 2012-10-06          288 15420
```

```r
unique(dailySteps$observations)
```

```
## [1]   0 288
```



### 1. Make a histogram of the total number of steps taken each day

We can now make a histogram of the daily data

```r
with(dailySteps, hist(steps, main = "Histogram of the total number of steps taken each day"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



### 2. Calculate and report the **mean** and **median** total number of steps taken per day
`dailSteps` contains the 

```r
stepsMean <- mean(dailySteps$steps, na.rm = T)
stepsMedian <- median(dailySteps$steps, na.rm = T)
stepsMean
```

```
## [1] 10766
```

```r
stepsMedian
```

```
## [1] 10765
```


So, each day, we have a mean of 10766.1886792453 and a median of 10765.

## What is the average daily activity pattern?

For this question we have to compute another summary of the data, average of steps by interval:


```r

intervalSteps <- ddply(dataset, .(interval), summarize, steps = mean(steps, 
    na.rm = T))
```


### 1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
It is then possible the draw the plot of `interval` data:

```r
with(intervalSteps, plot(type = "l", x = interval, y = steps))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
The maximum number of steps is given by:

```r
maxStepsInterval <- intervalSteps[intervalSteps$steps == max(intervalSteps$steps), 
    ]
maxStepsInterval
```

```
##     interval steps
## 104      835 206.2
```

The interval where the maximum steps (206.1698) are made is
*835*.
It means it starts at the time *08:35* and ends 5 minutes later.

## Imputing missing values

We can simply find the number of missing 

```r
missingObservation <- is.na(dataset$steps)
sum(missingObservation)
```

```
## [1] 2304
```

There are *2304* missing values in the dataset.
Actually, it's 8 full days missing.

We are going to use the averaged interval data to complete the dataset.
That way we will recreate average days that will have features really close to
the original dataset.
Also, we will round the number of steps to have data that look plosible.


```r
datasetImputed <- dataset

# listing interval steps to have faster lookups.
listIntervalSteps <- as.list(round(intervalSteps$steps))
names(listIntervalSteps) <- as.character(intervalSteps$interval)

# processing each na row
datasetImputed[is.na(dataset$steps), "steps"] <- unlist(listIntervalSteps[as.character(datasetImputed[is.na(dataset$steps), 
    "interval"])])
```



### 4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
We reapply the same analysis as the original dataset:

```r
dailyStepsImputed <- ddply(datasetImputed, .(date), summarize, observations = sum(!is.na(steps)), 
    steps = sum(steps))
with(dailyStepsImputed, hist(steps, main = "Histogram of the total number of steps taken each day (imputed data)"))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
stepsMeanImputed <- mean(dailyStepsImputed$steps, na.rm = T)
stepsMedianImputed <- median(dailyStepsImputed$steps, na.rm = T)
```


By comparing the new histogram we find that the frequencies are stressed more. The high frequencies are higher, low ones are lower.
Also the mean ande median of each dataset are:
* **Means** original: 10766.1886792453, imputed: 10765.6393442623.
* **Medians** original: 10765, imputed: 10762.

We see little differences from the estimate from the first part of the assigment.
The impact of imputing the data is that even when using data with specific features that we are looking for we are having an impact on the dataset and can reproduce a perfect set.

## Are there differences in activity patterns between weekdays and weekends?
We are going to add the distinction between weekday and weekend in the dataset.

### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekdaysList <- weekdays(dataset$date)
weekdaysList[!(weekdaysList %in% c("Saturday", "Sunday"))] <- "weekday"
weekdaysList[(weekdaysList %in% c("Saturday", "Sunday"))] <- "weekend"
datasetImputed$weekday <- as.factor(weekdaysList)
```

### 2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using **simulated data**:

We have to average the daily data to obtain the pattern across all days in the week and weekend.

```r
dailyStepsByWeekdayImputed <- ddply(datasetImputed, c("interval", "weekday"), 
    summarize, steps = mean(steps))
library(lattice)
xyplot(steps ~ interval | weekday, data = dailyStepsByWeekdayImputed, layout = c(1, 
    2), type = "l", xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 


We can see the same patterns. You have 4 distinctive high activity periods at 8am, 12pm, 4pm, 7pm. This is explained by the meals/snacks times. Then where the patterns differ between the two periods are between 5am and 8am (more activity during the weekdays), and also globally during the rest of the day there are more activity during the weekend. This is explained easily by work hours where people tend to be sitting at there desk when they are potentialy moving during the weekend.
