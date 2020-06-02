---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---








## Loading and preprocessing the data


```r
activity_raw <- read.csv("activity.csv", stringsAsFactors=FALSE)
activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")
activity_raw <- data.frame(date=activity_raw$date,weekday=tolower(weekdays(activity_raw$date)), steps=activity_raw$steps,interval=activity_raw$interval)
activity_raw <- cbind(activity_raw, 
                      daytype=ifelse(activity_raw$weekday == "saturday" | 
                                     activity_raw$weekday == "sunday", "weekend", 
                                     "weekday"))
activity <- data.frame(date=activity_raw$date, 
                       weekday=activity_raw$weekday, 
                       daytype=activity_raw$daytype, 
                       interval=activity_raw$interval,
                       steps=activity_raw$steps)
head(activity)
```

```
##         date weekday daytype interval steps
## 1 2012-10-01  monday weekday        0    NA
## 2 2012-10-01  monday weekday        5    NA
## 3 2012-10-01  monday weekday       10    NA
## 4 2012-10-01  monday weekday       15    NA
## 5 2012-10-01  monday weekday       20    NA
## 6 2012-10-01  monday weekday       25    NA
```




## What is mean total number of steps taken per day?

### Make a histogram of the total number of steps taken each day.

#### Computing the total number of steps each day (NA values removed)
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)

#### Renaming the attributes
names(sum_data) <- c("date", "total")

#### Histogram plotting


```r
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="red", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

### Calculate and report the mean and median total number of steps taken each day.


```r
mean(sum_data$total)
```

```
## [1] 10766.19
```

```r
#### Mean is 9354.23
median(sum_data$total)
```

```
## [1] 10766.19
```

```r
#### Median is 10395.
```



## What is the average daily activity pattern?

### Make a time series plot of the 5 min interval and the average number of steps taken, averaged across all days.


#### Computing the means of steps accross all days for each interval
mean_data <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

#### Renaming the attributes
names(mean_data) <- c("interval", "mean")

#### Plotting the time series plot

```r
plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="red", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
     




### Which 5 min interval on average across all days in the dataset contains the maximum number of steps?


```r
#### Finding the position of the maximum mean
max_pos <- which(mean_data$mean == max(mean_data$mean))

#### Looking up the value of interval at this position
max_interval <- mean_data[max_pos, 1]
```



## Inputting missing values

### Calculate and report the total number of missing values in the dataset.


```r
#### Using the trick that a TRUE boolean value is equivalent to 1 and a FALSE to 0.
NA_count <- sum(is.na(activity$steps))

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


#### Finding the NA positions
na_pos <- which(is.na(activity$steps))

#### Creating a vector of means
mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))
```


### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#### Replacing the NAs by the means
activity[na_pos, "steps"] <- mean_vec
```


### Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#### Computing the total number of steps each day (NA values removed)
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum)

#### Renaming the attributes
names(sum_data) <- c("date", "total")
```


```r
#### Computing the histogram of the total number of steps each day
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="red", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)


```r
#### Mean and medians:

mean(sum_data$total)
```

```
## [1] 10766.19
```

```r
median(sum_data$total)
```

```
## [1] 10766.19
```

#### Mean is 10766.19, median is 10766.19


## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels - “weekdays” and “weekend” indicating whether a given date is a weekday or weekend day.



```r
#### The new factor variable "daytype" is already in the activity data frame
head(activity)
```

```
##         date weekday daytype interval   steps
## 1 2012-10-01  monday weekday        0 37.3826
## 2 2012-10-01  monday weekday        5 37.3826
## 3 2012-10-01  monday weekday       10 37.3826
## 4 2012-10-01  monday weekday       15 37.3826
## 5 2012-10-01  monday weekday       20 37.3826
## 6 2012-10-01  monday weekday       25 37.3826
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5- minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
#### Loading the lattice graphical library
library(lattice)

#### Computing the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)

#### Renaming the attributes
names(mean_data) <- c("daytype", "weekday", "interval", "mean")

#### Computing the time serie plot
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
# End
