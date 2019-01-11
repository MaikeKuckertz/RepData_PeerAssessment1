#Reproducible Research - Programming Assignment (Week2)

###Content
1. Loading and preprocessing the data
2. What is mean total number of steps taken per day?
3. What is the average daily activity pattern?
4. Imputing missing values
5. Are there differences in activity patterns between weekdays and weekends?

Configure global settings:

```r
##Configure defaults
knitr::opts_chunk$set(echo = TRUE)
```



##1. Loading and preprocessing the data

Show any code that is needed to  

- Load the data (i.e. read.csv())  

```r
activity <- read.csv("./activity.csv", header = TRUE, na.strings = "NA")
```
- Process/transform the data (if necessary) into a format suitable for your analysis  

```r
activity$date <- as.Date(activity$date, format= "%Y-%m-%d")
```
Variables:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken


##2. What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.  

- Make a histogram of the total number of steps taken each day  

```r
library(dplyr)
stepsPerDay <- summarize(group_by(activity, date), steps=sum(steps))
hist(stepsPerDay$steps, col="blue", breaks = 20, xlab = "Amount of steps per day", main = "Histogram of average number of steps per day")
```

![plot of chunk histogram](figure/histogram-1.png)

- Calculate and report the mean and median total number of steps taken per day  

```r
mean(stepsPerDay$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$steps, na.rm = TRUE)
```

```
## [1] 10765
```

##3. What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsPerInterval <- summarize(group_by(activity, interval), steps=mean(steps, na.rm = TRUE))
plot(steps~interval, data = stepsPerInterval, type = "l", col = "blue", main = "Average daily activity pattern", xlab = "5 minute intervals", ylab = "Average number of steps")
```

![plot of chunk timeseries](figure/timeseries-1.png)

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxSteps <- subset(stepsPerInterval, stepsPerInterval$steps == max(stepsPerInterval$steps))
maxSteps$interval
```

```
## [1] 835
```


##4. Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(subset(activity, is.na(activity$steps)==TRUE))
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
#replace NA values with mean of the interval
missingData <- numeric()
for (i in 1:nrow(activity)) {
        obs <- activity[i, ]
        if (is.na(obs$steps)) {
                steps <- subset(stepsPerInterval, interval == obs$interval)$steps
        } else {
                steps <- obs$steps
        }
        missingData <- c(missingData, steps)
}
rm(i,steps,obs)
```

- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newActivity <- activity
newActivity$steps <- missingData
rm(missingData)
#check number of missing values
nrow(subset(newActivity, is.na(newActivity$steps)==TRUE))
```

```
## [1] 0
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newStepsPerDay <- summarize(group_by(newActivity, date), steps=sum(steps))
hist(newStepsPerDay$steps, col="blue", breaks = 20, xlab = "Amount of steps per day", main = "Histogram of avg. number of steps per day, with cleaned data")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


##5. Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

- Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
newActivity$weekday <- weekdays(newActivity$date, abbreviate = TRUE)
newActivity$dayType[newActivity$weekday == "Sa" | newActivity$weekday == "So"] <- "weekend"
newActivity$dayType[!(newActivity$weekday == "Sa" | newActivity$weekday == "So")] <- "weekday"
table(newActivity$dayType)
```

```
## 
## weekday weekend 
##   12960    4608
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
newStepsPerInterval <- summarize(group_by(newActivity, interval, dayType), steps=mean(steps, na.rm = TRUE))

library(lattice)
xyplot(steps~interval|dayType, data=newStepsPerInterval, type = "l", xlab = "Interval", ylab = "Number of steps", layout = c(1, 2))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
