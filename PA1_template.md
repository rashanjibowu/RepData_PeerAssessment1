# Reproducible Research: Peer Assessment 1

Set global parameters


```r
library(knitr)
opts_chunk$set(cache=FALSE, fig.align='center')
```
## Loading and preprocessing the data

Unzip the file 


```r
filename <- c("activity.csv")

if (!file.exists(filename)) {
    unzip("activity.zip")    
}
```

Read the file into memory


```r
data <- read.csv(filename, na.strings = "NA")
```

Convert date column into the Date format


```r
data$date <- as.Date(data$date)
```


## What is the mean total number of steps taken per day?

Calculate total number of steps taken per day


```r
totalStepsPerDay <- tapply(data$steps, data$date, sum, na.rm = TRUE)
```

Make histogram of total steps taken each day


```r
hist(totalStepsPerDay, 
     main = "Histogram of Steps Taken Per Day", 
     xlab = "Steps Taken Per Day")
```

<img src="PA1_template_files/figure-html/Histogram of total steps taken each day-1.png" title="" alt="" style="display: block; margin: auto;" />

Mean number of steps per day


```r
mean(totalStepsPerDay)
```

```
## [1] 9354.23
```

Median number of steps per day


```r
median(totalStepsPerDay)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Calculate average steps taken per interval


```r
avgStepsPerInterval <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
```

Reformat and sort data


```r
# Convert to a data frame
df <- data.frame(avgStepsPerInterval)

# Make the row names their own column
df <- cbind(interval = rownames(df), df)

# Remove rownames
rownames(df) <- NULL

# Convert intervals to integers
df$interval <- as.integer(as.character(df$interval))

# Sort by increasing interval order
df <- df[order(df$interval), ]
```

Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
title <- c("Average Number of Steps Taken Per Interval")
ylabel <- c("Average Number of Steps Taken")
xlabel <- c("Interval (5 minutes)")

plot(x = df$interval, y = df$avgStepsPerInterval, type = "l", main = title, ylab = ylabel, xlab = xlabel)
```

<img src="PA1_template_files/figure-html/Time Series Plot-1.png" title="" alt="" style="display: block; margin: auto;" />


```r
# Find interval with maximum number of steps taken
sortedDescendingStepsPerInterval <- df[order(df$avgStepsPerInterval, decreasing = TRUE), ]
sortedDescendingStepsPerInterval[1,1]
```

```
## [1] 835
```

## Imputing missing values

Total number of missing values


```r
# Count total number of missing values
missing <- which(is.na(data$steps))
length(missing)
```

```
## [1] 2304
```

Imputing Missing Values


```r
# Create new dataset to include imputed missing values
dataWithImputedValues <- data

# Function to fill in missing values
fillin <- function() {
  
    # for each row of missing data, get and insert the average for the interval
    for (rowIndex in missing) {
  
        # get interval
        interval <- dataWithImputedValues[rowIndex, "interval"]
      
        # get interval average
        intervalAverage <- df[df$interval == interval, "avgStepsPerInterval"]
      
        # replace missing value with interval average
        dataWithImputedValues[rowIndex, "steps"] <<- intervalAverage
    }
}

# Actually fill in missing values
fillin()
```

Check again for missing values - should be 0!


```r
nrow(dataWithImputedValues[is.na(dataWithImputedValues$steps),])
```

```
## [1] 0
```

Recalculate total number of steps per day. This time, it includes imputed values


```r
# Calculate total number of steps taken per day
totalStepsPerDay <- tapply(dataWithImputedValues$steps, 
                           dataWithImputedValues$date, 
                           sum, 
                           na.rm = TRUE)
```

Another histogram of total steps taken each day. This time, it includes imputed values


```r
hist(totalStepsPerDay, 
     main = "Histogram of Steps Taken Per Day", 
     xlab = "Steps Taken Per Day")
```

<img src="PA1_template_files/figure-html/Histogram with imputed values-1.png" title="" alt="" style="display: block; margin: auto;" />

Mean number of steps per day (with imputed values)


```r
mean(totalStepsPerDay)
```

```
## [1] 10766.19
```

Median number of steps per day (with imputed values)


```r
median(totalStepsPerDay)
```

```
## [1] 10766.19
```

#### What is the impact on the mean and median with imputed values?

The mean and median of the steps taken per day are greater when we impute missing values than when we excluded them in prior calculations. Imputing missing values increases the estimate of the total monthly number of steps.

## Are there differences in activity patterns between weekdays and weekends?

Load the data.table library and convert data frame to a data table


```r
library(data.table)

dt <- data.table(dataWithImputedValues)
```

Create new column for day of the week


```r
dt[,dayofweek:=weekdays(date)]
```

```
##            steps       date interval dayofweek
##     1: 1.7169811 2012-10-01        0    Monday
##     2: 0.3396226 2012-10-01        5    Monday
##     3: 0.1320755 2012-10-01       10    Monday
##     4: 0.1509434 2012-10-01       15    Monday
##     5: 0.0754717 2012-10-01       20    Monday
##    ---                                        
## 17564: 4.6981132 2012-11-30     2335    Friday
## 17565: 3.3018868 2012-11-30     2340    Friday
## 17566: 0.6415094 2012-11-30     2345    Friday
## 17567: 0.2264151 2012-11-30     2350    Friday
## 17568: 1.0754717 2012-11-30     2355    Friday
```

Create column for weekday status and converge into a 2-level factor


```r
dt[,weekdayStatus:=ifelse(dayofweek %in% c("Saturday", "Sunday"), c("weekend"), c("weekday"))]
```

```
##            steps       date interval dayofweek weekdayStatus
##     1: 1.7169811 2012-10-01        0    Monday       weekday
##     2: 0.3396226 2012-10-01        5    Monday       weekday
##     3: 0.1320755 2012-10-01       10    Monday       weekday
##     4: 0.1509434 2012-10-01       15    Monday       weekday
##     5: 0.0754717 2012-10-01       20    Monday       weekday
##    ---                                                      
## 17564: 4.6981132 2012-11-30     2335    Friday       weekday
## 17565: 3.3018868 2012-11-30     2340    Friday       weekday
## 17566: 0.6415094 2012-11-30     2345    Friday       weekday
## 17567: 0.2264151 2012-11-30     2350    Friday       weekday
## 17568: 1.0754717 2012-11-30     2355    Friday       weekday
```

```r
# Converge to a 2-level factor variable
dt$weekdayStatus <- factor(dt$weekdayStatus)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Panel plot setup


```r
# For panel plots, let's use the lattice plotting system
library(lattice)

# Set up plot parameters
title <- c("Average Number of Steps Taken Per Interval")
ylabel <- c("Average Number of Steps Taken")
xlabel <- c("Interval (5 minutes)")
```

Format data for plotting


```r
plotData <- dt[,list(avgSteps = mean(steps)), by = c("interval", "weekdayStatus")]
```

Make panel plot


```r
xyplot(avgSteps ~ interval | weekdayStatus, 
       data = plotData, 
       layout = c(1,2), 
       type = "l", 
       xlab = xlabel,
       ylab = ylabel,
       main = paste(title, c("By Weekday Status"), sep = " "))
```

<img src="PA1_template_files/figure-html/Make Panel Plot-1.png" title="" alt="" style="display: block; margin: auto;" />
