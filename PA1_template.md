---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
1. Package loading

```r
library(ggplot2)
library(stringr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```
2. Download data

```r
Origin_dir <- getwd()
if(!file.exists('repdata_data_activity.zip')){
  download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip', 'repdata_data_activity.zip')
}

unzip('repdata_data_activity.zip')
```
3. Read and transform data

```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d" )
```
## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day, plot in a histogram chart

```r
activityByday <- activity %>% group_by(date) %>% summarise( stepsPerday = sum(steps, na.rm = TRUE))

ggplot(activityByday, aes(x=stepsPerday)) + geom_histogram(binwidth = 2000, color = "white", fill = "grey")+
labs(title="Histogram of steps per day (with na)",x="Steps/Day", y = "Counts") 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
  
2. Calculate and report the mean of the total number of steps taken per day

```r
mean(activityByday$stepsPerday, na.rm = TRUE)  
```

```
## [1] 9354.23
```
   
3.Calculate and report the median of the total number of steps taken per day

```r
median(activityByday$stepsPerday, na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
activityByinterval <- activity %>% group_by(interval) %>% summarise(avgsteps = mean(steps, na.rm = TRUE))
## qplot(interval, avgsteps , data = activityByinterval, geom = "line", main = 'Average steps per interval', ylab = 'Average Steps')
ggplot(activityByinterval, aes(x=interval, y=avgsteps)) + geom_line( color = "orange", size =1.2 )+
  labs(title="Average steps per interval (5 minutes)",x="interval", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max <- activityByinterval  %>% filter ( avgsteps == max(activityByinterval$avgsteps))
max
```

```
## # A tibble: 1 x 2
##   interval avgsteps
##      <int>    <dbl>
## 1      835     206.
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset. (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
2. Create a new dataset that is equal to the original dataset but with the missing data filled with mean of steps of the interval.

```r
activity1 <-  activity %>% group_by(interval) %>% mutate(stepsAdjusted = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```
3. Calculate the total number of steps taken per day, plot in a histogram chart.

```r
activityByday1 <- activity1 %>% group_by(date) %>% summarise( stepsPerday = sum(stepsAdjusted, na.rm = TRUE))
## hist(activityByday1$stepsPerday, main = 'Histogram of steps per day (with fills)', xlab = 'steps') 
ggplot(activityByday1, aes(x=stepsPerday)) + geom_histogram(binwidth = 2000, color = "white", fill = "lightblue")+
  labs(title="Histogram of steps per day (fill na)",x="Steps/Day", y = "Counts")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

4. Calculate and report the mean of the total number of steps taken per day after fill-na

```r
mean(activityByday1$stepsPerday, na.rm = TRUE)
```

```
## [1] 10766.19
```
5. Calculate and report the median of the total number of steps taken per day after fill-na

```r
median(activityByday1$stepsPerday, na.rm = TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
activity1 <- activity1 %>% mutate(weekday = weekdays(date)) %>% mutate( dayType = ifelse( weekday == 'Sunday' | weekday == 'Saturday', 'Weekend', 'weekday'))
activityByinterval1 <- activity1 %>% group_by(interval, dayType) %>% summarise(avgsteps = mean(steps, na.rm = TRUE))

ggplot(activityByinterval1, aes(x=interval, y=avgsteps)) + geom_line( color = "blue", size =0.5 )+
  labs(title = "Average steps per interval (5 minutes) by day type", x = "interval", y = "Steps")+
  facet_wrap(dayType ~. , ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


