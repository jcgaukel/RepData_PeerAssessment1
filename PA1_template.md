# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
library("tools")

# file informationn
filelink <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filename <- "activity.zip"
filechecksum <- "61e0c1f8c4a736ff1ab0f8863344b301"

# check to see if file is missing or checksum has changed 
if (!file.exists(filename)|md5sum(filename) != filechecksum) {
        # download the file.
        download.file(filelink, filename)
        }         

# unzip and read in CSV file and remove incomplete cases
unzip("activity.zip")
activity <- read.csv("activity.csv")
#activity <- activity[complete.cases(activity),]
```
The data set that was used for the analysis is the one that was included in the [RepData_PeerAssessment1 Git repository](https://github.com/rdpeng/RepData_PeerAssessment1) (commit 80edf39c3bb508fee88e3394542f967dd3fd3270).  It can also be downloaded from [https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

## What is mean total number of steps taken per day?
The following histogram shows the distribution of frequencies of the number of steps taken each day:

```r
library(plyr)
daily_steps <- subset(ddply(activity, c("date"), summarise, total_steps = sum(steps, na.rm = TRUE)), total_steps != 0)
hist(daily_steps$total_steps,  breaks = 20 ,col="blue", xlab = "Total Daily Steps", main = "Histogram of Total Daily Steps")
```

![](PA1_template_files/figure-html/dialy_steps-1.png) 

```r
mean_daily_steps <- prettyNum(round(mean(daily_steps$total_steps), 0), big.mark = ",")
median_daily_steps <- prettyNum(median(daily_steps$total_steps), big.mark = ",")
```
The average number of steps taken was 10,766 and the median was 10,765.

## What is the average daily activity pattern?
The following chart

```r
activity_pattern <- subset(ddply(activity, c("interval"), summarize, average_steps = mean(steps, na.rm = TRUE)),  average_steps != 0)
plot(activity_pattern, type="l", col="blue", xlab="Time Interval", ylab="Average Steps", main="Average Steps Taken per Time Interval")
```

![](PA1_template_files/figure-html/activity_pattern-1.png) 

```r
max_steps <- activity_pattern[with(activity_pattern, order(-average_steps)),][1,]
max_interval <- prettyNum(max_steps$interval[1], big.mark = ",")
max_avg <- prettyNum(round(max_steps$average_steps[1], 0), big.mark = ",")
```
The interval with the highest average number of steps was 835 with an average of 206 steps.

## Imputing missing values



```r
msbi <- ddply(activity, c("interval"), summarize, median_steps = median(steps, na.rm = TRUE))
activity_2 <- data.frame(interval = activity$interval, date = activity$date, steps=msbi[match(activity$interval, msbi$interval), 2])
daily_steps_2 <- subset(ddply(activity_2, c("date"), summarize, total_steps = sum(steps, na.rm = TRUE)), total_steps != 0)
hist(daily_steps$total_steps,  breaks = 20 ,col="blue", xlab = "Total Daily Steps", main = "Histogram of Total Daily Steps")
```

![](PA1_template_files/figure-html/Imputing_values-1.png) 

```r
mean_daily_steps_2 <- prettyNum(round(mean(daily_steps_2$total_steps), 0), big.mark = ",")
median_daily_steps_2 <- prettyNum(median(daily_steps_2$total_steps), big.mark = ",")
```
The average number of steps taken was 1,141 and the median was 1,141.


## Are there differences in activity patterns between weekdays and weekends?
