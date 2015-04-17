---
title: 'Reproducible Research: Peer Assessment 1'
output:
html_document:
keep_md: yes
pdf_document: default
word_document: default
---

## Loading and preprocessing the data

```r
# loading all required libraries
library(tools)
library(plyr)
library(ggplot2)
library(gridExtra)


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
```
The data set that was used for the analysis is the one that was included in the [RepData_PeerAssessment1 Git repository](https://github.com/rdpeng/RepData_PeerAssessment1) (commit 80edf39c3bb508fee88e3394542f967dd3fd3270).  It can also be downloaded from [https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

## What is mean total number of steps taken per day?
The following histogram shows the distribution of frequencies of the number of steps taken each day:

```r
# summarize the data by date
daily_steps <- ddply(activity,
                     c("date"),
                     summarise,
                     total_steps = sum(steps,
                                       na.rm = TRUE)
                     )

# create histogram
ggplot(daily_steps, 
       aes(x=total_steps)
       ) +
        geom_histogram(colour = "black",
                       fill = "blue"
                       )+ 
        labs(title = "Histogram of Total Daily Steps", 
             y = "Frequency",
             x = "Total Daily Steps"
             )
```

![plot of chunk dialy_steps](figure/dialy_steps-1.png) 



The average number of steps taken was 9,354 and the median was 10,395.

## What is the average daily activity pattern?
The following chart

```r
# simmarize the data by interval
activity_pattern <- ddply(activity,
                          c("interval"),
                          summarize,
                          average_steps = mean(steps,
                                               na.rm = TRUE
                                               )
                          )

# create time series plot by interval
ggplot(activity_pattern,
       aes(x = interval,
           y = average_steps
           )
       ) +
        geom_line(colour = "blue",
                  size = 1
                  ) + 
        labs(title = "Average Steps per Interval", 
             y = "Average Steps",
             x = "Interval"
             ) 
```

![plot of chunk activity_pattern](figure/activity_pattern-1.png) 

```r
# find peak interval
max_steps <- activity_pattern[with(activity_pattern, order(-average_steps)),][1,]
max_steps
```

```
##     interval average_steps
## 104      835      206.1698
```




The interval with the highest average number of steps was 835 with an average of 206 steps.

## Imputing missing values
Many of the values were missing from the data set provided.  In order to fill in the missing values, the median value per interval was substituted for the missing values for the corresponding intervals.

```r
# get the median number of steps per interval
msbi <- ddply(activity,
              c("interval"),
              summarize,
              median_steps = median(steps,
                                    na.rm = TRUE
                                    )
              )

# create a new data frame replacing NA values with the meidan value for that interval
activity_2 <- data.frame(interval = activity$interval,
                         date = activity$date, 
                         steps=ifelse(is.na(activity$steps), 
                                      msbi[match(activity$interval, msbi$interval), 2],
                                      activity$steps
                                      ), 
                         steps_old = activity$steps
                         )

# summarize the data by date
daily_steps_2 <- subset(ddply(activity_2, 
                              c("date"),
                              summarize, 
                              total_steps = sum(steps,
                                                na.rm = TRUE
                                                )
                              ),
                        total_steps != 0
                        )

# Generate histogram
ggplot(daily_steps_2, 
       aes(x=total_steps)
       ) +
        geom_histogram(colour = "black",
                       fill = "blue"
                       )+ 
        labs(title = "Histogram of Total Daily Steps", 
             y = "Frequency",
             x = "Total Daily Steps"
             )
```

![plot of chunk Imputing_values](figure/Imputing_values-1.png) 


The average number of steps taken was 9,504 (a change of 150) and the median was 10,395 (a change of 0).

## Are there differences in activity patterns between weekdays and weekends?
The following charts show the difference in activity for weekdays vs weekends (using the imputed data):

```r
# get day of week from date and check to see if it's a weekend or weekday
activity_2$daytype <- ifelse(weekdays(as.Date(activity_2$date)) %in% c("Saturday","Sunday"),
                             "Weekend", 
                             "Weekday"
                             )
# simmarize the data by interval and day type
activity_pattern_2 <- ddply(activity_2,
                            c("daytype", "interval"),
                            summarize,
                            average_steps = mean(steps,
                                                 na.rm = TRUE
                                                 )
                            )

# generate weekday and weekend timeseries plots by interval
p_weekday <- ggplot(subset(activity_pattern_2, 
                           daytype == "Weekday", 
                           select= c(interval, average_steps)
                           ),
                    aes(x = interval,
                        y = average_steps
                        )
                    ) +
        geom_line(color = "blue",
                  size = 1
                  ) + 
        labs(title = "Average Steps per Interval for Weekdays", 
             y = "Average Steps",
             x = "Interval"
             ) +
        guides(color=guide_legend(title="Day Type")) +
        theme(legend.position="bottom")

p_weekend <- ggplot(subset(activity_pattern_2, 
                           daytype == "Weekend", 
                           select= c(interval, average_steps)
                           ),
                    aes(x = interval,
                        y = average_steps
                        )
                    ) +
        geom_line(color = "blue",
                  size = 1
                  ) + 
        labs(title = "Average Steps per Interval for Weekends", 
             y = "Average Steps",
             x = "Interval"
             ) +
        guides(color=guide_legend(title="Day Type")) +
        theme(legend.position="bottom")

grid.arrange(p_weekday, p_weekend, nrow=2)
```

![plot of chunk weekday_vs_weekend](figure/weekday_vs_weekend-1.png) 
