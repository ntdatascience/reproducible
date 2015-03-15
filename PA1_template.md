---
output: pdf_document
---
Reproducible Research  Programming Assigment 1
========================================================

* Introduction

This Programming Assignment 1 is described at: [coursera Reproducible Research PA 1](https://class.coursera.org/repdata-011/human_grading/view/courses/973512/assessments/3/submissions)

* Loading and preprocessing the data

Load data from url, unzip, translate data into R date object, and prepare for weekday/weekend comparisons.
  

```r
library(plyr)
#downloader::download("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
#             "activity.zip", 
#             mode = "wb")  
#unzip("activity.zip", "activity.csv")
activity.df <- read.csv( "activity.csv", na.strings = "NA")
activity.df$date <- as.Date(activity.df$date, "%Y-%m-%d")
activity.df$day.of.week <- format(activity.df$date, "%a")
activity.df$day.type <- activity.df$day.of.week
activity.df$day.type[which(activity.df$day.type == 'Sat' || activity.df$day.type == 'Sun' )] <- "Weekend"
activity.df$day.type[which(activity.df$day.type != 'Weekend')] <- "Weekday"

activity.cleaned.df <-activity.df[which(!is.na(activity.df$steps)),]
```

* What is mean total number of steps taken per day?

Histogram number of steps per day


```r
dsummary.steps <- tapply(activity.cleaned.df$steps, activity.cleaned.df$date, sum, na.rm = TRUE)

hist(dsummary.steps, main="Total Steps By Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Overall Mean Steps


```r
mean(dsummary.steps)
```

```
## [1] 10766.19
```

Overall Median Steps 


```r
median(dsummary.steps)
```

```
## [1] 10765
```



# What is the average daily activity pattern?


```r
daily.df <- tapply(activity.cleaned.df$steps, activity.cleaned.df$interval, mean)
plot( y = daily.df, x = names(daily.df), type = "l", 
     xlab = "5-Minute-Interval", 
     ylab = "Average Steps", 
     main = "Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
str(daily.df)
```

```
##  num [1:288(1d)] 1.717 0.3396 0.1321 0.1509 0.0755 ...
##  - attr(*, "dimnames")=List of 1
##   ..$ : chr [1:288] "0" "5" "10" "15" ...
```

What interval shows as having the max actvity

Max for interval

```r
daily.df[daily.df==max(daily.df)]
```

```
##      835 
## 206.1698
```

* Inputing missing values

For instructional purposes of this exercise, I am choosing to pick the mean for the day to fill in missing values of "NA"

INCOMPLETE


```r
activity.est.df <- activity.df 
mean.df <- ddply(activity.cleaned.df, "date", summarize, steps=mean(steps))
str(mean.df)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Date, format: "2012-10-02" "2012-10-03" ...
##  $ steps: num  0.438 39.417 42.069 46.16 53.542 ...
```

```r
summary(mean.df)
```

```
##       date                steps        
##  Min.   :2012-10-02   Min.   : 0.1424  
##  1st Qu.:2012-10-16   1st Qu.:30.6979  
##  Median :2012-10-29   Median :37.3785  
##  Mean   :2012-10-30   Mean   :37.3826  
##  3rd Qu.:2012-11-16   3rd Qu.:46.1597  
##  Max.   :2012-11-29   Max.   :73.5903
```

```r
for ( i in 1:nrow(activity.est.df)) {
  if (is.na(activity.est.df[i,"steps"])) {
    dt <- activity.est.df[i,"date"]
    #str(mean.df[mean.df$date == dt,])
    #$activity.est.df[i,"steps"] <- daily.steps.avg[daily.steps.avg$date == dt ,"mean"]  
  }
}
```





* Are there differences in activity patterns between weekdays and weekends?

