---
title: "**Reproducible Research: Peer Assessment 1**"
author: "Atharva Phand"
date: "24/07/2020"
output: 
  html_document:
    keep_md: true
---


### **Loading and pre-processing the data**

*Reading data into a data frame*


```r
df <- read.csv("activity.csv")
df[,2] <- as.Date(df[,2])
```


### **What is mean total number of steps taken per day?**

*1. Calculate the total number of steps taken per day*


```r
good <- complete.cases(df)
df1 <- df[good,]
s <- sapply(split(df1$steps,df1$date),sum)
```


*2. Make a histogram of the total number of steps taken each day*


```r
hist(s,xlab = "total number of steps taken per day",main = "Histogram of total steps for 2 months")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

*3. Calculate and report the mean and median total number of steps taken per day*


```r
m1 <-  as.integer(sum(s)/length(unique(df1$date)))
m2 <- sapply(split(df1$steps,df1$date),sum)
m2 <- median(m2)
```

**Mean - 10766**  
**Median - 10765**

### **What is the average daily activity pattern?**

*1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)* 
*and the average number of steps taken, averaged across all days (y-axis)*


```r
adap <-sapply(split(df1$steps,df1$interval),mean)
plot(unique(df1$interval),adap,type = "l",ylab = "average number of steps taken",xlab = "interval",
     main = "Time series plot")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

*2. Which 5-minute interval, on average across all the days in the data set, contains the maximum number of steps?*

**Maximum number of steps - 206.1698113**  
**Interval - 835**

### **Imputing missing values**

*1. Calculate and report the total number of missing values in the data set (i.e. the total number of rows with NA's)*


```r
x <- is.na(df[,1])
df2 <- df[x,]
```

**Total number of missing values - 2304**

*2. Devise a strategy for filling in all of the missing values in the data set. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*


```r
df2[,1] <- rep(adap,length=2304)
```

**Strategy - means of 5 minute intervals used to fill NA values**

*3. Create a new data set that is equal to the original data set but with the missing data filled in.*


```r
library(dplyr)
m <- merge(df1,df2,all=TRUE)
m <- arrange(m,date,interval)
```

*4. Make a histogram of the total number of steps taken each day*


```r
s1 <- sapply(split(m$steps,m$date),sum)
hist(s1,xlab = "total number of steps taken per day",main = "Histogram of total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

*5. Calculate and report the mean and median total number of steps taken per day*


```r
m3 <-  as.integer(sum(s1)/length(unique(m$date)))
m4 <- sapply(split(m$steps,m$date),sum)
m4 <- median(m4)
```

**Mean - 10766**  
**Median - 10766**

*6. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

**Mean value remains the same**  
**Median changes by 1 (increase)**  
**Impact - The frequency of total number of steps in 10000-15000 range increases while all other ranges remain constant.**
**This is because of the strategy used**

### **Are there differences in activity patterns between weekdays and weekends?**

*1. Create a new factor variable in the data set with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*


```r
f <- factor(1*(weekdays(m[,2]) %in% c("Saturday","Sunday")))
levels(f) <- c("weekday","weekend")
m$daytype <- f
```

*2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*


```r
library(ggplot2)
library(tibble)
```


```r
p1 <- split(m,m$daytype)
adap1 <- sapply(split(p1$weekday$steps,p1$weekday$interval),mean)
adap1 <- as.numeric(adap1)
adap2 <- sapply(split(p1$weekend$steps,p1$weekend$interval),mean)
adap2 <- as.numeric(adap2)
df3 <- tibble(interval = rep(unique(p1$weekday$interval),times=2), steps = c(adap1,adap2))
df3$daytype <- rep(levels(f),each=288)
df3 <- as.data.frame(df3)
ggplot(df3,aes(interval,steps)) + geom_line() + facet_grid(daytype~.) + labs(y="Number of steps", x="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
