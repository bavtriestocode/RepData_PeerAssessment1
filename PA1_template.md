---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: yes
---

## Loading and preprocessing the data


```r
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

```r
library(ggplot2)

Raw_Data<-read.csv("activity.csv")
Raw_Data$date<-as.Date(Raw_Data$date)
Clean_Data<-na.omit(Raw_Data)
```

## What is mean total number of steps taken per day?


```r
Total_Steps_Data<-Clean_Data%>%
    group_by(date)%>%
    summarise(Daily_steps = sum(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(Total_Steps_Data$Daily_steps,
     main = "Total number of steps taken per day",
     xlab = "Total steps taken per day",
     breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/mean-1.png)<!-- -->

```r
mean<-mean(Total_Steps_Data$Daily_steps)
median<-median(Total_Steps_Data$Daily_steps)
```

The mean is 1.0766189\times 10^{4} and the median is 10765.


## What is the average daily activity pattern?


```r
Interval_Steps_Data<-Clean_Data%>%
    group_by(interval)%>%
    summarise(Avg_steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with(Interval_Steps_Data,plot(interval,Avg_steps,
                              type="l",
                              lwd = 2,
                              xlab="Interval",
                              ylab="Average number of steps",
                              main="Average number of steps per intervals"))
```

![](PA1_template_files/figure-html/avg-1.png)<!-- -->

```r
interval<-max(Interval_Steps_Data$Avg_steps)
```

The 5-minute interval that, on average, contains the maximum number of steps is 206.1698113.

## Imputing missing values


```r
Missing_Values_Count<-nrow(Raw_Data)-nrow(Clean_Data)

Complete_Data<-Raw_Data
for(i in 1:nrow(Complete_Data)){
    if(is.na(Complete_Data$steps[i])){
      if(i%%288==0){j=288}
      else{j=0}
        Complete_Data$steps[i]<-Interval_Steps_Data$Avg_steps[i%%288+j]
    }
}

Complete_Total_Steps_Data<-Complete_Data%>%
    group_by(date)%>%
    summarise(Daily_steps = sum(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(Complete_Total_Steps_Data$Daily_steps,
     main = "Total number of steps taken per day",
     xlab = "Total steps taken per day",
     breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/Missing Values-1.png)<!-- -->

```r
c_mean<-mean(Complete_Total_Steps_Data$Daily_steps)
c_median<-median(Complete_Total_Steps_Data$Daily_steps)
```

There are 2304 missing values in the dataset.
After entering the missing values, the mean is 1.0766189\times 10^{4} and the median is 1.0766189\times 10^{4}.

## Are there differences in activity patterns between weekdays and weekends?


```r
Complete_Data$Day<-weekdays(Complete_Data$date)

for(k in 1:nrow(Complete_Data)){
    if(Complete_Data$Day[k]=="Saturday"|Complete_Data$Day[k]=="Sunday"){
        Complete_Data$Week[k]<-"Weekend"
    }
    else{
        Complete_Data$Week[k]<-"Weekday"
    }
}

Week_Interval_Steps_Data<-Complete_Data%>%
    group_by(interval,Week)%>%
    summarise(Avg_steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
plot<- ggplot(Week_Interval_Steps_Data,
              aes(x = interval, y = Avg_steps,color = Week)) +
    geom_line() +
    labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
    facet_wrap(~Week, ncol = 1, nrow=2)
print(plot)
```

![](PA1_template_files/figure-html/pattern-1.png)<!-- -->
