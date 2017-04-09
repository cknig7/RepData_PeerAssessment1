---
title: "Reproducible Research: Activity Data"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(data.table)
require(plyr)
require(Hmisc)
library(ggplot2)
```


## Load and process the data

After downloading the data set from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip and setting your working directory, load the data.

setwd("C:\\Users\\cknight\\Documents\\R Coursera Course\\Week5 v3")
getwd()

```{r}
activityData <- read.csv('./data/activity.csv', header = TRUE, sep = ",",
  colClasses=c("numeric", "character", "numeric"))
activityData$date <- as.Date(activityData$date,"%Y-%m-%d")
```

```{r opts, echo = FALSE}
opts_chunk$set(fig.path = "./figures/")
```


## What is the mean total number of steps taken per day? Ignore missing values

### 1.) Calculate the total number of steps taken per day
```{r}
  stepsPerDay <- aggregate(steps ~ date, activityData, sum)
```

## 2.) Make a histogram of the total number of steps taken each day
```{r, ggplot, echo=T}
  ggplot(stepsPerDay, aes(x = steps)) + 
         geom_histogram(fill = "green", binwidth = 1000) + 
          labs(title="Steps Taken per Day", 
               x = "Number of Steps per Day", y = "Count")
```

## 3.) Calculate and report the mean and median of the total number of steps taken each day

```{r}
  meanStepsPerDay  <- round(mean(stepsPerDay$steps, na.rm=TRUE), 2)
  meanStepsPerDay
  medianStepsPerDay <- round(median(stepsPerDay$steps, na.rm=TRUE), 2)
  medianStepsPerDay
```
The mean steps per day is 10,766.19 and the median is 10,765.


## What is the average daily activity pattern?

### 1.) Make a time series plot




```{r, echo=T}
stepsPerInterval <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)

ggplot(data=stepsPerInterval, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5 Minute Interval") +
    ylab("Average Steps")
```


### 2.) Which 5-min interval contains the max number of steps?

```{r}
  maxInterval <- stepsPerInterval[which.max(  
          stepsPerInterval$meanSteps),]$interval
  maxInterval
```
The 835th interval contains the max number of steps.


## Impute missing values

### 1. Calculate and report the total number of missing values in the dataset

```{r}
  sum(is.na(activityData$steps))  
```
2,304 values are missing.

### 2.) Devise a strategy for filling in all of the missing values in the dataset.

We will impute the missing values with the mean steps per interval.


### 3.) Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
  activityDataImp <- merge(x = activityData, y = stepsPerInterval, by = "interval", all.x = TRUE)
  activityDataImp$meanSteps <- round(activityDataImp$meanSteps,0)
  activityDataImp$steps[is.na( activityDataImp$steps)] <-  activityDataImp$meanSteps
  activityDataImp <- activityDataImp[,-(4)]
```


### 4.) Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.

```{r, echo=T}
  stepsPerDay2 <- aggregate(steps ~ date, activityDataImp, sum)

  ggplot(stepsPerDay, aes(x = steps)) + 
         geom_histogram(fill = "green", binwidth = 1000) + 
          labs(title="Steps Taken per Day", 
               x = "Number of Steps per Day", y = "Count")

  meanStepsPerDay2  <- round(mean(stepsPerDay2$steps, na.rm=TRUE), 2)
  meanStepsPerDay2
  medianStepsPerDay2 <- round(median(stepsPerDay2$steps, na.rm=TRUE), 2)
  medianStepsPerDay2
```

Afterimputation, the mean steps per day is 9,369.23 and the median is 10,395.These numbers are lower than before imputing.


## Weekends and weekdays

### 1.) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
  activityDataImp$day <- weekdays(as.Date(activityDataImp$date))
  activityDataImp$wknd <- ifelse(activityDataImp$day %in% c("Saturday", "Sunday"), "weekend", "weekday")
  
  stepsPerDay2$day <- weekdays(as.Date(stepsPerDay2$date))
  stepsPerDay2$wknd <- ifelse(stepsPerDay2$day %in% c("Saturday", "Sunday"), "weekend", "weekday")
```

## 2.) Make a panel plot containing a time series plot 

```{r, echo=T}
  activityDataImp2 <- aggregate(steps ~ interval + wknd, data=activityDataImp, mean)
  
  ggplot(activityDataImp2, aes(x=interval, y=steps)) + 
          geom_line(color="green", size=1) + 
          facet_wrap(~ wknd, nrow=2, ncol=1) +
          labs(x="Interval", y="Number of steps")
```