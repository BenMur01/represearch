---
title: "Reproducible Research Peer Assessment I"
author: "Brendan Murphy"
date: "Friday, October 17, 2014"
output: html_document
---

###About
This **Project I** for the John Hopkins **Reproducible Research** course in Coursera.


##Assignment
The purpose of this project was to practice:

* Loading and preprocessing data
* Finding the mean of the total number of Steps taken in a day
* Finding the average daily activity pattern
* Imputing missing values
* Discerning differences in activity patterns between weekdays and weekends

## Data
The data for this assignment can be downloaded from the course website site:

Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as `NA`)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

Load data into data frame `stepData`. 
```{r}
stepData <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
Total steps, make a Histogram, and calculate mean and median of total number of steps taken each day.
```{r} 
stepsPerDay <- aggregate(steps ~ date, stepData, sum)
hist(stepsPerDay$steps, main = paste("Total number of Steps Each Day"), col="wheat", xlab="Daily Number of Steps")
stepsMean <- mean(stepsPerDay$steps)
stepsMedian <- median(stepsPerDay$steps)
```
The mean is `r stepsMean``.
The median is `r stepsMedian`.

## What is the average daily activity pattern?

* Make a time series plot of 5 min interval(x-axis) and average number of steps taken,
averaged across all days
* Determine 5 min interval with maximum number of average steps. 
```{r}
stepsInterval <- aggregate(steps ~ interval, stepData, mean)

plot(stepsInterval$interval,stepsInterval$steps, type="l", col = "blue", lwd = 2,
     xlab="Interval", ylab="Avg No. of Steps",main="Avg Number of Steps per Day by Interval")

maxInterval <- stepsInterval[which.max(stepsInterval$steps),1]
```

The 5 minute interval that contains maximum number of steps is `r maxInterval`

## Impute missing values.
There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

* Missing values were imputed by inserting the average for each interval and Zeroes were imputed for 10-01-2012 because it was the first day.

* we then create a new dataset that is equal to the original dataset but with the missing data filled in.

* We make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```{r}
missingVals <- sum(!complete.cases(stepData))
```
The total number of missing values in the dataset is `r missingVals`

```{r}
newStepsData <- transform(stepData, steps = ifelse(is.na(stepData$steps), stepsInterval$steps[match(stepData$interval, stepsInterval$interval)], stepData$steps))
newStepsData[as.character(newStepsData$date) == "2012-10-01", 1] <- 0

newStepsPerDay <- aggregate(steps ~ date, newStepsData, sum)
hist(newStepsPerDay$steps, main = paste("Total number of Steps Each Day"), col="red", xlab="Daily Number of Steps")

newStepsMean <- mean(newStepsPerDay$steps)
newStepsMedian <- median(newStepsPerDay$steps)

meanDiff <- newStepsMean - stepsMean
medianDiff <- newStepsMedian - stepsMedian
totalStepsDiff <- sum(newStepsPerDay$steps) - sum(stepsPerDay$steps)
```
* The new mean is `r newStepsMean`.
* The new median is `r newStepsMedian`.
* The difference between old and new mean is `r meanDiff`.
* The difference between old and new median is `r medianDiff`.

## Are there differences in activity patterns between weekdays and weekends?
Here we create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

We then have a plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
``` {r}
weekDay <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
newStepsData$dType = as.factor(ifelse(is.element(weekdays(as.Date(newStepsData$date)),weekDay), "Weekday", "Weekend"))

dtypeData <- aggregate(steps ~ interval + dType, newStepsData, mean)

library(lattice)

xyplot(dtypeData$steps ~ dtypeData$interval|dtypeData$dType, main="Average Steps per Day by Interval",
       xlab="Interval", ylab="Steps",layout=c(1,2), type="l",col = "blue", lwd = 1.5)

```