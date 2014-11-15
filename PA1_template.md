---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
- Download the raw activty data:

```r
    activity_zip_file <- "activity.zip"
    source_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download_time <- format(Sys.time(), "%a %b %d %X %Y")
    download.file(url = source_url, destfile = activity_zip_file, mode = "wb")
```
Downloaded activity data downloaded from url https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip on Fri Nov 14 8:06:39 PM 2014

- Expand the zip file:

```r
    activity_csv_file <- "activity.csv"
    unzip(activity_zip_file, exdir = ".")
```

- Read the activity data file into a data frame:

```r
    activity_data <- read.csv(activity_csv_file, na.strings=("NA"))
```

## What is mean total number of steps taken per day?

```r
    library(dplyr, quietly = TRUE)
    step_sum_data <- activity_data %>% group_by(date) %>% dplyr::summarise(stepsperday=sum(steps, na.rm=TRUE))
    hist(step_sum_data$stepsperday, main = "Histogram of Steps per Day (NAs Removed)", xlab = "Steps per Day (NAs Removed)", col = "red")
```

![plot of chunk calculate_steps_per_day](figure/calculate_steps_per_day.png) 

```r
    mean_steps_per_day_na_removed <- as.integer(mean(step_sum_data$stepsperday, na.rm = TRUE))
    median_steps_per_day_na_removed <- as.integer(median(step_sum_data$stepsperday, na.rm = TRUE))
```

**Steps per day(mean - NAs removed) = 9354** 

**Steps per day(median - NAs removed) = 10395** 

## What is the average daily activity pattern?


```r
   daily_avg_data <- activity_data %>% group_by(interval) %>% dplyr::summarise(avgstepsperinterval=mean(steps, na.rm=TRUE))
   with(daily_avg_data,
        plot(x=interval, y=avgstepsperinterval, xlab="Interval", ylab="Avg Steps per Interval", type="l", col="blue"))
```

![plot of chunk avg_daily_activity](figure/avg_daily_activity.png) 

```r
   maximum_avg_step_interval <- daily_avg_data[daily_avg_data$avgstepsperinterval == max(daily_avg_data$avgstepsperinterval), ]
```

**Interval with maximum average number of steps (NAs removed) = 835**

**Maximum average number of steps (NAs removed) = 206.1698**

## Imputing missing values
- Find the number of NA values in the original data set

```r
na_sum <- sum(is.na( activity_data ))
```

**Number of NA values in the original data set = 2304**
- Impute missing number of steps.  If the number of steps is missing substitute the average number of steps for that interval


```r
    # make a copy of the steps from the original data
    imputed_steps <- activity_data$steps

    # fill in the missing step values
    for(i in 1:length(imputed_steps)) {
        if (is.na(imputed_steps[i])) {
            # if steps is missing
            # use the average daily steps for this interval calculated above
            imputed_steps[i] = as.integer(daily_avg_data$avgstepsperinterval[daily_avg_data$interval == activity_data$interval[i] ])
        }
    }
   
    # create a data frame with imputed steps, date and interval
    imputed_activity_data <- data.frame(steps=imputed_steps, date=activity_data$date, interval=activity_data$interval)
    imputed_step_sum_data <- imputed_activity_data %>% group_by(date) %>% dplyr::summarise(stepsperday=sum(steps, na.rm=TRUE))
    hist(imputed_step_sum_data$stepsperday, main = "Histogram of Steps per Day (NAs Imputed)", xlab = "Steps per Day (NAs Imputed)", col = "red")
```

![plot of chunk imput_nas](figure/imput_nas.png) 

```r
    mean_steps_per_day_na_imputed <- as.integer(mean(imputed_step_sum_data$stepsperday, na.rm = TRUE))
    median_steps_per_day_na_imputed <- as.integer(median(imputed_step_sum_data$stepsperday, na.rm = TRUE))
```

**Steps per day(mean - NAs imputed) = 10749** 

**Steps per day(median - NAs imputed) = 10641** 

- What is the impact of imputing the missing step data?


```r
   mean_impact <- ifelse(mean_steps_per_day_na_imputed > mean_steps_per_day_na_removed, "greater", "less")
   mean_difference <- abs(mean_steps_per_day_na_imputed - mean_steps_per_day_na_removed)
   median_impact <- ifelse(median_steps_per_day_na_imputed > median_steps_per_day_na_removed, "greater", "less")
   median_difference <- abs(median_steps_per_day_na_imputed - median_steps_per_day_na_removed)
```

Imputed mean steps per day are greater than mean steps per day with NAs removed.
Difference between means = 1395.
Imputed median steps per day are greater than median steps per day with NAs removed.
Difference between medians = 246.


## Are there differences in activity patterns between weekdays and weekends?

```r
    library(ggplot2, quietly = TRUE)
    imputed_activity_data <- mutate(imputed_activity_data, dayclassification=ifelse((weekdays(ymd(date)) == "Saturday" | weekdays(ymd(date)) == "Sunday"), "weekend", "weekday"))
    avg_activity_per_day <- imputed_activity_data %>% group_by(interval, dayclassification) %>% dplyr::summarise(stepsperday=mean(steps, na.rm=TRUE))
    p <- qplot(x = interval, y = stepsperday, data=avg_activity_per_day, geom="line") + 
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), panel.border = element_rect(fill = NA, colour="black"), 
               strip.background = element_rect(fill ="beige", colour="black"), 
               axis.ticks = element_line(colour="black"), axis.line = element_line(colour="black"), axis.text = element_text(colour = "black")) + 
         geom_line(colour="blue") +
         facet_wrap(~ dayclassification, ncol=1) +
         xlab("Interval") + ylab("Number of Steps") +
         ggtitle("Weekday vs Weekend Activity Patterns") 
    print(p)
```

![plot of chunk weekend_vs_weekday](figure/weekend_vs_weekday.png) 
