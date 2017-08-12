# Reproducible Research - Week 2 Assignment

### Purpose
The purpose of the assignment is to produce a single R markdown document that can be processed by knitr and be transformed into an HTML file. The report must include the code used to generate the output and the output itself.  


### Loading and preprocessing the data
The assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November 2012 and include the number of steps taken in 5 minute intervals each day. It is located at (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).  

#### Libraries

```r
library(dplyr) 
library(ggplot2)
```


#### Reading Activity Data  

```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
activity <- read.csv(unz(temp, "activity.csv"))
unlink(temp)
##activity <- read.csv("activity.csv")
rowcounts <- formatC(count(activity), format = "d", big.mark = ",")
activityset1 <- select(activity, steps, date)
activityset1 <- filter(activityset1, !is.na(steps))
activityset1$date <- as.Date(activityset1$date)
activityset1 <- group_by(activityset1, date)
stepsbydate <- summarise(activityset1, sum_steps = sum(steps))
```
**17,568** records have been loaded from the activity data set. 

***

### What is mean total number of steps taken per day?
 
Histogram of the total steps per day.  

```r
ggplot(data=stepsbydate, aes(x=date, weights=sum_steps)) + geom_histogram(binwidth=1) + labs(x="Date", y="Total Steps")
```

<img src="PA1_template_files/figure-html/plot1-1.png" style="display: block; margin: auto;" />

```r
ggsave("total_steps.png")
invisible(dev.off())
```

***

#### Mean and Median of the Total Steps per Day  

```r
options(scipen=999)
mean_steps <- formatC(as.double(summarise(stepsbydate, mean_steps = mean(sum_steps))),format="d",big.mark=",")
med_steps <- formatC(as.double(summarise(stepsbydate, median_steps = median(sum_steps))),format="d",big.mark=",")
```

The mean total number of steps per day is **10,766**.  
The median total number of steps per day is **10,765**.  

***

### What is the average daily activity pattern?  
  

```r
activityset2 <- select(activity, steps, interval)
activityset2 <- filter(activityset2, !is.na(steps))
activityset2 <- group_by(activityset2, interval)
stepsbyinterval <- summarise(activityset2, avg_steps = mean(steps), sum_steps = sum(steps))
ggplot(data=stepsbyinterval, aes(interval, avg_steps)) + geom_line() + labs(x="5 Min Interval", y="Avg Steps")
```

<img src="PA1_template_files/figure-html/plot2-1.png" style="display: block; margin: auto;" />

```r
ggsave("avg_steps_per_interval.png")
invisible(dev.off())
```

***

#### Interval with Highest Average Step Count  

```r
options(scipen=999)
maxavg_steps <- as.double(summarise(stepsbyinterval, max_steps = max(avg_steps)))
stepsbyinterval$avg_steps <- as.double(stepsbyinterval$avg_steps)
maxinterval <- filter(stepsbyinterval, avg_steps == maxavg_steps)
maxavg_steps <- round(maxavg_steps)
intervalname <- maxinterval$interval
```

The interval with the highest average steps is **835** with **206**.  

***

### Imputing missing values
  

```r
nasteps <- filter(activity, is.na(steps))
nas <- formatC(count(nasteps),format="d",big.mark=",")
```

There are **2,304** NAs in the data set.  

***

#### Missing Value Strategy 

I am substituting the NAs with the average number of steps for the interval. 

```r
intervalsteps <- inner_join(activity, stepsbyinterval) 
intervalsteps$steps[is.na(intervalsteps$steps)] <- intervalsteps$avg_steps[is.na(intervalsteps$steps)]
intervalsteps$date <- as.Date(intervalsteps$date)
intervalsteps <- group_by(intervalsteps, date)
stepsbydate2 <- summarise(intervalsteps, sum_steps = sum(steps))
```

***

#### Total Steps per Day (after replacing NAs) 


```r
ggplot(data=stepsbydate2, aes(x=date, weights=sum_steps)) + geom_histogram(binwidth=1) + labs(x="Date", y="Total Steps")
```

<img src="PA1_template_files/figure-html/plot3-1.png" style="display: block; margin: auto;" />

```r
ggsave("total_steps_no_nas.png")
invisible(dev.off())
```

***

#### Mean and Median of the Total Steps per Day (after replacing NAs) 

```r
options(scipen=999)
mean_steps2 <- formatC(as.double(summarise(stepsbydate2, mean_steps = mean(sum_steps))),format="d",big.mark=",")
med_steps2 <- formatC(as.double(summarise(stepsbydate2, median_steps = median(sum_steps))),format="d",big.mark=",")
```

The mean total number of steps per day is **10,766**.  
The median total number of steps per day is **10,766**.  

***

### Are there differences in activity patterns between weekdays and weekends?

For this analysis I am using the data set where NAs have been replaced by the average steps per interval. 

```r
intervalsteps <- mutate(intervalsteps, date_category = 
                           ifelse(weekdays(date) == "Saturday", "weekend",
                           ifelse(weekdays(date) == "Sunday", "weekend",                              
                           "weekday")))                                      
```

#### Comparing Activity: Weekday vs Weekend

```r
##intervalsteps <- 
plotx <- ggplot(data=intervalsteps, aes(x=interval,steps)) + geom_line() + labs(x="5 Min Interval", y="Avg Steps")
plotx + facet_grid(date_category ~ .)
```

<img src="PA1_template_files/figure-html/plot4-1.png" style="display: block; margin: auto;" />

```r
ggsave("weekday_vs_weekend.png")
invisible(dev.off())
```
