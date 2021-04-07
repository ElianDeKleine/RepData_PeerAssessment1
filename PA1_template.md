---
title: "PA1_template.Rmd"
author: "Elian de Kleine"
date: "7-4-2021"
output:
        html_document:
                keep_md: true 
---



## Loading and preprocessing the data

This code is used to load and preprocess the data:



```r
#download file
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
unzip(temp, "activity.csv")
Activity_monitoring_data <- read.table("activity.csv", sep=",", header=T)
Activity_monitoring_data$date <- as.Date(Activity_monitoring_data$date)

#calculate number of steps taken each day:
Daysteps<-aggregate(Activity_monitoring_data$steps, by=list(Category=Activity_monitoring_data$date), FUN=sum, na.rm=TRUE)
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day.


```r
ggplot(Daysteps, aes(x=x)) + geom_histogram(binwidth=2000, na.rm = TRUE)+labs(title="Histogram; Number of steps taken each day",x="Number steps per day", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
mean(Daysteps$x)
```

```
## [1] 9354.23
```

```r
median.default(Daysteps$x)
```

```
## [1] 10395
```
The mean of the total number of steps taken per day is 9354
The median of the total number of steps taken per day is 10395

## What is the average daily activity pattern?
1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averagesteps<-aggregate(Activity_monitoring_data$steps, by=list(Category=Activity_monitoring_data$interval), na.rm=TRUE, FUN=mean)

plot(averagesteps$Category, averagesteps$x, type = "l", main = "Average number of steps taken, averaged across all days", xlim = c(0, 2400),xlab = "5-minute interval", ylim = c(0, 300), ylab = "Mean number of steps"     )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 


```r
averagesteps[which(averagesteps$x == max(averagesteps$x)),]
```

```
##     Category        x
## 104      835 206.1698
```
The 5-minute interval 835 contains on average the maximum number of steps.

## Imputing missing values


1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

```r
colSums(is.na(Activity_monitoring_data))
```

```
##    steps     date interval 
##     2304        0        0
```
The total number of rows with NA's is 2304, they all appear in the column steps.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I chose to use the mean for that inteval.

```r
newdata <- Activity_monitoring_data %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps)) %>%
  ungroup()
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

The new dataset I created is named newdata (see r-code question 2.)

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
Daysteps_newdata<-aggregate(newdata$steps, by=list(Category=newdata$date), FUN=sum)

mean(Daysteps_newdata$x)
```

```
## [1] 10766.19
```

```r
median.default(Daysteps_newdata$x)
```

```
## [1] 10766.19
```
The mean of the total number of steps taken per day is 10766  
The median of the total number of steps taken per day is 10766  
There is (almost) no impact on the estimates of imputing the missing data.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
newdata$weekday <- format(as.Date(newdata$date), "%a")

newdata$wdays <- as.factor(ifelse((newdata$weekday) %in% c("za", "zo"), "weekend", "weekday"))
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
dataPlot <- aggregate(newdata$steps, by = list(newdata$interval, newdata$wdays), FUN = mean)
names(dataPlot) <- c("interval", "weekday", "steps")
xyplot(dataPlot$steps ~ dataPlot$interval | dataPlot$weekday, 
       type = "l", 
       ylab = "Average number of steps", 
       xlab = "5-minute interval", 
       layout = c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
knit2html()

