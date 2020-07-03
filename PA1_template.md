---
output:
  html_document:
    keep_md: true
---
# Reproducible Research Project 1  

## Loading and Preprocessing the Data



```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filePath <- tempfile()
download.file(fileUrl, filePath)
unzip(filePath)

activity <- read.csv("activity.csv")

#change date column from factor to posix and interval to factor

activity$date <- as.POSIXct(as.character(activity$date), format = "%Y-%m-%d")
activity$interval <- as.factor(activity$interval)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

  
## Steps Per Day 

Below is a histogram representing the total number of steps taken per day.  

```r
perday <- aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)
names(perday) <- c("date", "steps")

library(ggplot2)
g = ggplot(perday, aes(x = perday$steps))
g = g + geom_histogram(color = "black", fill = "darkslategrey")
g = g + ylab("Frequency") + xlab("Steps per Day")
g = g + ggtitle("Steps per Day Frequency")
g = g + theme(plot.title = element_text(hjust = 0.5))
g = g + scale_y_continuous(breaks = seq(1, 10, 1))
g
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
dev.copy(png, file = "plot1.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

As shown in the histogram, the individual frequently has 6,000-15,000 steps per day, but most frequently has 0 steps per day. 

The mean and median number of steps per day are shown below. 


```r
summary(perday$steps)["Mean"]
```

```
##    Mean 
## 9354.23
```

```r
summary(perday$steps)["Median"]
```

```
## Median 
##  10395
```

  
## Average Daily Activity Pattern

The following code produces a time series plot of the average number of steps per 5-minute interval, averaged across days. 


```r
intervalMeans <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
intervalMeans <- data.frame("Average.Steps" =  intervalMeans)
intervalMeans$Interval <- as.numeric(as.character(unique(activity$interval)))
intervalMeans$Average.Steps <- as.numeric(intervalMeans$Average.Steps)
head(intervalMeans)
```

```
##    Average.Steps Interval
## 0      1.7169811        0
## 5      0.3396226        5
## 10     0.1320755       10
## 15     0.1509434       15
## 20     0.0754717       20
## 25     2.0943396       25
```

```r
g = ggplot(data = intervalMeans, 
           aes(x = Interval, y = Average.Steps),
           group = 1)
g = g +geom_line(size = .75)
g = g + ylab("Steps")
g = g + ggtitle("Average Number of Steps per Interval")
g = g + theme(plot.title = element_text(hjust = 0.5))
g
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dev.copy(png, file = "plot2.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

As shown below, the interval 835 contains the maximum amount of steps relative to the other 5-minute intervals.  


```r
intervalMeans[intervalMeans$Average.Steps == max(intervalMeans$Average.Steps),]
```

```
##     Average.Steps Interval
## 835      206.1698      835
```


  
## Imputing Missing Values

The following code finds the number of missing values in the dataset to be 2304.

```r
na <- activity[is.na(activity),]
nrow(na)
```

```
## [1] 2304
```

My strategy to fill in the missing values in the dataset was to use the mean for the corresponding interval across all days. I felt this was a reliable and precise measure, plus I had already created the dataset IntervalMeans that contains the average number of steps for each interval across all days.


```r
filled <- activity

i = 1
while(i <= nrow(filled)){
        if(!is.na(filled[i,1])){
                i = i +1 
                next()}
        int <- as.numeric(as.character(filled[i, 3]))
        avg <- intervalMeans[intervalMeans$Interval == int, 1]
        filled[i, 1] <- avg
        i = i + 1
}

apply(activity, 2, anyNA)
```

```
##    steps     date interval 
##     TRUE    FALSE    FALSE
```

```r
apply(filled, 2, anyNA)
```

```
##    steps     date interval 
##    FALSE    FALSE    FALSE
```

As the output shows, the activity dataset has NA values in the steps column, but the newly created filled dataset does not contain any NA values. 

The following is a histogram of the total number of steps taken per day with missing values replaced by their means. Already it is evident that zero-step days are less frequent than in the first part of the assignment. 


```r
dayfilled <- aggregate(filled$steps, by = list(filled$date), FUN = sum)
names(dayfilled) <- c("date", "steps")

g = ggplot(dayfilled, aes(x = steps))
g = g + geom_histogram(color = "black", fill = "darkslategrey")
g = g + xlab("Steps per Day") + ylab("Frequency") 
g = g + ggtitle("Steps per Day Frequency (NAs removed)")
g = g + theme(plot.title = element_text(hjust = 0.5))
g
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
dev.copy(png, file = "plot3.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

Replacing the missing values with their mean caused the mean to be identical to the median. Additionally, the filled dataset has a greater mean and greater median than the dataset that contains NAs. 

```r
filledmedian <- summary(dayfilled$steps)["Median"]
filledmean <- summary(dayfilled$steps)["Mean"]

namedian <- summary(perday$steps)["Median"]
namean <-summary(perday$steps)["Mean"]
data.frame(mean = c(filledmean, namean), median = c(filledmedian, namedian), row.names = c("Filled", "Contains NAs"))
```

```
##                  mean   median
## Filled       10766.19 10766.19
## Contains NAs  9354.23 10395.00
```

  
## Difference in Activity Patterns Between Weekends and Weekdays

The following code creates a new column that determines whether the date is a weekday or weekend. 


```r
isweekend <- function(x){
        day <- weekdays(x)
        if(day == "Saturday" | day == "Sunday"){
                new <- "weekend"}
        else{new <- "weekday"}
        new
        
}

filled$weekType <- as.factor(
        sapply(dayfilled$date, isweekend)
)
```

Below is a panel plot representing the average number of steps per five minute interval, seperated by weekends and weekdays. 


```r
panel <- aggregate(steps~interval + weekType, data = filled, FUN = mean)

library(ggplot2)
g = ggplot(panel, aes(x = as.numeric(as.character(interval)), y = steps, color = weekType))
g = g + geom_line(size = .75)
g = g + facet_wrap(~weekType, ncol = 1, nrow = 2)
g = g + ggtitle("Average Number of Steps per Interval")
g = g + xlab("Interval") + ylab("Steps")
g = g + theme(plot.title = element_text(hjust = 0.5))
g
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
dev.copy(png, file = "plot4.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
