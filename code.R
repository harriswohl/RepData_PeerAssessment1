## Reading in the file

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filePath <- tempfile()
download.file(fileUrl, filePath)
unzip(filePath)

activity <- read.csv("activity.csv")

#change date column from factor to posix and interval to factor

activity$date <- as.POSIXct(as.character(activity$date), format = "%Y-%m-%d")
activity$interval <- as.factor(activity$interval)

## Mean number of steps per day

# total per day

perday <- aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)
names(perday) <- c("date", "steps")

# histogram of steps per day 

library(ggplot2)
theme(plot.title = element_text(hjust = 0.5))
g = ggplot(perday, aes(x = perday$steps))
g = g + geom_histogram(color = "black", fill = "darkslategrey")
g = g + ylab("Frequency") + xlab("Steps per Day")
g = g + ggtitle("Steps per Day Frequency")
g = g + theme(plot.title = element_text(hjust = 0.5))
g = g + scale_y_continuous(breaks = seq(1, 10, 1))
g

# mean and median

summary(perday$steps)["Mean"]
summary(perday$steps)["Median"]


## average daily activity 

intervalMeans <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
intervalMeans <- data.frame("Average.Steps" =  intervalMeans)
intervalMeans$Interval <- as.numeric(as.character(unique(activity$interval)))
intervalMeans$Average.Steps <- as.numeric(intervalMeans$Average.Steps)
head(intervalMeans)

g = ggplot(data = intervalMeans, 
           aes(x = Interval, y = Average.Steps),
           group = 1)
g = g +geom_line(size = .75)
g = g + ylab("Steps")
g = g + ggtitle("Average Number of Steps per Interval")
g = g + theme(plot.title = element_text(hjust = 0.5))
g

# obtaining max interval

intervalMeans[intervalMeans$Average.Steps == max(intervalMeans$Average.Steps),]

## imputing missing values

# how many na

na <- activity[is.na(activity),]
nrow(na)

# replace NA with the mean of that interval 
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

#checking if there are any NA

apply(activity, 2, anyNA)
apply(filled, 2, anyNA)

# histogram of total steps per day 

dayfilled <- aggregate(filled$steps, by = list(filled$date), FUN = sum)
names(dayfilled) <- c("date", "steps")
head(dayfilled)

g = ggplot(dayfilled, aes(x = steps))
g = g + geom_histogram(color = "black", fill = "darkslategrey")
g = g + xlab("Steps per Day") + ylab("Frequency") 
g = g + ggtitle("Steps per Day Frequency (NAs removed)")
g = g + theme(plot.title = element_text(hjust = 0.5))
g

sum(dayfilled$steps)
sum(perday$steps, na.rm = TRUE)

summary(dayfilled$steps)["Median"]
summary(dayfilled$steps)["Mean"]

summary(perday$steps)["Mean"]
summary(perday$steps)["Median"]

## Weekdays vs weekends

#outputs weekend or weekday 
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

# panel plot of average steps taken for each 5 min interval 
# among weekend and weekday 

panel <- aggregate(steps~interval + weekType, data = filled, FUN = mean)

library(ggplot2)
g = ggplot(panel, aes(x = as.numeric(as.character(interval)), y = steps, color = weekType))
g = g + geom_line(size = .75)
g = g + facet_wrap(~weekType, ncol = 1, nrow = 2)
g = g + ggtitle("Average number of Steps per Interval")
g = g + xlab("Interval") + ylab("Steps")
g = g + theme(plot.title = element_text(hjust = 0.5))
g