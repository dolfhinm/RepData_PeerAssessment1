---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
---

Loading and preprocessing the data
------------------------------------

1. Load, unzip and read data
```{r}
#url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#download.file(url, destfile="Factivity.zip")
#unzip("Factivity.zip", overwrite=TRUE)
data<-read.csv("activity.csv")
```

What is mean total number of steps taken per day?
-------------------------------------------------
1. Filter data from "NA". Then calculate the number of steps taken per day
```{r}
data_without_na<-na.omit(data)
library(plyr)
res_steps_sum<-ddply(data_without_na,.(Date=as.Date(data_without_na$date)),function(x) sum(x$steps)) 
```

2. Create histogram
```{r, echo=FALSE}
hist(res_steps_sum$V1, main="Histogram of the total number of steps taken each day", xlab="Number of steps")
```

3. Calculate mean and median total number of steps taken per day.
```{r}
mean(res_steps_sum$V1)
median(res_steps_sum$V1)
```

What is the average daily activity pattern?
-----------

1. Calculate the average number of of steps
```{r}
res_interval_mean<-ddply(data_without_na,.(interval=data_without_na$interval),function(x) mean(x$steps)) 
```

Make a time series plot of the 5-minute interval 
```{r, echo=FALSE}
plot(res_interval_mean$interval, res_interval_mean$V1, type="l", xlab="interval", ylab="mean of steps", main="Average number of steps averaged over all days")
```

2. Calculate the 5-minute interval which contains the maximum number of steps
```{r}
res_interval_mean$interval[which.max(res_interval_mean$V1)]
```

Imputing missing values
---------
1. Calculate and report the total number of missing values in the dataset
```{r}
count<-nrow(data)-nrow(data_without_na)
count
```
2. Devise a strategy for filling in all of the missing values in the dataset. Strategy in this case - data of "NA" equal to the minimum step
```{r}
data_na <- data[!complete.cases(data),]
for(i in 1:count){
    data_na$steps[i]=min(data_without_na$steps)
}
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
data_new<-rbind(data_without_na, data_na)
```
4. Make a histogram of the total number of steps taken each day.
```{r}
res_steps_sum_all<-ddply(data_new,.(Date=as.Date(data_new$date)),function(x) sum(x$steps))
hist(res_steps_sum_all$V1, main="Histogram of the total number of steps taken each day", xlab="Number of steps")
```

Calculate and report the mean and median total number of steps taken per day.
```{r}
mean(res_steps_sum_all$V1)
median(res_steps_sum_all$V1)
```


Are there differences in activity patterns between weekdays and weekends?
----------
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. Then combine data of weekend and weekday.
```{r}
Sys.setlocale("LC_TIME", "English")
week<-weekdays(as.Date(data_without_na$date, "%Y-%m-%d"))

for(i in 1:length(week)){
    if((week[i]=="Saturday")|(week[i]=="Sunday"))
        week[i]="weekend" else week[i]="weekday"
}
data_without_na$week<-as.factor(week)

weekday<-data_without_na[data_without_na$week=="weekday", ]
weekend<-data_without_na[data_without_na$week=="weekend", ]

weekend_mean<-ddply(weekend,.(interval=weekend$interval),function(x) mean(x$steps))
week1="weekend"
weekend_mean$week<-as.factor(week1)

weekday_mean<-ddply(weekday,.(interval=weekday$interval),function(x) mean(x$steps))
week1="weekday"
weekday_mean$week<-as.factor(week1)

output<-rbind(weekday_mean, weekend_mean)
```
2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
library(lattice)
xyplot(V1 ~ interval | week, data = output, 
       layout = c(1, 2), ylab = "number of steps", 
       main = "Average number of steps for all weekday days or weekend days", 
       scales=list(cex=.8, col="red"), type="l")
```