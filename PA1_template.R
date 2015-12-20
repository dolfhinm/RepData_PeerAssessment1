#Loading and preprocessing the data
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile="Factivity.zip")
unzip("Factivity.zip", overwrite=TRUE)
data<-read.csv("activity.csv")

#What is mean total number of steps taken per day?
data_without_na<-na.omit(data)

library(plyr)
res_steps_sum<-ddply(data_without_na,.(Date=as.Date(data_without_na$date)),function(x) sum(x$steps)) 

png(filename="035-plot1.png", width=480, heigh=480, units="px")
hist(res_steps_sum$V1, main="Histogram of the total number of steps taken each day", xlab="Number of steps")
x<-dev.off()

mean(res_steps_sum$V1)
median(res_steps_sum$V1)


#What is the average daily activity pattern?

res_interval_mean<-ddply(data_without_na,.(interval=data_without_na$interval),function(x) mean(x$steps)) 

png(filename="035-plot2.png", width=480, heigh=480, units="px")
plot(res_interval_mean$interval, res_interval_mean$V1, type="l", xlab="interval", ylab="mean of steps", main="Average number of steps averaged over all days")
x<-dev.off()

res_interval_mean$interval[which.max(res_interval_mean$V1)]

#Imputing missing values
count<-nrow(data)-nrow(data_without_na)
count

data_na <- data[!complete.cases(data),]
for(i in 1:count){
    data_na$steps[i]=min(data_without_na$steps)
}

data_new<-rbind(data_without_na, data_na)

res_steps_sum_all<-ddply(data_new,.(Date=as.Date(data_new$date)),function(x) sum(x$steps)) 

png(filename="035-plot5.png", width=480, heigh=480, units="px")
hist(res_steps_sum_all$V1, main="Histogram of the total number of steps taken each day", xlab="Number of steps")
x<-dev.off()

mean(res_steps_sum_all$V1)
median(res_steps_sum_all$V1)

#Are there differences in activity patterns between weekdays and weekends?


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

library(lattice)

output<-rbind(weekday_mean, weekend_mean)

png(filename="035-plot4.png", width=480, heigh=480, units="px")
xyplot(V1 ~ interval | week, data = output, 
       layout = c(1, 2), ylab = "Number of steps", 
       main = "Average number of steps for all weekday days or weekend days", 
       scales=list(cex=.8, col="red"), type="l")
x<-dev.off()

write.table(, file="PA1_template.md", row.names=FALSE, quote=FALSE, col.names=FALSE, sep="\t")
