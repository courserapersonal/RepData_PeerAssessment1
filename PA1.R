##load data

unzip('activity.zip')
data<-read.csv('activity.csv')

## Calculate the total number of steps taken per day

TotalSteps<-tapply(data$steps,data$date, sum, na.rm=TRUE)


##histogram

hist(TotalSteps,breaks=10,xlab='Total Steps Taken Each Day',ylab='Count',main='Histogram of Total Steps Taken Each Day')


##mean
mean(TotalSteps)


##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

interval<- aggregate(steps~interval, data,mean,na.rm=TRUE)

plot(interval$interval,interval$steps,type='l',xlab='5-min interval',ylab='Average number of steps',main='Average steps taken in 5-min interval across all days')


##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
interval[which.max(interval$steps),]


##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
totalNA<-sum(is.na(data$steps))

##Devise a strategy for filling in all of the missing values in the dataset: mean for that 5-min interval
NoNa<-data
for (i in 1:nrow(NoNa) ) {
    if (is.na(NoNa$steps[i]) ){
      int<-NoNa$interval[i]
      steps<-interval[interval$interval==int,]
      NoNa$steps[i]<-steps$steps
    }
}

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.



TotalSteps2<-tapply(NoNa$steps,data$date, sum)
hist(TotalSteps2,breaks=10,xlab='Total Steps Taken Each Day',ylab='Count',main='Histogram of Total Steps Taken Each Day')
mean(TotalSteps2)
median(TotalSteps2)


##Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
NoNa$weekendORweekday<-weekdays(as.Date(NoNa$date))
for (i in 1:nrow(NoNa)){
    if (NoNa$weekendORweekday[i] %in% c('Saturday','Sunday')) 
      {NoNa$weekendORweekday[i]<-'weekend'}
    else {NoNa$weekendORweekday[i]<-'weekday'}
}
NoNa$weekendORweekday<-as.factor(NoNa$weekendORweekday)


##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


interval2<- aggregate(steps~interval+weekendORweekday, NoNa,mean)
library(ggplot)

ggplot(interval2, 
       aes(interval, steps)) + 
       geom_line() + 
       facet_grid(weekendORweekday ~ .) + 
       xlab("interval") + 
       ylab("Number of steps")
       )









