library(ggplot2)

activity <- read.csv("./activity.csv")

activity$date <- as.Date(activity$date, "%Y%m%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity, weekday)
activity_nona <- subset(activity, !is.na(activity$steps))

StepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(StepsPerDay) <- c("Date", "Steps")
StepsPerDay

g <- ggplot(StepsPerDay, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkgreen", fill="lightgreen")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,18,2))

mean(StepsPerDay$Steps, na.rm=TRUE)

median(StepsPerDay$Steps, na.rm=TRUE)

StepsPerTime <- aggregate(steps~interval,
                          data=activity,
                          FUN=mean,
                          na.action=na.omit)
StepsPerTime$time <- StepsPerTime$interval/100
h <- ggplot(StepsPerTime, aes(time, steps))
h+geom_line(col="brown")
+ggtitle("Average steps per time interval")
+xlab("Time")+ylab("Steps")
+theme(plot.title = element_text(face="bold", size=12))

ST <- tibble::as_tibble(StepsPerTime)
ST %>% select(time, steps) %>% filter(steps==max(ST$steps))

ACT <- tibble::as_tibble(activity)
ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())

activity$Complete <- ifelse(is.na(activity$steps),round(StepsPerTime$steps[match(activity$interval,StepsPerTime$interval)],0), activity$steps)

activityFull <- data.frame(steps=activity$Complete, 
                           interval=activity$interval, 
                           date=activity$date)

StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")

g <- ggplot(StepsPerDayFull, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkblue", fill="lightblue")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))

mean(StepsPerDayFull$Steps)

median(StepsPerDayFull$Steps)

activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
activityFull$weekday <- weekdays(activityFull$RealDate)
activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')

StepsPerTimeDT <- aggregate(steps~interval+DayType,
                            data=activityFull,
                            FUN=mean,
                            na.action=na.omit)
StepsPerTimeDT$time <- StepsPerTime$interval/100
j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="darkred")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)