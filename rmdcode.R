unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
dailysteps <- aggregate(activity$steps,list(activity$date),sum)
names(dailysteps) <- c("date","steps")
library(ggplot2)
qplot(dailysteps$date, dailysteps$steps) + geom_bar(stat = "identity") + labs(x = "Date", y = "Steps")
meansteps <- mean(dailysteps$steps, na.rm = TRUE)
mediansteps <- median(dailysteps$steps, na.rm = TRUE)

averagesteps <- aggregate(activity$steps,list(activity$interval), mean, na.rm=TRUE)
names(averagesteps) <- c("interval","meansteps")

qplot(averagesteps$interval, averagesteps$meansteps) + geom_line() + labs(x = "interval", y = "Average steps per day")

maxstepsinterval <- averagesteps$interval[averagesteps$meansteps == max(averagesteps$meansteps)]

activity_original <- activity
dailysteps_original <- dailysteps
missingsteps <- activity[!complete.cases(activity),]
NAsteps <- nrow(missingsteps)

idx <- match(activity$interval[!complete.cases(activity)],averagesteps$interval)
activity$steps[!complete.cases(activity)] <- averagesteps$meansteps[idx]

dailysteps <- aggregate(activity$steps,list(activity$date),sum)
names(dailysteps) <- c("date","steps")

qplot(dailysteps$steps)

meansteps_new <- mean(dailysteps$steps)
mediansteps_new <- median(dailysteps$steps)

meandiff <- meansteps - meansteps_new
mediandiff <- mediansteps - mediansteps_new

activity$dayofweek <- ifelse(weekdays(activity$date) %in% c("Saturday","Sunday"), "weekend","weekday")

dayofweeksteps <- aggregate(activity$steps,list(activity$dayofweek, activity$interval),mean)
names(dayofweeksteps) <- c("dayofweek","interval","meansteps")

ggplot(dayofweeksteps, aes(interval, meansteps)) + geom_line(color = "blue") + facet_grid(dayofweek ~ .) + theme_bw() + labs(x = "Interval", y= "Number of steps") + theme(panel.grid = element_blank())