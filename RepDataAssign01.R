filename <- "./data/activity.csv"
mydata <- read.csv(filename, colClasses = c("integer", "Date", "factor"))
mydata$month <- as.numeric(format(mydata$date, "%m"))
nonNAdata <- na.omit(mydata)
rownames(nonNAdata) <- 1:nrow(nonNAdata)
dim(mydata) # [1] 17568     4
dim(nonNAdata) # [1] 15264     4
ggplot(nonNAdata, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
totalSteps <- aggregate(nonNAdata$steps, list(Date = nonNAdata$date), FUN = "sum")$x
mean(totalSteps)
median(totalSteps)
avgSteps <- aggregate(nonNAdata$steps, list(interval = as.numeric(as.character(nonNAdata$interval))), FUN = "mean")
names(avgSteps)[2] <- "meanOfSteps"
ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
sum(is.na(mydata))
newData <- mydata 
for (i in 1:nrow(newData)) {
+     if (is.na(newData$steps[i])) {
+         newData$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
+     }
+ }
head(newData)
sum(is.na(newData))
ggplot(newData, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "steelblue",
                                             fill = "steelblue",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
newTotalSteps <- aggregate(newData$steps, 
                           list(Date = newData$date), 
                           FUN = "sum")$x
newMean <- mean(newTotalSteps)
newMean
newMedian <- median(newTotalSteps)
newMedian
oldMean <- mean(totalSteps)
oldMedian <- median(totalSteps)
newMean - oldMean
newMedian - oldMedian
head(newData)
newData$weekdays <- factor(format(newData$date, "%A"))
levels(newData$weekdays)
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(newData$weekdays)
table(newData$weekdays)
avgSteps <- aggregate(newData$steps, 
                      list(interval = as.numeric(as.character(newData$interval)), 
                           weekdays = newData$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
