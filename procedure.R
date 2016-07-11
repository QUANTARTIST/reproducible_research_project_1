## Question 1:
## Load the dataset

data <- read.csv("activity.csv")

## Question 2:
## What is the total number of steps taken per day? This code will sum steps by day and create a histogram

steps_per_day <- aggregate(steps ~ date, data, sum)
hist(steps_per_day$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps")

## Question 3:
## This code will calculate the mean and median number of steps per day

mean <- mean(steps_per_day$steps)
median <- median(steps_per_day$steps)


## Question 4:
## Time series plot of the average number of steps taken. First, exclude NA values

clean <- data[!is.na(data$steps),]


## Question 5:
## Construct average number of steps per interval

intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))

## Construct line plot of average number of steps per interval

p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

## The 5-minute interval that, on average, contains the maximum number of steps
## Maximum steps by interval

maxSteps <- max(intervalTable$Avg)

## Which interval contains the maximum average number of steps?

intervalTable[intervalTable$Avg==maxSteps,1]

## Question 6:
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
## Number of NAs in original data set

nrow(data[is.na(data$steps),])

## This strategy for filling in NAs substitutes the missing steps with the average 5-minute interval based on the day of the week
## Create the average number of steps per weekday and interval

avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))

## Form a dataset with all NAs for substitution

nadata<- data[is.na(data$steps),]

## Combine NA data with average weekday interval for substitution

newdata<-merge(nadata, avgTable, by=c("interval", "day"))

## Create a new dataset that is equal to the original dataset but with the missing data populated
## Reorder the new substituded data in the same format as clean data set

nd<- nd[,c(6,4,1,2,5)]
colnames(nd)<- c("steps", "date", "interval", "day", "DateTime")

## Merge the NA averages and non NA data together

mergeData <- rbind(clean, nd)

## Question 7
## Form a sum of steps per date to compare with step 1

sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(sumTable2)<- c("Date", "Steps")

## Mean of Steps with NA data taken care of
as.integer(mean(sumTable2$Steps))

## Median of Steps with NA data taken care of
as.integer(median(sumTable2$Steps))

## Build the histogram of total steps per day, categorized by data set to show impact
  hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
  hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
  legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
  
## The new mean of the imputed data is 10821 steps compared to the old mean of 10766 steps. This equates a difference of 55 steps on average per day. The new median of the imputed data is 11015 steps compared to the old median of 10765 steps. This leads to a difference of 250 steps for the median. The overall shape of the distribution is consistent.
  
## Question 8
  
## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
  
## Create new category based on the days of the week
  
  mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  
## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
  
  library(lattice) 
  
## Summarize data by interval and type of day
  
  intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))
  
## Plot data in a panel plot 

xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
         main="Average Steps per Interval Based on Type of Day", 
         ylab="Average Number of Steps", xlab="Interval")


