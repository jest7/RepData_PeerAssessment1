library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')

### Confirm/view working directory ###
getwd()

### Assign working directory
setwd(readClipboard())

## Read Data in
data <- read.csv("activity.csv")

## Sample data
head(data)



## What is mean total number of steps taken per day?
## For this part of the assignment, you can ignore the missing values in the dataset.
## Calculate the total number of steps taken per day
## If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
## Calculate and report the mean and median of the total number of steps taken per day

## Load ggplot2 library
library(ggplot2)


## Create table of aggregates by day
TSteps <- aggregate(data$steps, by=list(data$date), FUN = sum)

## Remove NA records
TSteps <- subset(TSteps, !(is.na(x)))

## View output
TSteps 

## Output total steps taken per day
qplot(TSteps$x, xlab = "Total Steps Taken per Day", binwidth = 1000)

## Produce metrics
StepMets <- c(mean(TSteps$x, na.rm = TRUE), median(TSteps$x, na.rm = TRUE))

## Output metrics
StepMets


## What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


StepsTaken <- aggregate(data$steps, by = list(interval = data$interval), FUN=mean, na.rm=TRUE)

StepsTaken

## Plot lines
ggplot(StepsTaken, aes(x=interval, y=x)) +
  geom_line(color="red", size=1) 

## Maximum 5 min interval with most steps
StepsTaken[which.max(StepsTaken$x),]
##835




## Imputing missing values

##  Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
##  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
Missing <- sum(is.na(data$steps))

## Output missing
Missing
## 2304

##  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##  Create a new dataset that is equal to the original dataset but with the missing data filled in.

##  Create new data set for the nas
nadata <- subset(data, is.na(data$steps))
## Create data set for non na
nonnadata <- subset(data, is.na(data$steps)==0)

## Join with steps taken to get imputed steps
na_impute <- merge(nadata, StepsTaken,by="interval")

## Rename columns
names(na_impute)[2] <- "oldsteps"
names(na_impute)[4] <- "steps"

## combine data sets
data2 <- rbind.fill(nonnadata, na_impute)


head(data2)

##  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

StepsTaken2 <- aggregate(steps ~ date, data2, sum)

head(StepsTaken2)

##plotting the histogram
ggplot(StepsTaken2, aes(x = steps)) + 
  geom_histogram(fill = "red", binwidth = 1000) + 
  labs(title="Steps Taken per Day", 
       x = "Steps per Day", y = "Frequency")


## Produce metrics
StepMets2 <- c(mean(StepsTaken2$steps, na.rm = TRUE), median(StepsTaken2$steps, na.rm = TRUE))

## Output metrics
StepMets2

## Compared to StepMets
StepMets

## Slightly different


##  Are there differences in activity patterns between weekdays and weekends?
##  For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
##  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
##  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
data3 <- data2

data3$day <- ifelse(weekdays(as.Date(data3$date)) %in%  c("Saturday", "Sunday"),'weekend','weekday')


##  Weekends only
weekends <- data3[which(data3$day=='weekend'), ]

##  Weekdays only
weekdays <- data3[which(data3$day=='weekday'), ]

head(weekdays)


## Plot weekends
ggplot(weekends, aes(x=interval, y=steps)) + 
  geom_line(color="red") +
  labs(x="Interval", y="steps")

## Plot weekdays
ggplot(weekdays, aes(x=interval, y=steps)) + 
  geom_line(color="blue") +
  labs(x="Interval", y="steps")


