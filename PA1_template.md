# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activ <- read.csv("~/activity.csv")
intervalID <- rep(1:288,61) 
dateID <- rep(1:61, each = 288)
activ <- cbind(activ, dateID, intervalID)
```


## What is mean total number of steps taken per day?

```r
daysums <- c()
for (i in 1:61){
        value <- sum(activ[activ$dateID == i,1], na.rm=TRUE)
        daysums <- c(daysums, value)
}

par(mar = c(4,4,2,2)) 

hist (daysums, breaks =8, col ="steelblue", 
      main = "Histogram of Total Steps Taken per Day",
      xlab = "Total Steps per Day", ylab = "Frequency in Days",
      xlim = c(0, 25000))
```

![plot of chunk total_steps](./PA1_template_files/figure-html/total_steps.png) 

```r
mymean <- round(mean(daysums), 2)
mymedian <- median(daysums)
meanpaste <- paste ("Mean total amount of steps per day is", mymean)
medianpaste <- paste ("Median total amount of steps per day is", mymedian)
print (meanpaste)
```

```
## [1] "Mean total amount of steps per day is 9354.23"
```

```r
print (medianpaste)
```

```
## [1] "Median total amount of steps per day is 10395"
```


## What is the average daily activity pattern?

```r
intervalmeans <- c()
for (i in 1:288){
        value2 <- mean(activ[activ$intervalID == i,1], na.rm=TRUE)
        intervalmeans <- c(intervalmeans,value2)
}
plot(intervalmeans, type="l", col="palegreen2",
     lwd = 3,
     main = "Average Steps Taken by Interval",
     xlab = "Interval ID (0='0:00', 288='23:55')", 
     ylab = "Average number of steps",
     xlim = c(0,288))
```

![plot of chunk average_daily](./PA1_template_files/figure-html/average_daily.png) 

```r
maxinterval <- which(intervalmeans == max(intervalmeans))
maxintvalue <- activ[maxinterval,3]
maxpaste <- paste ("The interval that, on average, contains the largest amount of steps is ", 
                maxintvalue, ". It is represented as IntervalID", maxinterval, 
                "on the attached plot.")
print(maxpaste)
```

```
## [1] "The interval that, on average, contains the largest amount of steps is  835 . It is represented as IntervalID 104 on the attached plot."
```
## Imputing missing values
### 1. Calculate and report total number of missing values

```r
missing <- which(is.na(activ$steps)) # gives rows in which steps measurements are NA
totalmiss <- length(missing)
missingpaste <- paste ("There are", totalmiss, "missing values.")
print (missingpaste)
```

```
## [1] "There are 2304 missing values."
```

### 2. Devise a strategy for filling in missing values

I opt for the mean of the interval in question.  I cannot rely on measures
for the DAY in question as the missing values tend to be a result of
entire days of missing data.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

for every row in "missing":
identify the interval containing the missing data, using intervalID
get the value of intervalmeans[that particular intervalID]
replace the NA for that row with the obtained value

```r
activ2 <- activ
for (i in 1:length(missing)) {
        val1 <- missing[i] # get the row number
        val2 <- activ2[val1, "intervalID"] # identify the interval
        val3 <- intervalmeans[val2] # get the mean for that interval
        activ2[val1, "steps"] <- round(val3, 1) # change the NA value to the mean taken above
}
```

### 4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median number of steps taken each day.

```r
daysums2 <- c()
for (i in 1:61){
        value2 <- sum(activ2[activ2$dateID == i,1], na.rm=TRUE)
        daysums2 <- c(daysums2, value2)
}
 
hist (daysums2, breaks =8, col ="dodgerblue2", 
      main = "Histogram of Total Steps Taken per Day (NAs Imputed)",
      xlab = "Total Steps per Day", ylab = "Frequency in Days",
      xlim = c(0, 25000), ylim = c(0,25))
```

![plot of chunk imputed_totalsteps](./PA1_template_files/figure-html/imputed_totalsteps.png) 

```r
mymean2 <- round(mean(daysums), 2)
mymedian2 <- median(daysums)

meanpaste2 <- paste ("With NAs imputed, mean total amount of steps per day is", mymean2)
medianpaste2 <- paste ("With NAs imputed, median total amount of steps per day is", mymedian2)

print (meanpaste2)
```

```
## [1] "With NAs imputed, mean total amount of steps per day is 9354.23"
```

```r
print (medianpaste2)
```

```
## [1] "With NAs imputed, median total amount of steps per day is 10395"
```


## Are there differences in activity patterns between weekdays and weekends?
### 1. Create a new factor variable with levels "weekday" and "weekend" indicating status of any given day.

```r
myweek <- rep("Weekday", 1440)
myweekend <- rep("Weekend", 576)
wholeweek <- c(myweek, myweekend)
daycategory <- factor(c(rep(wholeweek,8), myweek))

activ2 <- cbind(activ2,  daycategory)
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
activ3 <- activ2[activ2$dateID == 1|activ2$dateID == 41,] # data frame with 1 wkdy, 1 wknd, imputed mean values

library(lattice)
mypanel <- function(x,y){
        panel.xyplot(x,y,pch=19, type="l")
        }
xyplot(steps~intervalID|daycategory, data = activ3,
       layout = c(1,2),
       main="Activity Comparison: Weekday vs. Weekend",
       xlab="Interval ID",
       ylab="Average Steps",
       panel = mypanel)
```

![plot of chunk timeseries_panel](./PA1_template_files/figure-html/timeseries_panel.png) 


