# RRPA1revised.R
# RRPA1.R alternate version

#############################################################
################   CODE TO READ DATA   ######################
#############################################################
activ <- read.csv("activity.csv")

#############################################################
################   PROCESSING CODE   ########################
#############################################################
intervalID <- rep(1:288,61) 
dateID <- rep(1:61, each = 288)
activ <- cbind(activ, dateID, intervalID)

#############################################################
################   ANALYSIS CODE     ########################
#############################################################

daysums <- c()
for (i in 1:61){
        value <- sum(activ[activ$dateID == i,1], na.rm=TRUE)
        daysums <- c(daysums, value)
}
intervalmeans <- c()
for (i in 1:288){
        value2 <- mean(activ[activ$intervalID == i,1], na.rm=TRUE)
        intervalmeans <- c(intervalmeans,value2)
}

#############################################################
################   PLOTTING CODE   ##########################
#############################################################
# create histogram of total steps per day
par(mar = c(4,4,3,1)) 
hist (daysums, breaks =8, col ="steelblue", 
      main = "Histogram of Total Steps Taken per Day",
      xlab = "Total Steps per Day", ylab = "Frequency in Days",
      xlim = c(0, 25000))
mymean <- round(mean(daysums), 2)
mymedian <- median(daysums)
meanpaste <- paste ("Mean total amount of steps per day is", mymean)
medianpaste <- paste ("Median total amount of steps per day is", mymedian)
print (meanpaste)
print (medianpaste)

plot(intervalmeans, type="l", col="darkcyan",
     main = "Average Steps Taken, Intervals 1-288",
     xlab = "Interval ID", 
     ylab = "Average number of steps",
     xlim = c(0,288))

maxinterval <- which(intervalmeans == max(intervalmeans))
maxintvalue <- activ[maxinterval,3]
maxpaste <- paste ("The interval that, on average, contains the largest amount of steps is ", 
                maxintvalue, ". It is represented as IntervalID", maxinterval, 
                "on the attached plot.")
print(maxpaste)

###########################################################
############   IMPUTATION STRATEGY   ######################
###########################################################

# 1. calculate and report total number of missing values

missing <- which(is.na(activ$steps)) # gives rows in which steps measurements are NA
totalmiss <- length(missing)
missingpaste <- paste ("There are", totalmiss, "missing values.")
print (missingpaste)

# 2. Devise a strategy for filling in missing values

#       I opt for the mean of the interval in question.  I cannot rely on measures
#       for the DAY in question as the missing values tend to be a result of
#       entire days of missing data.

# 3. Create a new dataset that is equal to the original dataset
#    but with the missing data filled in.

#       for every row in "missing":
#               identify the interval containing the missing data, using intervalID
#               get the value of intervalmeans[that particular intervalID]
#               replace the NA for that row with the obtained value

activ2 <- activ
for (i in 1:length(missing)) {
        val1 <- missing[i] # get the row number
        val2 <- activ2[val1, "intervalID"] # identify the interval
        val3 <- intervalmeans[val2] # get the mean for that interval
        activ2[val1, "steps"] <- val3 # change the NA value to the mean taken above
}

# 4. Make a histogram of the total number of steps taken each day and 
# calculate and report the mean and median number of steps taken each day.

daysums2 <- c()
for (i in 1:61){
        value2 <- sum(activ2[activ2$dateID == i,1], na.rm=TRUE)
        daysums2 <- c(daysums2, value2)
}
 
hist (daysums2, breaks =8, col ="green", 
      main = "Histogram of Total Steps Taken per Day (NAs Imputed)",
      xlab = "Total Steps per Day", ylab = "Frequency in Days",
      xlim = c(0, 25000))
mymean2 <- round(mean(daysums), 2)
mymedian2 <- median(daysums)
meanpaste2 <- paste ("With NAs imputed, mean total amount of steps per day is", mymean2)
medianpaste2 <- paste ("With NAs imputed, median total amount of steps per day is", mymedian2)
print (meanpaste2)
print (medianpaste2)



