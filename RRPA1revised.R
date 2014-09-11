# RRPA1revised.R
# RRPA1.R alternate version

# read the dataset
activ <- read.csv("activity.csv")
# process the data
stepvec <- activ[,1]
myindex <- seq(1,17568, 288) # creates index containing first row number for each day
daysums <- c() # sets up vector to contain amount of steps taken in each of 61 days 
for (i in 1:61){
        dbeg <- myindex[i]
        dend <- myindex[i]+287
        daysum <- sum(stepvec[dbeg:dend], na.rm=TRUE)
        daysums <- c(daysums, daysum)
}

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

intervalvec <- activ[,3]
