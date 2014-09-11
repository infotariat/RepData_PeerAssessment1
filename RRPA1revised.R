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


