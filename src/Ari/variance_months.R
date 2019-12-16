library(zoo)
library(xts)
#-------------------------------------
#This code is not used
#-------------------------------------

#Load data and format it
TS <- read.csv("../Data/Complete_TS.csv")
TST <- read.csv("../Data/TST.csv")
row.names(TST) <- TST$datetime
TST = subset(TST, select = -c(X, datetime))
row.names(TS) <- TS$datetime
TS = subset(TS, select = -c(X, datetime))


TSmeans <- rowMeans(TS[1:29414, ])
TSTmeans <- rowMeans(TST[1:29414, ])
mean_temp <- mean(TSTmeans)
data <- as.data.frame(cbind(TSTmeans, TSmeans))



times <- as.POSIXct(row.names(data), format = "%Y-%m-%d %H:%M:")
timesNAomit <- na.omit(times) 
load.means.zoo <- zoo(
  x         = data$TSmeans,
  order.by  = timesNAomit,
  frequency = 24
)




N_obs <- 31*24*5
D <- matrix(data=NA,nrow=N_obs,ncol=12)
colnames(D) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")



years = c("2004-", "2005-", "2006-", "2007-", "2008-", "2009-") #2009 ekki í datasetti
months = c("01-", "02-", "03-", "04-", "05-", "06-", "07-", "08-", "09-", "10-", "11-", "12-", "01-")



MaxIndex <- 1

for (y in 1:5){ 
  year <- years[y]
  for (m in 1:12){
    MaxIndex_month <- 1
    month <- months[m]
    nextmonth <- months[m+1]
    
    if (m == 12)
    {
      nextyear <- years[y+1]
      temp_start <-  paste(c(year, month, "01"), collapse = "")
      temp_end  <-  paste(c(nextyear, nextmonth, "01"), collapse = "")
    }
    else{
      temp_start <-  paste(c(year, month, "01"), collapse = "")
      temp_end  <-  paste(c(year, nextmonth, "01"), collapse = "")
    }
    
    sDay <- temp_start
    eDay <-temp_end
    
    monthData <- coredata(window(load.means.zoo, start = as.POSIXct(sDay), end = as.POSIXct(eDay)))
    lengthMonth <- length(monthData)

    D[MaxIndex:(MaxIndex+lengthMonth-1),m] <-monthData[1:lengthMonth]
    
    if (lengthMonth > MaxIndex_month) {MaxIndex_month <- lengthMonth}
  }
  MaxIndex <- MaxIndex_month + MaxIndex
}


#Now boxplot with D
boxplot(D,
        main = "Average energy load over the zones vs months",
        na.action = NULL,
        xlab = "Months",
        ylab = "Energy load",
        col = "orange",
        border = "black")
      

library(Rfast)
mins <- colMins(D)
maxs <- colMaxs(D)
minmaxData <- cbind(mins,maxs)
minmaxData
row.names(minmaxData) <- colnames(D)
barplot(t(minmaxData), col=c("Blue","Red"), legend = c("Min load", "Max load"),
        main = "Min-max load per month",
        ylab = "Energy load")


        
  


