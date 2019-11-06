# script
TimeSeries  <- function(zid, Load_his) {
  datetime<-c()
  value<-c()
  for(i in 1:length(Load_his$zone_id)){
    if(as.numeric(Load_his[i, 1])==zid){
      for(j in 1:24){
        dtime=paste(toString(Load_his$year[i]), "-", toString(Load_his$month[i]),"-", toString(Load_his$day[i]), " ",j, ":00:00", sep="")
        datetime<-rbind(datetime, dtime)
        if(Load_his[i, j+4]=="" || is.na(Load_his[i, j+4])){
          # prevent plot bugs such string values interpreted as categorical varibles
          value <-rbind(value,NA)
        }else{
          value <-rbind(value,as.numeric(sub(",", ".", Load_his[i, j+4], fixed = TRUE)))
        }
      }
    }
  }
  return(data.frame("datetime"= datetime, "1"= value))
}

# Load data
library(data.table)
Load_his= fread("Francisco/completeLoad.csv", sep=",")
Load_his= data.frame(Load_his)

gen= TimeSeries(1, Load_his)

for(i in 2:20){
  print(i)
  temp = TimeSeries(i, Load_his)
  gen[, i+1]= temp[,2]
}

names(gen)[2] <- "V2"
write.csv(gen, paste("Data/Complete_TS", ".csv", sep=""))








