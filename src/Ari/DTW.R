library(fields)
library(gplots)
#Load the data
dataLoad <- function(){
  TS <- read.csv("./../../Data/Complete_TS.csv")
  TST <- read.csv("./../../Data/TST.csv")
  row.names(TST) <- TST$datetime
  TST = subset(TST, select = -c(X, datetime))
  row.names(TS) <- TS$datetime
  TS = subset(TS, select = -c(X, datetime, V10))  #Remove Z10 which is V11 because it behaves strangely
  colnames(TS) <- c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L10", "L11", "L12", "L13", "L14", "L15", "L16", "L17", "L18", "L19", "L20")
  colnames(TST) <- c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11")
  return(list(TS, TST))
}
#Normalize the data
Normalize <- function(TS, TST){
  #library(energy) #for dcor
  TS_mean <- colMeans(TS) #Mean of each Zone for standardization
  TST_mean <- colMeans(TST)
  TS_std <- apply(TS, 2, sd)
  TST_std <-  apply(TST, 2, sd)
  
  TST_standardized <- matrix(data=NA, nrow = dim(TST)[1], ncol =  dim(TST)[2])
  for (i in 1:dim(TST)[1]){
    for (j in 1:dim(TST)[2]){
      TST_standardized[i,j] <- (TST[i,j]-TST_mean[j])/TST_std[j]
    }
  }
  
  TS_standardized <- matrix(data=NA, nrow = dim(TS)[1], ncol =  dim(TS)[2])
  for (i in 1:dim(TS)[1]){
    for (j in 1:dim(TS)[2]){
      TS_standardized[i,j] <- (TS[i,j]-TS_mean[j])/TS_std[j]
    }
  }
  return(list(TS_standardized, TST_standardized))
}
#A correlation loop. Finds correlation between two vectors
correlation <- function(TS, TST){
  D <- matrix(data=NA, nrow = dim(TS)[2], ncol = dim(TST)[2])
  for(i in 1:dim(TS)[2]){ #The rows are i
    for(j in 1:dim(TST)[2]){ #The columns are j
      x <- TS[,i]
      y <- TST[,j]
      
      #temp <-  cor(x,y)
      D[i,j] <-cor(x,y)#temp$distance
    }
  }
  return(D)
}
#This function creates a correlation plot with a image function.
correlationPlot <- function(s, last, TS, TST, title){
  
  TS <- TS[(s+1):(last+s),]
  TST <- TST[1:last,]
  
  #Normalize
  #temp <- Normalize(TS, TST)
  #TS <- as.matrix(temp[[1]])
  #TST <- as.matrix(temp[[2]])
  
  C <- correlation(TS, TST)
  colors <- colorpanel(100, "red","white", "blue")
  N<- length( colors)
  breaks <- seq(min(C), max(C),  length.out= N+1 )
  

  image.plot(x=1:dim(C)[1], y=1:dim(C)[2], ((C)), col = colors, breaks=breaks, 
             xlab="Load Zones", ylab="Temperature Zones", 
             main = title)

}



temp <- dataLoad()
TS <- temp[[1]]
TST <- temp[[2]]

s <- 0
last <- 15000 # 
L <- c(0, 6, 12, 18)

counter = 1
mustore <- vector(length = length(L))
titles <- c("Correlation with no lag", "Correlation with lag 6","Correlation with lag 12","Correlation with lag 18")
par(mfrow=c(2,2))
for (s in L){
  mu <- correlationPlot(s, last, TS, TST, titles[counter])
  counter = counter + 1 
}





