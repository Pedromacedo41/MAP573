library(fields)
library(gplots)
#Load and format data
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
#Correlation function
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

#temperature data
temp <- dataLoad()
TS <- as.matrix(temp[[1]])
TST <- as.matrix(temp[[2]])
TST <- TST[1:39412,] #remove last na values in TST



D <- matrix(data=NA, nrow = dim(TST)[2], ncol = dim(TST)[2])
for (i in 1:(dim(TST)[2])){
  for (j in 1:(dim(TST)[2])){
    temp1 <- TST[,i]
    temp2 <- TST[,j]
    D[i,j] <- cor(temp1, temp2)
  }
}

par(mfrow=c(1,2))
colors <- colorpanel(100, "white", "grey", "black")
N<- length(colors)
breaks <- (seq(0, max(D),  length.out= N+1))



image.plot(x=1:dim(D)[1], y=1:dim(D)[2], ((D)), col = colors, breaks=breaks, 
           xlab="Zones", ylab="Zones", 
           main = "Correlation of temperature stations")



#load data

temp <- dataLoad()
TS <- as.matrix(temp[[1]])
TST <- as.matrix(temp[[2]])

TS <- TS[1:39575,] #remove last na values in TST




D <- matrix(data=NA, nrow = dim(TS)[2], ncol = dim(TS)[2])

for (i in 1:(dim(TS)[2])){
  for (j in 1:(dim(TS)[2])){
    temp1 <- TS[,i]
    temp2 <- TS[,j]
    D[i,j] <- cor(temp1, temp2)
  }
}


colors <- colorpanel(100, "white","grey", "black")
N<- length(colors)
breaks <- (seq(0, max(D),  length.out= N+1))

image.plot(x=1:dim(D)[1], y=1:dim(D)[2], ((D)), col = colors, breaks=breaks, 
           xlab="Zones", ylab="Zones", 
           main = "Correlation of load stations")






