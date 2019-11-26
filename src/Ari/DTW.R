



TS <- read.csv("/Users/arijoh/Documents/Skóli/École Polytechnique/Data Analysis and Unsupervised Learning /Project/MAP573/Data/Complete_TS.csv")
TST <- read.csv("/Users/arijoh/Documents/Skóli/École Polytechnique/Data Analysis and Unsupervised Learning /Project/MAP573/Data/TST.csv")


row.names(TST) <- TST$datetime
TST = subset(TST, select = -c(X, datetime))

row.names(TS) <- TS$datetime
TS = subset(TS, select = -c(X, datetime, V10))  #Remove Z10 which is V11 because it behaves strangely

colnames(TS) <- c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L10", "L11", "L12", "L13", "L14", "L15", "L16", "L17", "L18", "L19", "L20")
colnames(TST) <- c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11")


TS_sum <- rowSums(TS[(1:dim(TS)[1]), ])
TST_mean <- rowMeans(TST[(1:dim(TS)[1]), ])
data <- as.data.frame(cbind(TST_mean, TS_sum))

plot(TST_mean, TS_sum, 
     main = "Relationship between energy load and temperature", 
     xlab = "Temperature [F]", 
     ylab = "Load [xW]", 
     cex = 0.1,
     panel.first = grid(nx = NULL, ny = NULL, col = "red", lty = "dotted"))



last <- 2000#10000 will try #39408 is the last matching times, the dtw function cannot handle such long vectors
TS <- TS[1:last,]
TST <- TST[1:last,]




dim(TS)
dim(TST)










#Normalize

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





#Now we can perform dtw
#do
library(dtw)
library(energy) #for dcor
dim(TS_standardized)
dim(TST_standardized)

D <- matrix(data=NA, nrow = dim(TS_standardized)[2], ncol = dim(TST_standardized)[2])
for(i in 1:dim(TS_standardized)[2]){ #The rows are i
  print(c("Outer loop:",i))
  for(j in 1:dim(TST_standardized)[2]){ #The columns are j
    print(c("Inner loop:",j))
    x <- TS_standardized[,i]
    y <- TST_standardized[,j]
    
    #temp <-  dtw(x,y, distance.only = T)
    D[i,j] <-dcor(x,y)#temp$distance
  }
}


dim(D)

#image(x=1:dim(D)[1], y=1:dim(D)[2], (exp(D)), xlab="Load Zones", ylab="Temperature Zones", main = "Distance correlation")

library(fields)
image.plot(x=1:dim(D)[1], y=1:dim(D)[2], (exp(D)), xlab="Load Zones", ylab="Temperature Zones", main = "Distance correlation")











#library(TSdist) # crash
#TSdist::CCorDistance(x,y)


#temp1 <- x-x[1]
#temp2 <- y-y[1]

#dtw(temp1, temp2)$distance

#TS_standardized <- abs(diff(TS_standardized))
#TST_standardized  <- abs(diff(TST_standardized))





TS <- read.csv("/Users/arijoh/Documents/Skóli/École Polytechnique/Data Analysis and Unsupervised Learning /Project/MAP573/Data/Complete_TS.csv")
TST <- read.csv("/Users/arijoh/Documents/Skóli/École Polytechnique/Data Analysis and Unsupervised Learning /Project/MAP573/Data/TST.csv")

row.names(TST) <- TST$datetime
TST = subset(TST, select = -c(X, datetime))

row.names(TS) <- TS$datetime
TS = subset(TS, select = -c(X, datetime, V10))  #Remove Z10 which is V11 because it behaves strangely

colnames(TS) <- c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L10", "L11", "L12", "L13", "L14", "L15", "L16", "L17", "L18", "L19", "L20")
colnames(TST) <- c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11")


#Zones 13-18 are the most dependent on temperature
##HERE we try to remove zones that do not seem to be temperature dependent.
TS = subset(TS, select = c("L13", "L14", "L15", "L16", "L17", "L18"))


TS_sum <- rowSums(TS[(1:dim(TS)[1]), ])
TST_mean <- rowMeans(TST[(1:dim(TS)[1]), ])
data <- as.data.frame(cbind(TST_mean, TS_sum))

plot(TST_mean, TS_sum, 
     main = "Relationship between energy load and temperature", 
     xlab = "Temperature [F]", 
     ylab = "Load [xW]", 
     cex = 0.1,
     panel.first = grid(nx = NULL, ny = NULL, col = "red", lty = "dotted"))





