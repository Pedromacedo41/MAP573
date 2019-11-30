
TS <- read.csv("./../../Data/Complete_TS.csv")
TST <- read.csv("./../../Data/TST.csv")



TST = subset(TST, select = -c(X, datetime))
TS = subset(TS, select = -c(X, datetime, V10))  #Remove Z10 which is V11 because it behaves strangely

TS_sum <- rowSums(TS[(1:dim(TS)[1]), ])
TST_mean <- rowMeans(TST[(1:dim(TS)[1]), ])

n <- 15000
TST_mean <- as.matrix(TST_mean[1:n])
TS_sum <- as.matrix(TS_sum[1:n])


counter <- 1
Imax <- 24*2
C <- matrix(data=NA, nrow = Imax+1, ncol = 2)
for (i in (0:(length(C)-1))){
  
  TS_sum_lagged <- as.matrix(TS_sum[(i+1):length(TS_sum)])
  TST_mean_lagged <- as.matrix(TST_mean[1:(length(TST_mean)-i)])
  
  C[counter,1] <- i
  C[counter,2] <- cor(TST_mean_lagged, TS_sum_lagged)
  counter = counter + 1
}

C <- as.data.frame(C)
colnames(C) <- c("Lag", "Correlation")
library(ggplot2)
ggplot(C, aes(x=Lag, y=Correlation)) +
      theme_bw() + 
      geom_point() +
      labs(title="Correlation betw. temperature and load")








