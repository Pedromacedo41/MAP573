
TS <- read.csv("/Users/arijoh/Documents/Skóli/École Polytechnique/Data Analysis and Unsupervised Learning /Project/MAP573/Data/Complete_TS.csv")
TST <- read.csv("/Users/arijoh/Documents/Skóli/École Polytechnique/Data Analysis and Unsupervised Learning /Project/MAP573/Data/TST.csv")



TST = subset(TST, select = -c(X, datetime))
TS = subset(TS, select = -c(X, datetime, V10))  #Remove Z10 which is V11 because it behaves strangely

TS_sum <- rowSums(TS[(1:dim(TS)[1]), ])
TST_mean <- rowMeans(TST[(1:dim(TS)[1]), ])

n <- 10000
TST_mean <- as.matrix(TST_mean[1:n])
TS_sum <- as.matrix(TS_sum[1:n])


counter <- 1
Imax <- 24*7
C <- vector(length = Imax+1)
for (i in (0:(length(C)-1))){
  
  TS_sum_lagged <- as.matrix(TS_sum[(i+1):length(TS_sum)])
  TST_mean_lagged <- as.matrix(TST_mean[1:(length(TST_mean)-i)])
  
  C[counter] <- cor(TST_mean_lagged, TS_sum_lagged)
  counter = counter + 1
}

plot(C, main = "Correlation betw. temperature and Load", ylav="Correlation", xlab = "Lag")
print("Most correlation is at lag:")
which(min(C)==C)-1 #because index 1 is lag 0









