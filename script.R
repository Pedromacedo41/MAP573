library(shiny)
library(dygraphs)
library(data.table)
TS= fread("./Data/Complete_TS.csv", sep=",")
load_table= data.frame(TS)

View(load_table$V2)
h <-as.numeric(load_table$V2)
View(h)
g <- (h-min(h))/(max(h)-min(h))
View(g)

reshape_X_3d <- function(X) {
  print(dim(X))
  dim(X) <- c(dim(X)[1],1, dim(X)[2])
  X
}
g <- reshape_X_3d(g)


build_matrix <- function(tseries, overall_timesteps) {
  
  t(sapply(1:(length(tseries) - overall_timesteps + 1), 
           function(x) tseries[x:(x + overall_timesteps - 1)]))
}

window <-100
wi <- g[30000:30099]


as <-c()
for(i in 1:300){
  test_matrix <- build_matrix(wi, window)
  data <- model  %>% predict(reshape_X_3d(test_matrix))
  as <- c(as,data[1,1])
  wi[1: (length(wi)-1)] <- wi[2:length(wi)]
  wi[length(wi)] =data[1,1]
}

View(as*(max(h)-min(h))+min((h)))
write.csv(as*(max(h)-min(h))+min((h)), file = "./Data/MyData2.csv")

View(reshape_X_3d(test_matrix))

reshape_X_3d(test_matrix)

library(keras)
model <- load_model_hdf5("./Data/Models/lstm_1h_window100.h5")

View(data)
View(data[,1]*(max(h)-min(h))+min((h)))
write.csv(data[,1]*(max(h)-min(h))+min((h)), file = "./Data/MyData.csv")

xd5= t(fread("./Data/MyData.csv", sep=","))
xd5= data.frame(t(xd5))
View(xd5$x)




