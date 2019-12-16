library(shiny)
library(dygraphs)
library(data.table)
library(TSA)
library(keras)
library(fields)
library(forecast)
library(gplots)
library(gridExtra)

model1 <- load_model_hdf5("./Data/Models/lstm_672_168.h5")
model2 <- load_model_hdf5("./Data/Models/lstm_100_1.h5")
model3 <- load_model_hdf5("./Data/Models/lstm_100_24.h5")
model4 <- load_model_hdf5("./Data/Models/lstm_1000_168.h5")


TS= fread("./Data/Complete_TS.csv", sep=",")
load_table= data.frame(TS)

TST= fread("./Data/TST.csv", sep=",")
temp_table= data.frame(TST)

xd1= t(fread("./Data/PredictHyper/bidirectionallstm_predictor_168h_window672_37708_38212.csv", sep=","))
xd2=t(fread("./Data/PredictHyper/lstm_predictor_1h_window100_0_39575.csv", sep=","))
xd3= t(fread("./Data/PredictHyper/lstm_predictor_24h_window100_32660_32784.csv", sep=","))
xd4= t(fread("./Data/PredictHyper/lstm_predictor_168h_window1000_32160_33328.csv", sep=","))
xd1= data.frame(xd1)
xd2= data.frame(xd2)
xd3= data.frame(xd3)
xd4= data.frame(xd4)

library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(sqldf)

load_table$datetime <- ymd_hms(load_table$datetime)
temp_table$datetime <- ymd_hms(temp_table$datetime)

predictions1 <- xts(x = as.numeric(xd1$xd1), order.by = load_table$datetime[37709:38212])
predictions2 <- xts(x = as.numeric(xd2$xd2), order.by = load_table$datetime[1:39576])
predictions3 <- xts(x = as.numeric(xd3$xd3), order.by = load_table$datetime[32661:32784])
predictions4 <- xts(x = as.numeric(xd4$xd4), order.by = load_table$datetime[32161:33328])

k= list()
new_data = c()

a=list()
for(i in 1:21){
  k <- c(k, paste("Zone", i, " Load"))
  a <- c(a, paste("Zone", i, " Load"))
}

l= list()
for(i in 1:11){
  l <- c(l, paste("St.", i, " Temp."))
  a <- c(a, paste("St.", i, " Temp."))
}

a=c(k,l)

d= list("Forecast Bidirectional LSTM 168h window 672", "Forecast LSTM 1h window 100", "Forecast LSTM 24h window 100", "Forecast LSTM 168h window 1000")

Table_period  <- function(tb_init, period) {
  i <-1
  period
  gd=c()
  tb=c(tb_init[1,1])
  while(TRUE){
    if((i+period)>=length(tb_init[,1])){
      tb <- c(tb,tb_init[i,1])
      k <- c(tb_init[i:(i+length(tb_init[,1])%%period),2])
      k <- c(k,rep(NA,times=c(period-length(k))))
      gd <- rbind(gd,k)
      break;
    }else{
      tb <- c(tb,tb_init[i,1])
      gd <- rbind(gd,tb_init[i:(i+period-1),2])
      i=i+period
    }
  }
  #print(tb)
  return(data.frame("datetime"= tb[2:length(tb)], gd))
}

reshape_X_3d <- function(X) {
  print(dim(X))
  dim(X) <- c(dim(X)[1],1, dim(X)[2])
  X
}

build_matrix <- function(tseries, overall_timesteps) {
  
  t(sapply(1:(length(tseries) - overall_timesteps + 1), 
           function(x) tseries[x:(x + overall_timesteps - 1)]))
}

dataLoad <- function(){
  TS <- read.csv("./Data/Complete_TS.csv")
  TST <- read.csv("./Data/TST.csv")
  row.names(TST) <- TST$datetime
  row.names(TS) <- TS$datetime
  TST = TST[,2:length(TST)]
  TS = TS[,2:length(TS)]
  TS = subset(TS, select = -c(V10))  #Remove Z10 which is V11 because it behaves strangely
  colnames(TS) <- c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L10", "L11", "L12", "L13", "L14", "L15", "L16", "L17", "L18", "L19", "L20")
  colnames(TST) <- c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11")
  return(list(TS, TST))
}


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

correlationPlot <- function(s, last, TS, TST, title){
  
  TS <- TS[(s+1):(last+s),]
  TST <- TST[1:last,]
  
  C <- correlation(TS, TST)
  colors <- colorpanel(100, "red","white", "blue")
  N<- length( colors)
  breaks <- seq(min(C), max(C),  length.out= N+1 )
  
  
  image.plot(x=1:dim(C)[1], y=1:dim(C)[2], ((C)), col = colors, breaks=breaks, 
             xlab="Load Zones", ylab="Temperature Zones", 
             main = title)
  
}

ui <- navbarPage("Energy Forecast Data Visualisation",
                 tabPanel("Time Series",
                          sidebarPanel(width=3,
                                       checkboxGroupInput("checkGroup",strong("Time Serie Select"),choices = a,selected = c(a[1]), inline = TRUE),
                                       checkboxGroupInput("checkGroup2",strong("Forecast Data"),choices = d,selected = c(d[2]), inline = TRUE),
                                       selectInput("selectt", strong("Make Forecast"),choices =c(d, "Naive Predictor"),selected = 2),
                                       selectInput("selectg", strong("Zone"),choices =k,selected = 1),
                                       h6("Obs: Models trained for zone 1"),
                                       checkboxInput("checkbo", "Blind Forecast (Only for LSTM predicting 1h ahead. Slow option)", value = FALSE),
                                       actionButton("action", "Generate Data")
                                       
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Plot", dygraphOutput("dygraph")),
                              tabPanel("Summary Plot", verbatimTextOutput("summary")),
                              tabPanel("Summary Load", verbatimTextOutput("summary2")),
                              tabPanel("Summary Temp", verbatimTextOutput("summary3"))
                            )
                          )
                 ),
                 tabPanel("3D Time Series", 
                          sidebarPanel(
                            selectInput("selecto", strong("Zone"),choices =k,selected = 1),
                            selectInput("sselect2", strong("Stations"),choices =l,selected = 1),
                            textInput("text", h3("Periodicity"), value = "24"),
                            sliderInput("slider1", h6("Slider 1, Load Data"),
                                        min = 0, max = 90, value = 50),
                            sliderInput("sslider11", h6("Slider 2, Temp Data"),
                                        min = 0, max = 90, value = 50),
                            selectInput("ggselect", strong("Forecast Data"),choices =c(d,"New Data"),selected = 2),
                            checkboxInput("checkbox2", "Include Forecast Data", value = FALSE)
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Plot Load", plotOutput("dygraph3")),
                              tabPanel("Plot Temp", plotOutput("dygraph34"))
                            )
                          )
                 ),
                 tabPanel("TS Stats",
                          sidebarPanel(
                            selectInput("selec1", strong("Zone"),choices =k,selected = 1),
                            selectInput("selec2", strong("Stations"),choices =l,selected = 1),
                            selectInput("selec", strong("Predictions"),choices =d,selected = 2)
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Correlation Load x Temp", plotOutput("l6")),
                              tabPanel("ACF Load", plotOutput("acfl")),
                              tabPanel("ACF Temp", plotOutput("acft")),
                              tabPanel("ACF Pred", plotOutput("acfp")),
                              tabPanel("PACF Load", plotOutput("pacfl")),
                              tabPanel("PACF Temp", plotOutput("pacft")),
                              tabPanel("PACF Load", plotOutput("pacfp"))
                            )
                          )
                 ),
                 tabPanel("Overall Stats",
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Correlations", plotOutput("l3")),
                              tabPanel("Correlations with lag", plotOutput("l2"))
                            )
                          )
                 ),
                 tabPanel("Fourier Analysis",
                          sidebarPanel(
                            selectInput("select5", strong("Zone"),choices =k,selected = 1),
                            selectInput("select6", strong("Stations"),choices =l,selected = 1),
                            selectInput("select88", strong("Predictions"),choices =d,selected = 2)
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Plot Load", plotOutput("fload")),
                              tabPanel("Load Modes", verbatimTextOutput("summ")),
                              tabPanel("Plot Temp", plotOutput("ftemp")),
                              tabPanel("Temp Modes", verbatimTextOutput("summ2")),
                              tabPanel("Plot Predictions", plotOutput("fpred")),
                              tabPanel("Prediction Modes", verbatimTextOutput("summ3"))
                            )
                          )
                 ),
                 tabPanel("Statistical Analysis- Short Report",
                          mainPanel(
                            htmlOutput("inc")
                          )
                 )
) 

server <- function(input, output) {
  pred <- reactiveValues(data=c())
  output$dygraph <- renderDygraph({
    don <- pred$data
    for(i in (input$checkGroup)){
      for(j in 1:21){
        if(i==a[j]){
          #print(a[j])
          #print(j)
          temp  <- xts(x = load_table[,j+1], order.by = load_table$datetime)
          names(temp)<- a[j]
          don <- cbind(don,temp)
        }
      }
      for(j in 22:33){
        if(i==a[j]){
          temp  <- xts(x = temp_table[,j-20+2], order.by = temp_table$datetime)
          names(temp)<- a[j]
          don <- cbind(don,temp)
        }
      }
    }
  
    if(input$checkGroup2==d[1]){ don<- cbind(don,predictions1) }
    if(input$checkGroup2==d[2]){ don<- cbind(don,predictions2) }
    if(input$checkGroup2==d[3]){ don<- cbind(don,predictions3) }
    if(input$checkGroup2==d[4]){ don<- cbind(don,predictions4) }
 
    #don<- cbind(don, formula())
    
    dygraph(don) %>%
      dyOptions(drawGrid = TRUE) %>%
      dyRangeSelector(retainDateWindow = TRUE) %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
  })
  
  observeEvent(input$action, {
    n <-0
    for(j in 1:21){
      if(input$selectg==a[j]){
        n <-j
      }
    }
    query <- paste("select datetime,V", n+1," as value from load_table", sep="")
    ggg <- sqldf(query)
    J <- as.Date(input$dygraph_date_window[[1]])
    J2 <- as.Date(input$dygraph_date_window[[2]])
    table <- as.Date(ggg$datetime)
    a1 <- which(table==J)[1]
    a2 <- which(table==J2)[1]
    
    
    if(input$selectt==d[2]){
      window <- 100
      if(input$checkbo){
        h <-as.numeric(ggg[a1:(a1+window-1),2])
        g <- (h-min(h))/(max(h)-min(h))
        as <-c()
        for(i in 1:(a2-a1)){
          test_matrix <- build_matrix(g, window)
          data <- model2  %>% predict(reshape_X_3d(test_matrix))
          as <- c(as,data[1,1])
          g[1: (length(g)-1)] <- g[2:length(g)]
          g[length(g)] =data[1,1]
        }
        data <- (as*(max(h)-min(h))+min((h)))
        blind <- xts(x = as.numeric(data), order.by = load_table$datetime[(a1+window-1):(a2+window-2)])
        pred$data <- blind
      }else{
        h <-as.numeric(ggg[a1:a2,2])
        g <- (h-min(h))/(max(h)-min(h))
        test_matrix <- build_matrix(g, window)
        data <- model2  %>% predict(reshape_X_3d(test_matrix))
        data <- data[,1]*(max(h)-min(h))+min(h)
        fore<- xts(x = as.numeric(data), order.by = load_table$datetime[(a1+window-1):a2])
        pred$data <- fore
      }
    }
    
    if(input$selectt==d[3]){
      window <- 100
      h <-as.numeric(ggg[a1:(a1+window-1),2])
      g <- (h-min(h))/(max(h)-min(h))
      test_matrix <- build_matrix(g, window)
      data <- model3  %>% predict(reshape_X_3d(test_matrix))
      data <- t(data)*(max(h)-min(h))+min(h)
      fore<- xts(x = as.numeric(data), order.by = load_table$datetime[(a1+window):(a1+23+window)])
      pred$data <- fore
    }
    if(input$selectt==d[4]){
      window <- 1000
      h <-as.numeric(ggg[a1:(a1+window-1),2])
      g <- (h-min(h))/(max(h)-min(h))
      test_matrix <- build_matrix(g, window)
      data <- model4  %>% predict(reshape_X_3d(test_matrix))
      data <- t(data)*(max(h)-min(h))+min(h)
      fore<- xts(x = as.numeric(data), order.by = load_table$datetime[(a1+window):(a1+167+window)])
      pred$data <- fore
    }
    if(input$selectt==d[1]){
      window <- 672
      h <-as.numeric(ggg[a1:(a1+window-1),2])
      g <- (h-min(h))/(max(h)-min(h))
      test_matrix <- build_matrix(g, window)
      data <- model4  %>% predict(reshape_X_3d(test_matrix))
      data <- t(data)*(max(h)-min(h))+min(h)
      fore<- xts(x = as.numeric(data), order.by = load_table$datetime[(a1+window):(a1+167+window)])
      pred$data <- fore
    }
    if(input$selectt=="Naive Predictor"){
      h <- cbind(c(ggg[1,2]), t(c(ggg[,2])))
      h <- t(h)
      fore<- xts(x = as.numeric(h[a1:a2]), order.by = load_table$datetime[a1:a2])
      pred$data <- fore
    }
    
  })
  
  
  output$summary2 <-  renderPrint(summary(load_table))
  output$summary3 <-  renderPrint(summary(temp_table))
  output$summary <-  renderPrint({
    don <- c()
    for(i in (input$checkGroup)){
      for(j in 1:20){
        if(i==a[j]){
          #print(a[j])
          #print(j)
          temp  <- xts(x = load_table[,j+2], order.by = load_table$datetime)
          names(temp)<- a[j]
          don <- cbind(don,temp)
        }
      }
      for(j in 21:31){
        if(i==a[j]){
          temp  <- xts(x = temp_table[,j-20+2], order.by = temp_table$datetime)
          names(temp)<- a[j]
          don <- cbind(don,temp)
        }
      }
      
    }
    if(input$checkGroup2==d[1]){ don<- cbind(don,predictions1) }
    if(input$checkGroup2==d[2]){ don<- cbind(don,predictions2) }
    if(input$checkGroup2==d[3]){ don<- cbind(don,predictions3) }
    if(input$checkGroup2==d[4]){ don<- cbind(don,predictions4) }

    summary(don)
  })
  
  output$l1 <- renderPlot({
    n <-0
    for(j in 1:21){
      if(input$selec1==a[j]){
        n <-j
      }
    }
    query <- paste("select V", n+1," as value from load_table", sep="")
    ggg <- sqldf(query)
    
    n <-0
    for(j in 1:11){
      if(input$selec2==l[j]){
        n <-j
      }
    }
    query <- paste("select V", n+1," as value from temp_table", sep="")
    kkk <- sqldf(query)
    data <- as.data.frame(cbind(kkk, ggg$value[1:length(kkk$value)]))
    
    library(ggplot2)
    
    
    times <- as.POSIXct(row.names(data), format = "%Y-%m-%d %H:%M:")
    timesNAomit <- na.omit(times) #for NA values we omit 
    
    load_zoo_zones <- zoo( # all zones
      x         = TS,
      order.by  = timesNAomit,
      frequency = 24
    )
    
    load_zoo <- zoo( #sum of the zones
      x         = data$TS_sum,
      order.by  = timesNAomit,
      frequency = 24
    )
    
    temperature_zoo <- zoo(
      x         = data$TST_mean,
      order.by  = timesNAomit,
      frequency = 24
    )
    
    data_zoo <- cbind(load_zoo, temperature_zoo)
    
    years <- c("2004", "2005", "2006", "2007", "2008")
    year = years[1]
    temp_start <-  paste(c(year, "-01-", "01 01:00:00"), collapse = "")
    temp_end <-  paste(c(year, "-12-", "31 24:00:00"), collapse = "")
    as.POSIXct(temp_start)
    
    ystart <- which(as.POSIXct(temp_start) == index(load_zoo_zones))
    yend <- which(as.POSIXct(temp_end) == index(load_zoo_zones))-1 
    year_zoo_zones <- load_zoo_zones[ystart:yend]
    
    TS_year <- as.matrix(coredata(year_zoo_zones[1:dim(year_zoo_zones)[1],1:dim(year_zoo_zones)[2]]))
    
    
    
    
    size_year = c(0,0,0,0,0)
    for (i in (1:(length(years)))){
      year = years[i]
      temp_start <-  paste(c(year, "-01-", "01 01:00:00"), collapse = "")
      temp_end <-  paste(c(year, "-12-", "31 24:00:00"), collapse = "")
      
      if (i == 5){yend = length(load_zoo)} else {yend <- which(as.POSIXct(temp_end) == index(load_zoo))-1} 
      ystart <- which(as.POSIXct(temp_start) == index(load_zoo))
      
      size_year[i] <- length(load_zoo[ystart:yend])
    }
    
    N_obs <- sum(size_year)
    D <- matrix(data=NA,nrow=N_obs,ncol=12)
    colnames(D) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    
    
    years = c("2004-", "2005-", "2006-", "2007-", "2008-", "2009-") 
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
        
        monthData <- coredata(window(load_zoo, start = as.POSIXct(sDay), end = as.POSIXct(eDay)))
        lengthMonth <- length(monthData)
        
        D[MaxIndex:(MaxIndex+lengthMonth-1),m] <- monthData[1:lengthMonth]
        
        if (lengthMonth > MaxIndex_month) {MaxIndex_month <- lengthMonth}
      }
      MaxIndex <- MaxIndex_month + MaxIndex
    }
    
    boxplot(D,
            main = "Average energy load over the zones vs months",
            na.action = NULL,
            xlab = "Months",
            ylab = "Energy load",
            col = "orange",
            border = "black")
    
    
  })
  
  output$l2 <- renderPlot({
    temp <- dataLoad()
    TS <- temp[[1]]
    TST <- temp[[2]]
    
    s <- 0
    last <- 15000 
    L <- c(0, 6, 12, 18)
    
    counter = 1
    mustore <- vector(length = length(L))
    titles <- c("Correlation with no lag", "Correlation with lag 6","Correlation with lag 12","Correlation with lag 18")
    par(mfrow=c(2,2))
    for (s in L){
      mu <- correlationPlot(s, last, TS, TST, titles[counter])
      counter = counter + 1 
    }

  })
  
  output$l3 <- renderPlot({
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

    
  })
  
  
  output$l6 <- renderPlot({
    n <-0
    for(j in 1:21){
      if(input$selec1==a[j]){
        n <-j
      }
    }
    query <- paste("select V", n+1," as value from load_table", sep="")
    load <- sqldf(query)
    
    n <-0
    for(j in 1:11){
      if(input$selec2==l[j]){
        n <-j
      }
    }
    query <- paste("select V", n+1," as value from temp_table", sep="")
    temp <- sqldf(query)
    load <- load$value[1:length(temp$value)]
    plot(as.numeric(unlist(temp)), as.numeric(unlist(load)), 
         main = "Relationship between energy load and temperature", 
         xlab = "Temperature [F]", 
         ylab = "Load [GW]", 
         cex = 0.1,
         panel.first = grid(nx = NULL, ny = NULL, col = "red", lty = "dotted"))
    
  })
  

  
  output$acfl <- renderPlot({
    n <-0
    for(j in 1:21){
      if(input$selec1==a[j]){
        n <-j
      }
    }
    query <- paste("select V", n+1," as value from load_table", sep="")
    load <- sqldf(query)
    
    p1 <- ggAcf(load)
    p2 <- ggAcf(load, lag.max = 24*35)
    grid.arrange(p1, p2, nrow = 2)
  })
  
  output$acft <- renderPlot({
    n <-0
    for(j in 1:11){
      if(input$selec2==l[j]){
        n <-j
      }
    }
    query <- paste("select V", n+1," as value from temp_table", sep="")
    load <- sqldf(query)
    load <- na.omit(load)
    p1 <- ggAcf(load)
    p2 <- ggAcf(load, lag.max = 24*35)
    grid.arrange(p1, p2, nrow = 2)
  })
  
  output$acfp <- renderPlot({
    p1 <- c()
    if(input$selec==d[1]){ p1 <- predictions1 }
    if(input$selec==d[2]){ p1 <- predictions2 }
    if(input$selec==d[3]){ p1 <- predictions3 }
    if(input$selec==d[4]){ p1 <- predictions4 }
    load <- na.omit(p1)
    p1 <- ggAcf(load)
    p2 <- ggAcf(load, lag.max = 24*35)
    grid.arrange(p1, p2, nrow = 2)
  })
  
  output$pacfp <- renderPlot({
    p1 <- c()
    if(input$selec==d[1]){ p1 <- predictions1 }
    if(input$selec==d[2]){ p1 <- predictions2 }
    if(input$selec==d[3]){ p1 <- predictions3 }
    if(input$selec==d[4]){ p1 <- predictions4 }
    load <- na.omit(p1)
    
    p1 <- ggAcf(load, type = "partial")
    p2 <- ggAcf(load, type = "partial", lag.max = 24*7)
    p3 <- ggAcf(load, type = "partial", lag.max = 24*7*4)
    
    grid.arrange(p1, p2, p3, ncol = 2, layout_matrix = rbind(c(1,2), c(3,3)))
  })
  
  
  output$pacfl <- renderPlot({
    n <-0
    for(j in 1:21){
      if(input$selec1==a[j]){
        n <-j
      }
    }
    query <- paste("select V", n+1," as value from load_table", sep="")
    load <- sqldf(query)
    
    p1 <- ggAcf(load, type = "partial")
    p2 <- ggAcf(load, type = "partial", lag.max = 24*7)
    p3 <- ggAcf(load, type = "partial", lag.max = 24*7*4)
    
    grid.arrange(p1, p2, p3, ncol = 2, layout_matrix = rbind(c(1,2), c(3,3)))
  })
  
  output$pacft <- renderPlot({
    n <-0
    for(j in 1:21){
      if(input$selec1==a[j]){
        n <-j
      }
    }
    query <- paste("select V", n+1," as value from temp_table", sep="")
    load <- sqldf(query)
    
    p1 <- ggAcf(load, type = "partial")
    p2 <- ggAcf(load, type = "partial", lag.max = 24*7)
    p3 <- ggAcf(load, type = "partial", lag.max = 24*7*4)
    
    grid.arrange(p1, p2, p3, ncol = 2, layout_matrix = rbind(c(1,2), c(3,3)))
  })

  
  output$dygraph3 <- renderPlot({
    angle2=input$slider1
    period <-as.numeric(input$text)
    n <-0
    for(j in 1:21){
      if(input$selecto==a[j]){
        n <-j
      }
    }
    query <- paste("select datetime,V", n+1," as value from load_table", sep="")
    kkk <- sqldf(query)
    tim <- c()
    pred2 <- Table_period(kkk,period)
    tim <- xts(x = pred2[,2:(1+period)]/1000, order.by = pred2$datetime)
    tim <- na.locf(tim)
    Z <-tim
    cnames <- colnames(Z)
    col=c("yellow","red")
    col2=c("white","black")
    yred <- colorRampPalette(col)
    yred2 <- colorRampPalette(col2)
    par(mar=c(3,1,1,1))
    time.axis <- axTicksByTime(Z)
    Z <- as.xts(t(apply(Z,1,function(x) spline(as.vector(coredata(x)), n=1*length(x))$y)))
    Z <- Z/max(Z)*40
    pm <- persp(z=Z-min(Z),
                x=(1:NROW(Z))/length(time.axis),
                y=(1:NCOL(Z))/1,
                shade=.3, ltheta=20,
                r=10,
                theta=angle2,
                col=rep(rep(yred(NCOL(Z)/1)),each=(NROW(Z)-1)),
                scale=FALSE, border=NA,box=FALSE)
    
    x_axis <- seq(1, NROW(Z), length.out=length(time.axis))/length(time.axis)
    y_axis <- seq(1, NCOL(Z), length.out=NCOL(Z)/1)/1
    
    # x-axis
    xy0 <- trans3d(x_axis,y_axis[1],0,pm)
    xy1 <- trans3d(x_axis,y_axis[1]-0.3,0,pm)
    lines(trans3d(x_axis,y_axis[1],0,pm),col="#555555")
    segments(xy0$x,xy0$y,xy1$x,xy1$y, col="#555555")
    #text(xy1$x, xy1$y, labels=as.character(format(index(Z)[x_axis*10],"%m/%d/%y")), pos=1, offset=.25,cex=x.cex, srt=srt)
    text(xy1$x, xy1$y, labels=names(time.axis), pos=1, offset=.25,cex=0.75, srt=0)
    
    # y-axis
    xy0 <- trans3d(x_axis[length(x_axis)], y_axis, 0, pm)
    xy1 <- trans3d(x_axis[length(x_axis)]+.3, y_axis, 0, pm)
    yz0 <- trans3d(x_axis[length(x_axis)], y_axis, coredata(Z)[NROW(Z),seq(1,NCOL(Z),by=1)], pm) # vertical y
    lines(trans3d(x_axis[length(x_axis)], y_axis, 0, pm),col="#555555")
    segments(xy0$x,xy0$y,xy1$x,xy1$y,col="#555555")
    #text(xy1$x, xy1$y, labels=cnames, pos=4, offset=.5,cex=.75)
    
    #segments(xy0$x,xy0$y,yz0$x,yz0$y, col="#555555") # y-axis vertical lines
    
    # z-axis
    z_axis <- seq(trunc(min(Z,na.rm=TRUE)), round(max(Z, na.rm=TRUE)), by= 5)
    xy0 <- trans3d(x_axis[length(x_axis)], y_axis[length(y_axis)], z_axis- min(Z), pm)
    xy1 <- trans3d(x_axis[length(x_axis)]+0.3, y_axis[length(y_axis)], z_axis- min(Z), pm)
    lines(trans3d(x_axis[length(x_axis)], y_axis[length(y_axis)], z_axis- min(Z), pm))
    segments(xy0$x,xy0$y,xy1$x,xy1$y)
    
    
    if(input$checkbox2){
      dn <- c()
      l <- c()
      if(input$ggselect==d[1]){ dn<- predictions1 ; l <- load_table$datetime[37709:38212]}
      if(input$ggselect==d[2]){ dn<- predictions2 ; l <- load_table$datetime[1:39576]}
      if(input$ggselect==d[3]){ dn<- predictions3 ; l <- load_table$datetime[32661:32784]}
      if(input$ggselect==d[4]){ dn<- predictions4 ; l <-  load_table$datetime[32161:33328]}
      
      ff <- Table_period(data.frame("datetime"= kkk[,1] , "value"= dn[,1]),period)
      ff <- na.omit(ff)
      ff <- xts(x = ff[,2:(1+period)], order.by = ff$datetime)
      gj <- max(ff)
      gjj <- min(ff)
      Z["/2008-07-07",] <- NA
      D <- as.xts(t(apply(ff,1,function(x) spline(as.vector(coredata(x)), n=1*length(x))$y)))
      gh <- merge(Z,D, join= "left")
      Z<-gh[,(period+1):(2*period)]
      Z <- Z/gj*40
      cnames <- colnames(Z)
      par(mar=c(3,1,1,1))
      par(new=TRUE)
      pm2<-persp(z=Z-gjj,
                 x=(1:NROW(Z))/length(time.axis),
                 y=(1:NCOL(Z))/1,
                 shade=.3, ltheta=20,
                 r=200,
                 theta=angle2,
                 col=rep(rep(yred2(NCOL(Z)/1)),each=(NROW(Z)-1)),
                 scale=FALSE, border=NA,box=FALSE)
    }
    
    
  })
  
  output$dygraph34 <- renderPlot({
    angle2=input$sslider11
    period <-as.numeric(input$text)
    n <-0
    for(j in 1:11){
      if(input$sselect2==l[j]){
        n <-j
      }
    }
    query <- paste("select datetime,V", n+1," as value from temp_table", sep="")
    kkk <- sqldf(query)
    tim <- c()
    pred2 <- Table_period(kkk,period)
    tim <- xts(x = pred2[,2:(1+period)], order.by = pred2$datetime)
    tim <- na.locf(tim)
    Z <-tim
    cnames <- colnames(Z)
    col=c("yellow","red")
    col2=c("white","black")
    yred <- colorRampPalette(col)
    yred2 <- colorRampPalette(col2)
    par(mar=c(3,1,1,1))
    time.axis <- axTicksByTime(Z)
    Z <- as.xts(t(apply(Z,1,function(x) spline(as.vector(coredata(x)), n=1*length(x))$y)))
    Z <- Z/max(Z)*20
    pm <- persp(z=Z-min(Z),
                x=(1:NROW(Z))/length(time.axis),
                y=(1:NCOL(Z))/1,
                shade=.3, ltheta=20,
                r=10,
                theta=angle2,
                col=rep(rep(yred(NCOL(Z)/1)),each=(NROW(Z)-1)),
                scale=FALSE, border=NA,box=FALSE)
    
    x_axis <- seq(1, NROW(Z), length.out=length(time.axis))/length(time.axis)
    y_axis <- seq(1, NCOL(Z), length.out=NCOL(Z)/1)/1
    
    # x-axis
    xy0 <- trans3d(x_axis,y_axis[1],0,pm)
    xy1 <- trans3d(x_axis,y_axis[1]-0.3,0,pm)
    lines(trans3d(x_axis,y_axis[1],0,pm),col="#555555")
    segments(xy0$x,xy0$y,xy1$x,xy1$y, col="#555555")
    #text(xy1$x, xy1$y, labels=as.character(format(index(Z)[x_axis*10],"%m/%d/%y")), pos=1, offset=.25,cex=x.cex, srt=srt)
    text(xy1$x, xy1$y, labels=names(time.axis), pos=1, offset=.25,cex=0.75, srt=0)
    
    # y-axis
    xy0 <- trans3d(x_axis[length(x_axis)], y_axis, 0, pm)
    xy1 <- trans3d(x_axis[length(x_axis)]+.3, y_axis, 0, pm)
    yz0 <- trans3d(x_axis[length(x_axis)], y_axis, coredata(Z)[NROW(Z),seq(1,NCOL(Z),by=1)], pm) # vertical y
    lines(trans3d(x_axis[length(x_axis)], y_axis, 0, pm),col="#555555")
    segments(xy0$x,xy0$y,xy1$x,xy1$y,col="#555555")
    #text(xy1$x, xy1$y, labels=cnames, pos=4, offset=.5,cex=.75)
    
    #segments(xy0$x,xy0$y,yz0$x,yz0$y, col="#555555") # y-axis vertical lines
    
    # z-axis
    z_axis <- seq(trunc(min(Z,na.rm=TRUE)), round(max(Z, na.rm=TRUE)), by= 5)
    xy0 <- trans3d(x_axis[length(x_axis)], y_axis[length(y_axis)], z_axis- min(Z), pm)
    xy1 <- trans3d(x_axis[length(x_axis)]+0.3, y_axis[length(y_axis)], z_axis- min(Z), pm)
    lines(trans3d(x_axis[length(x_axis)], y_axis[length(y_axis)], z_axis- min(Z), pm))
    segments(xy0$x,xy0$y,xy1$x,xy1$y)
    
    
  })

  
  output$ftemp <- renderPlot({
    n <-0
    for(j in 1:11){
      if(input$select6==l[j]){
        n <-j
      }
    }
    
    query <- paste("select V", n+1," as value from temp_table", sep="")
    p1 <- sqldf(query)
    par(mfrow=c(1,2))
    spectrum(na.locf(p1))
    periodogram(na.locf(p1))
    
  })
  
  output$fload <- renderPlot({
    n <-0
    for(j in 1:21){
      if(input$select5==k[j]){
        n <-j
      }
    }
    
    query <- paste("select V", n+1," as value from load_table", sep="")
    p1 <- sqldf(query)
    par(mfrow=c(1,2))
    spectrum(na.locf(p1))
    periodogram(na.locf(p1))
  })
  
  output$fpred <- renderPlot({
    p1 <- c()
    if(input$select88==d[1]){ p1 <- predictions1 }
    if(input$select88==d[2]){ p1 <- predictions2 }
    if(input$select88==d[3]){ p1 <- predictions3 }
    if(input$select88==d[4]){ p1 <- predictions4 }
    p1 <- na.omit(p1)
    par(mfrow=c(1,2))
    spectrum(p1)
    periodogram(p1)
  })
  
  
  
  
  output$summ2 <-  renderPrint({
    n <-0
    for(j in 1:11){
      if(input$select6==l[j]){
        n <-j
      }
    }
  
    query <- paste("select V", n+1," as value from temp_table", sep="")
  
    p1 <- sqldf(query)
    p1 <- na.locf(p1)
    
    p = periodogram(p1)
    
    dd = data.frame(freq=p$freq, spec=p$spec)
    order = dd[order(-dd$spec),]
    top2 = head(order, 9)
    
    time = 1/top2$f
    print(cbind(top2, time))
  })
  
  output$summ <-  renderPrint({
    n <-0
    for(j in 1:21){
      if(input$select5==k[j]){
        n <-j
      }
    }
    
    query <- paste("select V", n+1," as value from load_table", sep="")
    
    p1 <- sqldf(query)
    p1 <- na.locf(p1)
    
    p = periodogram(p1)
    
    dd = data.frame(freq=p$freq, spec=p$spec)
    order = dd[order(-dd$spec),]
    top2 = head(order, 9)
    
    time = 1/top2$f
    print(cbind(top2, time))
  })
  
  output$summ3 <-  renderPrint({
    p1 <- c()
    if(input$select88==d[1]){ p1 <- predictions1 }
    if(input$select88==d[2]){ p1 <- predictions2 }
    if(input$select88==d[3]){ p1 <- predictions3 }
    if(input$select88==d[4]){ p1 <- predictions4 }
    p1 <- na.omit(p1)
    
    p = periodogram(p1)
    
    dd = data.frame(freq=p$freq, spec=p$spec)
    order = dd[order(-dd$spec),]
    top2 = head(order, 9)
    
    time = 1/top2$f
    print(cbind(top2, time))
  })
  
  
  getPage<-function() {
    return(includeHTML("./TSMarkdown.html"))
  }
  output$inc<-renderUI({getPage()})
  
  
}

shinyApp(ui = ui, server = server)
