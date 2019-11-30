library(shiny)
library(dygraphs)
library(data.table)
TS= fread("./../Data/TS.csv", sep=",")
load_table= data.frame(TS)


TST= fread("./../Data/TST.csv", sep=",")
temp_table= data.frame(TST)

xd1= t(fread("./../Data/PredictHyper/bidirectionallstm_predictor_168h_window672_37708_38212.csv", sep=","))
xd2=t(fread("./../Data/PredictHyper/lstm_predictor_1h_window100_0_39575.csv", sep=","))
xd3= t(fread("./../Data/PredictHyper/lstm_predictor_24h_window100_32660_32784.csv", sep=","))
xd4= t(fread("./../Data/PredictHyper/lstm_predictor_168h_window1000_32160_33328.csv", sep=","))
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

predictions1 <- xts(x = as.numeric(xd1$xd1/1000), order.by = load_table$datetime[37709:38212])
predictions2 <- xts(x = as.numeric(xd2$xd2/1000), order.by = load_table$datetime[1:39576])
predictions3 <- xts(x = as.numeric(xd3$xd3/1000), order.by = load_table$datetime[32661:32784])
predictions4 <- xts(x = as.numeric(xd4$xd4/1000), order.by = load_table$datetime[32161:33328])


k= list()

a=list()
for(i in 1:20){
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
  tb=c(tb_init$datetime[1])
  while(TRUE){
    if((i+period)>length(tb_init$datetime)){
      tb <- c(tb,tb_init$datetime[i])
      k <- c(tb_init$value[i:(i+length(tb_init$datetime)%%period)])
      k <- c(k,rep(NA,times=c(period-length(k))))
      gd <- rbind(gd,k)
      break;
    }else{
      tb <- c(tb,tb_init$datetime[i])
      gd <- rbind(gd,tb_init$value[i:(i+period-1)])
      i=i+period
    }
  }
  #print(tb)
  return(data.frame("datetime"= tb[2:length(tb)], gd))
}


ui <- navbarPage("Energy Forecast Data Visualisation",
                 tabPanel("Time Series",
                          sidebarPanel(width=3,
                                       checkboxGroupInput("checkGroup",strong("Time Serie Select"),choices = a,selected = c(a[1]), inline = TRUE),
                                       checkboxGroupInput("checkGroup2",strong("Forecast Data"),choices = d,selected = c(d[1]), inline = TRUE)
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
                            selectInput("select", strong("Zone"),choices =k,selected = 1),
                            textInput("text", h3("Periodicity"), value = "24"),
                            checkboxInput("checkbox", "Normalize", value = FALSE),
                            sliderInput("slider1", h3("Sliders"),
                                        min = 0, max = 90, value = 50),
                            selectInput("select", strong("Forecast Zone"),choices =k,selected = 1),
                            checkboxInput("checkbox2", "Include Forecast Data", value = TRUE)
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Plot", plotOutput("dygraph3")),
                              tabPanel("Summary", verbatimTextOutput("summary5")),
                              tabPanel("Table", tableOutput("table5"))
                            )
                          )
                 ),
                 tabPanel("3D Time Series", 
                          sidebarPanel(
                            selectInput("select2", strong("Stations"),choices =l,selected = 1),
                            textInput("text2", h3("Periodicity"), value = "24"),
                            sliderInput("slider11", h3("Sliders"),
                                        min = 0, max = 90, value = 50),
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Plot", plotOutput("dygraph33")),
                              tabPanel("Summary", verbatimTextOutput("summary55")),
                              tabPanel("Table", tableOutput("table55"))
                            )
                          )
                 ),
                 tabPanel("Temp-Load Correlations",
                          sidebarPanel(
                            selectInput("select3", strong("Zone"),choices =k,selected = 1),
                            selectInput("select4", strong("Stations"),choices =l,selected = 1)
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Plot", plotOutput("dygraph333")),
                              tabPanel("Summary", verbatimTextOutput("summar")),
                              tabPanel("Table", tableOutput("tabl"))
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
                              tabPanel("Plot", plotOutput("dygraph5")),
                              tabPanel("Load Modes", verbatimTextOutput("summ")),
                              tabPanel("Temp Modes", verbatimTextOutput("summ2")),
                              tabPanel("Prediction Modes", verbatimTextOutput("summ3"))
                            )
                          )
                 ),
                 tabPanel("Statistical Analysis",
                          mainPanel(
                            htmlOutput("inc")
                          )
                 )
) 

server <- function(input, output) {
  output$dygraph <- renderDygraph({
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
  
    if(input$checkGroup2==d[1]){ don<- cbind(don,na.locf(predictions1)) }
    if(input$checkGroup2==d[2]){ don<- cbind(don,predictions2) }
    if(input$checkGroup2==d[3]){ don<- cbind(don,predictions3) }
    if(input$checkGroup2==d[4]){ don<- cbind(don,predictions4) }
    
    dygraph(don) %>%
      dyOptions(drawGrid = TRUE) %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
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
    summary(don)
  })
  
  output$dygraph3 <- renderPlot({
    period <-as.numeric(input$text)
    angle=input$slider1
    d <-0
    for(j in 1:20){
      if(input$select==a[j]){
        d <-j
      }
    }
    query <- paste("select datetime,V", d+1," as value from load_table", sep="")
      
    ggg <- sqldf(query)
    
    
    tim <- c()
    
    pred2 <- Table_period(ggg,period)
    
    if(input$checkbox2){
      pred <- Table_period(predictions,period)
      predfin= rbind(pred,pred2)
      tim <- xts(x = predfin[,2:(1+period)], order.by = predfin$datetime)
    }else{
      tim <- xts(x = pred2[,2:(1+period)], order.by = pred2$datetime)
    }
    
    
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
    par(mfrow=c(1,1))
    pm <- persp(z=Z-min(Z),
                x=(1:NROW(Z))/length(time.axis),
                y=(1:NCOL(Z))/1,
                shade=.3, ltheta=20,
                r=10,
                theta=angle,
                col=rep(rep(yred(NCOL(Z)/1)),each=(NROW(Z)-1)),
                scale=input$checkbox, border=NA,box=FALSE)
    
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
      h <-Z
      h["/2008-07-06",1:period] <- NA
      par(new=TRUE)
      pm2<-persp(z=h-min(Z),
                 x=(1:NROW(Z))/length(time.axis),
                 y=(1:NCOL(Z))/1,
                 shade=.3, ltheta=20,
                 r=200,
                 theta=angle,
                 col=rep(rep(yred2(NCOL(Z)/1)),each=(NROW(Z)-1)),
                 scale=input$checkbox, border=NA,box=FALSE)
    }
    
  
  })
  
  output$dygraph33 <- renderPlot({
    period <-as.numeric(input$text2)
    angle=input$slider11
    d <-0
    for(j in 1:11){
      if(input$select2==l[j]){
        d <-j
      }
    }
    query <- paste("select datetime,V", d+1," as value from temp_table", sep="")
    
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
    par(mfrow=c(1,1))
    pm <- persp(z=Z-min(Z),
                x=(1:NROW(Z))/length(time.axis),
                y=(1:NCOL(Z))/1,
                shade=.3, ltheta=20,
                r=10,
                theta=angle,
                col=rep(rep(yred(NCOL(Z)/1)),each=(NROW(Z)-1)),
                scale=input$checkbox, border=NA,box=FALSE)
    
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
  
  output$dygraph333 <- renderPlot({
    d <-0
    for(j in 1:11){
      if(input$select3==l[j]){
        d <-j
      }
    }
    f <-0
    for(j in 1:20){
      if(input$select4==a[j]){
        f <-j
      }
    }
    
    query1 <- paste("select datetime,V", d+1," as value from load_table", sep="")
    kkk <- sqldf(query1)
    
    query2  <- paste("select datetime,V", f+1," as value from temp_table", sep="")
    ggg <- sqldf(query2)
    
  
   
  })
  
  output$dygraph5 <- renderPlot({
    d <-0
    for(j in 1:11){
      if(input$select6==l[j]){
        d <-j
      }
    }
    f <-0
    for(j in 1:20){
      if(input$select5==a[j]){
        f <-j
      }
    }

    query <- paste("select V", d+1," as value from temp_table", sep="")
    query2 <- paste("select V", f+1," as value from load_table", sep="")
    p1 <- sqldf(query)
    p2 <- sqldf(query2)
    p1 <- na.locf(p1)
    p2 <- na.locf(p2)
    
    par(mfrow=c(2,2))
    p <- periodogram(p1)
    spectrum(p1)
    
    pp <- periodogram(p2)
    spectrum(p2)
  
  })
  
  
  output$summ2 <-  renderPrint({
    d <-0
    for(j in 1:11){
      if(input$select6==l[j]){
        d <-j
      }
    }
  
    query <- paste("select V", d+1," as value from temp_table", sep="")
  
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
    d <-0
    for(j in 1:21){
      if(input$select5==k[j]){
        d <-j
      }
    }
    
    query <- paste("select V", d+1," as value from load_table", sep="")
    
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
    p1 <- na.locf(p1)
    
    p = periodogram(p1)
    
    dd = data.frame(freq=p$freq, spec=p$spec)
    order = dd[order(-dd$spec),]
    top2 = head(order, 9)
    
    time = 1/top2$f
    print(cbind(top2, time))
  })
  
  
  getPage<-function() {
    return(includeHTML("./../src/Ari/TSMarkdown.html"))
  }
  output$inc<-renderUI({getPage()})
  
  
}

shinyApp(ui = ui, server = server)
