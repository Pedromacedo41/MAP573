library(dygraphs)
library(data.table)
load_table= fread("./../Data/TS.csv", sep=",")
load_table= data.frame(load_table)
temp_table= fread("./../Data/TST.csv", sep=",")
temp_table= data.frame(temp_table)

ts= fread("./../Data/TSDaily.csv", sep=",")
ts= data.frame(ts)

library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(DT)
library(sqldf)

load_table$datetime <- ymd_hms(load_table$datetime)
temp_table$datetime <- ymd_hms(temp_table$datetime)
ts$datetime <- ymd_hms(ts$datetime)
#don <- xts(x = load_table$V2, order.by = load_table$datetime)
#zt <- xts(x = load_table[,3], order.by = load_table$datetime)

a= list()
for(i in 1:20){
  a <- c(a, paste("Zone", i, " Load"))
}

for(i in 1:11){
  a <- c(a, paste("St.", i, " Temp."))
}

d= list()
for(i in 1:20){
  d <- c(d, paste("Zone", i, " Forecast"))
}


ui <- navbarPage("Energy Forecast Data Visualisation",
                 tabPanel("Time Series",
                          sidebarPanel(width=3,
                                       checkboxGroupInput("checkGroup",strong("Time Serie Select"),choices = a,selected = c(a[21], a[1], a[18]), inline = TRUE),
                                       checkboxGroupInput("checkGroup",strong("Forecast Data"),choices = d,selected = c(), inline = TRUE)
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
                            dateRangeInput("dates", strong("Date range")),
                            selectInput("select", strong("Data source (Zones,stations)"),choices =a,selected = 1),
                            textInput("text", h3("Periodicity"), value = " "),
                            checkboxInput("checkbox", "Include Forecast Data (if it exists)", value = FALSE)
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Plot", dygraphOutput("dygraph3")),
                              tabPanel("Summary", verbatimTextOutput("summary5")),
                              tabPanel("Table", tableOutput("table5"))
                            )
                          )
                ),
                tabPanel("Statistical Analysis")
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
    dygraph(don) %>%
      dyOptions(drawGrid = TRUE) %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 24)
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

  output$dygraph <- renderPlot({
      gg <- sqldf("select * from ts where X1 = 1")
      gg <- na.locf(gg)
      temp  <- xts(x = gg[,3:26], order.by = gg$datetime)

      temp <- na.locf(temp)

      Z <- temp
      cnames <- colnames(Z)
      col=c("yellow","red")
      yred <- colorRampPalette(col)
      par(mar=c(3,1,1,1))
      time.axis <- axTicksByTime(Z)
      Z <- as.xts(t(apply(Z,1,function(x) spline(as.vector(coredata(x)), n=1*length(x))$y)))
      pm <- persp(z=Z-min(Z),
                  x=(1:NROW(Z))/length(time.axis),
                  y=(1:NCOL(Z))/1,
                  shade=.3, ltheta=20,
                  r=10,
                  theta=50,
                  col=rep(rep(yred(NCOL(Z)/1),each=1),each=(NROW(Z)-1)),
                  scale=F, border=NA,box=FALSE)



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
  
  
}

shinyApp(ui = ui, server = server)