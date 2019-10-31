library(dygraphs)
library(data.table)
load_table= fread("./../Data/TS.csv", sep=",")
s=fread("./../Data/TS.csv", sep=",")
load_table= data.frame(load_table)
temp_table= fread("./../Data/TST.csv", sep=",")
temp_table= data.frame(temp_table)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(DT)
library(sqldf)

load_table$datetime <- ymd_hms(load_table$datetime)
temp_table$datetime <- ymd_hms(temp_table$datetime)
#don <- xts(x = load_table$V2, order.by = load_table$datetime)
#zt <- xts(x = load_table[,3], order.by = load_table$datetime)

a= list()
for(i in 1:20){
    a <- c(a, paste("Zone", i, " Load"))
}

for(i in 1:11){
    a <- c(a, paste("St.", i, " Temp."))
}

ui <- navbarPage("Energy Forecast Data Visualisation",
                 tabPanel("Time Series",
                          sidebarPanel(width=3,
                            checkboxGroupInput("checkGroup",strong("Time Serie Select"),choices = a,selected = c(a[21], a[1], a[18]), inline = TRUE)
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
                 tabPanel("SQL Time Series",
                    sidebarPanel("SQL query on Dataframe:",
                            withTags(div(textarea(id = "response2", value= "select * from load_table", class = "form-control shiny-bound-input",style = "width: 400px; height: 100px"))),
                            helpText("SQLite is used .See table tab to see the Time Series column names. Result must contain datetime column to be ploted "),
                            actionButton("action", "Run Query"),
                            actionButton("action2", "Save to CSV"),
                            helpText("(/Data/result.csv)")
                          ),
                           mainPanel(
                            tabsetPanel(
                              tabPanel("Plot ", dygraphOutput("dygraph2")),
                              tabPanel("Result Table", tableOutput("table2")),
                              tabPanel("Summary", verbatimTextOutput("summary4")),
                              tabPanel("Table Load", DTOutput('table3')), 
                              tabPanel("Table Temp", DTOutput('table4'))
                            )
                          )
                 ),
                 tabPanel("3D Time Series", 
                    sidebarPanel(
                            radioButtons("radio", strong("Z axis quantity"),choices = list("Temperature" = 1, "Energy" = 2),selected = 1),
                            dateRangeInput("dates", strong("Date range")),
                            selectInput("select", strong("Zone_id Select"),choices = list("Zone1" = 1,"Zone2" = 2,"Zone3" = 3, "Zone4" = 4, "Zone5" = 5, "Zone6" = 6, "Zone7" = 7,
                                                                                          "Zone8" = 8,"Zone9" = 9,"Zone10" = 10,"Zone11" = 11,"Zone12" = 12,"Zone13" = 13,
                                                                                          "Zone14" = 14,"Zone15" = 15, "Zone16" = 16,"Zone17" = 17,"Zone18" = 18, "Zone19" = 19,
                                                                                          "Zone20" = 20),selected = 1)
                          ),
                           mainPanel(
                            tabsetPanel(
                              tabPanel("Plot", dygraphOutput("dygraph3")),
                              tabPanel("Summary", verbatimTextOutput("summary5")),
                              tabPanel("Table", tableOutput("table5"))
                            )
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
      dygraph(don) %>%
        dyOptions(drawGrid = TRUE) %>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
        dyRoller(rollPeriod = 24)
  })

  #output$table <-  renderDT(load_table)
 # output$table2 <-  renderDT(temp_table)
  output$table3 <-  renderDT(load_table)
  output$table4 <-  renderDT(temp_table)
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
  output$summary4 <-  renderPrint({
      print(input$response2)
     #gg <-  sqldf(input$response2)
     #print(gg)
  })
  output$table2 <-  renderDT({
      #gg <-  sqldf(input$response2)
      #datatable(gg)
  })

  output$action <- function(){
      print("kkkk")
  }
      
}

shinyApp(ui = ui, server = server)