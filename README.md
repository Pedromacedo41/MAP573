# MAP573 - Time Series Forecast

## Useful links:

https://www.kaggle.com/apoorvabhide/energy-consumption-time-series-forecasting-in-r/notebook#Introduction
https://www.kaggle.com/c/global-energy-forecasting-competition-2012-load-forecasting/dat
Recency Effect https://www.sciencedirect.com/science/article/pii/S0169207015001557?via%3Dihub

## About usage and folder structure:

The python jupyter notebook (with the code of the RNN) is whitin the root directory. 
Other important files/folders: 

+ **./Data** stores all used data
+ **./powerpoint** stores ppt presentations
+ **./src** stores python an R scripts 
+ **./RVis** keeps all the code to generate the R Visualisation tool
+ **./Suppor Material** keeps articles, notebooks, etc, with relevant information



## Necessary python libraries in notebook: 

+ TensorFlow
+ Keras
+ tqdm
+ pandas
+ matplotlib
+ numpy
+ sklearn

## How to run R visualisation app:

Required libraries:

+ dygraphs (interactive Time Serie Plot)
+ data.table
+ xts (time serie object)
+ tidyverse
+ lubridate
+ sqldf (sql commands in dataframes)
+ shiny

Run, in this root folder: 

```{r}
   > library(shiny)
   > shinyApp("RVis")
```