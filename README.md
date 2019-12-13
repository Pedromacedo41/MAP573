# MAP573 - Time Series Forecast

## Presentation:

All presentations during the project and the final presentation are inside the powerpoint folder

## Useful links:

[Kaggle Challenge Link 1](https://www.kaggle.com/apoorvabhide/energy-consumption-time-series-forecasting-in-r/notebook#Introduction)
[Kaggle Challenge Link 2](https://www.kaggle.com/c/global-energy-forecasting-competition-2012-load-forecasting/dat)
[Recency Effect](https://www.sciencedirect.com/science/article/pii/S0169207015001557?via%3Dihub)
[Weather Station Select](https://www.sciencedirect.com/science/article/pii/S0169207014001319?via%3Dihub)
[Hierchical Forecasting](https://www.sciencedirect.com/science/article/pii/S0169207013000757)


## About usage and folder structure:

The python jupyter notebook (with the code of the RNN) is whitin the root directory. 
Other important files/folders: 

+ **./Data** stores all used data
+ **./powerpoint** stores ppt presentations
+ **./src** stores python an R scripts 
+ **./RVis** keeps all the code to generate the R Visualisation tool
+ **./Suppor Material** keeps articles, notebooks, etc, with relevant information


## How to run R visualisation app

There is a deployed web version of the app [here](https://pedromacedo41.shinyapps.io/rvis/).
Tu run locally, open app.R (inside RVis) in RStudio and install the necessary libraries. 

All packages installation is straightforward, except keras installation, that requires 
running library(keras) and install_keras() after running install.packages("keras"). 
This is a required preliminar step to set up keras backend (tensorflow). 

After set up enviroment, the app is ready to go. 

## Necessary python libraries in notebook: 

+ TensorFlow
+ Keras
+ tqdm
+ pandas
+ matplotlib
+ numpy
+ sklearn

