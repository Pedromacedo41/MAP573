# MAP573 - Time Series Forecast

## Presentation:

All presentations during the project and the final presentation are inside the powerpoint folder

## Useful links:

+[Kaggle Challenge Link 1](https://www.kaggle.com/apoorvabhide/energy-consumption-time-series-forecasting-in-r/notebook#Introduction)
+[Kaggle Challenge Link 2](https://www.kaggle.com/c/global-energy-forecasting-competition-2012-load-forecasting/dat)
+[Recency Effect](https://www.sciencedirect.com/science/article/pii/S0169207015001557?via%3Dihub)
+[Weather Station Select](https://www.sciencedirect.com/science/article/pii/S0169207014001319?via%3Dihub)
+[Hierchical Forecasting](https://www.sciencedirect.com/science/article/pii/S0169207013000757)


## About usage and folder structure:

Files/folders: 

+ **./Data** stores all forecasting data, model data, saved models and all other relevant data.
+ **./powerpoint** stores ppt presentations
+ **./src** stores python an R scripts. This folder is split into a Forecasting and Visualisation sections.
+ **./RVis** keeps all the code to generate the R Visualisation tool
+ **./Suppor Material** keeps articles, notebooks, etc, with relevant information

## Source Files: 

+ Everything related to the R statistic analysis and shiny app is whithin the single file "app.R" 
+ The python code for the Neural Networks are spread through the notebook files in **./src** folder

## How to run R visualisation app

There is a deployed web version of the app [here](https://pedromacedo41.shinyapps.io/rvis/).
To run locally, open app.R (inside RVis) in RStudio and install the necessary libraries. 

All packages installation is straightforward, except keras installation, that requires 
running library(keras) and install_keras() after running install.packages("keras"). 
This is a required preliminar step to set up keras backend (tensorflow). 

After set up enviroment, the app is ready to go. 

## How to run Keras Python Scripts and understanding src/Forecasting

Install the necassary libraries. 
src/Forecasting contains results from hyperparameter tuning in the folder ../hyperparameter_tuning_results.
The importants files are:

+	**generating_all_results.ipynb** - generates all the results in the report sequentially
+	**LSTM_basic_multivariable.ipynb** - main file with experiments and visualisations for most basic LSTM architecture
		This is the python file which has the most detailed comments. 
		Other files are created from the basic structure in this one.
+	**LSTM_bidirectional.ipynb** - experiments with bidirectional LSTM
+	**GRU.ipynb** - experiments with bidirectional LSTM
+	**hyperparameter_tuning.ipynb** - A reduced version of the **LSTM_experiments_COLAB_GPU.ipynb**.
		The original file has more functionality but can only be run on GOOGLE COLAB.

Other files such:

+	**data_reformatting.py**/ **data_reformatting.R** - data reformatting in both Python and R.
+	**initial_spyder_scripts.py** - scripts similar to **LSTM_basic_multivariable.ipynb** intended to by run on any Python IDLE (not used).
	
## Necessary python libraries in notebook: 

+ TensorFlow
+ Keras
+ tqdm
+ pandas
+ matplotlib
+ numpy
+ sklearn
+ talos

