# -*- coding: utf-8 -*-
"""
Created on Tue Oct 22 15:42:49 2019

@author: fvice
"""

import pandas as pd

# PRE PROCESSING
df_sol_data = pd.read_csv("C:/Users/fvice/Downloads/GEFCom2012/GEFCOM2012_Data/Load/Load_solution.csv")
df_hist = pd.read_csv("C:/Users/fvice/Downloads/GEFCom2012/GEFCOM2012_Data/Load/Load_history.csv")
df_sol = df_sol_data.drop(labels = ["id", "weight"], axis=1)
col_index = df_sol.columns

# VISUALIZATGION
df_sol[:10]
df_hist[:10]
df_hist.iloc[:, :4][:10]
sum(pd.isna(df_hist).values.any(axis=1).astype(int))
df_hist[pd.isna(df_hist).values.any(axis=1)]

# EXCEL CONCATENATION
df_hist_clean = df_hist.dropna()
df_sol_clean = df_sol.dropna()
df_histHk = df_hist_clean.set_index(['zone_id', 'year', 'month', 'day'])
df_solHk = df_sol_clean.set_index(['zone_id', 'year', 'month', 'day'])

res = pd.concat([df_histHk, df_solHk], verify_integrity=True)
PATH = "C:/Users/fvice/Downloads/GEFCom2012/GEFCOM2012_Data/Load/completeLoad.csv"
res_sorted = res.sort_index()
res_sorted.to_csv(PATH, columns = col_index.tolist()[4:])
