import os
import datetime
import numpy as np
import pandas as pd
import random as rdm
from io import StringIO

from openalea.vmango.simulation.fruitmodel.fruit_growth import fruit_growth as CROISSANCE_MF_TEMPERATURE

k1runquant = """
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.0000000 0.00000000
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.0000000 0.00000000
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.0000000 0.00000000
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.0000000 0.00000000
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.2375000 0.02743902
0.000000000 0.000000000 0.001329784 0.053595792 0.47606834 0.7327253 0.8624674 0.24520585
0.000000000 0.002576737 0.127518165 0.903799567 1.00000000 1.0000000 1.0000000 0.62010823
0.006793106 0.059716180 0.587955259 1.000000000 1.00000000 1.0000000 1.0000000 0.76637666
0.016730759 0.148387686 0.914365914 1.000000000 1.00000000 1.0000000 1.0000000 0.83301961
0.115527740 0.585545097 0.991459349 1.000000000 1.00000000 1.0000000 1.0000000 0.89266429
0.201523652 0.810345489 1.000000000 1.000000000 1.00000000 1.0000000 1.0000000 0.92396048
0.386997989 0.837956288 1.000000000 1.000000000 1.00000000 1.0000000 1.0000000 0.93454137
0.306776618 0.774916717 1.000000000 1.000000000 1.00000000 1.0000000 1.0000000 0.92159764
0.051453450 0.373464741 0.989380223 1.000000000 1.00000000 1.0000000 1.0000000 0.88707134
0.037174480 0.111291792 0.796152968 1.000000000 1.00000000 1.0000000 1.0000000 0.81507673
0.004194766 0.018924779 0.372936050 0.925591051 1.00000000 1.0000000 1.0000000 0.68545019
0.000000000 0.001522419 0.046407966 0.176417148 0.57779795 0.8168222 0.8997635 0.32116490
0.000000000 0.000000000 0.000270248 0.005688648 0.05270888 0.1273495 0.1660462 0.04223630
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.0000000 0.00000000
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.0000000 0.00000000
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.0000000 0.00000000
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.0000000 0.00000000
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.0000000 0.00000000
0.000000000 0.000000000 0.000000000 0.000000000 0.00000000 0.0000000 0.0000000 0.00000000"""

def fruit_model_main(bloom_date, nb_fruits, nb_leaves, verbose=False, idsimu=np.nan, MS_Init=np.nan, k1_fruit_sample=np.nan):

    LF = nb_leaves / nb_fruits
    if np.isnan(MS_Init):
        MS_Init = 0.97 * np.random.normal(13.9, 4.1) + 0.03 * np.random.normal(29.2, 0.66)

    k1runquant_df = pd.read_csv(StringIO(k1runquant), '\s+', header=None, names=['q5', 'q10', 'q25', 'q50', 'q75', 'q90', 'q95', 'moy'])
    envirlum = k1runquant_df[['q10', 'q25', 'q50', 'q75', 'q90']]
    if np.isnan(k1_fruit_sample):
        k1_fruit = envirlum.iloc[:,rdm.randrange(0, 5)].to_numpy()
    else:
        k1_fruit = envirlum.iloc[:,k1_fruit_sample].to_numpy()

    Position = 'O'
    Pos = 'Ouest'
    Ombre = False
    temp = False   # le mod?le avec la temp?rature simul?e en 3D
    if Position == 'N': Pos = 'Nord'
    if Position == 'E': Pos = 'Est'
    if Position == 'O': Pos = 'Ouest'
    if Position == 'S': Pos = 'Sud'

    #### attention les donn?es sont celles de 2002 ####
    Meteo = pd.read_csv('../../../../../share/environment/rayostpierre2002.csv', sep=';', parse_dates=['Date', 'DATE'], dayfirst=True)
    Meteo['DATE'] = Meteo['Date'].dt.date
    Meteo_journalier = pd.read_csv('../../../../../share/environment/tempstpierre2002.csv', sep=';', parse_dates=['DATE'], dayfirst=True)
    Meteo_journalier['DATE'] = Meteo_journalier['DATE'].dt.date

    Res = CROISSANCE_MF_TEMPERATURE(
        Jour_Flo = bloom_date,
        Tab_journalier = pd.DataFrame(Meteo_journalier[['DATE', 'TM']]),
        Tab_horaire = pd.DataFrame(Meteo[['DATE', 'HEURE', 'Rayonnement', 'Temperature_Air', 'HR', 'Date']]),
        MF_Init = 23.647 * MS_Init**0.6182,
        envirlum = k1_fruit,
        MS_Init_Division_Cellulaire = MS_Init,
        MS_Debut_Sim = MS_Init,
        LF = LF,
        verbose=verbose
    )

    don = Res[1]
    don.loc[:,'LF'] = LF
    don.loc[:,'environ_lum'] = np.sum(k1_fruit) / 24
    don.loc[:,'DAB'] = np.nan
    don.loc[0:min(Res[0].shape[0], don.shape[0]) - 1,'DAB'] = Res[0][0:min(Res[0].shape[0], don.shape[0])]

    # pd.DataFrame([[local[0], type(local[1]).__name__] for local in locals().items()], columns=['name', 'type']).to_csv(f'{__name__ }_vars.csv', index=False, sep=';')

    return don
