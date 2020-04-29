import os, datetime
import numpy as np
import pandas as pd
import random as rdm

from .fruit_growth import growth

def growth_main(
    bloom_date,
    nb_fruits,
    nb_leaves,
    sunlit_bs,
    weather_daily_df,
    weather_hourly_df,
    params,
    DM_fruit_0=np.nan,
    DM_fruit_ini=np.nan,
    sim_date_ini=None,
    dd_thresh=np.nan,
    stop_sim_ddcum=np.nan,
    verbose=False,
):

    LF = nb_leaves / nb_fruits
    print(DM_fruit_0)
    if np.isnan(DM_fruit_0):
        DM_fruit_0 = 0.97 * np.random.normal(13.9, 4.1) + 0.03 * np.random.normal(29.2, 0.66)
    if np.isnan(DM_fruit_ini):
        DM_fruit_ini = DM_fruit_0

    result = growth(
        bloom_date,
        weather_daily_df,
        weather_hourly_df,
        sunlit_bs,
        LF,
        DM_fruit_0,
        DM_fruit_ini,
        sim_date_ini,
        dd_thresh,
        stop_sim_ddcum,
        params,
        verbose
    )

    DAB, growth_df = result
    growth_df.loc[:,'LF'] = LF
    growth_df.loc[:,'sunlit_bs'] = np.sum(sunlit_bs) / 24
    growth_df.loc[:,'DAB'] = np.nan
    growth_df.loc[0:min(DAB.shape[0], growth_df.shape[0]) - 1,'DAB'] = DAB[0:min(DAB.shape[0], growth_df.shape[0])]

    return growth_df
