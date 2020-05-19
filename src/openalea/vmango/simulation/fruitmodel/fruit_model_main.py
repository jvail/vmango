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

    LFratio = nb_leaves / nb_fruits
    if np.isnan(DM_fruit_0):
        weight_1 = params.main.fruitDM0_weight_1
        mu_1 = params.main.fruitDM0_mu_1
        sigma_1 = params.main.fruitDM0_sigma_1
        weight_2 = params.main.fruitDM0_weight_2
        mu_2 = params.main.fruitDM0_mu_2
        sigma_2 = params.main.fruitDM0_sigma_2
        DM_fruit_0 = weight_1 * np.random.normal(mu_1, sigma_1) + weight_2 * np.random.normal(mu_2, sigma_2)
    if np.isnan(DM_fruit_ini):
        DM_fruit_ini = DM_fruit_0

    result = growth(
        bloom_date,
        weather_daily_df,
        weather_hourly_df,
        sunlit_bs,
        LFratio,
        DM_fruit_0,
        DM_fruit_ini,
        sim_date_ini,
        dd_thresh,
        stop_sim_ddcum,
        params,
        verbose
    )

    DAB, growth_df = result
    growth_df.loc[:,'LFratio'] = LFratio
    growth_df.loc[:,'sunlit_bs'] = np.sum(sunlit_bs) / 24
    growth_df.loc[:,'DAB'] = np.nan
    growth_df.loc[0:min(DAB.shape[0], growth_df.shape[0]) - 1,'DAB'] = DAB[0:min(DAB.shape[0], growth_df.shape[0])]

    return growth_df
