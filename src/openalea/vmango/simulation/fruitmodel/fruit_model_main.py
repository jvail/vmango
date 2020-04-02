import os
import datetime
import numpy as np
import pandas as pd
import random as rdm
from io import StringIO

from openalea.vmango.simulation.fruitmodel.fruit_growth import growth

def growth_main(bloom_date, nb_fruits, nb_leaves, DM_fruit_0, sunlit_bs, weather_hourly_df, weather_daily_df, params, verbose=False):

    LF = nb_leaves / nb_fruits
    FM_fruit_ini = 23.647 * DM_fruit_0 ** 0.6182

    result = growth(
        bloom_date,
        weather_daily_df,
        weather_hourly_df,
        FM_fruit_ini,
        sunlit_bs,
        DM_fruit_0,
        DM_fruit_0,
        LF,
        params,
        verbose=verbose
    )

    DAB, growth_df = result
    growth_df.loc[:,'LF'] = LF
    growth_df.loc[:,'sunlit_bs'] = np.sum(sunlit_bs) / 24
    growth_df.loc[:,'DAB'] = np.nan
    growth_df.loc[0:min(DAB.shape[0], growth_df.shape[0]) - 1,'DAB'] = DAB[0:min(DAB.shape[0], growth_df.shape[0])]

    return growth_df
