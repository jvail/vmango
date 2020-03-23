import pandas as pd
import numpy as np

from openalea.vmango.simulation.fruitmodel_generator.fruit_dry_matter_growth import growth_DM
from openalea.vmango.simulation.fruitmodel_generator.fruit_fresh_matter_growth import growth_FM

def growth(
    bloom_date,
    weather_daily_df,
    weather_hourly_df,
    FM_fruit_ini,
    sunlit_bs,
    DM_fruit_0,
    DM_fruit_ini,
    LF,
    params,
    verbose=False
):

    weather_daily_df = weather_daily_df.drop(weather_daily_df[weather_daily_df['DATE'] < bloom_date].index)
    weather_hourly_df = weather_hourly_df.drop(weather_hourly_df[weather_hourly_df['DATE'] < bloom_date].index)

    weather_daily_df.reset_index(inplace=True, drop=True)
    weather_daily_df.loc[:,'DAB'] = weather_daily_df.index.copy()

    Tbase = params.growth.Tbase
    weather_daily_df.loc[:,'dd_cum'] = np.cumsum([max(0, tm - Tbase) for tm in weather_daily_df['TM'].values])

    dd_cum_0 = params.growth.dd_cum_0
    weather_daily_fruit_df = weather_daily_df.drop(weather_daily_df[weather_daily_df['dd_cum'] < dd_cum_0].index)
    weather_hourly_fruit_df = weather_daily_fruit_df.merge(weather_hourly_df, on='DATE')

    DAB = weather_hourly_fruit_df['DAB'].unique()
    hourly_GR = weather_hourly_fruit_df['GR'].values
    hourly_TM = weather_hourly_fruit_df['TM'].values
    hourly_T = weather_hourly_fruit_df['T'].values
    hourly_HR = weather_hourly_fruit_df['HR'].values
    hourly_dd_cum = weather_hourly_fruit_df['dd_cum'].values

    reserve_leaf_ini = 0.8 * 0.074 * LF * 0.4051
    reserve_stem_ini  = 0.1 * 0.4387 * (41.83 + 77.41) / 2.0
    W_fleshpeel_ini = 0.4086 * (FM_fruit_ini - DM_fruit_ini)**0.7428 + 0.5874 * (FM_fruit_ini - DM_fruit_ini)**1.0584

    growth_df = pd.DataFrame({
        'DATE': [weather_hourly_fruit_df['DATE'].iat[0]],
        'FM_fruit': [FM_fruit_ini],
        'DM_fruit': [DM_fruit_ini],
        'W_fleshpeel': [W_fleshpeel_ini],
        'reserve_leaf': [reserve_leaf_ini],
        'reserve_stem': [reserve_stem_ini],
        'water_potential': [np.nan],
        'turgor_pressure': [np.nan],
        'osmotic_pressure': [np.nan],
        'flux_xyleme': [np.nan],
        'flux_phloeme': [np.nan],
        'transpiration': [np.nan],
        'sucrose': [np.nan],
        'soluble_sugars': [np.nan],
        'organic_acids': [np.nan],
        'dd_cum': [np.nan]
    })

    DM = (DM_fruit_ini, (reserve_leaf_ini, reserve_stem_ini))

    growth_DM_ = growth_DM(params.dry_matter)
    growth_FM_ = growth_FM(params.fresh_matter)
    
    next(growth_DM_)
    next(growth_FM_)

    for i, date in enumerate(weather_hourly_fruit_df['DATE'].unique()):

        start = i * 24
        stop = start + 24

        DM_fruit_previous = DM[0]
        GR_day = hourly_GR[start:stop]
        T_air_day = hourly_TM[start:stop]
        T_air_day_daily = hourly_T[start:stop]
        HR_day = hourly_HR[start:stop]
        dd_cum_day = hourly_dd_cum[start:stop]

        DM  = growth_DM_.send((
            GR_day,
            T_air_day,
            T_air_day,
            sunlit_bs,
            DM_fruit_0,
            DM_fruit_previous,
            DM[1][1],
            DM[1][0],
            LF
        ))

        FM = growth_FM_.send((
            date,
            T_air_day_daily,
            GR_day,
            HR_day,
            dd_cum_day,
            np.mean(T_air_day),
            (DM_fruit_previous, DM[0]),
            FM_fruit_ini,
            W_fleshpeel_ini,
            DM_fruit_0
        ))

        growth_df.loc[i, 6:16] = (*FM[0], dd_cum_day[0])
        growth_df.loc[i + 1, 0:6] = FM[1] + DM[1]

        sucrose = FM[0][6]
        if sucrose >= 0.04:
            if verbose == True:
                print('Le fruit est mur')
            break

    return (DAB, growth_df)
