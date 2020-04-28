import pandas as pd
import numpy as np

from .fruit_dry_matter_growth import growth_DM
from .fruit_fresh_matter_growth import growth_FM

def growth(
    bloom_date,
    weather_daily_df,
    weather_hourly_df,
    FM_fruit_ini,
    sunlit_bs,
    DM_fruit_0,
    DM_fruit_0,
    LF,
    dd_thresh,
    stop_sim_ddcum
    params,
    verbose
):

    ## -- selection of daily weather data from the date of bloom :
    weather_daily_df = weather_daily_df.drop(weather_daily_df[weather_daily_df['DATE'] < bloom_date].index)
    weather_hourly_df = weather_hourly_df.drop(weather_hourly_df[weather_hourly_df['DATE'] < bloom_date].index)

    ## -- addition of days after bloom to daily weather data :
    weather_daily_df.reset_index(inplace=True, drop=True)
    weather_daily_df.loc[:,'DAB'] = weather_daily_df.index.copy()

    ## -- addition of cumulated degree-days to daily weather data :
    Tbase = params.growth.Tbase
    TM = ((weather_daily_df['TX'] + weather_daily_df['TN']) / 2).values
    dd_delta = [max(0, tm - Tbase) for tm in TM]
    weather_daily_df.loc[:,'dd_delta'] = dd_delta
    weather_daily_df.loc[:,'dd_cum'] = np.cumsum(dd_delta)

    ## -- selection of daily weather data from the date of cell division ending :
    ##    (i.e. the date for which |dd_cum - dd_cum_0| is minimal)
    dd_cum_0 = params.growth.dd_cum_0
    weather_daily_fruit_df = weather_daily_df.drop(weather_daily_df[weather_daily_df['dd_cum'] < dd_cum_0].index)

    ## -- merging of hourly and daily weather data :
    weather_hourly_fruit_df = weather_daily_fruit_df.merge(weather_hourly_df, on='DATE')

    ## -- daily DAB values from the date of cell division ending :
    DAB = weather_hourly_fruit_df['DAB'].unique()

    hourly_GR = weather_hourly_fruit_df['GR'].values
    hourly_TM = weather_hourly_fruit_df['TM'].values
    hourly_TN = weather_hourly_fruit_df['TN'].values
    hourly_TX = weather_hourly_fruit_df['TX'].values
    hourly_T = weather_hourly_fruit_df['T'].values
    hourly_RH = weather_hourly_fruit_df['RH'].values
    hourly_dd_cum = weather_hourly_fruit_df['dd_cum'].values
    hourly_dd_delta = weather_hourly_fruit_df['dd_delta'].values

    # ========================================================================================================================
    # DATA INITIALIZATION
    # ========================================================================================================================

    r_DM_leaf_ini = params.dry_matter.r_DM_leaf_ini
    cc_leaf = params.dry_matter.cc_leaf
    r_DM_stem_ini = params.dry_matter.r_DM_stem_ini
    cc_stem = params.dry_matter.cc_stem

    a20 = params.growth.a20
    a21 = params.growth.a21
    a22 = params.growth.a22
    a23 = params.growth.a23

    a5 = params.fresh_matter.a5
    a6 = params.fresh_matter.a6
    a7 = params.fresh_matter.a7
    a8 = params.fresh_matter.a8
    a9 = params.fresh_matter.a9
    a10 = params.fresh_matter.a10

    ## -- initial amount of carbon in leaf and stem reserves :
    reserve_leaf_ini = (DM_leaf_unit * LF) * r_DM_leaf_ini * cc_leaf
    reserve_stem_ini = DM_stem * r_DM_stem_ini * cc_stem

    ## -- initial fresh and dry mass of fruit compartements :
    ##    from empirical relationships in Léchaudel (2004)
    FM_fruit_ini = 23.647 * DM_fruit_ini ** 0.6182
    W_fleshpeel_ini = (a22 * (FM_fruit_ini - DM_fruit_ini) ** a23) + (a20 * (FM_fruit_ini - DM_fruit_ini) ** a21)
    DM_fleshpeel_ini = (a7 * DM_fruit_ini ** a8) + (a5 * DM_fruit_ini ** a6)
    W_flesh_ini = a10 * W_fleshpeel_ini
    DM_flesh_ini = a9 * DM_fleshpeel_ini

    growth_df = pd.DataFrame({
        'DATE': [weather_hourly_fruit_df['DATE'].iat[0]],
        'FM_fruit': [FM_fruit_ini],
        'DM_fruit': [DM_fruit_ini],
        'W_fleshpeel': [W_fleshpeel_ini],
        'DM_fleshpeel': [DM_fleshpeel_ini],
        'W_flesh': [W_flesh_ini],
        'DM_flesh': [DM_flesh_ini],
        'reserve_leaf': [reserve_leaf_ini],
        'reserve_stem': [reserve_stem_ini],
        'water_potential': [np.nan],
        'turgor_pressure': [np.nan],
        'osmotic_pressure': [np.nan],
        'flux_xylem_phloem': [np.nan],
        'transpiration': [np.nan],
        'sucrose': [np.nan],
        'glucose': [np.nan],
        'fructose': [np.nan],
        'soluble_sugars': [np.nan],
        'starch': [np.nan],
        'organic_acids': [np.nan],
        'dd_cum': [np.nan]
    })

    DM_fruit_previous = DM_fruit_ini
    FM_fruit_previous = FM_fruit_ini
    reserve_leaf_previous = reserve_leaf_ini
    reserve_stem_previous = reserve_stem_ini
    W_fleshpeel_previous = W_fleshpeel_ini

    for i, date in enumerate(weather_hourly_fruit_df['DATE'].unique()):

        start = i * 24
        stop = start + 24

        # effectively daily
        TN_air = hourly_TN[start:stop][0]
        TX_air = hourly_TX[start:stop][0]
        TM_air = hourly_TM[start:stop][0]
        dd_delta = hourly_dd_delta[start:stop][0]
        dd_cum = hourly_dd_cum[start:stop][0]

        # real hourly data
        GR = hourly_GR[start:stop]
        T_air = hourly_T[start:stop]
        RH = hourly_RH[start:stop]

        DM = growth_DM(
            GR,
            TN_air,
            TX_air,
            TM_air,
            T_fruit=TM_air,
            sunlit_bs,
            DM_fruit_0=DM_fruit_0,
            DM_fruit_previous,
            reserve_stem=reserve_leaf_previous,
            reserve_leaf=reserve_stem_previous,
            LF,
            dd_delta,
            params=params.dry_matter
        )

        FM = growth_FM(
            date,
            T_air,
            GR=np.mean(GR),
            RH=np.mean(RH),
            dd_cum,
            TM_air=np.mean(TM_air),
            DM_fruit_0,
            DM_fruit=DM[0],
            DM_fruit_previous,
            FM_fruit_previous
            W_fleshpeel_previous,
            dd_thresh,
            params=params.fresh_matter
        )

        ## -- outputs of the current day :
        growth_df.loc[i, 6:19] = (*FM[0], dd_cum)

        ## -- outputs of the next day :
        growth_df.loc[i + 1, 0:9] = FM[1] + (DM[1], DM[2])

        ## -- cache some variables
        DM_fruit_previous = DM[0]
        FM_fruit_previous = FM[1][1]
        reserve_leaf_previous = DM[1]
        reserve_stem_previous = DM[2]
        W_fleshpeel_previous = FM[1][3]

        ## -- end of fruit growth - simulation is stopped :
        if np.isnan(stop_sim_ddcum): # if based on sucrose content (default)
            sucrose = FM[0][6]
            if sucrose >= params.growth.sucrose_ripe_thresh:
                if verbose:
                     print('The fruit is ripe: Sucrose threshhold reached.')
                break
        else: # else based on cumulated degree-days
            if dd_cum >= params.growth.stop_sim_ddcum:
                if verbose:
                     print('The fruit is ripe: Degree-days threshhold reached.')
                break

    return (DAB, growth_df)
