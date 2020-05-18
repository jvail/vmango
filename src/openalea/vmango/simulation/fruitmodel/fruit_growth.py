import pandas as pd
import numpy as np
import math

from .fruit_dry_matter_growth import growth_DM
from .fruit_fresh_matter_growth import growth_FM

def growth(
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
):

    ## -- selection of daily weather data from the date of bloom :
    weather_daily_df = weather_daily_df.drop(weather_daily_df[weather_daily_df['DATE'] < bloom_date].index)
    weather_hourly_df = weather_hourly_df.drop(weather_hourly_df[weather_hourly_df['DATE'] < bloom_date].index)

    ## -- addition of days after bloom to daily weather data :
    weather_daily_df.reset_index(inplace=True, drop=True)
    weather_daily_df.loc[:,'DAB'] = weather_daily_df.index.copy()

    ## -- addition of cumulated degree-days to daily weather data :
    Tbase = params.dry_matter.Tbase_fruit
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

    sucrose_ripe_thresh = params.growth.sucrose_ripe_thresh
    stop_sim_ddcum = params.growth.stop_sim_ddcum
    e_fruitDM2FM_1 = params.growth.e_fruitDM2FM_1
    e_fruitDM2FM_2 = params.growth.e_fruitDM2FM_2

    r_DM_leaf_ini = params.dry_matter.r_DM_leaf_ini
    cc_leaf = params.dry_matter.cc_leaf
    r_DM_stem_ini = params.dry_matter.r_DM_stem_ini
    cc_stem = params.dry_matter.cc_stem

    e_fruit2peelW_1 = params.growth.e_fruit2peelW_1
    e_fruit2peelW_2 = params.growth.e_fruit2peelW_2
    e_fruit2fleshW_1 = params.growth.e_fruit2fleshW_1
    e_fruit2fleshW_2 = params.growth.e_fruit2fleshW_2

    e_fruit2peelDM_1 = params.fresh_matter.e_fruit2peelDM_1
    e_fruit2peelDM_2 = params.fresh_matter.e_fruit2peelDM_2
    e_fruit2fleshDM_1 = params.fresh_matter.e_fruit2fleshDM_1
    e_fruit2fleshDM_2 = params.fresh_matter.e_fruit2fleshDM_2
    e_fleshpeel2fleshDM = params.fresh_matter.e_fleshpeel2fleshDM
    e_fleshpeel2fleshW = params.fresh_matter.e_fleshpeel2fleshW
    DM_leaf_unit = params.dry_matter.DM_leaf_unit
    DM_stem = params.dry_matter.DM_stem

    ## -- initial amount of carbon in leaf and stem reserves :
    reserve_leaf_ini = (DM_leaf_unit * LFratio) * r_DM_leaf_ini * cc_leaf
    reserve_stem_ini = DM_stem * r_DM_stem_ini * cc_stem

    ## -- initial fresh and dry mass of fruit compartements :
    ##    from empirical relationships in Léchaudel (2004)
    FM_fruit_ini = e_fruitDM2FM_1 * DM_fruit_ini ** e_fruitDM2FM_2
    W_fleshpeel_ini = (e_fruit2fleshW_1 * (FM_fruit_ini - DM_fruit_ini) ** e_fruit2fleshW_2) + (e_fruit2peelW_1 * (FM_fruit_ini - DM_fruit_ini) ** e_fruit2peelW_2)
    DM_fleshpeel_ini = (e_fruit2fleshDM_1 * DM_fruit_ini ** e_fruit2fleshDM_2) + (e_fruit2peelDM_1 * DM_fruit_ini ** e_fruit2peelDM_2)
    W_flesh_ini = e_fleshpeel2fleshW * W_fleshpeel_ini
    DM_flesh_ini = e_fleshpeel2fleshDM * DM_fleshpeel_ini

    growth_df = pd.DataFrame({
        'date': [weather_hourly_fruit_df['DATE'].iat[0]],
        'FM_fruit': [FM_fruit_ini],
        'DM_fruit': [DM_fruit_ini],
        'W_fleshpeel': [W_fleshpeel_ini],
        'DM_fleshpeel': [DM_fleshpeel_ini],
        'W_flesh': [W_flesh_ini],
        'DM_flesh': [DM_flesh_ini],
        'reserve_leaf': [reserve_leaf_ini],
        'reserve_stem': [reserve_stem_ini],
        'water_potential_fruit': [np.nan],
        'turgor_pressure_fruit': [np.nan],
        'osmotic_pressure_fruit': [np.nan],
        'flux_xylem_phloem': [np.nan],
        'transpiration_fruit': [np.nan],
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

        result_DM = growth_DM(
            GR,
            TN_air,
            TX_air,
            TM_air,
            T_fruit=TM_air,
            sunlit_bs=sunlit_bs,
            DM_fruit_0=DM_fruit_0,
            DM_fruit_previous=DM_fruit_previous,
            reserve_stem_previous=reserve_stem_previous,
            reserve_leaf_previous=reserve_leaf_previous,
            LFratio=LFratio,
            dd_delta=dd_delta,
            params=params.dry_matter
        )

        result_FM = growth_FM(
            date,
            T_air,
            GR=np.mean(GR),
            RH=np.mean(RH),
            dd_cum=dd_cum,
            TM_air=np.mean(TM_air),
            DM_fruit_0=DM_fruit_0,
            DM_fruit=result_DM[0],
            DM_fruit_previous=DM_fruit_previous,
            FM_fruit_previous=FM_fruit_previous,
            W_fleshpeel_previous=W_fleshpeel_previous,
            dd_thresh=dd_thresh,
            params=params.fresh_matter
        )

        ## -- outputs of the current day :
        growth_df.loc[i, 9:21] = (*result_FM[0], dd_cum)

        ## -- outputs of the next day :
        growth_df.loc[i + 1, 0:9] = result_FM[1] + (result_DM[1], result_DM[2])

        ## -- cache some variables
        DM_fruit_previous = result_DM[0]
        FM_fruit_previous = result_FM[1][1]
        reserve_leaf_previous = result_DM[1]
        reserve_stem_previous = result_DM[2]
        W_fleshpeel_previous = result_FM[1][3]

        ## -- end of fruit growth - simulation is stopped :
        if np.isnan(stop_sim_ddcum): # if based on sucrose content (default)
            sucrose = result_FM[0][5]
            if sucrose >= sucrose_ripe_thresh:
                if verbose:
                     print('The fruit is ripe: Sucrose threshhold reached.')
                break
        else: # else based on cumulated degree-days
            if dd_cum >= stop_sim_ddcum:
                if verbose:
                     print('The fruit is ripe: Degree-days threshhold reached.')
                break

    return (DAB, growth_df)
