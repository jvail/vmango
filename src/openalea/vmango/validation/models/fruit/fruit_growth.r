
growth <- function( bloom_date,
                    weather_daily_DATE,
                    weather_daily_TM,
                    weather_daily_TX,
                    weather_daily_TN,
                    weather_hourly_DATE,
                    weather_hourly_HOUR,
                    weather_hourly_DATETIME,
                    weather_hourly_GR,
                    weather_hourly_T,
                    weather_hourly_RH,
                    sunlit_bs,
                    LFratio,
                    DM_fruit_0,
                    DM_fruit_ini,
                    sim_date_ini,
                    dd_thresh,
                    stop_sim_ddcum,
                    verbose = FALSE
)

{

  # ========================================================================================================================
  # WEATHER DATA
  # ========================================================================================================================

  ## -- daily weather data :
  weather_daily_df <- data.frame( DATE = weather_daily_DATE,
                                  TM = weather_daily_TM,
                                  TX = weather_daily_TX,
                                  TN = weather_daily_TN )

  ## -- hourly weather data :
  weather_hourly_df <- data.frame( DATE = weather_hourly_DATE,
                                   HOUR = weather_hourly_HOUR,
                                   DATETIME = weather_hourly_DATETIME,
                                   GR = weather_hourly_GR,
                                   T = weather_hourly_T,
                                   RH = weather_hourly_RH )

  ## -- selection of daily weather data from the date of bloom :
  weather_daily_df <- weather_daily_df[weather_daily_df$DATE >= bloom_date, ]

  ## -- addition of days after bloom to daily weather data :
  weather_daily_df$DAB <- c(0, seq(1:(nrow(weather_daily_df)-1)))

  ## -- addition of cumulated degree-days to daily weather data :
  dd <- vector(mode = "numeric", length = nrow(weather_daily_df))
  for (i in (1:nrow(weather_daily_df))) {
    if ((weather_daily_df$TX[i] + weather_daily_df$TN[i])/2 > Tbase_fruit)  {
      dd[i] <- (weather_daily_df$TX[i] + weather_daily_df$TN[i])/2 - Tbase_fruit
    }
    else {
      dd[i] <- 0
    }
  }
  weather_daily_df$dd_cum <- cumsum(dd)

  ## -- selection of daily weather data from the date of cell division ending :
  ##    (i.e. the date for which |dd_cum - dd_cum_0| is minimal)
  dd_cum_ini <- weather_daily_df$dd_cum[which.min(abs(weather_daily_df$dd_cum - dd_cum_0))]
  weather_daily_fruit_df <- weather_daily_df[weather_daily_df$dd_cum >= dd_cum_ini, ]

  ## -- merging of hourly and daily weather data :
  weather_hourly_fruit_df <- merge( weather_daily_fruit_df, weather_hourly_df, by="DATE")

  ## -- dates and daily DAB values from the date of cell division ending :
  ##    (i.e. dates with dd_cum ≥ dd_cum_0)
  DATE <- unique(weather_hourly_fruit_df[,"DATE"])
  DAB <- unique(weather_hourly_fruit_df$DAB)

  ## -- dates and daily DAB values from the first simulation date :
  ##    (only if sim_date_ini is an model input)
  if (!is.nan(sim_date_ini)) {
    DAB <- DAB[DATE >= sim_date_ini]
    DATE <- DATE[DATE >= sim_date_ini]
  }


  # ========================================================================================================================
  # DATA INITIALIZATION
  # ========================================================================================================================

  ## -- initial amount of carbon in leaf and stem reserves :
  reserve_leaf_ini <- (DM_leaf_unit * LFratio) * r_DM_leaf_ini * cc_leaf
  reserve_stem_ini <- DM_stem * r_DM_stem_ini * cc_stem

  ## -- initial fresh and dry mass of fruit compartements :
  ##    from empirical relationships in Léchaudel (2004)
  FM_fruit_ini <- e_fruitDM2FM_1 * DM_fruit_ini ^ e_fruitDM2FM_2
  W_fleshpeel_ini <- (e_fruit2fleshW_1 * (FM_fruit_ini - DM_fruit_ini) ^ e_fruit2fleshW_2) + 
                     (e_fruit2peelW_1  * (FM_fruit_ini - DM_fruit_ini) ^ e_fruit2peelW_2)
  DM_fleshpeel_ini <- (e_fruit2fleshDM_1 * DM_fruit_ini ^ e_fruit2fleshDM_2) + (e_fruit2peelDM_1 * DM_fruit_ini ^ e_fruit2peelDM_2)
  W_flesh_ini <- e_fleshpeel2fleshW * W_fleshpeel_ini
  DM_flesh_ini <- e_fleshpeel2fleshDM * DM_fleshpeel_ini

  growth_df <- data.frame( date = DATE[1],
                           FM_fruit = FM_fruit_ini,
                           DM_fruit = DM_fruit_ini,
                           W_fleshpeel = W_fleshpeel_ini,
                           DM_fleshpeel = DM_fleshpeel_ini,
                           W_flesh = W_flesh_ini,
                           DM_flesh = DM_flesh_ini,
                           reserve_leaf = reserve_leaf_ini,
                           reserve_stem =  reserve_stem_ini,
                           water_potential_fruit = NA,
                           turgor_pressure_fruit = NA,
                           osmotic_pressure_fruit = NA,
                           flux_xylem_phloem = NA,
                           transpiration_fruit = NA,
                           sucrose = NA,
                           glucose = NA,
                           fructose = NA,
                           soluble_sugars = NA,
                           starch = NA,
                           organic_acids = NA,
                           dd_cum = NA
  )

  result_DM <- list( DM_fruit = DM_fruit_ini,
              reserve_leaf = reserve_leaf_ini,
              reserve_stem =  reserve_stem_ini
  )

  # ========================================================================================================================
  # LOOP OVER TIME
  # ========================================================================================================================

  if(verbose) {
    ## -- initialisazion of the time progress bar :
    cat("\n")
    pb <- txtProgressBar(min=0, max=length(DATE), char="|", width=72, style=3)
  }

  for (i in 1:length(DATE)) {

    ## -- daily data selection :
    weather_hourly_fruit_day_df <- weather_hourly_fruit_df[weather_hourly_fruit_df$DATE == DATE[i],]

    ## -- data ordering according to the hour of the day :
    weather_hourly_fruit_day_df <- weather_hourly_fruit_day_df[order(weather_hourly_fruit_day_df$HOUR),]

    ## -- fruit growth in dry mass :
    result_DM <- growth_DM( GR = weather_hourly_fruit_day_df$GR,
                     TN_air = weather_hourly_fruit_day_df$TN,
                     TX_air = weather_hourly_fruit_day_df$TX,
                     TM_air = weather_hourly_fruit_day_df$TM,
                     T_fruit = weather_hourly_fruit_day_df$TM,
                     sunlit_bs = sunlit_bs,
                     DM_fruit_0 = DM_fruit_0,
                     DM_fruit_previous = result_DM$DM_fruit,
                     reserve_stem_previous = result_DM$reserve_stem,
                     reserve_leaf_previous = result_DM$reserve_leaf,
                     LFratio = LFratio
    )

    ## -- fruit growth in fresh mass :
    result_FM <- growth_FM( date = unique(weather_hourly_fruit_day_df$DATE),
                     T_air = weather_hourly_fruit_day_df$T,
                     GR = weather_hourly_fruit_day_df$GR,
                     RH = weather_hourly_fruit_day_df$RH,
                     dd_cum = mean(weather_hourly_fruit_day_df$dd_cum),
                     TM_air = mean(weather_hourly_fruit_day_df$TM),
                     DM_fruit_0 = DM_fruit_0,
                     DM_fruit = result_DM$DM_fruit,
                     DM_fruit_previous = growth_df[nrow(growth_df),"DM_fruit"],
                     FM_fruit_previous = growth_df[nrow(growth_df),"FM_fruit"],
                     W_fleshpeel_previous = growth_df[nrow(growth_df),"W_fleshpeel"],
                     dd_thresh = dd_thresh
    )


    ## -- outputs of the current day :
    growth_df[i,10:20] <- result_FM[["Results_day"]]
    growth_df[i,21] <- mean(weather_hourly_fruit_day_df$dd_cum)

    ## -- outputs of the next day :
    growth_df[i+1,1:7] <- result_FM[["Results_day_next"]]
    growth_df[i+1,8] <- result_DM$reserve_leaf
    growth_df[i+1,9] <- result_DM$reserve_stem

    ## -- end of fruit growth - simulation is stopped :
    ##    if based on sucrose content (default)
    if (is.nan(stop_sim_ddcum)) {
      if (growth_df[i, "sucrose"] >= sucrose_ripe_thresh) {
        if (verbose) { print("The fruit is ripe") }
        break
      }
    }
    ##    if based on cumulated degree-days
    if (!is.nan(stop_sim_ddcum)) {
      if (growth_df[i, "dd_cum"] >= stop_sim_ddcum) {
        break
      }
    }

    ## -- time progress bar :
    if(verbose) { setTxtProgressBar(pb,i) }
  }


  # ========================================================================================================================
  # OUTPUTS
  # ========================================================================================================================

  Results <- list( DAB = DAB,
                   growth_df = growth_df)

  return(Results)

}
