
growth <- function( bloom_date,
                    weather_daily_DATE,
                    weather_daily_TM,
                    weather_hourly_DATE,
                    weather_hourly_HOUR,
                    weather_hourly_DATETIME,
                    weather_hourly_GR,
                    weather_hourly_T,
                    weather_hourly_RH,
                    sunlit_bs,
                    DM_fruit_0,
                    DM_fruit_ini,
                    FM_fruit_ini, 
                    LF, 
                    verbose = FALSE
)

{
  
  # =============================================================================
  # WEATHER DATA 
  # =============================================================================
  
  ## -- daily weather data :
  weather_daily_df <- data.frame( DATE = weather_daily_DATE,
                                  TM = weather_daily_TM )
  
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
  
  ## -- addition of cumulated degree-days to daily weather data  :
  dd <- vector(mode = "numeric", length = nrow(weather_daily_df))
  for (i in (1:nrow(weather_daily_df))) {
    if  (weather_daily_df$TM[i] > Tbase)  {
      dd[i] <- weather_daily_df$TM[i] - Tbase
    }  
    else {
      dd[i] <- 0}
  }
  weather_daily_df$dd_cum <- cumsum(dd)
  
  ## -- selection of daily weather data from the date of cell division ending :
  ##    (i.e. dates with dd_cum > dd_cum_0)
  dd_cum_ini <- min(weather_daily_df$dd_cum[weather_daily_df$dd_cum > dd_cum_0])
  weather_daily_fruit_df <- weather_daily_df[weather_daily_df$dd_cum >= dd_cum_ini, ]
  
  ## -- merging of hourly and daily weather data :
  weather_hourly_fruit_df <- merge( weather_daily_fruit_df, weather_hourly_df, by="DATE")
  
  ## -- dates and dalily DAB values from the date of cell division ending :
  ##    (i.e. dates with dd_cum > dd_cum_0)
  DATE <- unique(weather_hourly_fruit_df[,"DATE"])
  DAB <- unique(weather_hourly_fruit_df$DAB)
  
  
  # =============================================================================
  # DATA INITIALIZATION
  # =============================================================================
  
  ## -- initial amount of carbon in leaf and stem reserves :
  reserve_leaf_ini <- (DM_leaf_unit * LF) * r_DM_leaf_ini * cc_leaf
  reserve_stem_ini <- DM_stem * r_DM_stem_ini * cc_stem 
  
  ## -- initial fruit flesh and peel water mass :
  ##    from empirical relationships in Léchaudel (2004)
  W_fleshpeel_ini <- a20 * (FM_fruit_ini - DM_fruit_ini) ^ a21 + 
                     a22 * (FM_fruit_ini - DM_fruit_ini) ^ a23
  
  growth_df <- data.frame( DATE = DATE[1],
                           FM_fruit = FM_fruit_ini,
                           DM_fruit = DM_fruit_ini,
                           W_fleshpeel = W_fleshpeel_ini,
                           reserve_leaf = reserve_leaf_ini,
                           reserve_stem =  reserve_stem_ini,
                           water_potential = NA,
                           turgor_pressure = NA,
                           osmotic_pressure = NA,
                           flux_xylem = NA,
                           flux_phloem = NA,
                           transpiration = NA,
                           sucrose = NA,
                           soluble_sugars = NA,
                           organic_acids = NA,
                           dd_cum = NA
  )
  
  DM <- list( DM_fruit = DM_fruit_ini,
              reserve_leaf = reserve_leaf_ini,
              reserve_stem =  reserve_stem_ini 
  )
  
  # =============================================================================
  # LOOP OVER TIME
  # =============================================================================
  
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
    DM_fruit_previous <- DM$DM_fruit
    DM <- growth_DM( GR = weather_hourly_fruit_day_df$GR,
                     T_air = weather_hourly_fruit_day_df$TM,
                     T_fruit = weather_hourly_fruit_day_df$TM,
                     sunlit_bs = sunlit_bs,
                     DM_fruit_0 = DM_fruit_0,
                     DM_fruit_previous = DM$DM_fruit,
                     reserve_stem = DM$reserve_stem,
                     reserve_leaf = DM$reserve_leaf,
                     LF = LF
    )
    
    ## -- fruit growth in fresh mass :
    FM <- growth_FM( date = unique(weather_hourly_fruit_day_df$DATE),
                     T_air = weather_hourly_fruit_day_df$T,
                     GR = weather_hourly_fruit_day_df$GR,
                     RH = weather_hourly_fruit_day_df$RH,
                     dd_cum = mean(weather_hourly_fruit_day_df$dd_cum),
                     T_air_daily = mean(weather_hourly_fruit_day_df$TM),
                     DM_fruit = c(DM_fruit_previous, DM$DM_fruit),
                     FM_fruit_ini = FM_fruit_ini,
                     W_fleshpeel_ini = W_fleshpeel_ini,
                     DM_fruit_0 = DM_fruit_0
    )
    
    ## -- outputs of the previous day :
    growth_df[i,7:15] <- FM[["Results_day"]]
    growth_df[i,16] <- mean(weather_hourly_fruit_day_df$dd_cum)
    
    ## -- outputs of the current day :
    growth_df[i+1,1:4] <- FM[["Results_day_next"]]
    growth_df[i+1,5] <- DM$reserve_leaf
    growth_df[i+1,6] <- DM$reserve_stem
    
    ## -- end of fruit growth (when sucrose content ≥ 0.04)  :
    if ( growth_df[i,"sucrose"] >= 0.04)
    {
      ## simulation is stopped
      if(verbose) { print("The fruit is ripe") }
      break                         
    }
    if(verbose) {setTxtProgressBar(pb,i) }
  }
  
  
  # =============================================================================
  # OUTPUTS
  # =============================================================================
  
  Results <- list( DAB = DAB,
                   growth_df = growth_df)
  
  return(Results)
  
} 



