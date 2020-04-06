
localdir = getSrcDirectory(function(x) {x})
envdata = getSrcDirectory(function(x) {x})


fruitmodel <- function( bloom_date,
                        nb_fruits,
                        nb_leaves,
                        DM_fruit_0 = NaN,
                        light_envir_sample = NaN,
                        verbose = FALSE
)
  
{
  
  # =============================================================================
  # DATA AND FUNCTION LOADING
  # =============================================================================
  
  ## -- model parameters :
  source(paste(localdir,"parameters_cogshall.r",sep=''), keep.source = TRUE)
  
  ## -- model functions :
  source(paste(localdir,"fruit_growth.r",sep=''),keep.source = TRUE)
  source(paste(localdir,"fruit_dry_matter_growth.r",sep=''), keep.source = TRUE)
  source(paste(localdir,"fruit_fresh_matter_growth.r",sep=''),keep.source = TRUE)
  
  ## -- weather data :
  weather_hourly_df <- read.table(paste(envdata, "weather_hourly_stpierre_2002.csv", sep=''), sep=";", dec=".", header=T)
  weather_hourly_df$DATETIME = strptime(weather_hourly_df$DATETIME, "%d/%m/%Y %H:%M")
  weather_hourly_df$DATE = as.Date(weather_hourly_df$DATETIME, "%d/%m/%Y")
  
  weather_daily_df <- read.table (paste(envdata, "weather_daily_stpierre_2002.csv", sep=''), sep=";", dec=".", header=T)
  weather_daily_df$DATE <- as.Date(weather_daily_df$DATE, "%d/%m/%Y")
  
  ## -- ligth environement (characterized by sunlit fractions of leaves) :
  sunlit_fractions_df <- read.table(paste(envdata,"sunlit_fractions.csv",sep=''), header=T, sep="\t")
  sunlit_fractions_df <- sunlit_fractions_df[,c("q10","q25","q50","q75","q90")]
  if (is.nan(light_envir_sample)) {
    ## if not fixed, randomly sampled
    sunlit_bs <- sunlit_fractions_df[, sample(5,1,T)]                                            
  } else {
    sunlit_bs <- sunlit_fractions_df[, light_envir_sample]
  }
  
  ## -- fruit dry mass at the end of cell division :                              # ???? @ Fred: il y avait une erreur ?
  if (is.nan(DM_fruit_0)) {
    ## if not fixed, randomly sampled within a bimodal distribution (from Léchaudel)
    DM_fruit_0 <- 0.97 * rnorm(1,mean=13.9,sd=4.1) + 
                  0.03 * rnorm(1,mean=29.2,sd=0.66)                               # _parameters to be added in parameter R file
  }
  
  ## -- leaf to fruit ratio :
  LF <- nb_leaves / nb_fruits
  
  
  # =============================================================================
  # RUN THE MODEL
  # =============================================================================
  
  result <- growth( bloom_date = as.Date(bloom_date, "%d/%m/%Y"),
                    weather_daily_DATE = as.Date(weather_daily_df$DATE, "%d/%m/%Y"),
                    weather_daily_TM = weather_daily_df$TM,
                    weather_hourly_DATE = as.Date(weather_hourly_df$DATE, "%d/%m/%Y"),
                    weather_hourly_HOUR = weather_hourly_df$HOUR,
                    weather_hourly_DATETIME = weather_hourly_df$DATETIME,
                    weather_hourly_GR = weather_hourly_df$GR,
                    weather_hourly_T = weather_hourly_df$T,
                    weather_hourly_RH = weather_hourly_df$RH,
                    sunlit_bs = sunlit_bs, 
                    DM_fruit_0 = DM_fruit_0,
                    DM_fruit_ini = DM_fruit_0,
                    FM_fruit_ini = 23.647 * DM_fruit_0 ^ 0.6182,                  # _parameters to be added in parameter R file
                    LF = LF, 
                    verbose = verbose
  )
  

  # =============================================================================
  # OUTPUTS
  # =============================================================================

  growth_df <- result$growth_df
  growth_df$LF <- rep(LF, dim(growth_df)[[1]])
  growth_df$sunlit_bs <- rep(sum(sunlit_bs)/24, dim(growth_df)[[1]])
  growth_df$DAB <- result$DAB[1:length(result$growth_df$DATE)]
  
  return(growth_df)
}

