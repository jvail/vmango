
localdir = './' #getSrcDirectory(function(x) {x})
envdata = '../../../../../../share/environment/' #getSrcDirectory(function(x) {x})
fruitmodel <- function( bloom_date,
                        nb_fruits,
                        nb_leaves,
                        light_envir_sample = NaN,
                        DM_fruit_0 = NaN,
                        DM_fruit_ini = NaN,
                        sim_date_ini = NaN,
                        weather_fname,
                        cultivar_fname = "cogshall",
                        dd_thresh = NaN,
                        stop_sim_ddcum = NaN,
                        verbose = FALSE
)

{

  # ========================================================================================================================
  # DATA AND FUNCTIONS LOADING
  # ========================================================================================================================

  ## -- model parameters :
  source(paste(localdir,"parameters_",cultivar_fname,".r",sep=''), keep.source = TRUE)

  ## -- model functions :
  source(paste(localdir,"fruit_growth.r",sep=''),keep.source = TRUE)
  source(paste(localdir,"fruit_dry_matter_growth.r",sep=''), keep.source = TRUE)
  source(paste(localdir,"fruit_fresh_matter_growth.r",sep=''),keep.source = TRUE)

  ## -- weather data :
  weather_hourly_df <- read.table(paste(envdata,"weather_hourly_",weather_fname,".csv", sep=''), sep=";", dec=".", header=T)
  weather_hourly_df$DATETIME <- strptime(weather_hourly_df$DATETIME, "%d/%m/%Y %H:%M")
  weather_hourly_df$DATE <- as.Date(weather_hourly_df$DATETIME, "%d/%m/%Y")

  weather_daily_df <- read.table (paste(envdata,"weather_daily_",weather_fname,".csv", sep=''), sep=";", dec=".", header=T)
  weather_daily_df$DATE <- as.Date(weather_daily_df$DATE, "%d/%m/%Y")

  ## -- ligth environement (characterized by sunlit fractions of leaves) :
  sunlit_fractions_df <- read.table(paste(envdata,"sunlit_fractions.csv",sep=''), sep="\t", header=T)
  sunlit_fractions_df <- sunlit_fractions_df[,c("q10","q25","q50","q75","q90")]
  if (is.nan(light_envir_sample)) {
    ## if not fixed as input, randomly sampled
    sunlit_bs <- sunlit_fractions_df[, sample(5,1,T)]
  } else {
    sunlit_bs <- sunlit_fractions_df[, light_envir_sample]
  }

  ## -- fruit dry mass at the end of cell division :
  if (is.nan(DM_fruit_0)) {
    ## if not fixed as input, randomly sampled within a bimodal distribution (from Léchaudel)
    DM_fruit_0 <- fruitDM0_weight_1 * rnorm(1, mean = fruitDM0_mu_1, sd = fruitDM0_sigma_1) + fruitDM0_weight_2 * rnorm(1, mean = fruitDM0_mu_2, sd = fruitDM0_sigma_2)
  }

  ## -- fruit dry mass at the first simulation date :
  if (is.nan(DM_fruit_ini)) {
    ## if not fixed as input, set to fruit dry mass at the end of cell division
    DM_fruit_ini <- DM_fruit_0
  }

  ## -- leaf to fruit ratio :
  LFratio <- nb_leaves / nb_fruits


  # ========================================================================================================================
  # RUN THE MODEL
  # ========================================================================================================================

  result <- growth( bloom_date = as.Date(bloom_date, "%d/%m/%Y"),
                    weather_daily_DATE = as.Date(weather_daily_df$DATE, "%d/%m/%Y"),
                    weather_daily_TM = weather_daily_df$TM,
                    weather_daily_TX = weather_daily_df$TX,
                    weather_daily_TN = weather_daily_df$TN,
                    weather_hourly_DATE = as.Date(weather_hourly_df$DATE, "%d/%m/%Y"),
                    weather_hourly_HOUR = weather_hourly_df$HOUR,
                    weather_hourly_DATETIME = weather_hourly_df$DATETIME,
                    weather_hourly_GR = weather_hourly_df$GR,
                    weather_hourly_T = weather_hourly_df$T,
                    weather_hourly_RH = weather_hourly_df$RH,
                    sunlit_bs = sunlit_bs,
                    LFratio = LFratio,
                    DM_fruit_0 = DM_fruit_0,
                    DM_fruit_ini = DM_fruit_ini,
                    sim_date_ini = ifelse(is.nan(sim_date_ini),
                                          sim_date_ini,
                                          as.Date(sim_date_ini, "%d/%m/%Y")),
                    dd_thresh = dd_thresh,
                    stop_sim_ddcum = stop_sim_ddcum,
                    verbose = verbose
  )


  # ========================================================================================================================
  # OUTPUTS
  # ========================================================================================================================

  growth_df <- result$growth_df
  growth_df$LFratio <- rep(LFratio, nrow(growth_df))
  growth_df$sunlit_bs <- rep(sum(sunlit_bs)/24, nrow(growth_df))
  growth_df$DAB <- result$DAB[1:nrow(result$growth_df)]

  return(growth_df)
}
