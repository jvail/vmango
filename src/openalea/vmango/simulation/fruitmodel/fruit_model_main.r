
#localdir = getSrcDirectory(function(x) {x})
envdata = paste(localdir,"/../../../../../share/environment/",sep='')

############################### Lancement des simulations pour x fruits #######################################################################
##### nom du fichier : Utilisation de la fonction MS MF_arbre_simp.r
fruitmodel = function(bloom_date, nb_fruits, nb_leaves, verbose=FALSE, DM_fruit_0=NaN, sunlit_bs_sample=NaN)

{
#********************

LF = nb_leaves/nb_fruits
if (is.nan(DM_fruit_0)) {
    DM_fruit_0 = 0.97 * rnorm(1,mean=13.9,sd=4.1) + 0.03 * rnorm(1,mean=29.2,sd=0.66)   # bimodale de ML
}

sunlit_fractions_df = read.table(paste(envdata,"sunlit_fractions.csv",sep=''), header=T, sep="\t")
sunlit_fractions <- cbind(sunlit_fractions_df[,"q10"],sunlit_fractions_df[,"q25"],sunlit_fractions_df[,"q50"],sunlit_fractions_df[,"q75"],sunlit_fractions_df[,"q90"])
if (is.nan(sunlit_bs_sample)) {
    sunlit_bs <- sunlit_fractions[,sample(5,1,T)]                                            # indice pour choisir un des 5 environnements lumineux ? chaque tour
} else {
    sunlit_bs <- sunlit_fractions[,sunlit_bs_sample]
}

#********************

# Ouverture des fonctions n?cessaires
source(paste(localdir,"/fruit_growth.r",sep=''),keep.source = TRUE)
dry_mass_growth = paste(localdir,"/fruit_dry_mass_growth.r",sep='')
source(dry_mass_growth,keep.source = TRUE)
source(paste(localdir,"/fruit_fresh_mass_growth.r",sep=''),keep.source = TRUE)


#### attention les donn?es sont celles de 2002 ####
weather_hourly_df =   read.table (paste(envdata,"weather_hourly_stpierre_2002.csv",sep=''), sep=";", dec=".", header=T)
weather_hourly_df$DATETIME = strptime(weather_hourly_df$DATETIME, "%d/%m/%Y %H:%M")                             #rayonnement station m?t?o Ligne Paradis en J cm-2
weather_hourly_df$DATE = as.Date(weather_hourly_df$DATETIME, "%d/%m/%Y")

weather_daily_df <- read.table (paste(envdata,"weather_daily_stpierre_2002.csv",sep=''), sep=";", dec=".", header=T)
weather_daily_df$DATE = as.Date(weather_daily_df$DATE, "%d/%m/%Y")

result  = growth (

                        bloom_date = as.Date(bloom_date, "%d/%m/%Y"),
                        weather_daily_DATE = as.Date(weather_daily_df$DATE, "%d/%m/%Y"),
                        #Tab_DATE = weather_daily_df$DATE,
                        weather_daily_TM = weather_daily_df$TM,

                        weather_hourly_DATE = as.Date(weather_hourly_df$DATE, "%d/%m/%Y"),
                        weather_hourly_HOUR = weather_hourly_df$HOUR,
                        weather_hourly_DATETIME = weather_hourly_df$DATETIME,
                        weather_hourly_GR = weather_hourly_df$GR,
                        weather_hourly_T = weather_hourly_df$T,
                        weather_hourly_HR = weather_hourly_df$HR,

#--------------------- Arguments Croissance MF ------------------------------------
                        FM_fruit_ini = 23.647 * DM_fruit_0^0.6182,                      # cf fichier allom?trie fruit dont L < 105 mm,

#--------------------- Arguments Croissance MS ---------------------------------
                        sunlit_bs = sunlit_bs,                                    # Evolution de l'environnement lumineux dans la journ?e
                        DM_fruit_0 = DM_fruit_0,                  # Poids du fruit ? la fin de la division cellulaire en gramme de MS
                        DM_fruit_ini = DM_fruit_0,
                        LF = LF,                                             # Rapport feuille / fruit [10, 150]
                        verbose=verbose
                        )


#************************************ Lance la fonction ethyl?ne ***************

growth_df = result$growth_df
growth_df$LF = rep(LF,dim(growth_df)[[1]])
growth_df$sunlit_bs = rep(sum(sunlit_bs)/24,dim(growth_df)[[1]])
growth_df$DAB = result$DAB[1:length(result$growth_df$DATE)]

return(growth_df)
}
