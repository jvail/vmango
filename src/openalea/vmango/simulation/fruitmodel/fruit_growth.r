############################### Combinaison des Models de Temperature (Simplifi?) et du model de croissance en MF #######################################################################

##### nom du fichier : FONCTION SYNTHESE TEMPERATURE & MF & MS.r

growth = function
                        (
                        bloom_date,                                                   # Jour floraison (DAB = 0)
                        weather_daily_DATE,
                        weather_daily_TM,
                        weather_hourly_DATE,
                        weather_hourly_HOUR,
                        weather_hourly_DATETIME,
                        weather_hourly_GR,
                        weather_hourly_T,
                        weather_hourly_HR,
#--------------------- Arguments Croissance MF ---------------------------------
                        FM_fruit_ini,                                                # Mati?re fraiche initiale

#--------------------- Arguments Croissance MS ---------------------------------
                        sunlit_bs,                                               # Evolution de l'environnement lumineux dans la journ?e
                        DM_fruit_0,                            # Poids du fruit ? la fin de la division cellulaire en gramme de MS
                        DM_fruit_ini,
                        LF,                                                       # Rapport feuille / fruit [10, 150]
                        verbose = FALSE
                        )

{

weather_daily_df = data.frame(DATE = weather_daily_DATE, TM = weather_daily_TM)
weather_daily_df = weather_daily_df[weather_daily_df$DATE >= bloom_date,]
weather_daily_df$DAB = c(0,seq (1:c(length(weather_daily_df$DATE)-1)))              # construction colonne DAB

weather_hourly_df = data.frame(DATE = weather_hourly_DATE, HOUR = weather_hourly_HOUR, GR = weather_hourly_GR, T = weather_hourly_T,
                         HR = weather_hourly_HR, DATETIME = weather_hourly_DATETIME)

Tbase = 16                                                                      # calcul des degr?s jours ? partir DAB = 0
dd = vector(mode="numeric",length=nrow(weather_daily_df))
for (i in (1:nrow(weather_daily_df)))
{
    if  (weather_daily_df$TM[i] > Tbase)  dd[i] = weather_daily_df$TM[i] - Tbase  else dd[i] = 0
}
dd_cum = as.vector(cumsum(dd))
weather_daily_df$dd_cum = dd_cum
dd_cum_0 = 352.72                                                                  # fin de la multiplication cellulaire dans le fruit (temp?rature base 16 ?C)
dd_cum_ini = min(weather_daily_df$dd_cum[weather_daily_df$dd_cum>dd_cum_0])                      # valeur la plus proche et sup?rieure ? dd_cum_0 dans weather_daily_df

weather_daily_fruit_df = weather_daily_df[weather_daily_df$dd_cum >= dd_cum_ini,]
# DAB_Init = weather_daily_fruit_df[weather_daily_fruit_df$dd_cum==dd_cum_ini,"DAB"]                         # DAB r??lle du fruit ? laquelle d?bute la simulation

weather_hourly_fruit_df = merge(weather_daily_fruit_df,weather_hourly_df,by="DATE")
DATE = unique(weather_hourly_fruit_df[,"DATE"])
DAB = unique(weather_hourly_fruit_df$DAB)
###----------------------------------------------------------------------------------------------------

#---Initialisation des valeurs des param?tres
reserve_leaf_ini = 0.8 * 0.074 * LF * 0.4051
reserve_stem_ini  = 0.1 * 0.4387 * (41.83 + 77.41)/2
W_fleshpeel_ini = 0.4086 * (FM_fruit_ini - DM_fruit_ini)^0.7428 + 0.5874 * (FM_fruit_ini - DM_fruit_ini)^1.0584

#### MODIF MAY17

growth_df = data.frame(DATE = DATE[1],
                        FM_fruit = FM_fruit_ini,
                        DM_fruit = DM_fruit_ini,
                        W_fleshpeel = W_fleshpeel_ini,
                        #### MODIF MAY17
                        reserve_leaf = reserve_leaf_ini,
                        reserve_stem =  reserve_stem_ini,
                        #### MODIF MAY17
                        water_potential = NA,
                        turgor_pressure = NA,
                        osmotic_pressure = NA,
                        flux_xyleme = NA,
                        flux_phloeme = NA,
                        transpiration = NA,
                        sucrose = NA,
                        soluble_sugars = NA,
                        organic_acids = NA)

DM =          list( DM_fruit =        DM_fruit_ini,
                    reserve_leaf = reserve_leaf_ini,
                    reserve_stem =  reserve_stem_ini )

#--------------------------------------------------------- DEBUT DE LA BOUCLE -----------------------------------------------------------------------------------------------------------------

if(verbose) {
  cat("\n")
  pb = txtProgressBar(min=0,max=length(DATE), char="|", width=72, style=3)        # Initialisation barre progression, sur boucle i
}

for (i in 1:length(DATE)[1])
{
    weather_hourly_fruit_day_df = weather_hourly_fruit_df[weather_hourly_fruit_df$DATE==DATE[i],]
    weather_hourly_fruit_day_df = weather_hourly_fruit_day_df[order(weather_hourly_fruit_day_df$HOUR),]

    #----------------- Fonction croissance en Mati?re s?che
    DM_fruit_previous = DM$DM_fruit
    DM = growth_DM (  GR =       weather_hourly_fruit_day_df$GR,         # en J.cm-2 cumul? sur l'heure
                          T_air =   weather_hourly_fruit_day_df$TM,                  # Temp?rature journali?re de l'air
                          T_fruit = weather_hourly_fruit_day_df$TM,                  # Temp?rature journali du fruit.
                          sunlit_bs =          sunlit_bs,                             # Evolution de l'environnement lumineux dans la journ?e
                          DM_fruit_0 =   DM_fruit_0,         # Poids du fruit ? la fin de la division cellulaire en gramme de MS
                          DM_fruit_previous = DM_fruit_previous,                         # en gramme de MS
                          reserve_stem =     DM$reserve_stem,                   # en gramme de carbone
                          reserve_leaf =    DM$reserve_leaf,                  # en gramme de carbone
                          LF = LF                                                   # Rapport feuille / fruit [10, 150]
                        )
    #------------------ Fonction de croissance en Mati?re fraiche
    FM = growth_FM  ( date =              unique(weather_hourly_fruit_day_df$DATE),
                          T_air =   weather_hourly_fruit_day_df$T,     # dynamique horaire
                          GR =              weather_hourly_fruit_day_df$GR,         # dynamique horaire
                          RH =          weather_hourly_fruit_day_df$HR,                  # dynamique horaire
                          dd_cum =               weather_hourly_fruit_day_df$dd_cum,
                          T_air_daily =      mean(weather_hourly_fruit_day_df$TM),            # donn?e journali?re moyenne
                          DM_fruit =           c(DM_fruit_previous, DM$DM_fruit),
                          FM_fruit_ini =           FM_fruit_ini,
                          W_fleshpeel_ini =       W_fleshpeel_ini,
                          DM_fruit_0 =  DM_fruit_0         # Poids du fruit ? la fin de la division cellulaire en gramme de MS                                          # Parametres pour calculer la conductivit? hydraulique (param papier 2007 * 24)
                         )

      growth_df[i,7:15]  = FM[["Results_jour"]]
      growth_df[i+1,1:4] = FM[["Results_jour_suivant"]]

      #### MODIF MAY17
      growth_df[i+1,5] = DM$reserve_leaf
      growth_df[i+1,6] = DM$reserve_stem

    if ( growth_df[i,"sucrose"] >= 0.04)
    {
      if(verbose) {print("Le fruit est mur")}
    	break                         # sort de la boucle et arr?te les simulations
    }
    if(verbose) {setTxtProgressBar(pb,i) }
}                                                                        # Fin de la boucle sur i

return(list(DAB=DAB, growth_df=growth_df))
}                                                                                # Fin de la fonction
