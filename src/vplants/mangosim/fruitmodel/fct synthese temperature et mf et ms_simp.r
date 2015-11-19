############################### Combinaison des Models de Temperature (Simplifié) et du model de croissance en MF #######################################################################

##### nom du fichier : FONCTION SYNTHESE TEMPERATURE & MF & MS.r

CROISSANCE_MF_TEMPERATURE = function
                        (
                        Jour_Flo,                                                   # Jour floraison (DAB = 0)
                        Tab_DATE,
                        Tab_temperature_Air,
                        Tab_horaire_DATE,
                        Tab_horaire_HEURE,
                        Tab_horaire_Date,
                        Tab_horaire_Rayonnement,
                        Tab_horaire_Temperature_Air,
                        Tab_horaire_HR,                                              
#--------------------- Arguments Croissance MF ---------------------------------
                        MF_Init,                                                # Matière fraiche initiale
                        
#--------------------- Arguments Croissance MS ---------------------------------                                                                                                                
                        envirlum,                                               # Evolution de l'environnement lumineux dans la journée
                        MS_Init_Division_Cellulaire,                            # Poids du fruit à la fin de la division cellulaire en gramme de MS
                        MS_Debut_Sim, 
                        LF                                                      # Rapport feuille / fruit [10, 150] 
                        )

    {   

Tab_journalier = data.frame(DATE = Tab_DATE, TM = Tab_temperature_Air)
Tab_journalier = Tab_journalier[Tab_journalier$DATE >= Jour_Flo,]
Tab_journalier$DAB = c(0,seq (1:c(length(Tab_journalier$DATE)-1)))              # construction colonne DAB

Tab_horaire = data.frame(DATE = Tab_horaire_DATE, HEURE = Tab_horaire_HEURE, Rayonnement = Tab_horaire_Rayonnement, Temperature_Air = Tab_horaire_Temperature_Air,
                         HR = Tab_horaire_HR, Date = Tab_horaire_Date)

Tbase = 16                                                                      # calcul des degrés jours à partir DAB = 0
m = vector(mode="numeric",length=nrow(Tab_journalier))
for (i in (1:nrow(Tab_journalier)))
{if  (Tab_journalier$TM[i] > Tbase)  m[i] = Tab_journalier$TM[i] - Tbase  else m[i] = 0  }
calcddj = as.vector(cumsum(m))
Tab_journalier$ddj = calcddj
ddjy = 352.72                                                                  # fin de la multiplication cellulaire dans le fruit (température base 16 °C) 
ddjini = min(Tab_journalier$ddj[Tab_journalier$ddj>ddjy])                      # valeur la plus proche et supérieure à ddjy dans Tab_journalier

DDJ_Init =  ddjini
Tab_journalier_fruit = Tab_journalier[Tab_journalier$ddj >= DDJ_Init,]
DAB_Init = Tab_journalier_fruit[Tab_journalier_fruit$ddj==DDJ_Init,"DAB"]                         # DAB réélle du fruit à laquelle débute la simulation

Tab_horaire_fruit = merge(Tab_journalier_fruit,Tab_horaire,by="DATE")
DATE = unique(Tab_horaire_fruit[,"DATE"])
DAB_sim = unique(Tab_horaire_fruit$DAB)
###----------------------------------------------------------------------------------------------------

#---Initialisation des valeurs des paramètres

Croissance = data.frame(Date = DATE[1], Masse_Fruit = MF_Init, MS_Fruit = MS_Debut_Sim,
                        Eaupepu = 0.4086 * (MF_Init - MS_Debut_Sim)^0.7428 + 0.5874 * (MF_Init - MS_Debut_Sim)^1.0584, Potentiel_Hydrique = NA,
                        P_Turgescence = NA, P_Osmotique = NA, Xyleme = NA, Phloeme = NA, Transpiration = NA, Saccharose = NA, sucres_solubles = NA,
                        acides_organiques = NA)
                        
MS =          list( MS_Fruit =        MS_Debut_Sim,
                    Reserve_Feuille = 0.1 * 0.4387 * (41.83 + 77.41)/2,
                    Reserve_Rameau =  0.8 * 0.074 * LF * 0.4051 )  

#--------------------------------------------------------- DEBUT DE LA BOUCLE ----------------------------------------------------------------------------------------------------------------- 

cat("\n")
pb = txtProgressBar(min=0,max=length(DATE), char="|", width=72, style=3)        # Initialisation barre progression, sur boucle i

  for (i in 1:length(DATE)[1])                                                                           
{
Table_Croissance = Tab_horaire_fruit[Tab_horaire_fruit$DATE==DATE[i],]
Table_Croissance = Table_Croissance[order(Table_Croissance$HEURE),]

#----------------- Fonction croissance en Matière sèche
MS_Fruit_Precedent = MS$MS_Fruit 
MS = CROISSANCE_MS (  Rayonnement =       Table_Croissance$Rayonnement,         # en J.cm-2 cumulé sur l'heure
                      Temperature_Air =   Table_Croissance$TM,                  # Température journalière de l'air
                      Temperature_Fruit = Table_Croissance$TM,                  # Température journali du fruit.
                      envirlum =          envirlum,                             # Evolution de l'environnement lumineux dans la journée
                      Poids_Fruit_Init =   MS_Init_Division_Cellulaire,         # Poids du fruit à la fin de la division cellulaire en gramme de MS
                      MS_Fruit_Precedent = MS$MS_Fruit,                         # en gramme de MS
                      Reserve_Rameau =     MS$Reserve_Rameau,                   # en gramme de carbone
                      Reserve_Feuille =    MS$Reserve_Feuille,                  # en gramme de carbone
                      LF = LF                                                   # Rapport feuille / fruit [10, 150] 
                    )
#------------------ Fonction de croissance en Matière fraiche
MF = CROISSANCE_MF  ( Date =              unique(Table_Croissance$DATE),
                      Temperature_Air =   Table_Croissance$Temperature_Air,     # dynamique horaire
                      rayo =              Table_Croissance$Rayonnement,         # dynamique horaire
                      humirela =          Table_Croissance$HR,                  # dynamique horaire  
                      ddj =               Table_Croissance$ddj,
                      Temp_air_moy =      mean(Table_Croissance$TM),            # donnée journalière moyenne
                      MSfruit =           c(MS_Fruit_Precedent, MS$MS_Fruit),
                      MF_Init =           Croissance[dim(Croissance)[1],"Masse_Fruit"],
                      Eaupepu_Init =      Croissance[dim(Croissance)[1],"Eaupepu"],
                      Poids_Fruit_Init =   MS_Init_Division_Cellulaire,         # Poids du fruit à la fin de la division cellulaire en gramme de MS
                      H = 0.002027 ,                                            # Pression Seuil Y ~ H * Volume Fruit, pour la croissance, Parametre à Estimer.
                      Phiini = 0.414,                                           #  Taux d'accroissement cellulaire (MPa / jour, article 2007)
                      DDini = 2000,                                             # DDJ à partir duquel extensibilité cellulaire (Phiini décroit).
                      Tau = 0.966,                                              # Tau de décroissance de l'extensibilité cellulaire.
                      aLf = 0.3732                                              # Parametres pour calculer la conductivité hydraulique (param papier 2007 * 24)
                     )

  Croissance[i,5:13]  = MF[["Results_jour"]]
  Croissance[i+1,1:4] = MF[["Results_jour_suivant"]]

if ( Croissance[i,"Saccharose"] >= 0.04)
{
	cat("Le fruit est mûr \n")
	break                         # sort de la boucle et arrête les simulations 
}  
setTxtProgressBar(pb,i)
       }                                                                        # Fin de la boucle sur i

return(list(DAB = DAB_sim, Croissance=Croissance))                     
}                                                                                # Fin de la fonction
                   
