

############################### Lancement des simulations pour x fruits #######################################################################

##### nom du fichier : Utilisation de la fonction MS MF_arbre_simp.r
fruitmodel = function(bloom_date, nb_fruits, leaf_nbs)

{
                                                          
#********************

LF = leaf_nbs/nb_fruits
MS_Init = 0.97 * rnorm(1,mean=13.9,sd=4.1) + 0.03 * rnorm(1,mean=29.2,sd=0.66)   # bimodale de ML
k1runquant = read.table("C:/Users/install/Desktop/stage 3A/script modèle fruit/version juin/k1runquant.txt", header=T, sep="\t")
envirlum <- cbind(k1runquant[,"q10"],k1runquant[,"q25"],k1runquant[,"q50"],k1runquant[,"q75"],k1runquant[,"q90"])
k1_fruit <- envirlum[,sample(5,1,T)]                                            # indice pour choisir un des 5 environnements lumineux à chaque tour


Position = "O"
Ombre = F
temp = F   # le modèle avec la température simulée en 3D

if(Position == "N") Pos = "Nord"
if(Position == "E") Pos = "Est"
if(Position == "O") Pos = "Ouest"
if(Position == "S") Pos = "Sud"
#********************

if (temp == T)
{
# Mettre tout les packages utilsé
library(geometry)
library(rgl)
memory.size(max=4095)

# Ouverture des fonctions nécessaires
Repertoire_Fonction = "C:/Users/nordey/Desktop/Simulation Model Complet/Fonction/"
source(paste(Repertoire_Fonction,"Maillage3D_Fonction.r", sep=""))
source(paste(Repertoire_Fonction,"XYZ_Fonction.r", sep=""))
source(paste(Repertoire_Fonction,"Cos_Angle_Fonction.r", sep=""))
source(paste(Repertoire_Fonction,"Masse_Volumique_Air_Fonction.r", sep=""))
source(paste(Repertoire_Fonction,"Coefficient_Convection_Fonction2.r", sep=""))
source(paste(Repertoire_Fonction,"Maillage_Ultimate_Fonction.r", sep=""))
source(paste(Repertoire_Fonction,"Mesh_Amelio_Fonction.r", sep=""))
source(paste(Repertoire_Fonction,"Maillage_Fonction.r", sep=""))
source(paste(Repertoire_Fonction,"New_Maillage_Fonction.r", sep=""))
source(paste(Repertoire_Fonction,"FONCTION SYNTHESE TEMPERATURE & MF & MS.r", sep=""))
source(paste(Repertoire_Fonction,"Fonction_Croissance_MS.r", sep=""))
source(paste(Repertoire_Fonction, "Fonction_Model_Ethylene.r", sep=""))
source("C:/Users/nordey/Desktop/Simulation Model Complet/FONCTION A AJUSTER.r")

Meteo =   read.table ("C:/Users/nordey/Desktop/Simulation Model Complet/methoraire2000.csv", sep=";", dec=",", header=T)
Meteo$Date = strptime(Meteo$Date, "%d/%m/%Y %H:%M")

Res  = CROISSANCE_MF_TEMPERATURE (
#---------------------- Arguments Temperature ----------------------------------
                        
                        Fruit_Ombre = Ombre,                                   # True ou False
                        Position = Position,                                         # N, NE, E, SE, S, SO
                        Frequence = 30,                                         # Secondes
                        Distance = 10 * 10^-3,                                   # m
                        
                        Date = Tab$Date,                                        # jj/mm/yy hh:mm
                        Temperature_Air= Tab$Temperature_Air,                   # °C
                        Humidite_Relative = Tab$HR,                             # (80%)
                        Vitesse_Vent = Tab$Vent,                                           # m.s-1
                        R_Diffus =  Tab$Rayonnement_Diffus,                                               # Watts.m-2
                        R_Direct = Tab$Rayonnement_Direct,                                               # Watts.m-2
                        Elevation = Tab$Elevation,                                              # Degré
                        Azimut= Tab$Azimut,                                                 # Degré
                                                                       
                       
                        Conductivite = 0.52,
                        Densite = 987,
                        Cp = 3741.8,
                        Asw = 0.32,
                        Gw = 9.04 *10^-4,                                                     # Conductance de la peau, m.s. 
                        K = 0.52,                                                      # W.m-1.K-1              
                        Albedo_Sol = 0.2,                                             # Sans unité
                        Coef_Ray_Ombre = 0.1,                                         # Coefficient= Rayonnement_Ombre / Rayonnement_Soleil
                                               
                        Graphique = T,                                              # True / False      
                        Frequence_Moyenne = "Heure",                                      # Fréquence de la moyenne des température, Jour, Heure, Minute
                        Repertoire =          paste("C:/Users/nordey/Desktop/Simulation Model Complet/Simulations/", Charge, "F/", Pos,"/", sep =""),
                                              
#--------------------- Arguments Croissance MF ------------------------------------

                        DDJ_Init = 352,
                        Conductivite_A =  3.4,
                        Conductivite_B =  2.65,
                        Conductivite_C =  70,
                        Conductivite_D =  0.05,
                        Conductivite_E = 0.72,
                        DDini= 2000,
                        Tau = 1,
                        H = 0.29,
                        MF_Init = MS_Init/0.1,                                   

#--------------------- Arguments Croissance MS ---------------------------------                                                                                                                
                        envirlum = k1runquant[,"q95"],                          # Evolution de l'environnement lumineux dans la journée
                        MS_Init_Division_Cellulaire = MS_Init,                    # Poids du fruit à la fin de la division cellulaire en gramme de MS
                        MS_Debut_Sim = MS_Init,
                        LF = LF                                                # Rapport feuille / fruit [10, 150] 
                        )
}  else {
# Ouverture des fonctions nécessaires
Repertoire_Fonction = "C:/Users/install/Desktop/stage 3A/script modèle fruit/version juin/"
source(paste(Repertoire_Fonction, "FONCTION SYNTHESE TEMPERATURE & MF & MS_simp.r", sep=""))
source(paste(Repertoire_Fonction, "Fonction_Croissance_MS_simp.r", sep=""))
#source(paste(Repertoire_Fonction, "Fonction_Model_Ethylene.r", sep=""))
source(paste(Repertoire_Fonction, "Fonction_Croissance_MF_simp.r", sep=""))

#### attention les données sont celles de 2002 ####
Meteo =   read.table ("C:/Users/install/Desktop/stage 3A/script modèle fruit/version juin/rayostpierre2002.csv", sep=";", dec=".", header=T)
Meteo$Date = strptime(Meteo$Date, "%d/%m/%Y %H:%M")                             #rayonnement station météo Ligne Paradis en J cm-2 
Meteo$DATE = as.Date(Meteo$Date, "%d/%m/%Y")
Meteo_journalier <- read.table ("C:/Users/install/Desktop/stage 3A/script modèle fruit/version juin/tempstpierre2002.csv", sep=";", dec=".", header=T)
Meteo_journalier$DATE = as.Date(Meteo_journalier$DATE, "%d/%m/%Y")

#essai_jour = as.Date("19/08/02", "%d/%m/%y")

Res  = CROISSANCE_MF_TEMPERATURE (
                        
                        Jour_Flo = bloom_date,
                        Tab_DATE = as.Date(Meteo_journalier$DATE, "%d/%m/%Y"),
                        Tab_temperature_Air = Meteo_journalier$TM,
                        Tab_horaire_DATE = as.Date(Meteo$DATE, "%d/%m/%Y"),
                        Tab_horaire_HEURE = Meteo$HEURE,
                        Tab_horaire_Date = Meteo$Date,
                        Tab_horaire_Rayonnement = Meteo$Rayonnement,
                        Tab_horaire_Temperature_Air = Meteo$Temperature_Air,
                        Tab_horaire_HR = Meteo$HR,                                              

#--------------------- Arguments Croissance MF ------------------------------------
                        MF_Init = 23.647 * MS_Init^0.6182,                      # cf fichier allométrie fruit dont L < 105 mm,                                   

#--------------------- Arguments Croissance MS ---------------------------------                                                                                                                
                        envirlum = k1_fruit,                                    # Evolution de l'environnement lumineux dans la journée
                        MS_Init_Division_Cellulaire = MS_Init,                  # Poids du fruit à la fin de la division cellulaire en gramme de MS
                        MS_Debut_Sim = MS_Init,
                        LF = LF                                             # Rapport feuille / fruit [10, 150] 
                        )

}

#************************************ Lance la fonction ethylène ***************

don = Res$Croissance
don$LF = rep(LF,dim(don)[[1]])
don$environ_lum = rep(sum(k1_fruit)/24,dim(don)[[1]])
don$DAB = Res$DAB[1:length(Res$Croissance$Date)]


return(don)
}


#Date = seq(min(don$Date), max(don$Date), 60)
#Fun_MF =    approxfun(as.numeric(don$Date), don$Masse_Fruit)
#Fun_MS =    approxfun(as.numeric(don$Date), don$MS_Fruit)
#Fun_Temp =  approxfun(as.numeric(don$Date), don$Temperature_Fruit)

#MF = Fun_MF(as.numeric(Date))
#MS = Fun_MS(as.numeric(Date))
#temp = Fun_Temp = Fun_Temp(as.numeric(Date))

#Ethy    =     ETHY(   Date = Date,                                                         
#                      MF = MF,                                                          
#                      MS = MS,                                                          
 #                     temp = temp,                                                    
 #                 
#                      ks =  0.03879*0.91,
#                      ko =  0.00551*0.91,                                                    
#                      kMACC = 10^-3/60 ,                                                      
#                      beta = 500/60,                                                        
#                      Q10 = 1.61,                                                          
#                      k4= 5.96 * 10^4 /60,                                          
#                      k5= 1.18*10^5/60,                                             
#                      Perm_C2H4 = 1/(1.32*10^5) * 60,                               
#                      gc= 0.04,                                                     
#                      qm = 1.307 * 10^-6,                                           
                  
#                      MACCini = 5*10^-4,                                                      
#                      C2H4ini = 10^-6,                                                      
#                      ACCini =  10^-5,                                                       
                  
#                      K  = c(0.5, 0.5)                                                            
#                    )

#Fun_C2H4 = approxfun(as.numeric(Ethy$Date), Ethy$C2H4ppm)
#Fun_ACC = approxfun(as.numeric(Ethy$Date), Ethy$ACC)
#Fun_MACC = approxfun(as.numeric(Ethy$Date), Ethy$MACC)
#don$C2H4 = Fun_C2H4(as.numeric(don$Date))
#don$ACC = Fun_ACC(as.numeric(don$Date))
#don$MACC = Fun_MACC(as.numeric(don$Date))

#if(Position == "N") Pos = "Nord"
#if(Position == "E") Pos = "Est"
#if(Position == "O") Pos = "Ouest"
#if(Position == "S") Pos = "Sud"

#write.table (don, paste("C:/Users/nordey/Desktop/Simulation Model Complet/Simulations/", Charge, "F/", Pos, "/", "Resultats.csv", sep =""), sep=";", dec=",", row.names=F)                