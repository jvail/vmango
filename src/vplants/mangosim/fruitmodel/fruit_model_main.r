
#localdir = getSrcDirectory(function(x) {x})
envdata = paste(localdir,"/../../../../share/environment/",sep='')

############################### Lancement des simulations pour x fruits #######################################################################
##### nom du fichier : Utilisation de la fonction MS MF_arbre_simp.r
fruitmodel = function(bloom_date, nb_fruits, nb_leaves)

{
#********************

LF = nb_leaves/nb_fruits
MS_Init = 0.97 * rnorm(1,mean=13.9,sd=4.1) + 0.03 * rnorm(1,mean=29.2,sd=0.66)   # bimodale de ML
k1runquant = read.table(paste(envdata,"k1runquant.txt",sep=''), header=T, sep="\t")
envirlum <- cbind(k1runquant[,"q10"],k1runquant[,"q25"],k1runquant[,"q50"],k1runquant[,"q75"],k1runquant[,"q90"])
k1_fruit <- envirlum[,sample(5,1,T)]                                            # indice pour choisir un des 5 environnements lumineux ? chaque tour


Position = "O"
Ombre = F
temp = F   # le mod?le avec la temp?rature simul?e en 3D

if(Position == "N") Pos = "Nord"
if(Position == "E") Pos = "Est"
if(Position == "O") Pos = "Ouest"
if(Position == "S") Pos = "Sud"

#********************

# Ouverture des fonctions n?cessaires
source(paste(localdir,"/fruit_growth.r",sep=''),keep.source = TRUE)
dry_mass_growth = paste(localdir,"/fruit_dry_mass_growth.r",sep='')
source(dry_mass_growth,keep.source = TRUE)
source(paste(localdir,"/fruit_fresh_mass_growth.r",sep=''),keep.source = TRUE)

  
#### attention les donn?es sont celles de 2002 ####
Meteo =   read.table (paste(envdata,"rayostpierre2002.csv",sep=''), sep=";", dec=".", header=T)
Meteo$Date = strptime(Meteo$Date, "%d/%m/%Y %H:%M")                             #rayonnement station m?t?o Ligne Paradis en J cm-2 
Meteo$DATE = as.Date(Meteo$Date, "%d/%m/%Y")

Meteo_journalier <- read.table (paste(envdata,"tempstpierre2002.csv",sep=''), sep=";", dec=".", header=T)
Meteo_journalier$DATE = as.Date(Meteo_journalier$DATE, "%d/%m/%Y")

#Meteo_journalier <- read.table (paste(envdata,"meteo-BMA.csv",sep=''), sep=";", dec=".", header=T)
#Meteo_journalier$DATE = as.Date(Meteo_journalier$Date, "%d/%m/%y")
#Meteo_journalier$TM = Meteo_journalier$Tmoy
#Meteo_journalier = Meteo_journalier[!is.na(Meteo_journalier$TM),]

Res  = CROISSANCE_MF_TEMPERATURE (
                        
                        Jour_Flo = bloom_date,
                        Tab_DATE = as.Date(Meteo_journalier$DATE, "%d/%m/%Y"),
                        #Tab_DATE = Meteo_journalier$DATE,
                        Tab_temperature_Air = Meteo_journalier$TM,
                        
                        Tab_horaire_DATE = as.Date(Meteo$DATE, "%d/%m/%Y"),
                        Tab_horaire_HEURE = Meteo$HEURE,
                        Tab_horaire_Date = Meteo$Date,
                        Tab_horaire_Rayonnement = Meteo$Rayonnement,
                        Tab_horaire_Temperature_Air = Meteo$Temperature_Air,
                        Tab_horaire_HR = Meteo$HR,                                              

#--------------------- Arguments Croissance MF ------------------------------------
                        MF_Init = 23.647 * MS_Init^0.6182,                      # cf fichier allom?trie fruit dont L < 105 mm,                                   

#--------------------- Arguments Croissance MS ---------------------------------                                                                                                                
                        envirlum = k1_fruit,                                    # Evolution de l'environnement lumineux dans la journ?e
                        MS_Init_Division_Cellulaire = MS_Init,                  # Poids du fruit ? la fin de la division cellulaire en gramme de MS
                        MS_Debut_Sim = MS_Init,
                        LF = LF                                             # Rapport feuille / fruit [10, 150] 
                        )


#************************************ Lance la fonction ethyl?ne ***************

don = Res$Croissance
don$LF = rep(LF,dim(don)[[1]])
don$environ_lum = rep(sum(k1_fruit)/24,dim(don)[[1]])
don$DAB = Res$DAB[1:length(Res$Croissance$Date)]

return(don)
}
