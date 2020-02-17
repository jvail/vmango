# Calcul de la température de base pour les stades phénologiques reproducteurs du manguier regroupés par stade
# Section 4.4 "Données pour les modèles de temps thermique" du rapport

setwd("D:/MesDonnées/mes documents/manguier/modélisation phéno/stage 2014/Anne-Sarah Briand/données/fichiers de données/températures de base (Anaëlle)/TB stades phéno/")                # pour mon ordi

# Calcul de la température de base pour la croissance des UCs du manguier

# source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/Modèles de temps thermique/Approche deterministe/Development/Temp vs 1 sur n/creation base nTInflo dvlpt.r")           # original Anaëlle

#source("D:/MesDonnées/mes documents/manguier/modélisation phéno/stage 2014/Anne-Sarah Briand/données/fichiers de données/températures de base (Anaëlle)/TB stades phéno/creation base nTInflo dvlpt2.r")       # pour mon ordi
#nTInflo[1:10,]

# chargement directement du fichier de données:
nTInflo <- read.csv( "base developpement inflo.csv", sep=";", dec=".", header=T)
nTInflo[1:10,]
summary(nTInflo)


#Chargement des fonctions: 
#source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/Modèles de temps thermique/Approche deterministe/Development/Autres/fonctions/tempbasecor.r")
#source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/Modèles de temps thermique/Approche deterministe/Development/Autres/fonctions/tempbaseCVST.r")
#source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/Modèles de temps thermique/Approche deterministe/Development/Autres/fonctions/tempbasesdj.r")
#source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/Modèles de temps thermique/Approche deterministe/Development/Autres/fonctions/tempbasesdST.r")
#Les fonctions sont définies pour TMUC et nUC

# pour mon ordi
source("D:/MesDonnées/mes documents/manguier/thèses/Anaëlle/CD fin de thèse/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/Modèles de temps thermique/Approche deterministe/Croissance/Autres/fonctions/tempbasecor.r")
source("D:/MesDonnées/mes documents/manguier/thèses/Anaëlle/CD fin de thèse/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/Modèles de temps thermique/Approche deterministe/Croissance/Autres/fonctions/tempbaseCVST.r")
source("D:/MesDonnées/mes documents/manguier/thèses/Anaëlle/CD fin de thèse/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/Modèles de temps thermique/Approche deterministe/Croissance/Autres/fonctions/tempbasesdj.r")
source("D:/MesDonnées/mes documents/manguier/thèses/Anaëlle/CD fin de thèse/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/Modèles de temps thermique/Approche deterministe/Croissance/Autres/fonctions/tempbasesdST.r")


############ Cogshall############################################

######### Stade D = D1 + D2
UCcog<-nTInflo[nTInflo$cultivar=="cog",] 
UCcog<-UCcog[!is.na(UCcog$n_D1),] 
UCcog<-UCcog[!is.na(UCcog$n_D2),] 
head(UCcog)
dim(UCcog)

# calcul de la durée totale du stade D = D1 + D2
UCcog$n_D <- UCcog$n_D1 + UCcog$n_D2

# calcul de la température moyenne pendant ce stade (à partir des températures moyennes de chaque sous-stade et de leur durée
UCcog$T_D <- ((UCcog$T_D1 * UCcog$n_D1) + (UCcog$T_D2 * UCcog$n_D2))/ UCcog$n_D

#Ajustement  linéaire
y<- 1/(UCcog$n_D)
x<- UCcog$T_D
plot(x,y)
sum<-summary(lm( y ~ x))
-sum[[4]][1]/sum[[4]][2]	# 13.50785 °C

UCcog$nUC<-UCcog$n_D
UCcog$TMUC<-UCcog$T_D

# minimisation de l'écart type des sommes de température
nlm(tempb.sdST, 15, dat=UCcog)            # 16.8582°C

# minimisation du coefficient de variation des sommes de température
nlm(tempb.CVST, 15, dat=UCcog)            # 9.299172 °C

# minimisation de l'écart type des sommes de température en jours
nlm(tempb.sdj, 15, dat=UCcog)            # 9.952003°C

# minimisation du coef de corrélation entre les sommes de températures et les températures moyennes
nlm(tempb.cor, 15, dat=UCcog)            #  11.63077 °C

#TB moy : 
(13.50785 + 9.299172 + 9.952003 + 11.63077)/4			# 11.09745 °C

# calcul de la somme de température moyenne pour le stade D de l'inflo de Cogshall avec TB = 11.09745°C
STUCcog <- UCcog$n_D*(UCcog$T_D - 11.09745)
mean(STUCcog)                     # 70.56381 °C.j
sd(STUCcog)                       # 15.68497 °C.j



######### Stade E   (calcul sur l'ensemble des données non conservé (cf plus bas calcul avec retrait de certains points))

#UCcog<-nTInflo[nTInflo$cultivar=="cog",] 
#UCcog<-UCcog[!is.na(UCcog$n_E),] 
#head(UCcog)
#dim(UCcog)

  
#Ajustement linéaire
#y<- 1/(UCcog$n_E)
#x<- UCcog$T_E
#plot(x,y)
#sum<-summary(lm( y ~ x))
#-sum[[4]][1]/sum[[4]][2]	# 4.038026 °C


#UCcog$nUC<-UCcog$n_E
#UCcog$TMUC<-UCcog$T_E

# minimisation de l'écart type des sommes de température
#nlm(tempb.sdST, 10, dat=UCcog)            # 15.16614 °C

# minimisation du coefficient de variation des sommes de température
#nlm(tempb.CVST, 10, dat=UCcog)            #   5.959693°C

# minimisation de l'écart type des sommes de température en jours
#nlm(tempb.sdj, 10, dat=UCcog)            #  6.415527°C

# minimisation du coef de corrélation entre les sommes de températures et les températures moyennes
#nlm(tempb.cor, 10, dat=UCcog)            #  5.095514 °C

#TB moy : 
#mean(c(4.038026,5.959693,6.415527,5.095514))		# 5.37719

# calcul de la somme de température moyenne pour le stade E de l'inflo de Cogshall avec TB = 5.37719°C
#STUCcog <- UCcog$n_E*(UCcog$T_E - 5.37719)
#mean(STUCcog)                     #  172.3539 °C.j
#sd(STUCcog)                       # 31.76508 °C.j


 ######### Stade E en retirant les 3 points à des températures > 25°C      (calcul conservé)

UCcog<-nTInflo[nTInflo$cultivar=="cog",] 
UCcog<-UCcog[!is.na(UCcog$n_E),] 
UCcog<-UCcog[UCcog$T_E<25,]
head(UCcog)
dim(UCcog)

  
#Ajustement linéaire
y<- 1/(UCcog$n_E)
x<- UCcog$T_E
plot(x,y)
sum<-summary(lm( y ~ x))
-sum[[4]][1]/sum[[4]][2]	# 8.58667 °C


UCcog$nUC<-UCcog$n_E
UCcog$TMUC<-UCcog$T_E

# minimisation de l'écart type des sommes de température
nlm(tempb.sdST, 10, dat=UCcog)            # 16.05113 °C

# minimisation du coefficient de variation des sommes de température
nlm(tempb.CVST, 10, dat=UCcog)            #   8.553527°C

# minimisation de l'écart type des sommes de température en jours
nlm(tempb.sdj, 10, dat=UCcog)            #  8.943436°C

# minimisation du coef de corrélation entre les sommes de températures et les températures moyennes
nlm(tempb.cor, 10, dat=UCcog)            #  8.588423 °C

#TB moy : 
mean(c(8.58667,8.553527,8.943436,8.588423))		# 8.668014

# calcul de la somme de température moyenne pour le stade E de l'inflo de Cogshall avec TB = 8.668014°C
STUCcog <- UCcog$n_E*(UCcog$T_E - 8.668014)
mean(STUCcog)                     #  133.3164°C.j
sd(STUCcog)                       # 24.85125 °C.j



######### Stade F = F1 + F2

UCcog<-nTInflo[nTInflo$cultivar=="cog",] 
UCcog<-UCcog[!is.na(UCcog$n_F1),] 
UCcog<-UCcog[!is.na(UCcog$n_F2),] 
head(UCcog)
dim(UCcog)

# calcul de la durée totale du stade F = F1 + F2
UCcog$n_F <- UCcog$n_F1 + UCcog$n_F2

# calcul de la température moyenne pendant ce stade (à partir des températures moyennes de chaque sous-stade et de leur durée
UCcog$T_F <- ((UCcog$T_F1 * UCcog$n_F1) + (UCcog$T_F2 * UCcog$n_F2))/ UCcog$n_F

 
 #Ajustement linéaire
y<- 1/(UCcog$n_F)
x<- UCcog$T_F
plot(x,y)
sum<-summary(lm( y ~ x))
-sum[[4]][1]/sum[[4]][2]			# 15.08707°C

UCcog$nUC<-UCcog$n_F
UCcog$TMUC<-UCcog$T_F

# minimisation de l'écart type des sommes de température
nlm(tempb.sdST, 10, dat=UCcog)            # 17.40735 °C

# minimisation du coefficient de variation des sommes de température
nlm(tempb.CVST, 13, dat=UCcog)            # 14.99326°C

# minimisation de l'écart type des sommes de température en jours
nlm(tempb.sdj, 15, dat=UCcog)            #   15.22784°C

# minimisation du coef de corrélation entre les sommes de températures et les températures moyennes
nlm(tempb.cor, 10, dat=UCcog)            #   15.13225°C

#TB moy : 
mean(c(15.08707, 14.99326, 15.22784, 15.13225))			# 15.11011

# calcul de la somme de température moyenne pour l'élongation de l'inflo de Cogshall avec TB =  15.11011°C
STUCcog <- UCcog$n_F*(UCcog$T_F -  15.11011)
mean(STUCcog)                     # 230.4264°C.j
sd(STUCcog)                       # 40.18989 °C.j

