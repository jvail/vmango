# Mise en forme des données pour calculer la logvraisemblance des dates d'arrêt de croissance des UCs
# fonction de calcul de la logvraisemblance: modpopulation.R
# données de départ: le temps thermique total pour atteindre la fin de la croissance en longueur des UC
# STUCcog dans le script 'calcul temp base UC.r'
# Il faut avoir pour chaque valeur du temps thermique nécessaire à l'arrêt de la croissance (t), le nombre d'UC correspondantes, et le temps thermique la veille (t - 1)

setwd("D:/MesDonnées/mes documents/manguier/modélisation phéno/stage 2014/Anne-Sarah Briand/données/fichiers de données/températures de base (Anaëlle)/TB durée croissance/bases et scripts/")

 nT <- read.csv( "nT.csv", sep=";", dec=".", header=T)
head(nT)

# récupération des données Cogshall
UCcog<-nT[nT$vari=="cog",]
UCcog<-UCcog[!is.na(UCcog$nUC),]

# calcul de la somme de température moyenne pour l'élongation de l'UC de Cogshall avec TB =  9.202832°C
STUCcog <- UCcog$nUC*(UCcog$TMUC -  9.202832)
mean(STUCcog)                     # 178.7922°C.j
sd(STUCcog)                       # 29.8567 °C.j

# récupération des temps thermiques et de celui de la veille
STarret <- sort(unique(STUCcog))
STarretMin1 <- c(NA,STarret[-length(STarret)])

croisUC <- data.frame(table(STUCcog), STarret, STarretMin1)
croisUC <- croisUC[!is.na(croisUC$STarretMin1),]                  # retrait de la première ligne pour laquelle on n'a pas la valeur de la veille
croisUC <- croisUC[,-1]                                            # retrait de la colonne des libellés du table, équivalent à STarret
colnames(croisUC) <- c("nbUC", "dateTh", "dateThMin1")
croisUC

# Avec la température de base fixée, calculée par différentes méthodes stat.
source("D:/MesDonnées/mes documents/manguier/modélisation phéno/stage 2014/Anne-Sarah Briand/données/fichiers de données/modèle thermique stochastique/modpopulation.R")
modpopulation(c(178,0.5))

nlm(modpopulation, c(170, 1), print.level=1, iterlim=10000)

# $minimum
#[1] 443.4085
#$estimate
#[1] 176.038968   2.194371


# Avec la température de base comme paramètre à estimer
source("D:/MesDonnées/mes documents/manguier/modélisation phéno/stage 2014/Anne-Sarah Briand/données/fichiers de données/modèle thermique stochastique/modpopulation 3 param.R")
modpopulation3(c(178,2, 10))                       # verteur de paramètres: c(seuil, param de variance, TB)

nlm(modpopulation3, c(180, 2, 7), print.level=1, iterlim=10000)

$minimum
[1] 442.9561

$estimate
[1] 176.245888   2.195601   9.184354


