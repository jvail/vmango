setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth")

#### Relations allométriques :
#             - Base allometries :
#                             - Feuilles : longueur / surface
#                             - Feuilles : largeur / surface
#                             - Feuilles : longueur / largeur
#             - Base MA05 :
#                             - UCs : diamètre / longueur
#                             - UCs : diamètre / nombre de feuilles

# Ce script nous sert à déterminer de nombreuses relations allométriques nécessaires à la simulation des organes du manguiers
# Il permet notamment de déduire la longueur d'une feuille de sa surface, ce qui nous est nécessaire pour la modélisation
# Section 4.3 "Données relatives à la morphologie des UCs" du rapport

# On charge les données de la base allométrie
base=read.table("data/size_data/base_allometriesR.csv",header=TRUE,sep=";",dec=",")

# On ne garde que Cogshall, car c'est la variété à laquelle on s'intéresse
basecog=base[base$variete=="cogshall",]
attach(basecog)
surface=aire


##############################################
#### Relations allométriques sur les feuilles
##########

#### surface / longueur 
fit_long=lsfit(sqrt(surface),longueur)
#Intercept         X 
#0.1748717 2.3288180 

summary(lm(longueur ~ sqrt(surface)))   # L'intercepte n'est pas significatif, R2=0.98
summary(lm(longueur~-1+sqrt(surface)))  # R2=0.99

# On trace la longueur en fonction de la surface et de la racine de la surface
par(mfrow=c(1,2))
plot(surface,longueur,main="Longueur fonction de la surface")
plot(sqrt(surface),longueur,main="Longueur fonction de sqrt(surface)")
abline(fit_long,col=2)


#### surface / largeur


fit_larg=lsfit(sqrt(surface),largeur)
# Intercept          X 
#0.06559624 0.56410552

summary(lm(sqrt(surface)~largeur))      # L'intercepte n'est pas significatif, R2=0.97
summary(lm(sqrt(surface)~largeur-1))    # R2=0.99

# On trace la largeur en fonction de la surface et de la racine de la surface
plot(surface,largeur,main="Largeur fonction de l'surface")
plot(sqrt(surface),largeur,main="Longueur fonction de sqrt(surface)")


#### Longueur / Largeur 

fitlong_larg=lsfit(longueur,largeur)
# Intercept          X 
#0.1080185 0.2349332 
cor.test(longueur,largeur)

summary(lm(largeur~longueur))            # L'intercepte n'est pas significatif, R2=0.93
summary(lm(largeur~-1+longueur))         # R2=0.99

# On trace la largeur en fonction de la longueur
plot(longueur,largeur)   
points(longueur,longueur*0.24,col=2,type='l')
#plot(largeur - (0.11+longueur*0.24),col=2 )



#########################################
#### Relation sur le diamètre des UCs
##########

# On décroche les données de la base allometries et on charge celles de la base MA05
detach(basecog)
MA=read.table("data/size_data/MA05.csv",header=TRUE,sep=";",dec=",")
attach(MA)

####  diamètre / longueur  

fitdiam_longUC=lsfit(MA$Long,diametre)
# Intercept          X 
# 2.911564  0.182806 
cor.test(diametre,MA$Long)

summary(lm(diametre~MA$Long))

# On trace le diamètre en fonction de la longueur de l'UC
plot(MA$Long,diametre,main="Diamètre fonction de la longueur")
abline(lsfit(MA$Long,diametre),col=2)
hist(diametre/(2*pi))


# Différenciation apicale / latérale
plot(MA[position=="A",]$Long,MA[position=="A",]$diametre,main="Diamètre fonction de la longueur")
abline(lsfit(MA[position=="A",]$Long,MA[position=="A",]$diametre),col=2)
hist(MA[position=="A",]$diametre/(2*pi))
summary(lm(MA[position=="A",]$diametre~MA[position=="A",]$Long))

plot(MA[position=="L",]$Long,MA[position=="L",]$diametre,main="Diamètre fonction de la longueur")
abline(lsfit(MA[position=="L",]$Long,MA[position=="L",]$diametre),col=2)
hist(MA[position=="L",]$diametre/(2*pi))
summary(lm(MA[position=="L",]$diametre~MA[position=="L",]$Long))

# Diamètre en fonction de la longueur de l'UC pour les UCs en position apicale et latérale
par(mfrow=c(1,2))
plot(MA[position=="A",]$Long,MA[position=="A",]$diametre,main="UCs en position apicale",xlim=c(0,30),ylim=c(2,10),xlab="Longueur ",ylab="Diamètre")
abline(lsfit(MA[position=="A",]$Long,MA[position=="A",]$diametre),col=2)
plot(MA[position=="L",]$Long,MA[position=="L",]$diametre,main="UCs en position latérale",xlim=c(0,30),ylim=c(2,10),xlab="Longueur ",ylab="Diamètre")
abline(lsfit(MA[position=="L",]$Long,MA[position=="L",]$diametre),col=2)
#title("Diamètre fonction de la longueur ",outer=TRUE,line=-1)

summary(MA[position=="A",]$diametre/2)
summary(MA[position=="L",]$diametre/2)

#Analyse de covariance
summary(lm(MA$diametre ~ position*MA$Long))
summary(lm(MA$diametre ~ position*MA$Long-1))



######## Diamètre / nombre de feuilles 
plot(MA$NbFeu,MA$diametre)
lm.diam_nbfeu=lm(MA$diametre~MA$NbFeu)
summary(lm.diam_nbfeu)

# UCs en position apicale
plot(MA[position=="A",]$NbFeu,MA[position=="A",]$diametre)
lm.diam_nbfeu=lm(MA[position=="A",]$diametre~MA[position=="A",]$NbFeu)
summary(lm.diam_nbfeu)


# UCs en position latérale
plot(MA[position=="L",]$NbFeu,MA[position=="L",]$diametre)
lm.diam_nbfeu=lm(MA[position=="L",]$diametre~MA[position=="L",]$NbFeu)
summary(lm.diam_nbfeu)









