##### Etude du point d'inflexion
#Script permettant l'étude de la distribution des points d'inflexion
# Le point d'inflexion étant un des paramètres à déterminer pour définir la courbe des croissance des UCs et inflorescences
# Section 4.4 "Données pour les modèles de temps thermique" du rapport


setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/data")


# Données ajustées par Anaëlle sur les inflos
inflo0=read.csv("growth_data/paramAJUSTinfloglob.csv",header=TRUE,sep=";",dec=".")
inflo=inflo0[inflo0$var=="cog",]

# calcul de la somme de température moyenne pour l'élongation de l'UC de Cogshall avec TB = 11.12031°C
STInflocog <- inflo$nInflo*(inflo$TMInflo - 11.12031)
mean(STInflocog)                     #   346.0275°C.j
sd(STInflocog)                        # 48.174 °C.j
summary(STInflocog)

# calcul de la somme de température moyenne pour l'élongation de l'UC de Cogshall avec TB = 8.880998°C (sans les 3 points à 26°C)
STInflocogred <- inflo$nInflo*(inflo$TMInflo - 8.880998)
mean(STInflocogred)                     #   422.8411°C.j
sd(STInflocogred)                        # 60.38383 °C.j
summary(STInflocogred)

diff_inflo=STInflocogred/2-inflo$X00
summary(inflo$X00)
summary(diff_inflo)

plot(STInflocog,inflo$X00)


# Données ajustées par Anaëlle sur les UCs
UC0=read.csv("growth_data/paramAJUSTUCglob.csv",header=TRUE,sep=";",dec=".")
UC=UC0[UC0$var=="cog",]

# calcul de la somme de température moyenne pour l'élongation de l'UC de Cogshall avec TB = 11.12031°C
STUCcog <- UC$nUC*(UC$TMUC - 9.2)
mean(STUCcog,na.rm=T)                     #   178.8243°C.j
sd(STUCcog,na.rm=T)                        #  29.86°C.j
summary(STUCcog,na.rm=T)

diff_UC=STUCcog/2-UC$X00
summary(diff_UC)

# On regarde si il y a des relations entre la date du point d'inflexion et les autres paramètres
plot(inflo$A0,inflo$X00)
plot(inflo$B0,inflo$X00)
plot(inflo$ER0,inflo$X00)
plot(inflo$TMInflo,inflo$X00)
plot(inflo$nInflo,inflo$X00)
plot(inflo$diamInflo,inflo$X00)
plot(inflo$diamUCM,inflo$X00)
plot(inflo$haut,inflo$X00)



# Etude de la répartition des points d'inflexion
par(mfrow=c(1,2))
hist(inflo$X00,col=8,main="inflorescences",freq=F,breaks=20,xlab="Temps au point d'inflexion")
#curve(dnorm(x,mean(inflo$X00),sd(inflo$X00)),add=T)
hist(UC$X00,col="salmon",main="UCs",freq=F,breaks=20,xlab="Temps au point d'inflexion")
curve(dnorm(x,mean(UC$X00),sd(UC$X00)),add=T)                         # mu=79.40, sigma=10.75
#title("Répartition de la date du point d'inflexion estimé",outer=T,line=-1)

# On ne reconnaît aucunes distributions connues our les inflorescences, on cherche donc à voir si on peut différencier des cas
par(mfrow=c(1,3))
hist(inflo$X00,col=8,main="inflorescences",breaks=10,ylim=c(0,8))
hist(inflo$X00[inflo$pos=="A"],col=8,main="apicale",breaks=10,ylim=c(0,8))
hist(inflo$X00[inflo$pos=="L"],col=8,main="latérale",breaks=10,ylim=c(0,8))
title("Répartition de la date du point d'inflexion estimé pour les inflorescences",outer=T,line=-1)

par(mfrow=c(2,3))
hist(inflo$X00[inflo$pos=="A" & inflo$posUCm=="A"],col=8,main="apicale, mère apicale",breaks=10,ylim=c(0,8))
hist(inflo$X00[inflo$pos=="L" & inflo$posUCm=="A"],col=8,main="latérale,mère apicale",breaks=10,ylim=c(0,8))
hist(inflo$X00[inflo$pos=="A" & inflo$posUCm=="L"],col=8,main="apicale, mère latérale",breaks=10,ylim=c(0,8))

hist(inflo$X00,col=8,main="inflorescences",breaks=10,ylim=c(0,8))
title("Répartition de la date du point d'inflexion estimé pour les inflorescences",outer=T,line=-1)


# Il n'y a pas d'influence visible de la position de l'UC, on cherche donc à voir si il existe une relation avec la date de débourrement
paramInflo0=read.csv("bud_data/bourgeons_F.csv",header=TRUE,sep=";",dec=".")
paramInflo=paramInflo0[paramInflo0$stadeInflo=="B2" | paramInflo0$stadeInflo=="B2/C",]
paramInflo$date=strptime(as.character(paramInflo$date),"%d/%m/%Y %H:%M")

plot(paramInflo$date,paramInflo$T_ip,pch=15)
points(paramInflo$date[paramInflo$Verger=="BP"],paramInflo$T_ip[paramInflo$Verger=="BP"],pch=15,col="salmon")
points(paramInflo$date[paramInflo$Verger=="BM"],paramInflo$T_ip[paramInflo$Verger=="BM"],pch=15,col="cyan")
points(paramInflo$date[paramInflo$Verger=="GF"],paramInflo$T_ip[paramInflo$Verger=="GF"],pch=15,col="pink")
legend("topright",c("Bassin Plat","Bassin Martin","Grand Fond"),col=c("salmon","cyan","pink"),pch=15)

# On regarde dans les inflos du mois de novembre
parinflo=paramInflo[paramInflo$date$mon<10,]

plot(parinflo$date,parinflo$T_ip,pch=15,xlab="date de débourrement de l'inflorescence",ylab="temps thermique au point d'inflexion (dj)")
points(parinflo$date[parinflo$Verger=="BP"],parinflo$T_ip[parinflo$Verger=="BP"],pch=15,col="salmon")
points(parinflo$date[parinflo$Verger=="BM"],parinflo$T_ip[parinflo$Verger=="BM"],pch=15,col="cyan")
points(parinflo$date[parinflo$Verger=="GF"],parinflo$T_ip[parinflo$Verger=="GF"],pch=15,col="pink")
legend("bottomright",c("Bassin Plat","Bassin Martin","Grand Fond"),col=c("salmon","cyan","pink"),pch=15)

lm.inf_pt=lm(parinflo$T_ip~parinflo$date$yday)
summary(lm.inf_pt)


plot(parinflo$date$yday,parinflo$T_ip,pch=15,xlab="date de débourrement de l'inflorescence",ylab="temps thermique au point d'inflexion (dj)")
abline(lm.inf_pt$coeff,col=2)
points(parinflo$date[parinflo$Verger=="BP"]$yday,parinflo$T_ip[parinflo$Verger=="BP"],pch=15,col="salmon")
points(parinflo$date[parinflo$Verger=="BM"]$yday,parinflo$T_ip[parinflo$Verger=="BM"],pch=15,col="cyan")
points(parinflo$date[parinflo$Verger=="GF"]$yday,parinflo$T_ip[parinflo$Verger=="GF"],pch=15,col="pink")
legend("bottomright",c("Bassin Plat","Bassin Martin","Grand Fond"),col=c("salmon","cyan","pink"),pch=15)



# Etude par rapport à la date de fin de croissance

# Il n'y a pas d'influence visible de la position de l'UC, on cherche donc à voir si il existe une relation avec la date de débourrement
paramInf0=read.csv("bud_data/bourgeons_F.csv",header=TRUE,sep=";",dec=".")
paramInf=paramInf0[paramInf0$stadeInflo!="B2" & paramInf0$stadeInf!="B2/C",]
paramInf$date=strptime(as.character(paramInf$date),"%d/%m/%Y %H:%M")

plot(paramInf$date,paramInf$T_ip,pch=15)
points(paramInf$date[paramInf$Verger=="BP"],paramInf$T_ip[paramInf$Verger=="BP"],pch=15,col="salmon")
points(paramInf$date[paramInf$Verger=="BM"],paramInf$T_ip[paramInf$Verger=="BM"],pch=15,col="cyan")
points(paramInf$date[paramInf$Verger=="GF"],paramInf$T_ip[paramInf$Verger=="GF"],pch=15,col="pink")
legend("topright",c("Bassin Plat","Bassin Martin","Grand Fond"),col=c("salmon","cyan","pink"),pch=15)




