# Etude du point d'inflexion

setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/size_distribution")


# Données ajustées par Anaëlle sur les inflos
inflo0=read.csv("paramAJUSTinfloglob.csv",header=TRUE,sep=";",dec=".")
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


# Données ajustées par Anaëlle sur les UCs
UC0=read.csv("paramAJUSTUCglob.csv",header=TRUE,sep=";",dec=".")
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



# Etude de la répartition des points d'inflexion
par(mfrow=c(1,2))
hist(inflo$X00,col=8,main="inflorescences",freq=F,breaks=20)
curve(dnorm(x,mean(inflo$X00),sd(inflo$X00)),add=T)
hist(UC$X00,col="salmon",main="UCs",freq=F,breaks=20)
curve(dnorm(x,mean(UC$X00),sd(UC$X00)),add=T)
title("Répartition de la date du point d'inflexion estimé",outer=T,line=-1)


#### Recherche de la vitesse maximale

# On récupère les infos de la base de croissance
CroissInflo=read.csv("BaseDeCroissanceInflo.csv",sep=";",dec=",")
CroissInflo$date=strptime(as.character(CroissInflo$date),"%d/%m/%y %H:%M")
codeInflos=unique(CroissInflo$codeUC)               # Liste des inflorescences

# On fait une boucle sur chaque inflorescence pour calculer les vitesses de croissance
croiss=c();t_demi=list()
for (i in 1:length(codeInflos)){
	data=CroissInflo[CroissInflo$codeUC==codeInflos[i],]
	n=length(data$longueurInflo)
	vitesse=(data$longueurInflo[2:n]-data$longueurInflo[1:(n-1)])/as.numeric(data$date[2:n]-data$date[1:(n-1)])
	vitesse[is.na(vitesse)]=0
	date_max=data$date[which(vitesse==max(vitesse))+1]
	t_demi=c(t_demi,list(date_max))
	length_max=data$longueurInflo[data$date==date_max]
	croiss=c(croiss,length_max/max(data$longueurInflo,na.rm=T))
}


# On récupère lesinfos de la base de croissance
CroissUC=read.csv("BaseDeCroissanceVeg.csv",sep=";",dec=",")
CroissUC$date=strptime(as.character(CroissUC$date),"%d/%m/%y %H:%M")
codeUCs=unique(CroissUC$codeUC)               # Liste des inflorescences

# On fait une boucle sur chaque inflorescence pour calculer les vitesses de croissance
croissUC=c();t_demiUC=list()
for (i in 1:length(codeUCs)){
	data=CroissUC[CroissUC$codeUC==codeUCs[i],]
	n=length(data$longueurUC)
	vitesse=(data$longueurUC[2:n]-data$longueurUC[1:(n-1)])/as.numeric(data$date[2:n]-data$date[1:(n-1)])
	vitesse[is.na(vitesse)]=0
	date_max=data$date[which(vitesse==max(vitesse))+1]
	t_demiUC=c(t_demiUC,list(date_max))
	length_max=data$longueurUC[data$date==date_max]
	croissUC=c(croissUC,length_max/max(data$longueurUC,na.rm=T))
}





###########################################################
###### Longueur Axe II inflos
############
head(inflo)
axe2.lm=lm(inflo$longAxeII~inflo$longF)
summary(axe2.lm)                            # Long_axe_2=-3.97+0.687*LongI=_inflo

plot(inflo$longF,inflo$longAxeII)
abline(axe2.lm$coef,col="salmon")


