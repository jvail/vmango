#setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth")
setwd("/Users/fboudon/Develop/oagit/mangosim/src/vplants/mangosim/shoot_growth")
# Informations sur la taille des inflorescences :
#                                             - Distributions des longeurs
#                                             - Relation longueur / diam?tre
#                                             - Distribution des diam?tres

# Ce script nous permet de d?terminer toutes les informations n?cessaires ? la d?finition d'une onflorescence sous L-PY,
# C'est ? dire sa longueur, son nombre d'awes secondaires et son diam?tre.
# Section 4.3 "Donn?es relatives ? la morphologie des UCs" du rapport


########################################################
#########  Distribution des longueurs d'inflorescences  
##################

# On charge les donn?es de la Base de croissance v?g?tative
data0=read.csv("data/growth_data/BaseDeCroissanceInflo.csv",header=TRUE,sep=";",dec=",")

# On ne garde que Cogshall
data1=data0[data0$variete=="cog",]

# On s'int?resse ? la taille des inflorescences en fin de croissance
data2=data1[data1$stadeInflo=="PF",]

#Histogrammes des longueurs d'inflorescence
par(mfrow=c(1,3))
hist(data2$longueurInflo,breaks=20,freq=F,main="")
curve(dnorm(x,mean(data2$longueurInflo),sd(data2$longueurInflo)),col=2,add=T)
plot(ecdf(data2$longueurInflo))
curve(pnorm(x,mean(data2$longueurInflo),sd(data2$longueurInflo)),col=2,add=T)
qqnorm(data2$longueurInflo)
qqline(data2$longueurInflo,col=2)

shapiro.test(data2$longueurInflo)
ks.test(data2$longueurInflo,mean(data2$longueurInflo),sd(data2$longueurInflo))

#############################################################################
#########  Relation allom?trique entre diam?tre et longueur d'inflorescence  
##################
par(mfrow=c(1,1))
plot(data2$diamAxeI ,data2$longueurInflo)
abline(lsfit(data2$diamAxeI ,data2$longueurInflo),col=2)

lm.diam=lm(data2$longueurInflo ~ data2$diamAxeI)
summary(lm.diam)


# On trace les ICs pour la moyenne et pour les individus (d'apr?s Dagn?lie, Statistique th?orique et appliqu?e)
Variete.resid=sum(resid(lm.diam)^2)/df.residual(lm.diam)
mean.x = mean(data2$longueurInflo)
SCEx = sum((data2$diamAxeI - mean.x)^2)
n = length(data2$diamAxeI)
stud = qt(0.975, n-2)

conf.moy.inf=as.numeric(sort(lm.diam$fitted.values))-(stud*sqrt(Variete.resid*((1/n)+((sort(log(data2$diamAxeI))-mean.x)^2/SCEx))))
conf.moy.sup=as.numeric(sort(lm.diam$fitted.values))+(stud*sqrt(Variete.resid*((1/n)+((sort(log(data2$diamAxeI))-mean.x)^2/SCEx))))
conf.ind.inf=as.numeric(sort(lm.diam$fitted.values))-(stud*sqrt(Variete.resid*(((n+1)/n)+((sort(log(data2$diamAxeI))-mean.x)^2/SCEx))))
conf.ind.sup=as.numeric(sort(lm.diam$fitted.values))+(stud*sqrt(Variete.resid*(((n+1)/n)+((sort(log(data2$diamAxeI))-mean.x)^2/SCEx))))

plot(data2$diamAxeI,data2$longueurInflo,main="",xlab="Longueur inflo",ylab="Diam?tre inflo")
abline(lm.diam,col=2)
points(sort(data2$diamAxeI),conf.moy.inf,col=4,lty=2,type='l')
points(sort(data2$diamAxeI),conf.moy.sup,col=4,lty=2,type='l')
points(sort(data2$diamAxeI),conf.ind.inf,col=6,lty=2,type='l')
points(sort(data2$diamAxeI),conf.ind.sup,col=6,lty=2,type='l')
legend("bottomright",c("droite de r?gression", "IC moyenne","IC individu"),lty=c(1,2),col=c(2,4,6))



################################################
#########  Etude des distribution de diam?tre  
##################
# On charge la base de relev?s des diam?tres d'inflorescences
diam0=read.csv("data/size_data/rel?v? diam inflos Soizick.csv",header=TRUE,sep=";",dec=",",
								col.names=c("DateReleveUC","Traitement","Plant","UCdisparue","numeroUC",
										"Position","Diam?treBasalUC","IA Type","IADiamStadeF","IL1Type",
										"IL1DiamStadeF","IL2Type","IL2DiamStadeF","IL3Type","IL3DiamStadeF",
										"IL4Type","IL4DiamStadeF","IL5Type","IL5DiamStadeF","IL6Type","IL6DiamStadeF"))
attach(diam0)


par(mfrow=c(1,2))
# Inflorescences apicales
# IA= Inflorescence Apicale
hist(IADiamStadeF,breaks=40)

# Inflorescences lat?rales
# IL= Inflorescence Lat?rale
ILDiamStadeF=unlist(list(IL1DiamStadeF,IL2DiamStadeF,IL3DiamStadeF,IL4DiamStadeF,IL5DiamStadeF,IL6DiamStadeF))
ILType=unlist(list(IL1Type,IL2Type,IL3Type,IL4Type,IL5Type,IL6Type))
hist(ILDiamStadeF,breaks=30)

par(mfrow=c(2,2))
# On trace pour toutes les inflorescences y compris les "mixtes"
hist(ILDiamStadeF[ILType=="F"],xlim=c(2,8))
hist(ILDiamStadeF[ILType=="M"],xlim=c(2,8))
hist(ILDiamStadeF[ILType=="MF"],xlim=c(2,8))
hist(ILDiamStadeF[ILType=="MV"],xlim=c(2,8))
#hist(ILDiamStadeF[ILType=="V"]) pas de valeurs

# On trace l'histogramme, la fonction de r?partition et la droite de Henry pour d?montrer la normalit? 
# de la distrbution des longueurs d'inflorescences
par(mfrow=c(1,3))
hist(ILDiamStadeF[ILType=="F"],xlim=c(2,8),breaks=30,freq=F)
	curve(dnorm(x,mean(ILDiamStadeF[ILType=="F"],na.rm=T),sd(ILDiamStadeF[ILType=="F"],na.rm=T)),add=T,col=2)
plot(ecdf(ILDiamStadeF[ILType=="F"]))
	curve(pnorm(x,mean(ILDiamStadeF[ILType=="F"],na.rm=T),sd(ILDiamStadeF[ILType=="F"],na.rm=T)),add=T,col=2)
qqnorm(ILDiamStadeF[ILType=="F"])
	qqline(ILDiamStadeF[ILType=="F"],col=2)

# Tests de normalit?
shapiro.test(ILDiamStadeF[ILType=="F"])
ks.test(ILDiamStadeF[ILType=="F"],mean(ILDiamStadeF[ILType=="F"],na.rm=T),sd(ILDiamStadeF[ILType=="F"],na.rm=T))

######################@
## number of secondary axis
## Data from Goguey

inflolength = c(38,26,25,27,35.5,45,41,22,20,26.5,40,39.5,35,30,29.5,32,30,28,29.5,21.5,32,33,40,39,41,37.5,39.5,44.5,38.5,32,38,38.5,30.5,39,39,38.5)

nbsecaxis = c(47,35,36,33,59,53,48,36,22,35,59,50,39,29,37,42,41,32,41,22,40,42,43,40,47,41,44,44,43,39,42,22,42,50,46,46)

df = data.frame(inflolength = inflolength,nbsecaxis =nbsecaxis )

nbsecaxislm = lm(nbsecaxis ~ inflolength, data=df)
summary(nbsecaxislm)

nbsecaxislm0 = lm(nbsecaxis ~ inflolength + 0, data=df)
summary(nbsecaxislm0)

x = 20:45
plot(inflolength,nbsecaxis)
points(x,nbsecaxislm$coeff[[1]]+nbsecaxislm$coeff[[2]]*x,col=2,type='l')

#############################################################################
#### longeur des secondaires
#############################################################################

axesec.lm = lm(formula = longueurAxeSec ~ longueurInflo, data = data2)
summary(axesec.lm)

axesec.lm0 = lm(formula = longueurAxeSec ~ longueurInflo + 0, data = data2)
summary(axesec.lm0)

