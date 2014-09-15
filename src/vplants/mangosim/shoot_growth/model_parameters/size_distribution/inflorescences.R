# Distributions sur les longueurs d'inflorescences

setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/data")


####################################################################
#########     Récupération et mise en forme des données     ########
####################################################################
data0=read.csv("growth_data/BaseDeCroissanceInflo.csv",header=TRUE,sep=";",dec=",")

# On ne garde que Cogshall
data1=data0[data0$variete=="cog",]

# On s'intéresse à la taille des inflo en fin de croissance
data2=data1[data1$stadeInflo=="PF",]

#Histogramme des longueurs d'inflorescence
par(mfrow=c(1,3))
hist(data2$longueurInflo,breaks=20,freq=F,main="")
curve(dnorm(x,mean(data2$longueurInflo),sd(data2$longueurInflo)),col=2,add=T)
plot(ecdf(data2$longueurInflo))
curve(pnorm(x,mean(data2$longueurInflo),sd(data2$longueurInflo)),col=2,add=T)
qqnorm(data2$longueurInflo)
qqline(data2$longueurInflo,col=2)

shapiro.test(data2$longueurInflo)
ks.test(data2$longueurInflo,mean(data2$longueurInflo),sd(data2$longueurInflo))



# On recherche un relation d'allométrie entre diamètre et longueur d'inflorescence
plot(data2$diamAxeI ,data2$longueurInflo)
abline(lsfit(data2$diamAxeI ,data2$longueurInflo),col=2)

lm.diam=lm(data2$longueurInflo ~ data2$diamAxeI)
summary(lm.diam)


# On trace les ICs pour la moyenne et pour les individus (d'après Dagnélie, Statistique théorique et appliquée)
Variete.resid=sum(resid(lm.diam)^2)/df.residual(lm.diam)
mean.x = mean(data2$longueurInflo)
SCEx = sum((data2$diamAxeI - mean.x)^2)
n = length(data2$diamAxeI)
stud = qt(0.975, n-2)

conf.moy.inf=as.numeric(sort(lm.diam$fitted.values))-(stud*sqrt(Variete.resid*((1/n)+((sort(log(data2$diamAxeI))-mean.x)^2/SCEx))))
conf.moy.sup=as.numeric(sort(lm.diam$fitted.values))+(stud*sqrt(Variete.resid*((1/n)+((sort(log(data2$diamAxeI))-mean.x)^2/SCEx))))
conf.ind.inf=as.numeric(sort(lm.diam$fitted.values))-(stud*sqrt(Variete.resid*(((n+1)/n)+((sort(log(data2$diamAxeI))-mean.x)^2/SCEx))))
conf.ind.sup=as.numeric(sort(lm.diam$fitted.values))+(stud*sqrt(Variete.resid*(((n+1)/n)+((sort(log(data2$diamAxeI))-mean.x)^2/SCEx))))

plot(data2$diamAxeI,data2$longueurInflo,main="",xlab="Longueur inflo",ylab="Diamètre inflo")
abline(lm.diam,col=2)
points(sort(data2$diamAxeI),conf.moy.inf,col=4,lty=2,type='l')
points(sort(data2$diamAxeI),conf.moy.sup,col=4,lty=2,type='l')
points(sort(data2$diamAxeI),conf.ind.inf,col=6,lty=2,type='l')
points(sort(data2$diamAxeI),conf.ind.sup,col=6,lty=2,type='l')
legend("bottomright",c("droite de régression", "IC moyenne","IC individu"),lty=c(1,2),col=c(2,4,6))





####################################################################
#########     Récupération et mise en forme des données     ########
####################################################################
diam0=read.csv("size/relévé diam inflos Soizick.csv",header=TRUE,sep=";",dec=",",
								col.names=c("DateReleveUC","Traitement","Plant","UCdisparue","numeroUC",
										"Position","DiamètreBasalUC","IA Type","IADiamStadeF","IL1Type",
										"IL1DiamStadeF","IL2Type","IL2DiamStadeF","IL3Type","IL3DiamStadeF",
										"IL4Type","IL4DiamStadeF","IL5Type","IL5DiamStadeF","IL6Type","IL6DiamStadeF"))
attach(diam0)


par(mfrow=c(1,2))
# Inflorescences apicales
hist(IADiamStadeF,breaks=40)

# Inflorescences latérales
ILDiamStadeF=unlist(list(IL1DiamStadeF,IL2DiamStadeF,IL3DiamStadeF,IL4DiamStadeF,IL5DiamStadeF,IL6DiamStadeF))
ILType=unlist(list(IL1Type,IL2Type,IL3Type,IL4Type,IL5Type,IL6Type))


hist(ILDiamStadeF,breaks=30)

par(mfrow=c(2,2))
hist(ILDiamStadeF[ILType=="F"],xlim=c(2,8))
hist(ILDiamStadeF[ILType=="M"],xlim=c(2,8))
hist(ILDiamStadeF[ILType=="MF"],xlim=c(2,8))
hist(ILDiamStadeF[ILType=="MV"],xlim=c(2,8))
#hist(ILDiamStadeF[ILType=="V"]) pas de valeurs

par(mfrow=c(1,3))
hist(ILDiamStadeF[ILType=="F"],xlim=c(2,8),breaks=30,freq=F)
	curve(dnorm(x,mean(ILDiamStadeF[ILType=="F"],na.rm=T),sd(ILDiamStadeF[ILType=="F"],na.rm=T)),add=T,col=2)
plot(ecdf(ILDiamStadeF[ILType=="F"]))
	curve(pnorm(x,mean(ILDiamStadeF[ILType=="F"],na.rm=T),sd(ILDiamStadeF[ILType=="F"],na.rm=T)),add=T,col=2)
qqnorm(ILDiamStadeF[ILType=="F"])
	qqline(ILDiamStadeF[ILType=="F"],col=2)

shapiro.test(ILDiamStadeF[ILType=="F"])
ks.test(ILDiamStadeF[ILType=="F"],mean(ILDiamStadeF[ILType=="F"],na.rm=T),sd(ILDiamStadeF[ILType=="F"],na.rm=T))

