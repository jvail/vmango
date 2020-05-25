#setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth")
setwd("/Users/fboudon/Develop/oagit/mangosim/src/vplants/mangosim/shoot_growth")

# Etude des entrenoeuds :
#                           - Longueur de l'espace pr?-feuille (LEPF)
#                                    - Relation longueur UC / LEPF
#                           - Relation longueur entrenoeuds / position entrenoeuds

# Ce script nous permet de d?terminer les longueurs d'espace-pr?-feuille (premier entrenoeud)
# et de d?finir une relation de proportionalit? entre les diff?rentes longueurs d'entrenoeuds
# Section 4.3 "Donn?es relatives ? la morphologie des UCs" du rapport

# On charge les donn?es de la base de croissance v?g?tative
base=read.table("data/growth_data/BaseDeCroissanceVeg.csv",header=TRUE,sep=";",dec=",")

# On r?cup?re la derni?re mesure uniquement (correspond au stade ph?no H)
mesUC=base[base$stadeUC=="H",]
attach(mesUC)


###########################################
############## Etude LEPF 
###################

# On trace l'histogrammes des longueurs d'EPF pour voir si une distribution appara?t
hist(LEPF)

# On distinue les UCs en position apicale et lat?rale
apic=mesUC[positionUC=="A",]
lat=mesUC[positionUC=="L",]

# Histogrammes des LEPF pours les UCs apicales et lat?rales
par(mfrow=c(1,2))
hist(apic$LEPF, main = "UCs apicales",xlim=c(0,18),breaks=seq(0,15,1),freq=F,xlab="LEPF")    ;curve(dnorm(x,mean(apic$LEPF),sd(apic$LEPF)),col=2,add=T)
legend("topright",col=2,lty=1,"N(2.63,1.72)",bty="n")
hist(lat$LEPF, main="UCs lat?rales",xlim=c(0,18),breaks=seq(0,15,1),freq=F,xlab="LEPF")      ;curve(dnorm(x,mean(lat$LEPF,na.rm=T),sd(lat$LEPF,na.rm=T)),add=T,col=2)
legend("topright",col=2,lty=1,"N(5.94,3.17)",bty="n")
#title("LEPF",outer=TRUE,line=-1)

# ?tude de l'influence de la position de l'UC sur LEPF
LEPF.kruskal=kruskal.test(LEPF ~ positionUC)    # KW= 35.8302, df = 1, p-value = 2.153e-09


# Tests de KS pour v?rifier la normalit? des distributions de LEPF
ks.test(apic$LEPF,mean(apic$LEPF),sd(apic$LEPF))                     # 0.8312
ks.test(lat$LEPF,mean(lat$LEPF,na.rm=T),sd(lat$LEPF,na.rm=T))        # 0.9254

# On trace les r?sultats en ajustant des distributions gamma
par(mfrow=c(1,2))
fit.gamma.apic=fitdistr(apic$LEPF,"gamma")  #2.0068648 0.7631676 
hist(apic$LEPF,freq=F,main="Histogramme",xlab="LEPF des UCs apicales",ylim=c(0,0.3),xlim=c(0,16));curve(dgamma(x,fit.gamma.apic$est[1],fit.gamma.apic$est[2]),col=2,add=T)
ks.test(apic$LEPF,"pgamma",fit.gamma.apic$est[1],fit.gamma.apic$est[2])                     # 0.3264

lat.LEPF=lat$LEPF[-11]
fit.gamma.lat=fitdistr(lat.LEPF,"gamma")     # 3.3760921 0.5684268 
hist(lat.LEPF,freq=F,main="Histogramme",xlab="LEPF des UCs lat?rales",ylim=c(0,0.3),xlim=c(0,16));curve(dgamma(x,fit.gamma.lat$est[1],fit.gamma.lat$est[2]),col=2,add=T)
ks.test(lat.LEPF,"pgamma",fit.gamma.lat$est[1],fit.gamma.lat$est[2])                     # 0.7447



# Relation entre longeur d'UC et LEPF
plot(apic$longueurUC,apic$LEPF,xlab="Longueur UC",ylab="LEPF",main="apical")
#abline(lm(apic$LEPF ~ apic$longueurUC),col=2)
plot(lat$longueurUC,lat$LEPF,xlab="Longueur UC",ylab="LEPF",main="lat?ral")
abline(lm(lat$LEPF ~ lat$longueurUC),col=2)
#title("LEPF fonction de la longueur",outer=TRUE,line=-1)

summary(lm(apic$LEPF ~ apic$longueurUC))
summary(lm(lat$LEPF ~ lat$longueurUC))
summary(lm(lat$LEPF ~ lat$longueurUC + 0))


###########################################
#######  Longueurs entrenoeuds  
################

# renormalisation de la position des noeuds pour avoir des valeurs comprises entre 0 et 1
x=((1:10)-1)/9
internode_length=c(2,2.1,1.5,1,1,0.6,0.4,0.3,0.2,0.2)

# On teste une relation lin?aire
internode.lm=lm(internode_length ~ x)   ; summary(internode.lm)
# On teste une relation logarithmique
logInternode.lm=lm(log(internode_length)~ x)        ; summary(logInternode.lm)
#lm1=lm(log(internode_length)~log(1:10))    ; summary(lm1)



par(mfrow=c(1,3))
# Donn?es brutes
plot(internode_length,ylab="Longueur entrenoeud",xlab="Rang noeud"); 

# Log des donn?es avec ?chelle renormalis?e et droite de r?gression
plot(x,log(internode_length),xlab="Rang noeud") ; abline(logInternode.lm$coeff,col=2) 

# Donn?es brutes sur ?chelle renormalis?e avec courbe ajust?e
plot(x,internode_length,ylab="Longueur entrenoeud",xlab="Rang noeud",xlim=c(0,1),ylim=c(0,3)); 
points(x,exp(0.92)*exp(-2.64*x),col=2,type='l')


normed_internode_length = internode_length/sum(internode_length)

normed_internode.lm=lm(normed_internode_length ~ x)   ; summary(normed_internode.lm)
# On teste une relation logarithmique
logNormedInternode.lm=lm(log(normed_internode_length)~ x)        ; summary(logNormedInternode.lm)


par(mfrow=c(1,3))
# Donn?es brutes
plot(normed_internode_length,ylab="Longueur entrenoeud",xlab="Rang noeud"); 

# Log des donn?es avec ?chelle renormalis?e et droite de r?gression
plot(x,log(normed_internode_length),xlab="Rang noeud") ; abline(logNormedInternode.lm$coeff,col=2) 

# Donn?es brutes sur ?chelle renormalis?e avec courbe ajust?e
plot(x,normed_internode_length,ylab="Longueur entrenoeud",xlab="Rang noeud",xlim=c(0,1),ylim=c(0,0.5)); 
points(x,exp(logNormedInternode.lm$coeff[[1]])*exp(logNormedInternode.lm$coeff[[2]]*x),col=2,type='l')

sum(exp(0.92)*exp(-2.64*x))
sum(exp(0.92)*exp(-2.64*sum(x)))
sum(x)
