# Etude de la taille des organes
# 2004 et 2006.
#LEPF : Longueur de l'espace pré-feuille



setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/size_distribution")
base=read.table("BaseDeCroissanceVeg.csv",header=TRUE,sep=";",dec=",")

# Récupération de la dernière mesure uniquement (correspond au stade phéno H)
mesUC=base[base$stadeUC=="H",]
attach(mesUC)


###########################################
############## Etude LEPF #################
###########################################


hist(LEPF)

# Ucs en position apicale et latérale
apic=mesUC[positionUC=="A",]
lat=mesUC[positionUC=="L",]

# Histogrammes des LEPF pours les UCs apicales et latérales
par(mfrow=c(1,2))
hist(apic$LEPF, main = "UCs apicales",xlim=c(0,18),breaks=seq(0,15,1),freq=F,xlab="LEPF")    ;curve(dnorm(x,mean(apic$LEPF),sd(apic$LEPF)),col=2,add=T)
legend("topright",col=2,lty=1,"N(2.63,1.72)",bty="n")
hist(lat$LEPF, main="UCs latérales",xlim=c(0,18),breaks=seq(0,15,1),freq=F,xlab="LEPF")      ;curve(dnorm(x,mean(lat$LEPF,na.rm=T),sd(lat$LEPF,na.rm=T)),add=T,col=2)
legend("topright",col=2,lty=1,"N(5.94,3.17)",bty="n")
#title("LEPF",outer=TRUE,line=-1)

# étude de l'influence de la position de l'UC sur LEPF
LEPF.kruskal=kruskal.test(LEPF ~ positionUC)    # KW= 35.8302, df = 1, p-value = 2.153e-09


# Tests de KS pour vérifier la normalité des distributions de LEPF
ks.test(apic$LEPF,mean(apic$LEPF),sd(apic$LEPF))                     # 0.8312
ks.test(lat$LEPF,mean(lat$LEPF,na.rm=T),sd(lat$LEPF,na.rm=T))        # 0.9254

par(mfrow=c(1,2))
fit.gamma.apic=fitdistr(apic$LEPF,"gamma")  #2.0068648 0.7631676 
hist(apic$LEPF,freq=F,main="Histogramme",xlab="LEPF des UCs apicales",ylim=c(0,0.3),xlim=c(0,16));curve(dgamma(x,fit.gamma.apic$est[1],fit.gamma.apic$est[2]),col=2,add=T)
ks.test(apic$LEPF,"pgamma",fit.gamma.apic$est[1],fit.gamma.apic$est[2])                     # 0.3264

lat.LEPF=lat$LEPF[-11]
fit.gamma.lat=fitdistr(lat.LEPF,"gamma")     # 3.3760921 0.5684268 
hist(lat.LEPF,freq=F,main="Histogramme",xlab="LEPF des UCs latérales",ylim=c(0,0.3),xlim=c(0,16));curve(dgamma(x,fit.gamma.lat$est[1],fit.gamma.lat$est[2]),col=2,add=T)
ks.test(lat.LEPF,"pgamma",fit.gamma.lat$est[1],fit.gamma.lat$est[2])                     # 0.7447



# Relation entre longeur d'UC et LEPF
plot(apic$longueurUC,apic$LEPF,xlab="Longueur UC",ylab="LEPF",main="apical")
#abline(lm(apic$LEPF ~ apic$longueurUC),col=2)
plot(lat$longueurUC,lat$LEPF,xlab="Longueur UC",ylab="LEPF",main="latéral")
abline(lm(lat$LEPF ~ lat$longueurUC),col=2)
#title("LEPF fonction de la longueur",outer=TRUE,line=-1)

summary(lm(apic$LEPF ~ apic$longueurUC))
summary(lm(lat$LEPF ~ lat$longueurUC))




###########################################
#######  Longueurs entrenoeuds  ###########
###########################################

# renormalisation de la position des noeuds pour avoir des valeurs comprises entre 0 et 1
x=((1:10)-1)/9
internode_length=c(2,2.1,1.5,1,1,0.6,0.4,0.3,0.2,0.2)

Internode.lm=lm(internode_length ~ x)   ; summary(internode.lm)
logInternode.lm=lm(log(internode_length)~ x)        ; summary(logInternode.lm)
#lm1=lm(log(internode_length)~log(1:10))    ; summary(lm1)



par(mfrow=c(1,3))
# Données brutes
plot(internode_length,ylab="Longueur entrenoeud",xlab="Rang noeud"); 

# Log des données avec échelle renormalisée et droite de régression
plot(x,log(internode_length),xlab="Rang noeud") ; abline(logInternode.lm$coeff,col=2) 

# Données brutes sur échelle renormalisée avec courbe ajustée
plot(x,internode_length,ylab="Longueur entrenoeud",xlab="Rang noeud",xlim=c(0,1),ylim=c(0,3)); 
points(x,exp(0.92)*exp(-2.64*x),col=2,type='l')





