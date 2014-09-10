# Etude de la taille des organes
# Ici on cherche à connaître :
#       - les distributions suivies par le nombre de feuilles / la longueur des UCs
#       - la relation entre le nombre de feuilles / la longueur des UCs
# 2004 et 2006.

setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/size_distribution")


#################################################################################################################################
##########################################     Récupération et mise en forme des données     ####################################
#################################################################################################################################

####################
####  Tayllamin ####
####################

Tayllamin0=read.csv("FTayllamin.csv",header=TRUE,sep=";",dec=",",
			   col.names=c("Variete","CodeArbre","Branche","NumArbre","NbRamFlo","NbUC","NomAxePo","TypAxePo","NbAxePo",
					   "NumNdRam","AxePort","TypoPort","Long","DB","DS","NbFeu","NbFeuChut","NbNoeuds",
					   "Conicite","Elancement"))
Tayllamin0$NumNdRam=as.numeric(Tayllamin0$NumNdRam)-1

##### Suppression des données aberrantes
Tayllamin1=Tayllamin0[c(1:length(Tayllamin0$NbFeu))[!is.na(Tayllamin0$NbFeu)],]

# On cherche les UCs ou il y a eu trop de feuilles chutées 
par(mfrow=c(1,2))
boxplot(Tayllamin1$NbFeuChut)
plot(Tayllamin1$NbFeuChut)
title("Etude du nombre de feuilles chutées",outer=TRUE,line=-1)
summary(Tayllamin1$NbFeuChut,na.rm=T)
Tayllamin2=Tayllamin1[Tayllamin1$NbFeuChut < 7,]

# On cherche les UCs avec des longueurs très éloignées des autres
boxplot(Tayllamin2$Long)
plot(Tayllamin2$Long)
title("Etude de la longueur des Ucs",outer=TRUE,line=-1)
summary(Tayllamin2$Long,na.rm=T)
Tayllamin=Tayllamin2[(Tayllamin2$Long <25 & Tayllamin2$Long >5),]
Tayllamin=Tayllamin[c(1:length(Tayllamin$NbFeu))[!is.na(Tayllamin$NbFeu)],]

# pour axe_port, NA correspond à une Branche porteuse
porteuses=Tayllamin[Tayllamin$NumNdRam==7,]
port_apic=porteuses[porteuses$TypAxePo=="T",]
port_lat=porteuses[porteuses$TypAxePo=="R",]

# Séparation des arbres et des types
# NumNdRam = 0 : UC en position apicale, 0 < NumNdRam < 7 : UC en position latérale, NumNdRam = 7 : UC porteuse
B10=Tayllamin[Tayllamin$NumArbre==1,] ; B12=Tayllamin[Tayllamin$NumArbre==2,] ;
F2=Tayllamin[Tayllamin$NumArbre==3,]  ; F6=Tayllamin[Tayllamin$NumArbre==4,]  ; B14=Tayllamin[Tayllamin$NumArbre==5,]

apic_B10=B10[B10$NumNdRam==0,] ; apic_B12=B12[B12$NumNdRam==0,] ;
apic_F2=F2[F2$NumNdRam==0,]    ; apic_F6=F6[F6$NumNdRam==0,]    ; apic_B14=B14[B14$NumNdRam==0,]

lat_B10=B10[B10$NumNdRam > 0 & B10$NumNdRam < 7,] ; lat_B12=B12[B12$NumNdRam > 0 & B12$NumNdRam < 7,] ;
lat_F2=F2[F2$NumNdRam > 0 & F2$NumNdRam < 7,]     ; lat_F6=F6[F6$NumNdRam > 0 & F6$NumNdRam < 7,]     ; lat_B14=B14[B14$NumNdRam > 0 & B14$NumNdRam < 7,]

TA_apic=Tayllamin[Tayllamin$NumNdRam==0,];
TA_lat=Tayllamin[Tayllamin$NumNdRam > 0 & Tayllamin$NumNdRam < 7,];

# Tri en fonction du type de l'UC porteuse
apic_B10_port_apic=apic_B10[apic_B10$TypAxePo=="T",] ; apic_B12_port_apic=apic_B12[apic_B12$TypAxePo=="T",] ;
apic_F2_port_apic=apic_F2[apic_F2$TypAxePo=="T",]    ; apic_F6_port_apic=apic_F6[apic_F6$TypAxePo=="T",] ;
apic_B14_port_apic=apic_B14[apic_B14$TypAxePo=="T",]

lat_B10_port_apic=lat_B10[lat_B10$TypAxePo=="T",] ; lat_B12_port_apic=lat_B12[lat_B12$TypAxePo=="T",] ; 
lat_F2_port_apic=lat_F2[apic_F2$TypAxePo=="T",]   ; lat_F6_port_apic=lat_F6[lat_F6$TypAxePo=="T",] ;
lat_B14_port_apic=lat_B14[lat_B14$TypAxePo=="T",]

apic_B10_port_lat=apic_B10[apic_B10$TypAxePo=="R",] ; apic_B12_port_lat=apic_B12[apic_B12$TypAxePo=="R",] ;
apic_F2_port_lat=apic_F2[apic_F2$TypAxePo=="R",]    ; apic_F6_port_lat=apic_F6[apic_F6$TypAxePo=="R",] ;
apic_B14_port_lat=apic_B14[apic_B14$TypAxePo=="R",]

lat_B10_port_lat=lat_B10[lat_B10$TypAxePo=="R",] ; lat_B12_port_lat=lat_B12[lat_B12$TypAxePo=="R",];
lat_F2_port_lat=lat_F2[apic_F2$TypAxePo=="R",]   ; lat_F6_port_lat=lat_F6[lat_F6$TypAxePo=="R",] ;
lat_B14_port_lat=lat_B14[lat_B14$TypAxePo=="R",]

TA_apic_port_apic=TA_apic[TA_apic$TypAxePo=="T",] ; TA_lat_port_apic=TA_lat[TA_lat$TypAxePo=="T",];
TA_apic_port_lat=TA_apic[TA_apic$TypAxePo=="R",]  ; TA_lat_port_lat=TA_lat[TA_lat$TypAxePo=="R",];




#################
####   MA05  ####
#################

MA05_0=read.csv("MA05.csv",header=TRUE,sep=";",dec=",")

##### Suppression des données aberrantes
par(mfrow=c(1,2))
boxplot(MA05_0$NbFeu)
plot(MA05_0$NbFeu)
title("Etude du nombre de feuilles",outer=TRUE,line=-1)
summary(MA05_0$NbFeu,na.rm=T)

boxplot(MA05_0$Long)
plot(MA05_0$Long)
title("Etude de la longueur des Ucs",outer=TRUE,line=-1)
summary(MA05_0$Long,na.rm=T)
MA=MA05_0[(MA05_0$Long <25 & MA05_0$Long >1),]

# Séparation des UCs en fonction de leur type
MA_apic=MA[MA$position=="A",];MA_lat=MA[MA$position=="L",];


######################
####   inference  ####
######################

inference0=read.csv("inference.csv",header=TRUE,sep=";",dec=",")

##### Suppression des données aberrantes
par(mfrow=c(1,2))
boxplot(inference0$NbFeu)
plot(inference0$NbFeu)
title("Etude du nombre de feuilles",outer=TRUE,line=-1)
summary(inference0$NbFeu,na.rm=T)

boxplot(inference0$Long)
plot(inference0$Long)
title("Etude de la longueur des Ucs",outer=TRUE,line=-1)
summary(inference0$Long,na.rm=T)
inference0$Long=inference0$Long/10
inf=inference0[(inference0$Long <25 & inference0$Long >5),]


# Séparation des UCs en fonction de leur type
# Pas de séparation en fonction des arbres car pas assez de valeurs
inf_apic=inf[inf$position=="A",];inf_lat=inf[inf$position=="L",];





#################################################################################################################################
##########################################   Etude de la distribution du nombre de feuilles  ####################################
#################################################################################################################################

####################
####  Tayllamin ####
####################

# Histogrammes des UCs en position apicale pour chaque arbre et en global
par(mfrow=c(2,3))
hist(apic_B10$NbFeu,main="arbre B10",freq=FALSE,xlim=c(5,25),ylim=c(0,0.22),breaks=seq(2,25,1),xlab="Nombre de feuilles") ; 
hist(apic_B12$NbFeu,main="arbre B12",freq=FALSE,xlim=c(5,25),ylim=c(0,0.22),breaks=seq(2,25,1),xlab="Nombre de feuilles") ;
hist(apic_F2$NbFeu,main="arbre F2",freq=FALSE,xlim=c(5,25),ylim=c(0,0.22),breaks=seq(2,25,1),xlab="Nombre de feuilles")   ; 
hist(apic_F6$NbFeu,main="arbre F6",freq=FALSE,xlim=c(5,25),ylim=c(0,0.22),breaks=seq(2,25,1),xlab="Nombre de feuilles") ;
hist(apic_B14$NbFeu,main="arbre B14",freq=FALSE,xlim=c(5,25),ylim=c(0,0.22),breaks=seq(2,25,1),xlab="Nombre de feuilles") ;
hist(TA_apic$NbFeu,main="Tous les arbres",freq=FALSE,xlim=c(5,25),ylim=c(0,0.22),breaks=seq(2,25,1),xlab="Nombre de feuilles")
#title("Nombre de feuilles issues d'UCs en position apicale",outer=TRUE,line=-1)

summary(apic_B10$NbFeu) ; summary(apic_B12$NbFeu) ; summary(apic_F2$NbFeu) ; 
summary(apic_F6$NbFeu)  ; summary(apic_B14$NbFeu)
summary(TA_apic$NbFeu,na.rm = TRUE)


# Histogrammes des UCs en position latérale pour chaque arbre et en global
par(mfrow=c(2,3))
hist(lat_B10$NbFeu,main="arbre B10",freq=FALSE,xlim=c(3,19),ylim=c(0,0.45),breaks=seq(3,19,1),xlab="Nombre de feuilles") ; 
hist(lat_B12$NbFeu,main="arbre B12",freq=FALSE,xlim=c(3,19),ylim=c(0,0.45),breaks=seq(3,19,1),xlab="Nombre de feuilles") ;
hist(lat_F2$NbFeu,main="arbre F2",freq=FALSE,xlim=c(3,19),ylim=c(0,0.45),breaks=seq(3,19,1),xlab="Nombre de feuilles")   ; 
hist(lat_F6$NbFeu,main="arbre F6",freq=FALSE,xlim=c(3,19),ylim=c(0,0.45),breaks=seq(3,19,1),xlab="Nombre de feuilles")   ;
hist(lat_B14$NbFeu,main="arbre B14",freq=FALSE,xlim=c(3,19),ylim=c(0,0.45),breaks=seq(3,19,1),xlab="Nombre de feuilles") ;
hist(TA_lat$NbFeu,main="Tous les arbres",freq=FALSE,xlim=c(3,19),ylim=c(0,0.45),breaks=seq(3,19,1),xlab="Nombre de feuilles")
#title("Nombre de feuilles issues d'UCs en position latérale",outer=TRUE,line=-1)

summary(lat_B10$NbFeu) ; summary(lat_B12$NbFeu) ; summary(lat_F2$NbFeu) ;
summary(lat_F6$NbFeu)  ; summary(lat_B14$NbFeu)
summary(TA_lat$NbFeu,na.rm = TRUE)


mean(TA_apic$NbFeu)       # 14.99479
mean(TA_lat$NbFeu)        # 8.126126
sd(TA_apic$NbFeu)         # 3.624376
sd(TA_lat$NbFeu)          # 2.741064


##### Histogrammes pour voir le lien entre position des UCs mères et nbr de feuilles

par(mfrow=c(4,3))
hist(apic_B10_port_apic$NbFeu,main="arbre B10",freq=FALSE);hist(apic_B12_port_apic$NbFeu,main="arbre B12",freq=FALSE);hist(apic_F2_port_apic$NbFeu,main="arbre F2",freq=FALSE);hist(apic_F6_port_apic$NbFeu,main="arbre F6",freq=FALSE);hist(apic_B14_port_apic$NbFeu,main="arbre B14",freq=FALSE);
hist(TA_apic_port_apic$NbFeu,main="Tous les arbres",freq=FALSE)
hist(apic_B10_port_lat$NbFeu,main="arbre B10",freq=FALSE);hist(apic_B12_port_lat$NbFeu,main="arbre B12",freq=FALSE);hist(apic_F2_port_lat$NbFeu,main="arbre F2",freq=FALSE);hist(apic_F6_port_lat$NbFeu,main="arbre F6",freq=FALSE);hist(apic_B14_port_lat$NbFeu,main="arbre B14",freq=FALSE);
hist(TA_apic_port_lat$NbFeu,main="Tous les arbres",freq=FALSE)


par(mfrow=c(4,3))
hist(lat_B10_port_apic$NbFeu,main="arbre B10",freq=FALSE);hist(lat_B12_port_apic$NbFeu,main="arbre B12",freq=FALSE);hist(lat_F2_port_apic$NbFeu,main="arbre F2",freq=FALSE);hist(lat_F6_port_apic$NbFeu,main="arbre F6",freq=FALSE);hist(lat_B14_port_apic$NbFeu,main="arbre B14",freq=FALSE);
hist(TA_lat_port_apic$NbFeu,main="Tous les arbres",freq=FALSE)
hist(lat_B10_port_lat$NbFeu,main="arbre B10",freq=FALSE);hist(lat_F2_port_lat$NbFeu,main="arbre F2",freq=FALSE);hist(lat_B14_port_lat$NbFeu,main="arbre B14",freq=FALSE);
hist(TA_lat_port_lat$NbFeu,main="Tous les arbres",freq=FALSE)

# Histogramme 4 cas (apicale / latérale, pour UC et UC mère)
x=2:23
par(mfrow=c(2,2))
hist(TA_apic_port_apic$NbFeu,main="UC mère apicale, UC apicale",freq=FALSE,breaks=seq(2,23,1),xlim=c(2,23),ylim=c(0,0.5))
	points(dpois(x,mean(TA_apic_port_apic$NbFeu)),col=2,type='l')
hist(TA_apic_port_lat$NbFeu,main="UC mère latérale, UC apicale",freq=FALSE,breaks=seq(2,23,1),xlim=c(2,23),ylim=c(0,0.5))
	points(dpois(x,mean(TA_apic_port_lat$NbFeu)),col=2,type='l')
hist(TA_lat_port_apic$NbFeu,main="UC mère apicale, UC apicale",freq=FALSE,breaks=seq(2,23,1),xlim=c(2,23),ylim=c(0,0.5))
	points(dpois(x,mean(TA_lat_port_apic$NbFeu)),col=2,type='l')
hist(TA_lat_port_lat$NbFeu,main="UC mère latérale, UC latérale",freq=FALSE,breaks=seq(2,23,1),xlim=c(2,23),ylim=c(0,0.5))
title("Nombre de feuilles issues d'une UC",outer=TRUE,line=-1)


# Arbre B12
par(mfrow=c(1,3))
hist(apic_B12_port_lat$NbFeu,main="UC mère latérale",xlim=c(4,22),ylim=c(0,0.25),breaks=seq(3,19,1),xlab="Nombre de feuilles",freq=FALSE);
hist(apic_B12_port_apic$NbFeu,main="UC mère apicale",xlim=c(4,22),ylim=c(0,0.25),breaks=seq(2,25,1),xlab="Nombre de feuilles",freq=FALSE);
hist(apic_B12$NbFeu,main="Toutes UCs",xlim=c(4,22),ylim=c(0,0.25),breaks=seq(2,25,1),xlab="Nombre de feuilles",freq=FALSE);
#title("Nombre de feuilles issues d'UCs en position apicale, Arbre B12",outer=TRUE,line=-1)

mean(TA_apic_port_apic$NbFeu)       # 16.66667
mean(TA_apic_port_lat$NbFeu)        # 12.20833
mean(TA_lat_port_apic$NbFeu)        # 8.136364
mean(TA_lat_port_lat$NbFeu)         # 7
sd(TA_apic_port_apic$NbFeu)         # 2.879465
sd(TA_apic_port_lat$NbFeu)          # 2.97377
sd(TA_lat_port_apic$NbFeu)          # 2.744783
sd(TA_lat_port_lat$NbFeu)           # 2.828427


# UCs porteuses
par(mfrow=c(1,2))
hist(port_apic$NbFeu,main="en position apicale",freq=FALSE)
hist(port_lat$NbFeu,main="en position latérale",freq=FALSE)
title("Nombre de feuilles issues d'UCs porteuses",outer=TRUE,line=-1)


# Analyse de variance
TA_aov=Tayllamin[Tayllamin$NumNdRam<7,]
TA_aov[TA_aov$NumNdRam>0,]$NumNdRam=1
TA_aov$NumNdRam=factor(TA_aov$NumNdRam)

TA_aov_apic=TA_aov[TA_aov$NumNdRam==0,]
TA_aov_lat=TA_aov[TA_aov$NumNdRam==1,]

NbFeu_aov=aov(TA_aov$NbFeu ~ TA_aov$NumNdRam*TA_aov$TypAxePo)
summary(NbFeu_aov)
NbFeu_aov_apic=aov(TA_aov_apic$NbFeu ~ TA_aov_apic$TypAxePo)
summary(NbFeu_aov)
NbFeu_aov_lat=aov(TA_aov_lat$NbFeu ~ TA_aov_lat$TypAxePo)
summary(NbFeu_aov)

par(mfrow=c(1,1))
interaction.plot(TA_aov$NumNdRam, TA_aov$TypAxePo, TA_aov$NbFeu,main="", legend=F,xlab=c("Position UC") , ylab="Nombre de Feuilles",xaxt = "n")
axis(1, at = seq(2), labels = c("A","L"), tick = TRUE)
legend("topright",c("A","L"),lty=c(1,2),title="Position UC mère")

#### Tests sur les distributions 
arb_aov=aov(TA_aov$NbFeu ~ TA_aov$CodeArbre*TA_aov$NumNdRam)
summary(arb_aov)

par(mfrow=c(1,2))
TukeyHSD(arb_aov)
plot(TukeyHSD(arb_aov))



##### Ajustement d'une distribution
# Histogramme 3 cas 
x=2:25
par(mfrow=c(2,3))
hist(TA_apic_port_apic$NbFeu,main="UC mère apicale, UC apicale",freq=FALSE,breaks=seq(2,23,1),xlim=c(2,25),ylim=c(0,0.22),xlab="Nombre de feuilles")
	points(dpois(x,mean(TA_apic_port_apic$NbFeu)),col=2,type='l')
legend("topleft","P(16.66)",col=2,lty=1,bty="n")
hist(TA_apic_port_lat$NbFeu,main="UC mère latérale, UC apicale",freq=FALSE,breaks=seq(2,23,1),xlim=c(2,25),ylim=c(0,0.22),xlab="Nombre de feuilles")
	points(dpois(x,mean(TA_apic_port_lat$NbFeu)),col=2,type='l')
legend("topright","P(12.20)",col=2,lty=1,bty="n")
hist(TA_lat$NbFeu,main="UC mère apicale, UC apicale",freq=FALSE,breaks=seq(2,23,1),xlim=c(2,25),ylim=c(0,0.22),xlab="Nombre de feuilles")
	points(dpois(x,mean(TA_lat$NbFeu)),col=2,type='l')
legend("topright","P(8.13)",col=2,lty=1,bty="n")

plot(ecdf(TA_apic_port_apic$NbFeu),main="UC mère apicale, UC apicale",xlim=c(2,25))
	points(ppois(x,mean(TA_apic_port_apic$NbFeu)),col=2,type='l')
plot(ecdf(TA_apic_port_lat$NbFeu),main="UC mère latérale, UC apicale",xlim=c(2,25))
	points(ppois(x,mean(TA_apic_port_lat$NbFeu)),col=2,type='l')
plot(ecdf(TA_lat$NbFeu),main="UC mère apicale, UC apicale",xlim=c(2,25))
	points(ppois(x,mean(TA_lat$NbFeu)),col=2,type='l')
#title("Nombre de feuilles issues d'une UC",outer=TRUE,line=-1)

# Tests d'adéquation à la loi de Poisson
par(mfrow=c(1,3))
n=length(TA_apic_port_apic$NbFeu);lambda=mean(TA_apic_port_apic$NbFeu)
plot(qpois(c(1:n)/(n+1),lambda),quantile(TA_apic_port_apic$NbFeu,probs=c(1:n)/(n+1)),main="UC apicale de mère apicale",xlab="n",ylab="n")
abline(lsfit(qpois(c(1:n)/(n+1),lambda),quantile(TA_apic_port_apic$NbFeu,probs=c(1:n)/(n+1))),col=2)

n2=length(TA_apic_port_lat$NbFeu);lambda2=mean(TA_apic_port_lat$NbFeu)
plot(qpois(c(1:n2)/(n2+1),lambda2),quantile(TA_apic_port_lat$NbFeu,probs=c(1:n2)/(n2+1)),main="UC apicale de mère latérale",xlab="n",ylab="n")
abline(lsfit(qpois(c(1:n2)/(n2+1),lambda2),quantile(TA_apic_port_lat$NbFeu,probs=c(1:n2)/(n2+1))),col=2)

n3=length(TA_lat$NbFeu);lambda3=mean(TA_lat$NbFeu)
plot(qpois(c(1:n3)/(n3+1),lambda3),quantile(TA_lat$NbFeu,probs=c(1:n3)/(n3+1)),main="UC latérale",xlab="n",ylab="n")
abline(lsfit(qpois(c(1:n3)/(n3+1),lambda3),quantile(TA_lat$NbFeu,probs=c(1:n3)/(n3+1))),col=2)
#title("Droite quantile-quantile de NbFeu avec la Loi de Poisson ",outer=TRUE,line=-1)



#################
####   MA05  ####
#################

par(mfrow=c(1,2))
hist(MA_apic$NbFeu,main="UCs en position apicale",freq=FALSE,xlim=c(1,21),ylim=c(0,0.15),breaks=seq(1,21,1),xlab="Nombre de feuilles")
hist(MA_lat$NbFeu,main="UCs en position latérale",freq=FALSE,xlim=c(1,21),ylim=c(0,0.15),breaks=seq(1,18,1),xlab="Nombre de feuilles")
#title("Nombre de feuilles issues d'une UC ",outer=TRUE,line=-1)

summary(MA_apic$NbFeu,na.rm = TRUE)
summary(MA_lat$NbFeu,na.rm = TRUE)

mean(MA_apic$NbFeu)       # 10.91413
mean(MA_lat$NbFeu)        # 7.506536
sd(MA_apic$NbFeu)         # 3.956127
sd(MA_lat$NbFeu)          # 3.082864

# Analyse de variance
NbFeuMA_aov=aov(MA$NbFeu ~ MA$position)
summary(NbFeuMA_aov)

par(mfrow=c(2,2))
x=0:25
hist(MA_apic$NbFeu,main="UCs en position apicale",freq=FALSE,xlim=c(0,25),ylim=c(0,0.15),breaks=seq(1,21,1),xlab="Nombre de feuilles");points(dpois(x,mean(MA_apic$NbFeu) ),type='l',col=2)
legend("topright",lty=1,col=2,"P(10.91)",bty="n")
hist(MA_lat$NbFeu,main="UCs en position latérale",freq=FALSE,xlim=c(0,25),ylim=c(0,0.15),breaks=seq(1,21,1),xlab="Nombre de feuilles");points(dpois(x,mean(MA_lat$NbFeu) ),type='l',col=2)
legend("topright",lty=1,col=2,"P(7.51)",bty="n")
plot(ecdf(MA_apic$NbFeu),main="UCs en position apicale",xlim=c(0,25));
	points(ppois(x,mean(MA_apic$NbFeu) ),type='l',col=2)
plot(ecdf(MA_lat$NbFeu),main="UCs en position latérale",xlim=c(0,25));
	points(ppois(x,mean(MA_lat$NbFeu) ),type='l',col=2)
#title("Nombre de feuilles issues d'une UC ",outer=TRUE,line=-1)


# Tests d'adéquation à la loi de Poisson
#par(mfrow=c(1,2))
#n=length(MA_apic$NbFeu);lambda=mean(MA_apic$NbFeu)
#plot(qpois(c(1:n)/(n+1),lambda),quantile(MA_apic$NbFeu,probs=c(1:n)/(n+1)),main="UC apicale de mère apicale",xlab="n",ylab="n")
#abline(lsfit(qpois(c(1:n)/(n+1),lambda),quantile(MA_apic$NbFeu,probs=c(1:n)/(n+1))),col=2)

#n2=length(MA_lat$NbFeu);lambda2=mean(MA_lat$NbFeu)
#plot(qpois(c(1:n2)/(n2+1),lambda2),quantile(MA_lat$NbFeu,probs=c(1:n2)/(n2+1)),main="UC apicale de mère latérale",xlab="n",ylab="n")
#abline(lsfit(qpois(c(1:n2)/(n2+1),lambda2),quantile(MA_lat$NbFeu,probs=c(1:n2)/(n2+1))),col=2)


######################
####   inference  ####
######################

par(mfrow=c(1,2))
hist(inf_apic$NbFeu,main="UCs en position apicale",freq=FALSE,xlim=c(3,22),ylim=c(0,0.28),breaks=seq(3,22,1),xlab="Nombre de feuilles")
hist(inf_lat$NbFeu,main="UCs en position latérale",freq=FALSE,xlim=c(3,22),ylim=c(0,0.28),breaks=seq(3,22,1),xlab="Nombre de feuilles")
#title("Nombre de feuilles issues d'une UC ",outer=TRUE,line=-1)

summary(inf_apic$NbFeu,na.rm = TRUE)
summary(inf_lat$NbFeu,na.rm = TRUE)

mean(inf_apic$NbFeu)       # 17.26667
mean(inf_lat$NbFeu)        # 9.071429
sd(inf_apic$NbFeu)         # 2.083322
sd(inf_lat$NbFeu)          # 2.512339

# Analyse de variance
inf_aov=inf
NbFeu_aov_inf=aov(inf_aov$NbFeu ~ inf_aov$position)
summary(NbFeu_aov_inf)


#################################################################################################################################
##########################################   Etude de la distribution de la longueur des UCs  ###################################
#################################################################################################################################

####################
####  Tayllamin ####
####################

# UC en position apicale pour chaque arbre et en global

par(mfrow=c(2,3))
hist(apic_B10$Long,main="arbre B10",freq=FALSE,ylim=c(0,0.2),xlim=c(5,26),breaks=seq(0,30,1),xlab="Longueur UC")
hist(apic_B12$Long,main="arbre B12",freq=FALSE,ylim=c(0,0.2),xlim=c(5,26),breaks=seq(0,30,1),xlab="Longueur UC")
hist(apic_F2$Long,main="arbre F2",freq=FALSE,ylim=c(0,0.2),xlim=c(5,26),breaks=seq(0,30,1),xlab="Longueur UC")
hist(apic_F6$Long,main="arbre F6",freq=FALSE,ylim=c(0,0.2),xlim=c(5,26),breaks=seq(0,30,1),xlab="Longueur UC")
hist(apic_B14$Long,main="arbre B14",freq=FALSE,ylim=c(0,0.2),xlim=c(5,26),breaks=seq(0,30,1),xlab="Longueur UC")
hist(TA_apic$Long,main="Tous les arbres",freq=FALSE,ylim=c(0,0.2),xlim=c(5,26),breaks=seq(0,30,1),xlab="Longueur UC")
#title("Longueur des UCs en position apicale",outer=TRUE,line=-1)

summary(apic_B10$Long);summary(apic_B12$Long);summary(apic_F2$Long);summary(apic_F6$Long);summary(apic_B14$Long)
summary(TA_apic$Long,na.rm = TRUE)


# UC en position latérale pour chaque arbre et en global
par(mfrow=c(2,3))
hist(lat_B10$Long,main="arbre B10",freq=FALSE,ylim=c(0,0.25),xlim=c(6,25),breaks=seq(0,30,1),xlab="Longueur UC")
hist(lat_B12$Long,main="arbre B12",freq=FALSE,ylim=c(0,0.25),xlim=c(6,25),breaks=seq(0,30,1),xlab="Longueur UC")
hist(lat_F2$Long,main="arbre F2",freq=FALSE,ylim=c(0,0.25),xlim=c(6,25),breaks=seq(0,30,1),xlab="Longueur UC")
hist(lat_F6$Long,main="arbre F6",freq=FALSE,ylim=c(0,0.25),xlim=c(6,25),breaks=seq(0,30,1),xlab="Longueur UC")
hist(lat_B14$Long,main="arbre B14",freq=FALSE,ylim=c(0,0.25),xlim=c(6,25),breaks=seq(0,30,1),xlab="Longueur UC")
hist(TA_lat$Long,main="Tous les arbres",freq=FALSE,ylim=c(0,0.25),xlim=c(6,25),breaks=seq(0,30,1),xlab="Longueur UC")
#title("Longueur des UCs en position latérale",outer=TRUE,line=-1)

summary(lat_B10$Long);summary(lat_B12$Long);summary(lat_F2$Long);summary(lat_F6$Long);summary(lat_B14$Long)
summary(TA_lat$Long,na.rm = TRUE)

mean(TA_apic$Long)       # 16.50469
mean(TA_lat$Long)        # 12.59279
sd(TA_apic$Long)         # 4.605895
sd(TA_lat$Long)          # 3.391971



# Tous les arbres réunis 
par(mfrow=c(1,1))
hist(TA_apic$Long,main="Longueurs UCs apicales",freq=FALSE,ylim=c(0,0.25),xlim=c(6,25),breaks=seq(0,30,1),xlab="Longueur UC")
hist(TA_lat$Long,main="Longueurs UCs latérales",freq=FALSE,ylim=c(0,0.25),xlim=c(6,25),breaks=seq(0,30,1),xlab="Longueur UC")


##### Lien entre position de UC porteuse et nbr de feuilles
par(mfrow=c(4,3))
hist(apic_B10_port_apic$Long,main="arbre B10",freq=FALSE) ; hist(apic_B12_port_apic$Long,main="arbre B12",freq=FALSE) ;
hist(apic_F2_port_apic$Long,main="arbre F2",freq=FALSE)   ; hist(apic_F6_port_apic$Long,main="arbre F6",freq=FALSE)   ;
hist(apic_B14_port_apic$Long,main="arbre B14",freq=FALSE) ;
hist(TA_apic_port_apic$Long,main="Tous les arbres",freq=FALSE)
hist(apic_B10_port_lat$Long,main="arbre B10",freq=FALSE)  ; hist(apic_B12_port_lat$Long,main="arbre B12",freq=FALSE) ;
hist(apic_F2_port_lat$Long,main="arbre F2",freq=FALSE)    ; hist(apic_F6_port_lat$Long,main="arbre F6",freq=FALSE)   ;
hist(apic_B14_port_lat$Long,main="arbre B14",freq=FALSE);
hist(TA_apic_port_lat$Long,main="Tous les arbres",freq=FALSE)


par(mfrow=c(4,3))
hist(lat_B10_port_apic$Long,main="arbre B10",freq=FALSE);hist(lat_B12_port_apic$Long,main="arbre B12",freq=FALSE);hist(lat_F2_port_apic$Long,main="arbre F2",freq=FALSE);hist(lat_F6_port_apic$Long,main="arbre F6",freq=FALSE);hist(lat_B14_port_apic$NbFeu,main="arbre B14",freq=FALSE);
hist(TA_lat_port_apic$Long,main="Tous les arbres",freq=FALSE)
hist(lat_B10_port_lat$Long,main="arbre B10",freq=FALSE);hist(lat_F2_port_lat$Long,main="arbre F2",freq=FALSE);hist(lat_B14_port_lat$Long,main="arbre B14",freq=FALSE);
hist(TA_lat_port_lat$Long,main="Tous les arbres",freq=FALSE)

# Histogramme 4 cas (apicale / latérale, pour UC et UC mère)
par(mfrow=c(2,2))
hist(TA_apic_port_apic$Long,main="UC mère apicale, UC apicale",freq=FALSE,breaks=seq(2,25,1),xlim=c(5.5,24.8),ylim=c(0,0.2))
	curve(dnorm(x,mean(TA_apic_port_apic$Long),sd(TA_apic_port_apic$Long)),col=2,type='l',add=TRUE)
hist(TA_apic_port_lat$Long,main="UC mère latérale, UC apicale",freq=FALSE,breaks=seq(2,25,1),xlim=c(5.5,24.8),ylim=c(0,0.2))
	curve(dnorm(x,mean(TA_apic_port_lat$Long),sd(TA_apic_port_lat$Long)),col=2,type='l',add=TRUE)
hist(TA_lat_port_apic$Long,main="UC mère apicale, UC apicale",freq=FALSE,breaks=seq(2,25,1),xlim=c(5.5,24.8),ylim=c(0,0.2))
	curve(dnorm(x,mean(TA_lat_port_apic$Long),sd(TA_lat_port_apic$Long)),col=2,type='l',add=TRUE)
hist(TA_lat_port_lat$Long,main="UC mère latérale, UC latérale",freq=FALSE,breaks=seq(2,25,1),xlim=c(5.5,24.8),ylim=c(0,0.5))
title("Longueur d'une UC",outer=TRUE,line=-1)



# Arbre B12
par(mfrow=c(1,3))
hist(apic_B12_port_apic$Long,main="UC mère apicale",freq=FALSE,xlim=c(5,25),ylim=c(0,0.2),breaks=seq(0,30,1),xlab="Longueur UC");
hist(apic_B12_port_lat$Long,main="UC mère latérale",freq=FALSE,xlim=c(5,25),ylim=c(0,0.2),breaks=seq(0,30,1),xlab="Longueur UC");
hist(apic_B12$Long,main="Toutes UCs",freq=FALSE,xlim=c(5,25),ylim=c(0,0.2),breaks=seq(0,30,1),xlab="Longueur UC");
#title("Longueur des UCs en position apicale, Arbre B12",outer=TRUE,line=-1)


# Récapitulatif des résultats
mean(TA_apic_port_apic$Long)       # 18.13583
mean(TA_apic_port_lat$Long)        # 13.78611
mean(TA_lat_port_apic$Long)        # 12.59273
mean(TA_lat_port_lat$Long)         # 12.6

sd(TA_apic_port_apic$Long)         # 4.145926
sd(TA_apic_port_lat$Long)          # 4.033427
sd(TA_lat_port_apic$Long)          # 3.384823
sd(TA_lat_port_lat$Long)           # 5.798276


#UCs porteuses
hist(port_apic$Long,main="UCs porteuses en position apicale",freq=FALSE)
hist(port_lat$Long,main="UCs porteuses en position latérale",freq=FALSE)

#############################
####### Analyse de variance
###########

################# Effet position UC apicale / latérale
aov.posUC=aov(TA_aov$Long ~ TA_aov$NumNdRam)

# Test d'homoscédasticité
g = factor(rep(1:2, c(length(TA_aov_apic$Long),length(TA_aov_lat$Long))), labels = c("apic","lat"))
leveneTest(c(TA_aov_apic$Long,TA_aov_lat$Long),g)                                        #n=414  F=26.424   p-value=4.244e-07


# Test de normalité
shapiro.test(aov.posUC$res[TA_aov$NumNdRam==0])   #W = 0.9624, p-value = 5.267e-05
shapiro.test(aov.posUC$res[TA_aov$NumNdRam==1])   #W = 0.9793, p-value = 0.002377
ks.test(aov.posUC$res[TA_aov$NumNdRam==0],"pnorm",0,sd(aov.posUC$res));        #D = 0.1131, p-value = 0.01471
ks.test(aov.posUC$res[TA_aov$NumNdRam==1],"pnorm",0,sd(aov.posUC$res))         #D = 0.0592, p-value = 0.41
# la normalité est rejetée


# Kruskal-Wallis
kruskal.test(TA_aov$Long ~ TA_aov$NumNdRam)           #Kruskal-Wallis chi-squared = 73.3158, df = 1, p-value < 2.2e-16


################# Effet position UC mère apicale / latérale
#install.packages('Rcmdr')
library('Rcmdr')
Long_aov_apic=aov(TA_aov_apic$Long ~ TA_aov_apic$TypAxePo-1)
summary(Long_aov_apic)
coef_aov_apic=Long_aov_apic$coef     #   TypAxePoR    TypAxePoT
                       #   13.786111     18.13583  


# homoscédasticité
g_apic = factor(rep(1:2, c(length(TA_apic_port_apic$Long),length(TA_apic_port_lat$Long))), labels = c("apic_portapic","apic_portlat"))
leveneTest(c(TA_apic_port_apic$Long,TA_apic_port_lat$Long),g_apic)       # n=192  F=0.0186  p-value=0.8916

# normalité des résidus
res_aov_apic=Long_aov_apic$res
par(mfrow=c(2,3))
hist(res_aov_apic[TA_aov_apic$TypAxePo=="T" ],main="Histogramme",freq=F,xlab="Résidus des longueurs d'UC mère apicale");curve(dnorm(x,0,sd(res_aov_apic)),col=2,add=T)
plot(ecdf(res_aov_apic[TA_aov_apic$TypAxePo=="T" ]),main="Fonction de répartition");curve(pnorm(x,0,sd(res_aov_apic)),col=2,add=T)
qqnorm(res_aov_apic[TA_aov_apic$TypAxePo=="T" ],main="Droite de Henry");qqline(res_aov_apic[TA_aov_apic$TypAxePo=="T" ],col=2)

hist(res_aov_apic[TA_aov_apic$TypAxePo=="R" ],main="Histogramme",freq=F,xlab="Résidus des longueurs d'UC mère latérale");curve(dnorm(x,0,sd(res_aov_apic)),col=2,add=T)
plot(ecdf(res_aov_apic[TA_aov_apic$TypAxePo=="R" ]),main="Fonction de répartition");curve(pnorm(x,0,sd(res_aov_apic)),col=2,add=T)
qqnorm(res_aov_apic[TA_aov_apic$TypAxePo=="R" ],main="Droite de Henry");qqline(res_aov_apic[TA_aov_apic$TypAxePo=="R" ],col=2)

ks.test(res_aov_apic[TA_aov_apic$TypAxePo=="T" ],"pnorm",0,sd(res_aov_apic));   #D = 0.116, p-value = 0.07918
ks.test(res_aov_apic[TA_aov_apic$TypAxePo=="R" ],"pnorm",0,sd(res_aov_apic))    #D = 0.0931, p-value = 0.5601
shapiro.test(res_aov_apic[TA_aov_apic$TypAxePo=="T" ]);                         #W = 0.9345, p-value = 1.848e-05
shapiro.test(res_aov_apic[TA_aov_apic$TypAxePo=="R" ])                          #W = 0.9688, p-value = 0.07029               
      
# test d'égalité des variances (CN pour utilisation ANOVA)
var.test(res_aov_apic[TA_aov_apic$TypAxePo=="T" ],res_aov_apic[TA_aov_apic$TypAxePo=="R" ])



##### Ajustement d'une distribution
# Histogramme 3 cas
par(mfrow=c(2,3))
hist(TA_apic_port_apic$Long,main="UC apicale de mère apicale",freq=FALSE,breaks=seq(2,25,1),xlim=c(5.5,24.8),ylim=c(0,0.15),xlab="Longueur UC")
	curve(dnorm(x,mean(TA_apic_port_apic$Long),sd(TA_apic_port_apic$Long)),col=2,type='l',add=TRUE)
legend("topleft",lty=1,col=2,"N(18.14,4.14)",bty="n")
hist(TA_apic_port_lat$Long,main="UC apicale de mère latérale",freq=FALSE,breaks=seq(2,25,1),xlim=c(5.5,24.8),ylim=c(0,0.15),xlab="Longueur UC")
	curve(dnorm(x,mean(TA_apic_port_lat$Long),sd(TA_apic_port_lat$Long)),col=2,type='l',add=TRUE)
legend("topright",lty=1,col=2,"N(13.79,4.03)",bty="n")
hist(TA_lat$Long,main="UC latérale",freq=FALSE,breaks=seq(2,25,1),xlim=c(5.5,24.8),ylim=c(0,0.15),xlab="Longueur UC")
	curve(dnorm(x,mean(TA_lat_port_apic$Long),sd(TA_lat_port_apic$Long)),col=2,type='l',add=TRUE)
legend("topright",lty=1,col=2,"N(12.59,3.38)",bty="n")

plot(ecdf(TA_apic_port_apic$Long),main="UC apicale de mère apicale")
	curve(pnorm(x,mean(TA_apic_port_apic$Long),sd(TA_apic_port_apic$Long)),col=2,type='l',add=TRUE)
plot(ecdf(TA_apic_port_lat$Long),main="UC apicale de mère latérale")
	curve(pnorm(x,mean(TA_apic_port_lat$Long),sd(TA_apic_port_lat$Long)),col=2,type='l',add=TRUE)
plot(ecdf(TA_lat$Long),main="UC latérale")
	curve(pnorm(x,mean(TA_lat_port_apic$Long),sd(TA_lat_port_apic$Long)),col=2,type='l',add=TRUE)
#title("Longueur d'une UC",outer=TRUE,line=-1)


# Test d'adéquation à la distribution
#par(mfrow=c(1,3))
#qqnorm(TA_apic_port_apic$Long,main="UCs apicales de mère apicale");qqline(TA_apic_port_apic$Long,col=2)
#qqnorm(TA_apic_port_lat$Long,main="UCs apicales de mère latérale");qqline(TA_apic_port_lat$Long,col=2)
#qqnorm(TA_lat$Long,main="UCs latérales");qqline(TA_lat$Long,col=2)
#title("Droite de Henry, longueur des UCs ",outer=TRUE,line=-1)



ks.test(TA_apic_port_apic$Long,"pnorm",mean(TA_apic_port_apic$Long),sd(TA_apic_port_apic$Long))   # 0.08007
ks.test(TA_apic_port_lat$Long,"pnorm",mean(TA_apic_port_lat$Long),sd(TA_apic_port_lat$Long))      # 0.5524
ks.test(Long_lat,"pnorm",mean(TA_lat_port_apic$Long),sd(TA_lat_port_apic$Long))                   # 0.5224

#### Tests sur les distributions 
arb_aov_long=aov(TA_aov$Long ~ TA_aov$CodeArbre*TA_aov$NumNdRam)
summary(arb_aov_long)

par(mfrow=c(1,2))
TukeyHSD(arb_aov_long)
plot(TukeyHSD(arb_aov_long))


#################
####   MA05  ####
#################

par(mfrow=c(1,2))
hist(MA_apic$Long,main="UCs en position apicale",freq=FALSE,breaks=seq(0,30,1),xlab="Longueur UC");#curve(dnorm(x,mean(MA_apic$Long),sd(MA_apic$Long)),add=TRUE,col=2)
hist(MA_lat$Long,main="UCs en position latérale",freq=FALSE,breaks=seq(0,30,1),xlab="Longueur UC");#curve(dnorm(x,mean(MA_lat$Long),sd(MA_lat$Long)),add=TRUE,col=2)
#title("Longueur d'une UC ",outer=TRUE,line=-1)

summary(MA_apic$Long,na.rm = TRUE)
summary(MA_lat$Long,na.rm = TRUE)

mean(MA_apic$Long)       # 11.48312
mean(MA_lat$Long)        # 10.8972
sd(MA_apic$Long)         # 4.229558
sd(MA_lat$Long)          # 3.99177

# Analyse de variance
LongMA_aov=aov(MA$Long ~ MA$position)
summary(LongMA_aov)
hist(LongMA_aov$res)

# test d'égalité des variances (CN pour utilisation ANOVA)
g_apic = factor(rep(1:2, c(length(MA_apic$Long),length(MA_lat$Long))), labels = c("apic","lat"))
leveneTest(c(MA_apic$Long,MA_lat$Long),g_apic)


par(mfrow=c(2,2))
hist(MA_apic$Long,main="UCs en position apicale",freq=FALSE,xlab="Longueur UC",breaks=seq(0,30,1));curve(dnorm(x,mean(MA_apic$Long),sd(MA_apic$Long)),add=TRUE,col=2)
legend("topright",lty=1,col=2,"N(10.33,4.81)",bty="n")
hist(MA_lat$Long,main="UCs en position latérale",freq=FALSE,xlab="Longueur UC",breaks=seq(0,30,1));curve(dnorm(x,mean(MA_lat$Long),sd(MA_lat$Long)),add=TRUE,col=2)
legend("topright",lty=1,col=2,"N(9.45,4.69)",bty="n")
plot(ecdf(MA_apic$Long),main="UCs en position apicale");curve(pnorm(x,mean(MA_apic$Long),sd(MA_apic$Long)),add=TRUE,col=2)
plot(ecdf(MA_lat$Long),main="UCs en position latérale");curve(pnorm(x,mean(MA_lat$Long),sd(MA_lat$Long)),add=TRUE,col=2)

#title("Longueur d'une UC ",outer=TRUE,line=-1)

# Test d'adéquation à la distribution
#par(mfrow=c(1,2))
#qqnorm(MA_apic$Long,main="UCs apicales");qqline(MA_apic$Long,col=2)
#qqnorm(MA_lat$Long,main="UCs latérales");qqline(MA_lat$Long,col=2)

ks.test(MA_apic$Long,"pnorm",mean(MA_apic$Long),sd(MA_apic$Long))
ks.test(MA_lat$Long,"pnorm",mean(MA_lat$Long),sd(MA_lat$Long))
shapiro.test(MA_apic$Long)
shapiro.test(MA_lat$Long)


######################
####   inference  ####
######################

par(mfrow=c(1,2))
hist(inf_apic$Long,main="en position apicale",freq=FALSE,xlim=c(5,28),xlab="Longueur UC",ylim=c(0,0.18),breaks=seq(0,30,1));
hist(inf_lat$Long,main="en position latérale",freq=FALSE,xlim=c(5,28),xlab="Longueur UC",ylim=c(0,0.18),breaks=seq(0,30,1));
#title("Longueur d'une UC ",outer=TRUE,line=-1)

summary(inf_apic$Long,na.rm = TRUE)
summary(inf_lat$Long,na.rm = TRUE)

mean(inf_apic$Long)       # 17.73333
mean(inf_lat$Long)        # 13.51905
sd(inf_apic$Long)         # 4.210523
sd(inf_lat$Long)          # 3.746298

# Analyse de variance
Long_aov_inf=aov(inf_aov$Long ~ inf_aov$position)
summary(Long_aov_inf)
hist(Long_aov_inf$res)
qqnorm(Long_aov_inf$res)

# test d'égalité des variances (CN pour utilisation ANOVA)
g_apic = factor(rep(1:2, c(length(inf_apic$Long),length(inf_lat$Long))), labels = c("apic","lat"))
leveneTest(c(inf_apic$Long,inf_lat$Long),g_apic)

#################################################################################################################################
###########################     Correspondance des différents bases, Fusion, Estimation des lois    ##########################
#################################################################################################################################

##### Test  de correspondance des différents bases 

##### Nombre de feuilles par UC

# Analyse de variance
TA=Tayllamin[Tayllamin$NumNdRam<7,]
TA$NumNdRam[TA$NumNdRam==0]="A"   ; TA$NumNdRam[TA$NumNdRam!="A"]="L"
TA$NumNdRam=factor(TA$NumNdRam)
bases=cbind(c(TA$NbFeu,MA$NbFeu,inf$NbFeu),
		   c(TA$Long,MA$Long,inf$Long),
		   c(TA$NumNdRam,MA$position,inf$position),
		   c(rep(1,length(TA$NbFeu)),rep(2,length(MA$NbFeu)),rep(3,length(inf$NbFeu))))
base=data.frame(bases)
names(base)=c("NbFeu","Long","position","base")
base$position=factor(base$position)
base$base=factor(base$base)

anova_bases=aov(base$NbFeu ~ base$position*base$base)
summary(anova_bases)
par(mfrow=c(1,1))
interaction.plot(base$position, base$base,base$NbFeu,main="",xlab="Position de l'UC",ylab="Nombre de feuilles",legend=F,xaxt="n")
legend("topright",c("Tayllamin","MA05","Inférence"),title="Base",lty=c(3,2,1))
axis(1, at = seq(2), labels = c("A","L"), tick = TRUE)



# Test d'égalité des moyennes
TukeyHSD(anova_bases)
plot(TukeyHSD(anova_bases))

# Test d'égalité des médianes
wilcox.test(TA$NbFeu,MA$NbFeu,paired=F)
wilcox.test(TA$NbFeu,inf$NbFeu,paired=F)
wilcox.test(inf$NbFeu,MA$NbFeu,paired=F)


##### Longueur des UCs
anova_bases_long=aov(Long ~ base,data=base)
summary(anova_bases_long)

# test d'égalité des variances (CN pour utilisation ANOVA)
g = factor(rep(1:3, c(length(Tayllamin[,1]),length(MA[,1]),length(inf[,1]))), labels = c("TA","MA","inf"))
leveneTest(c(Tayllamin$Long,MA$Long,inf$Long),g)


par(mfrow=c(1,2))
TukeyHSD(anova_bases_long)
plot(TukeyHSD(anova_bases_long))

par(mfrow=c(1,1))
interaction.plot(base$position, base$base,base$Long,main="",xlab="Position de l'UC",ylab="Longueur UC",legend=F,xaxt="n")
legend("topright",c("Tayllamin","MA05","Inférence"),title="Base",lty=c(3,2,1))
axis(1, at = seq(2), labels = c("A","L"), tick = TRUE)

wilcox.test(inf$Long,Tayllamin$Long,paired=F)
wilcox.test(inf$Long,MA$Long,paired=F)
wilcox.test(MA$Long,Tayllamin$Long,paired=F)

ks.test(inf_lat$Long,TA_lat$Long)       #0.2976
ks.test(inf_lat$Long,MA_lat$Long)       #2.45e-06
ks.test(MA_lat$Long,TA_lat$Long)        #5.383e-10

ks.test(inf_apic$Long,TA_apic$Long)     #0.5595
ks.test(inf_apic$Long,MA_apic$Long)     #6.987e-08
ks.test(MA_apic$Long,TA_apic$Long)      #< 2.2e-16

######### Analyse de covariance
NbFeu.apic=c(inf_apic$NbFeu,TA_apic$NbFeu,MA_apic$NbFeu)
NbFeu.lat=c(inf_lat$NbFeu,TA_lat$NbFeu,MA_lat$NbFeu)
Long.apic=c(inf_apic$Long,TA_apic$Long,MA_apic$Long)
Long.lat=c(inf_lat$Long,TA_lat$Long,MA_lat$Long)
fich.apic=c(rep(1,length(inf_apic$NbFeu)),rep(2,length(TA_apic$NbFeu)),rep(3,length(MA_apic$NbFeu)))
fich.lat=c(rep(1,length(inf_lat$NbFeu)),rep(2,length(TA_lat$NbFeu)),rep(3,length(MA_lat$NbFeu)))

fich.cov=lm(Long.apic ~ NbFeu.apic*fich.apic)
summary(fich.cov)



############## Fusion des bases ###################
#Positions UC et UC mère
TA_pos=Tayllamin$NumNdRam
TA_pos[TA_pos==0]="A";TA_pos[TA_pos!="A"]="L"
TA_pos=factor(TA_pos)
pos=c(inf$position,TA_pos)
pos=factor(pos)

pos_me_TA=as.character(Tayllamin$TypAxePo)
pos_me_TA[pos_me_TA=="T"]="A";pos_me_TA[pos_me_TA!="A"]="L"
pos_me=c(inf$positionER,as.factor(pos_me_TA))
pos_me=factor(pos_me)


##### Nombre de feuilles
par(mfrow=c(1,2))
NbFeu=c(inf$NbFeu,Tayllamin$NbFeu)
NbFeu_apic=c(inf_apic$NbFeu,TA_apic$NbFeu)
NbFeu_lat=c(inf_lat$NbFeu,TA_lat$NbFeu)   

##### Longueur des UCs
par(mfrow=c(1,2))
Long=c(inf$Long,Tayllamin$Long)
Long_apic=c(inf_apic$Long,TA_apic$Long)  
Long_lat=c(inf_lat$Long,TA_lat$Long)      





#################################################################################################################################
##########################   Etude de la corrélation entre le nombre de feuilles et la taille de l'UC  ##########################
#################################################################################################################################

####################
####  Tayllamin ####
####################

cor(TA_apic$Long,TA_apic$NbFeu) 
cor(TA_lat$Long,TA_lat$NbFeu)


par(mfrow=c(1,2))
plot(TA_apic$Long,TA_apic$NbFeu,abline(lsfit(TA_apic$Long,TA_apic$NbFeu),col=2), main ="UCs apicales")
plot(TA_lat$Long,TA_lat$NbFeu,abline(lsfit(TA_lat$Long,TA_lat$NbFeu),col=2),main="UCs latérales")
title("Etude de la corrélation entre longueur d'UC et nbr de feuilles ",outer=TRUE,line=-1)



#################
####   MA05  ####
#################

cor.test(MA_apic$Long,MA_apic$NbFeu)         # 0.1800189, p-value = 0.001512
cor.test(MA_lat$Long,MA_lat$NbFeu)           # 0.3975795, p-value < 2.2e-16 

par(mfrow=c(1,2))
plot(MA_apic$Long,MA_apic$NbFeu, main ="UCs apicales",ylim=c(3,22),xlab="Longueur",ylab="Nombre de feuilles")      # abline(lsfit(MA_apic$Long,MA_apic$NbFeu),col=2),
plot(MA_lat$Long,MA_lat$NbFeu,main="UCs latérales",ylim=c(3,22),xlab="Longueur",ylab="Nombre de feuilles")         # abline(lsfit(MA_lat$Long,MA_lat$NbFeu),col=2),
#title("Etude de la corrélation entre longueur d'UC et nbr de feuilles ",outer=TRUE,line=-1)

apic.lm=lm(MA_apic$NbFeu ~ MA_apic$Long) ; summary(apic.lm)            # 0.02924, p-value: 0.001512   
lat.lm=lm(MA_lat$NbFeu ~ MA_lat$Long)    ; summary(lat.lm)             # 0.1564,  p-value: < 2.2e-16 



######################
####   inference  ####
######################

cor(inf_apic$Long,inf_apic$NbFeu)
cor(inf_lat$Long,inf_lat$NbFeu)

par(mfrow=c(1,2))
plot(inf_apic$Long,inf_apic$NbFeu,abline(lsfit(inf_apic$Long,inf_apic$NbFeu),col=2), main ="UCs apicales")
plot(inf_lat$Long,inf_lat$NbFeu,abline(lsfit(inf_lat$Long,inf_lat$NbFeu),col=2),main="UCs latérales")
title("Etude de la corrélation entre longueur d'UC et nbr de feuilles ",outer=TRUE,line=-1)



######################
####    Global    ####
######################

par(mfrow=c(1,2))
plot(TA_apic$Long,TA_apic$NbFeu,abline(lsfit(TA_apic$Long,TA_apic$NbFeu),col=2), main ="UCs apicales",col=2)
points(MA_apic$Long,MA_apic$NbFeu,abline(lsfit(MA_apic$Long,MA_apic$NbFeu),col=3), main ="UCs apicales",col=3)
points(inf_apic$Long,inf_apic$NbFeu,abline(lsfit(inf_apic$Long,inf_apic$NbFeu),col=4), main ="UCs apicales",col=4)

plot(TA_lat$Long,TA_lat$NbFeu,abline(lsfit(TA_lat$Long,TA_lat$NbFeu),col=2),main="UCs latérales",col=2)
points(MA_lat$Long,MA_lat$NbFeu,abline(lsfit(MA_lat$Long,MA_lat$NbFeu),col=3),main="UCs latérales",col=3)
points(inf_lat$Long,inf_lat$NbFeu,abline(lsfit(inf_lat$Long,inf_lat$NbFeu),col=4),main="UCs latérales",col=4)

title("Etude de la corrélation entre longueur d'UC et nbr de feuilles ",outer=TRUE,line=-1)


##### Etude de la corrélation entre le nombre de feuilles et la taille de l'UC

cor.test(Long_apic,NbFeu_apic)
cor.test(Long_lat,NbFeu_lat)

par(mfrow=c(1,2))
plot(Long_apic,NbFeu_apic,abline(lsfit(Long_apic,NbFeu_apic),col=2), main ="UCs apicales",xlim=c(5,26),ylim=c(0,26))
plot(Long_lat,NbFeu_lat,abline(lsfit(Long_lat,NbFeu_lat),col=2),main="UCs latérales",xlim=c(5,26),ylim=c(0,26))
title("Etude de la corrélation entre longueur d'UC et nbr de feuilles ",outer=TRUE,line=-1)

apic.lm=lm(NbFeu_apic ~ Long_apic);summary(apic.lm)
lat.lm=lm(NbFeu_lat ~ Long_lat);summary(lat.lm)


# On trace les ICs pour la moyenne et pour les individus (d'après Dagnélie, Statistique théorique et appliquée)
Variete.resid.apic=sum(resid(apic.lm)^2)/df.residual(apic.lm)
mean.x.apic = mean(log(Long_apic))
SCEx.apic = sum((log(Long_apic) - mean.x.apic)^2)
n.apic = length(Long_apic)
stud = qt(0.975, n-2)

conf.moy.inf.apic=as.numeric(sort(apic.lm$fitted.values))-(stud*sqrt(Variete.resid.apic*((1/n)+((sort(log(Long_apic))-mean.x.apic)^2/SCEx.apic))))
conf.moy.sup.apic=as.numeric(sort(apic.lm$fitted.values))+(stud*sqrt(Variete.resid.apic*((1/n)+((sort(log(Long_apic))-mean.x.apic)^2/SCEx.apic))))
conf.ind.inf.apic=as.numeric(sort(apic.lm$fitted.values))-(stud*sqrt(Variete.resid.apic*(((n+1)/n)+((sort(log(Long_apic))-mean.x.apic)^2/SCEx.apic))))
conf.ind.sup.apic=as.numeric(sort(apic.lm$fitted.values))+(stud*sqrt(Variete.resid.apic*(((n+1)/n)+((sort(log(Long_apic))-mean.x.apic)^2/SCEx.apic))))

Variete.resid.lat=sum(resid(lat.lm)^2)/df.residual(lat.lm)
mean.x.lat = mean(log(Long_lat))
SCEx.lat = sum((log(Long_lat) - mean.x.lat)^2)
n.lat = length(Long_lat)
stud = qt(0.975, n-2)

conf.moy.inf.lat=as.numeric(sort(lat.lm$fitted.values))-(stud*sqrt(Variete.resid.lat*((1/n)+((sort(log(Long_lat))-mean.x.lat)^2/SCEx.lat))))
conf.moy.sup.lat=as.numeric(sort(lat.lm$fitted.values))+(stud*sqrt(Variete.resid.lat*((1/n)+((sort(log(Long_lat))-mean.x.lat)^2/SCEx.lat))))
conf.ind.inf.lat=as.numeric(sort(lat.lm$fitted.values))-(stud*sqrt(Variete.resid.lat*(((n+1)/n)+((sort(log(Long_lat))-mean.x.lat)^2/SCEx.lat))))
conf.ind.sup.lat=as.numeric(sort(lat.lm$fitted.values))+(stud*sqrt(Variete.resid.lat*(((n+1)/n)+((sort(log(Long_lat))-mean.x.lat)^2/SCEx.lat))))


par(mfrow=c(1,2))
plot(Long_apic,NbFeu_apic,main="UCs apicales",xlim=c(5,26),ylim=c(0,26),xlab="Longueur UC",ylab="Nombre de feuilles")
abline(apic.lm,col=2)
points(sort(Long_apic),conf.moy.inf.apic,col=4,lty=2,type='l')
points(sort(Long_apic),conf.moy.sup.apic,col=4,lty=2,type='l')
points(sort(Long_apic),conf.ind.inf.apic,col=6,lty=2,type='l')
points(sort(Long_apic),conf.ind.sup.apic,col=6,lty=2,type='l')
legend("bottomright",c("droite de régression", "IC moyenne","IC individu"),lty=c(1,2),col=c(2,4,6))

plot(Long_lat,NbFeu_lat,main="UCs latérales",xlim=c(5,26),ylim=c(0,26),xlab="Longueur UC",ylab="Nombre de feuilles")
abline(lat.lm,col=2)
points(sort(Long_lat),conf.moy.inf.lat,col=4,lty=2,type='l')
points(sort(Long_lat),conf.moy.sup.lat,col=4,lty=2,type='l')
points(sort(Long_lat),conf.ind.inf.lat,col=6,lty=2,type='l')
points(sort(Long_lat),conf.ind.sup.lat,col=6,lty=2,type='l')
legend("topleft",c("droite de régression", "IC moyenne","IC individu"),lty=c(1,2),col=c(2,4,6))


# Analyse de coVariance (test même intercept et même pente)
lm.cov=lm(NbFeu ~ Long*pos)
anova(lm.cov)
summary(lm.cov)




