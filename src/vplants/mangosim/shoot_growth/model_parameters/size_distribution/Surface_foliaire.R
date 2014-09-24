setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth")

# Etude de la taille des feuilles en fonction de leur position le long de l'UC



#################################################################
############   Récupération et mise en forme des données  
##########################


#################
####   MA05  ####
#################

MA05_0=read.csv("data/size_data/MA05.csv",header=TRUE,sep=";",dec=",")

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
MA_apicSfol=MA_apic$fbsfol   ;MA_latSfol=MA_lat$fbsfol

# Mise en forme de la taille des feuilles 
sfolMA=MA$fhsfol
sfolMAb=MA$fbsfol
par(mfrow=c(1,3))
boxplot(sfolMA)
plot(sfolMA)
title("Etude de l'aire des feuilles",outer=TRUE,line=-1)
summary(sfolMA,na.rm=T)





#####################
####   Sfol2006  ####
#####################

Sfol20060=read.csv("data/size_data/Sfol2006.csv",header=TRUE,sep=";",dec=",")

##### Etude préliminaire
par(mfrow=c(1,2))
boxplot(Sfol20060$Area)
plot(Sfol20060$Area)
title("Etude de l'aire des feuilles",outer=TRUE,line=-1)
summary(Sfol20060$Area,na.rm=T)
Sfol6=Sfol20060
Sfol6$Area=Sfol20060$Area/100

# Distinction apicale latérale
Sfol6_apic=Sfol6[Sfol6$position=="A",]
Sfol6_lat=Sfol6[Sfol6$position=="L",]


#####################
####   Sfol2004  ####
#####################

Sfol20040=read.csv("data/size_data/Sfol2004.csv",header=TRUE,sep=";",dec=",")

##### Etude préliminaire
par(mfrow=c(1,2))
boxplot(Sfol20040$Area)
plot(Sfol20040$Area)
title("Etude de l'aire des feuilles",outer=TRUE,line=-1)
summary(Sfol20040$Area,na.rm=T)
Sfol4=Sfol20040
Sfol4$Area=Sfol20040$Area/100

# Distinction apicale latérale
Sfol4_apic=Sfol4[Sfol4$position=="A",]
Sfol4_lat=Sfol4[Sfol4$position=="L",]


############################
####     CogshallBMA    ####
############################

BMA=read.csv("data/size_data/CogshallBMA.csv",header=TRUE,sep=";",dec=",")

##### Etude préliminaire
par(mfrow=c(1,2))
boxplot(BMA$Area)
plot(BMA$Area)
title("Etude de l'aire des feuilles",outer=TRUE,line=-1)
summary(BMA$Area,na.rm=T)

# Distinction apicale latérale
BMA_apic=BMA[BMA$position=="A",]
BMA_lat=BMA[BMA$position=="L",]



##########################
####   CogshallGFOND  ####
##########################

GFOND=read.csv("data/size_data/CogshallGFOND.csv",header=TRUE,sep=";",dec=",")

##### Etude préliminaire
par(mfrow=c(1,2))
boxplot(GFOND$Area)
plot(GFOND$Area)
title("Etude de l'aire des feuilles",outer=TRUE,line=-1)
summary(GFOND$Area,na.rm=T)

# Distinction apicale latérale
GFOND_apic=GFOND[GFOND$position=="A",]
GFOND_lat=GFOND[GFOND$position=="L",]




################################################################################
############   Etude de la Surface foliaire sans tenir compte de la position
########################


#####################
####   Sfol2006  ####
#####################
par(mfrow=c(3,2))
hist(Sfol6$Area,freq=FALSE,breaks=seq(0,120,5))      ;curve(dnorm(x,mean(Sfol6$Area),sd(Sfol6$Area)),add=TRUE,col=2)
(Sfol6$Area)
plot(ecdf(Sfol6$Area),xlim=c(0,120))
curve(pnorm(x,mean(Sfol6$Area),sd(Sfol6$Area)),add=TRUE,col=2)

hist(Sfol6_apic$Area,freq=FALSE,xlim=c(0,120),breaks=seq(0,150,5))     ;curve(dnorm(x,mean(Sfol6_apic$Area),sd(Sfol6_apic$Area)),add=TRUE,col=2)
plot(ecdf(Sfol6_apic$Area),xlim=c(0,120))
curve(pnorm(x,mean(Sfol6_apic$Area),sd(Sfol6_apic$Area)),add=TRUE,col=2)
hist(Sfol6_lat$Area,freq=FALSE,xlim=c(0,120),breaks=seq(0,150,5))      ;curve(dnorm(x,mean(Sfol6_lat$Area),sd(Sfol6_lat$Area)),add=TRUE,col=2)
plot(ecdf(Sfol6_lat$Area),xlim=c(0,120))
curve(pnorm(x,mean(Sfol6_lat$Area),sd(Sfol6_lat$Area)),add=TRUE,col=2)
title("Surface foliaire 2006 ",outer=TRUE,line=-1)

mean(Sfol6$Area)              # 44.36481

mean(Sfol6_apic$Area)         # 51.16059
sd(Sfol6_apic$Area)           # 20.16725
mean(Sfol6_lat$Area)          # 34.48969
sd(Sfol6_lat$Area)            # 16.80647

# Etude de l'effet de la position dur la surface (anova)
Sfol6.aov=aov(Sfol6$Area ~ Sfol6$position)
summary(Sfol6.aov)


##### Détection des dernières feuilles
plot(Sfol6$Area,type='l')



#####################
####   Sfol2004  ####
#####################
par(mfrow=c(3,2))
hist(Sfol4$Area,freq=FALSE,breaks=seq(0,150,5),xlim=c(0,80))              ;curve(dnorm(x,mean(Sfol4$Area),sd(Sfol4$Area)),add=TRUE,col=2)
plot(ecdf(Sfol4$Area))
curve(pnorm(x,mean(Sfol4$Area),sd(Sfol4$Area)),add=TRUE,col=2)

hist(Sfol4_apic$Area,freq=FALSE,xlim=c(0,80),ylim=c(0,0.045),breaks=seq(0,150,5))     ;curve(dnorm(x,mean(Sfol6_apic$Area),sd(Sfol6_apic$Area)),add=TRUE,col=2)
plot(ecdf(Sfol4_apic$Area),xlim=c(0,80))
curve(pnorm(x,mean(Sfol4_apic$Area),sd(Sfol4_apic$Area)),add=TRUE,col=2)
hist(Sfol4_lat$Area,freq=FALSE,xlim=c(0,80),ylim=c(0,0.045),breaks=seq(0,150,5))      ;curve(dnorm(x,mean(Sfol6_lat$Area),sd(Sfol6_lat$Area)),add=TRUE,col=2)
plot(ecdf(Sfol4_lat$Area),xlim=c(0,80))
curve(pnorm(x,mean(Sfol4_lat$Area),sd(Sfol4_lat$Area)),add=TRUE,col=2)

mean(Sfol4$Area)             #39.02645
title("Surface foliaire 2004 ",outer=TRUE,line=-1)


# Etude de l'effet de la position dur la surface (anova)
Sfol4.aov=aov(Sfol4$Area ~ Sfol4$position)
summary(Sfol4.aov)

mean(Sfol4_apic$Area)         # 44.15534
sd(Sfol4_apic$Area)           # 15.71035
mean(Sfol4_lat$Area)          # 30.9968
sd(Sfol4_lat$Area)            # 11.18507


###################
#####  MA05  ######
###################
par(mfrow=c(3,2))
summary(sfolMAb,na.rm=T)
hist(sfolMAb,freq=FALSE,xlim=c(0,100),breaks=seq(0,150,5))                                ;curve(dnorm(x,mean(sfolMAb),sd(sfolMAb)),add=TRUE,col=2,breaks=30)
plot(ecdf(sfolMA))
curve(pnorm(x,mean(sfolMA),sd(sfolMA)),add=TRUE,col=2)

hist(MA_apicSfol,freq=FALSE,xlim=c(0,100),ylim=c(0,0.045),breaks=seq(0,150,5))     ;curve(dnorm(x,mean(MA_apicSfol),sd(MA_apicSfol)),add=TRUE,col=2)
plot(ecdf(MA_apicSfol),xlim=c(0,100))
curve(pnorm(x,mean(MA_apicSfol),sd(MA_apicSfol)),add=TRUE,col=2)
hist(MA_latSfol,freq=FALSE,xlim=c(0,100),ylim=c(0,0.045),breaks=seq(0,150,5))      ;curve(dnorm(x,mean(MA_latSfol),sd(MA_latSfol)),add=TRUE,col=2)
plot(ecdf(MA_latSfol),xlim=c(0,100))
curve(pnorm(x,mean(MA_latSfol),sd(MA_latSfol)),add=TRUE,col=2)

mean(MA$fbsfol)                    #47.5976
title("Surface foliaire MA05 ",outer=TRUE,line=-1)

# Etude de l'effet de la position dur la surface (anova)
MA.aov=aov(MA$fhsfol ~ MA$position)
summary(MA.aov)



############################
####     CogshallBMA    ####
############################
par(mfrow=c(3,2))
hist(BMA$Area,freq=FALSE,breaks=seq(0,150,5))      ;curve(dnorm(x,mean(BMA$Area),sd(BMA$Area)),add=TRUE,col=2)
plot(ecdf(BMA$Area),xlim=c(0,150))
curve(pnorm(x,mean(BMA$Area),sd(BMA$Area)),add=TRUE,col=2)

hist(BMA_apic$Area,freq=FALSE,xlim=c(0,150),breaks=seq(0,150,5))     ;curve(dnorm(x,mean(BMA_apic$Area),sd(BMA_apic$Area)),add=TRUE,col=2)
plot(ecdf(BMA_apic$Area),xlim=c(0,150))
curve(pnorm(x,mean(BMA_apic$Area),sd(BMA_apic$Area)),add=TRUE,col=2)
hist(BMA_lat$Area,freq=FALSE,xlim=c(0,150),breaks=seq(0,150,5))      ;curve(dnorm(x,mean(BMA_lat$Area),sd(BMA_lat$Area)),add=TRUE,col=2)
plot(ecdf(BMA_lat$Area),xlim=c(0,150))
curve(pnorm(x,mean(BMA_lat$Area),sd(BMA_lat$Area)),add=TRUE,col=2)

mean(BMA$Area)              # 59.69813
mean(BMA_apic$Area)         # 71.87368
mean(BMA_lat$Area)          # 45.818
sd(BMA_apic$Area)           # 41.02476
sd(BMA_lat$Area)            # 23.67394
title("Surface foliaire BMA ",outer=TRUE,line=-1)

# Etude de l'effet de la position dur la surface (anova)
BMA.aov=aov(BMA$Area ~ BMA$position)
summary(BMA.aov)



############################
####   CogshallGFOND    ####
############################
par(mfrow=c(3,2))
hist(GFOND$Area,freq=FALSE,breaks=seq(0,120,5))      ;curve(dnorm(x,mean(GFOND$Area),sd(GFOND$Area)),add=TRUE,col=2)
(GFOND$Area)
plot(ecdf(GFOND$Area),xlim=c(0,120))
curve(pnorm(x,mean(GFOND$Area),sd(GFOND$Area)),add=TRUE,col=2)

hist(GFOND_apic$Area,freq=FALSE,xlim=c(0,120),breaks=seq(0,150,5))     ;curve(dnorm(x,mean(GFOND_apic$Area),sd(GFOND_apic$Area)),add=TRUE,col=2)
plot(ecdf(GFOND_apic$Area),xlim=c(0,120))
curve(pnorm(x,mean(GFOND_apic$Area),sd(GFOND_apic$Area)),add=TRUE,col=2)
hist(GFOND_lat$Area,freq=FALSE,xlim=c(0,120),breaks=seq(0,150,5))      ;curve(dnorm(x,mean(GFOND_lat$Area),sd(GFOND_lat$Area)),add=TRUE,col=2)
plot(ecdf(GFOND_lat$Area),xlim=c(0,120))
curve(pnorm(x,mean(GFOND_lat$Area),sd(GFOND_lat$Area)),add=TRUE,col=2)

mean(GFOND$Area)         # 53.26412
title("Surface foliaire GFOND ",outer=TRUE,line=-1)

# Etude de l'effet de la position dur la surface (anova)
GFOND.aov=aov(GFOND$Area ~ GFOND$position)
summary(GFOND.aov)


mean(GFOND_apic$Area)         # 58.32553
sd(GFOND_apic$Area)           # 19.34455
mean(GFOND_lat$Area)          # 40.40541
sd(GFOND_lat$Area)            # 19.32708



################################
#######   Etude globale   ######

par(mfrow=c(2,4))  #Sfol_glob
hist(Sfol6_apic$Area,freq=FALSE,xlim=c(0,150),ylim=c(0,0.035),breaks=seq(0,150,8),xlab="Surface foliaire",main="Surface foliaire 6, UCs apicales");
	#curve(dnorm(x,mean(Sfol6_apic$Area),sd(Sfol6_apic$Area)),add=TRUE,col=2)
hist(Sfol4_apic$Area,freq=FALSE,xlim=c(0,150),ylim=c(0,0.035),breaks=seq(0,150,8),xlab="Surface foliaire",main="Surface foliaire 4, UCs apicales");
	#curve(dnorm(x,mean(Sfol4_apic$Area),sd(Sfol4_apic$Area)),add=TRUE,col=2)
hist(GFOND_apic$Area,freq=FALSE,xlim=c(0,150),ylim=c(0,0.035),breaks=seq(0,150,8),xlab="Surface foliaire",main="CogshallGFOND, UCs apicales");
	#curve(dnorm(x,mean(GFOND_apic$Area),sd(GFOND_apic$Area)),add=TRUE,col=2)
hist(BMA_apic$Area,freq=FALSE,xlim=c(0,150),ylim=c(0,0.035),breaks=seq(0,150,10),xlab="Surface foliaire",main="CogshallBMA, UCs apicales");
	#curve(dnorm(x,mean(BMA$Area),sd(BMA$Area)),add=TRUE,col=2)

hist(Sfol6_lat$Area,freq=FALSE,xlim=c(0,150),ylim=c(0,0.035),breaks=seq(0,150,8),xlab="Surface foliaire",main="Surface foliaire 6, UCs latérales");
	#curve(dnorm(x,mean(Sfol6_lat$Area),sd(Sfol6_lat$Area)),add=TRUE,col=2)
hist(Sfol4_lat$Area,freq=FALSE,xlim=c(0,150),ylim=c(0,0.035),breaks=seq(0,150,8),xlab="Surface foliaire",main="Surface foliaire 4, UCs latérales");
	#curve(dnorm(x,mean(Sfol4_lat$Area),sd(Sfol4_lat$Area)),add=TRUE,col=2)
hist(GFOND_lat$Area,freq=FALSE,xlim=c(0,150),ylim=c(0,0.035),breaks=seq(0,150,8),xlab="Surface foliaire",main="CogshallGFOND, UCs latérales");
	#curve(dnorm(x,mean(GFOND_lat$Area),sd(GFOND_lat$Area)),add=TRUE,col=2)
hist(BMA$Area,freq=FALSE,xlim=c(0,150),ylim=c(0,0.035),breaks=seq(0,150,10),xlab="Surface foliaire",main="CogshallBMA, UCs latérales");
	#curve(dnorm(x,mean(BMA$Area),sd(BMA$Area)),add=TRUE,col=2)


##hist(sfolMAb,freq=FALSE,xlim=c(0,120),ylim=c(0,0.025));curve(dnorm(x,mean(sfolMAb),sd(sfolMAb)),add=TRUE,col=2)



#plot(ecdf(Sfol6$Area),xlim=c(0,120))     ;curve(pnorm(x,mean(Sfol6$Area),sd(Sfol6$Area)),add=TRUE,col=2)
#plot(ecdf(Sfol4$Area),xlim=c(0,120))     ;curve(pnorm(x,mean(Sfol4$Area),sd(Sfol4$Area)),add=TRUE,col=2)
#plot(ecdf(sfolMA),xlim=c(0,120))         ;curve(pnorm(x,mean(sfolMA),sd(sfolMA)),add=TRUE,col=2)
#plot(ecdf(BMA$Area),xlim=c(0,120))     ;curve(pnorm(x,mean(BMA$Area),sd(BMA$Area)),add=TRUE,col=2)
#plot(ecdf(GFOND$Area),xlim=c(0,120))     ;curve(pnorm(x,mean(GFOND$Area),sd(GFOND$Area)),add=TRUE,col=2)


#### Fusion des fichiers
SFol=c(GFOND$Area,Sfol4$Area,Sfol6$Area,BMA$Area)
SFol_pos=c(GFOND$position,Sfol4$position,Sfol6$position,BMA$position)
SFol_base=c(rep(1,length(GFOND$position)),rep(2,length(Sfol4$position)),rep(3,length(Sfol6$position)),rep(4,length(BMA$position)))
SFol_apic=SFol[SFol_pos==1]
SFol_lat=SFol[SFol_pos==2]

# anova pour étudier l'influence de la base de donnée et de la position des UCs sur la surface foliaire
summary(aov(SFol ~ SFol_base*SFol_pos ))

par(mfrow=c(2,2))
#hist(SFol,main="Surface foliaire",freq=FALSE,breaks=seq(0,150,5),xlim=c(0,150));curve(dnorm(x,mean(SFol),sd(SFol)),add=TRUE,col=2)
#qqnorm(SFol);qqline(SFol,col=2)
hist(SFol_apic,main="position apicale",freq=FALSE,breaks=seq(0,150,5),xlim=c(0,150));curve(dnorm(x,mean(SFol_apic),sd(SFol_apic)),add=TRUE,col=2)
#qqnorm(SFol_apic);qqline(SFol_apic,col=2)
hist(SFol_lat,main="position latérale",freq=FALSE,breaks=seq(0,150,5),xlim=c(0,150));curve(dnorm(x,mean(SFol_lat),sd(SFol_lat)),add=TRUE,col=2)
#qqnorm(SFol_lat);qqline(SFol_lat,col=2)

plot(ecdf(SFol_apic),main="position apicale",xlim=c(0,150))
	curve(pnorm(x,mean(SFol_apic),sd(SFol_apic)),add=TRUE,col=2)
plot(ecdf(SFol_lat),main="position latérale",xlim=c(0,150))
	curve(pnorm(x,mean(SFol_lat),sd(SFol_lat)),add=TRUE,col=2)


mean(SFol_apic)     # 52.13814
sd(SFol_apic)       # 23.43136
mean(SFol_lat)      # 35.55546
sd(SFol_lat)        # 17.31505

ks.test(SFol,mean(SFol_apic),sd(SFol_apic))                    # 0.7786
ks.test(SFol,mean(SFol_lat),sd(SFol_lat))                      # 0.8176

summary(aov(SFol ~ SFol_pos))



###########################################################################
############   Etude de la Surface foliaire en fonction de la position 
########################
par(mfrow=c(1,4))
plot(Sfol4$Area,col=Sfol4$UC);abline(h=17,col=2)
plot(Sfol6$Area,col=Sfol6$UC);abline(h=20,col=2)
plot(GFOND$Area,col=GFOND$UC-3);abline(h=25,col=2)
plot(BMA$Area);abline(h=20,col=2)


# Pour chaque feuille renvoit le numéro de l'UC qui la porte (nouvelle numérotation continue)
num_UC=function(base){
UC=c(1);ram=1
for (i in 2:(length(base$Area)-1)){
	if ((base$UC[i] != base$UC[i-1]) || (base$ER[i] != base$ER[i-1])){
		ram=ram+1
	}
	UC=c(UC,ram)
}
c(UC,ram)
}


# Pour chaque UC retire les 2 plus petites feuilles et calcul le rapport 
  # de leur surface sur la surface moyenne des autres feuilles
list_pct=function(base){
numUC=num_UC(base)
pct_feu_min=c();  pct_feu_min2=c(); allFeu=c()
for (i in 1:length(unique(numUC))){
	Feu=base$Area[numUC==i]
	GFeu=Feu[Feu!=min(Feu)]    
	GdsFeu=GFeu[GFeu!=min(GFeu)]	

	pct_feu_min=c(pct_feu_min,min(Feu)/mean(GFeu))
	pct_feu_min2=c(pct_feu_min2,min(GFeu)/mean(GdsFeu))
	allFeu=c(allFeu,GdsFeu)}

list("min"=pct_feu_min,"min2"=pct_feu_min2,"Feu"=allFeu)
}

pct_GFOND=list_pct(GFOND)    ; pct_GFOND_apic=list_pct(GFOND_apic)    ; pct_GFOND_lat=list_pct(GFOND_lat)
pct_BMA=list_pct(BMA)        ; pct_BMA_apic=list_pct(BMA_apic)        ; pct_BMA_lat=list_pct(BMA_lat)
pct_Sfol4=list_pct(Sfol4)    ; pct_Sfol4_apic=list_pct(Sfol4_apic)    ; pct_Sfol4_lat=list_pct(Sfol4_lat)
pct_Sfol6=list_pct(Sfol6)    ; pct_Sfol6_apic=list_pct(Sfol6_apic)    ; pct_Sfol6_lat=list_pct(Sfol6_lat)
 
mean(pct_BMA$min2,na.rm=T)           ;mean(pct_BMA$min,na.rm=T)
mean(pct_GFOND$min2,na.rm=T)         ;mean(pct_GFOND$min,na.rm=T)
mean(pct_Sfol4$min2,na.rm=T)         ;mean(pct_Sfol4$min,na.rm=T)
mean(pct_Sfol6$min2,na.rm=T)         ;mean(pct_Sfol6$min,na.rm=T)

# Pourcentages de perte de surface moyen par base de donnée
mean(c(pct_BMA$min2,pct_GFOND$min2,pct_Sfol4$min2,pct_Sfol6$min2),na.rm=T)  ;                      # 0.5206125
	mean(c(pct_BMA$min,pct_GFOND$min,pct_Sfol4$min,pct_Sfol6$min),na.rm=T)                           # 0.3790924
mean(c(pct_BMA_apic$min2,pct_GFOND_apic$min2,pct_Sfol4_apic$min2,pct_Sfol6_apic$min2),na.rm=T)  ;  # 0.4994172 
	mean(c(pct_BMA_apic$min,pct_GFOND_apic$min,pct_Sfol4_apic$min,pct_Sfol6_apic$min),na.rm=T)       # 0.3446696
mean(c(pct_BMA_lat$min2,pct_GFOND_lat$min2,pct_Sfol4_lat$min2,pct_Sfol6_lat$min2),na.rm=T)  ;      # 0.53746
	mean(c(pct_BMA_lat$min,pct_GFOND_lat$min,pct_Sfol4_lat$min,pct_Sfol6_lat$min),na.rm=T)           # 0.4064541


# Etude des distributions de surface foliaire sur les plus grandes feuilles uniquement
Feu=c(pct_BMA$Feu,pct_GFOND$Feu,pct_Sfol4$Feu,pct_Sfol6$Feu)
Feu_apic=c(pct_BMA_apic$Feu,pct_GFOND_apic$Feu,pct_Sfol4_apic$Feu,pct_Sfol6_apic$Feu)
Feu_lat=c(pct_BMA_lat$Feu,pct_GFOND_lat$Feu,pct_Sfol4_lat$Feu,pct_Sfol6_lat$Feu)

par(mfrow=c(2,2))  # dist_Sfol
#hist(Feu,breaks=20,freq=F,xlim=c(0,120),ylim=c(0,0.04))
#	curve(dnorm(x,mean(Feu),sd(Feu)),add=TRUE,col=2)
hist(Feu_apic,breaks=20,freq=F,xlim=c(0,120),ylim=c(0,0.04),xlab="Surface foliaire",main="UCs en position apicale")
	curve(dnorm(x,mean(Feu_apic),sd(Feu_apic)),add=TRUE,col=2)
legend("topright",lty=1,col=2,"N(56.13,21.83)",bty="n")
hist(Feu_lat,breaks=20,freq=F,xlim=c(0,120),ylim=c(0,0.04),xlab="Surface foliaire",main="UCs en position latérale")
	curve(dnorm(x,mean(Feu_lat),sd(Feu_lat)),add=TRUE,col=2)
legend("topright",lty=1,col=2,"N(41.05,15.78)",bty="n")
plot(ecdf(Feu_apic),xlim=c(0,120),xlab="Surface foliaire",main="UCs en position apicale")
	curve(pnorm(x,mean(Feu_apic),sd(Feu_apic)),add=TRUE,col=2)
plot(ecdf(Feu_lat),xlim=c(0,120),xlab="Surface foliaire",main="UCs en position latérale")
	curve(pnorm(x,mean(Feu_lat),sd(Feu_lat)),add=TRUE,col=2)


#qqnorm(Feu);qqline(Feu,col=2)  ; 
# qqnorm(Feu_apic,main="UCs en position apicale");qqline(Feu_apic,col=2)  ;  qqnorm(Feu_lat,main="UCs en position latérale");qqline(Feu_lat,col=2)


mean(Feu_apic)     # 56.13
sd(Feu_apic)       # 21.83
mean(Feu_lat)      # 41.05
sd(Feu_lat)        # 15.78

# Tests de normalité
ks.test(Feu_apic,mean(Feu_apic),sd(Feu_apic))                 # 0.907
ks.test(Feu_lat,mean(Feu_lat),sd(Feu_lat))                    # 0.8865

# analyse de variance pour tester l'effet de la position sur les longueurs de feuille
leaf.aov=aov(c(Feu_apic,Feu_lat)~c(rep(1,length(Feu_apic)),rep(2,length(Feu_lat))) )
summary(leaf.aov)
res.aov=leaf.aov$res
aov.apic=leaf.aov$res[1:length(Feu_apic)]
aov.lat=leaf.aov$res[(length(Feu_apic)+1):(length(Feu_apic)+length(Feu_lat))]

# Test d'homoscédasticité
g = factor(rep(1:2, c(length(Feu_apic),length(Feu_lat))), labels = c("apic","lat"))
leveneTest(c(Feu_apic,Feu_lat),g)                                        #n=722  F=14.754   p-value=0.0001333

par(mfrow=c(2,3))
hist(aov.apic,freq=F,main="Histogramme",xlab="cas apical");curve(dnorm(x,0,sd(res.aov)),col=2,add=T)
plot(ecdf(aov.apic),main="Fonction de répartition empirique");curve(pnorm(x,0,sd(res.aov)),col=2,add=T)
qqnorm(aov.apic,main="Droite de Henry");qqline(aov.apic,col=2)
hist(aov.lat,freq=F,main="Histogramme",xlab="cas latéral");curve(dnorm(x,0,sd(res.aov)),col=2,add=T)
plot(ecdf(aov.lat),main="Fonction de répartition empirique");curve(pnorm(x,0,sd(res.aov)),col=2,add=T)
qqnorm(aov.lat,main="Droite de Henry");qqline(aov.lat,col=2)

ks.test(aov.apic,0,sd(res.aov));ks.test(aov.lat,0,sd(res.aov))
shapiro.test(aov.apic);shapiro.test(aov.lat)

# Test de kruskall-wallis
kruskal.test(c(Feu_apic,Feu_lat)~rep(1:2, c(length(Feu_apic),length(Feu_lat)))  )


#################################################
############   Conversion des aires en longeur  
########################

# utilisation des résultats obtenus dans "allometries.R"
Longueur=2.36*sqrt(Feu)
Long_apic=2.36*sqrt(Feu_apic)
Long_lat=2.36*sqrt(Feu_lat)

#hist(Longueur,freq=FALSE);curve(dnorm(x,mean(Longueur),sd(Longueur)),add=TRUE,col=2)
#plot(ecdf(Longueur))
#curve(pnorm(x,mean(Longueur),sd(Longueur)),add=TRUE,col=2)

mean(Longueur)    # 16.49
sd(Longueur)      # 3.3

ks.test(Longueur,16.49,3.3)                       # 0.9274

par(mfrow=c(2,2))   # LongFeu
hist(Long_apic,freq=FALSE,main="Histogramme",xlim=c(8,28),breaks=seq(1,30,1),xlab="Longueur des Feuilles en position apicale")     ;curve(dnorm(x,mean(Long_apic),sd(Long_apic)),add=TRUE,col=2)
legend("topright",lty=1,col=2,"N(17.37,3.26)",bty = "n")
#plot(ecdf(Long_apic),xlim=c(8,28))
#curve(pnorm(x,mean(Long_apic),sd(Long_apic)),add=TRUE,col=2)
#qqnorm(Long_apic);qqline(Long_apic,col=2)

hist(Long_lat,freq=FALSE,main="Histogramme",xlim=c(8,28),breaks=seq(1,30,1),xlab="Longueur des Feuilles en position latérale")     ;curve(dnorm(x,mean(Long_lat),sd(Long_lat)),add=TRUE,col=2)
legend("topright",lty=1,col=2,"N(14.87,2.72)",bty = "n")
#plot(ecdf(Long_lat),xlim=c(8,28))
#curve(pnorm(x,mean(Long_lat),sd(Long_lat)),add=TRUE,col=2)
#qqnorm(Long_lat);qqline(Long_lat,col=2)

plot(ecdf(Long_apic),xlim=c(8,28),main="Fonction de répartition empirique")     ;
	curve(pnorm(x,mean(Long_apic),sd(Long_apic)),add=TRUE,col=2)
plot(ecdf(Long_apic),xlim=c(8,28),main="Fonction de répartition empirique")     ;
	curve(pnorm(x,mean(Long_apic),sd(Long_apic)),add=TRUE,col=2)



mean(Long_apic)    # 17.06102
sd(Long_apic)      # 2.689568
mean(Long_lat)     # 14.87
sd(Long_lat)       # 2.72

# Tests de normalité
ks.test(Long_apic,17.1,2.7)   # 0.9464
ks.test(Long_lat,14.87,2.72)  # 0.9327
shapiro.test(Long_apic)
shapiro.test(Long_lat)

