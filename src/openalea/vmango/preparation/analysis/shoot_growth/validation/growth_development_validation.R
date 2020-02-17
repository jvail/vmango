setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth")

# Suivi de croissance des UCs végétatives, des feuilles et des inflorescences
# Validation de la croissance et du développement
# On valide en comparant les longueurs et les stades phénologiques simulés avec ceux des données observées
# Section 5 du Rapport "Résultats et validation"

################# Définition de fonctions
id=function(x){return(x)}

# Fonction de mise en forme des données issues de la simulation
MEF=function(fichier){
data=read.csv(fichier,header=TRUE,sep=",",dec=".")
# Mise en forme des données issues de la simulation
data$Date=strptime(as.character(data$Date), "%Y-%m-%d %H:%M")
data$CodeUC=as.character(data$CodeUC)
return( data)
}

# Fonction permettant l'affichage de la courbe de croisance d'une UC avec une couleur différente pour chaque stade phéno
affich_pheno=function(data,sim_data,codeUC){
	UC=data[data$codeUC==codeUC,]
	UCsim=sim_data[sim_data$CodeUC==codeUC,]
	plot(UC$date, cex.lab = 1.7, cex.axis = 1.5,UC$longueurUC,main=UCs[i],xlab="Date",ylab="Longueur axe UC",ylim=c(0,20))
	points(UC$date[UC$stadeUC<8],UC$longueurUC[UC$stadeUC<8],col=2,lwd=3)
	points(UC$date[UC$stadeUC>7 & UC$stadeUC<10],UC$longueurUC[UC$stadeUC>7 & UC$stadeUC<10],col=3,lwd=3)
	points(UC$date[UC$stadeUC>9 & UC$stadeUC<12],UC$longueurUC[UC$stadeUC>9 & UC$stadeUC<12],col=4,lwd=3)
	points(UC$date[UC$stadeUC>11 & UC$stadeUC<15],UC$longueurUC[UC$stadeUC>11 & UC$stadeUC<15],col=5,lwd=3)
	points(UC$date[UC$stadeUC==15 ],UC$longueurUC[UC$stadeUC==15 ],col=6,lwd=3)
		
	pheno=unlist(aggregate(UCsim$Pheno_stage,by=list(as.character(UCsim$Date)),FUN=id)[2])
	points(unique(UCsim$Date),unlist(aggregate(UCsim$Length,by=list(as.character(UCsim$Date)),FUN=sum)[2]),type='l',col=2,lwd=3)
	points(unique(UCsim$Date)[pheno>0],unlist(aggregate(UCsim$Length,by=list(as.character(UCsim$Date)),FUN=sum)[2])[pheno>0],type='l',col=3,lwd=3)
	points(unique(UCsim$Date)[pheno>1],unlist(aggregate(UCsim$Length,by=list(as.character(UCsim$Date)),FUN=sum)[2])[pheno>1],type='l',col=4,lwd=3)
	points(unique(UCsim$Date)[pheno>2],unlist(aggregate(UCsim$Length,by=list(as.character(UCsim$Date)),FUN=sum)[2])[pheno>2],type='l',col=5,lwd=3)
	points(unique(UCsim$Date)[pheno>3],unlist(aggregate(UCsim$Length,by=list(as.character(UCsim$Date)),FUN=sum)[2])[pheno>3],type='l',col=6,lwd=3)
}


# Fonction permettant de comparer les stades phénologiques réels et simulés pour les UCs
valid_pheno=function(data,sim_data,codeUC){
	UC=data[data$codeUC==codeUC,]
	UCsim=sim_data[sim_data$CodeUC==codeUC,]
	x=c();y=c()
	for (i in 1:length(UCsim$Date)){
		stade=UC$stadeUC[UC$date$yday==UCsim$Date[i]$yday & UC$date$year==UCsim$Date[i]$year]
		if (length(stade)!=0){
			x=c(x,stade[1])
			y=c(y,UCsim$Pheno_stage[i])}
	}
x[x<9]=0 ; x[x>8 & x<11]=1; x[x>10 & x<13]=2; x[x>12 & x<15]=3 ; x[x==15]=4
ax=c(0,0,1,1,1,2,2,2,3,3,3,4,4)
by=c(0,1,0,1,2,1,2,3,2,3,4,3,4)
s=c(sum(x==0&y==0),sum(x==0&y==1),sum(x==1&y==0),sum(x==1&y==1),sum(x==1&y==2),sum(x==2&y==1),sum(x==2&y==2),sum(x==2&y==3),sum(x==3&y==2),sum(x==3&y==3),sum(x==3&y==4),sum(x==4&y==3),sum(x==4&y==4))
col=rep(1,length(s))
col[s==0]=0
symbols(ax,by,circles=s/length(x), xaxt = "n",yaxt="n",inches=FALSE,fg=col,xlim=c(-0.5,4.5),bg=col,ylim=c(-0.5,4.5),lwd=2.5,xlab="stade observé",ylab="stade simulé", cex.lab = 1.7, cex.axis = 1.5)
axis(1, at =0:4, labels = c("D","E","F","G","H"), tick = TRUE, cex.axis = 1.5)
axis(2, at = 0:4, labels = c("D","E","F","G","H"), tick = TRUE, cex.axis = 1.5)
abline(0,1)
list(obs=x,sim=y)
}

# Fonction permettant de comparer les stades phénologiques réels et simulés pour les inflorescences
valid_pheno_inflo=function(data,sim_data,codeUC){
	UC=data[data$codeUC==codeUC,]
	UCsim=sim_data[sim_data$CodeUC==codeUC,]
	x=c();y=c()
	for (i in 1:length(UCsim$Date)){
		stade=UC$stadeInflo[UC$date$yday==UCsim$Date[i]$yday & UC$date$year==UCsim$Date[i]$year]
		if (length(stade)!=0){
			x=c(x,stade[1])
			y=c(y,UCsim$Pheno_stage[i])}
	}
x[x<12]=0; x[x>11 & x<14]=1 ; x[x==14 | x==16]=2 ; x[x==15]=3 ; 
ax=c(0,0,1,1,1,2,2,2,3,3)
by=c(0,1,0,1,2,1,2,3,2,3)
s=c(sum(x==0&y==0),sum(x==0&y==1),sum(x==1&y==0),sum(x==1&y==1),sum(x==1&y==2),sum(x==2&y==1),sum(x==2&y==2),sum(x==2&y==3),sum(x==3&y==2),sum(x==3&y==3))
col=rep(1,length(s))
col[s==0]=0
symbols(ax,by,circles=s/length(x), xaxt = "n",yaxt="n",inches=FALSE,fg=col,xlim=c(-0.5,4.5),bg=col,ylim=c(-0.5,3.5),lwd=2.5,xlab="stade observé",ylab="stade simulé", cex.lab = 1.7, cex.axis = 1.5)
axis(1, at =0:3, labels = c("D","E","F","G"), tick = TRUE, cex.axis = 1.5)
axis(2, at = 0:3, labels = c("D","E","F","G"), tick = TRUE, cex.axis = 1.5)
abline(0,1)
list(obs=x,sim=y)
}

# Fonction permettant de comparer les stades phénologiques obtnus avec les 2 découpages
comp_pheno=function(data,sim_data,sim_data2,codeUC){
	UC=data[data$codeUC==codeUC,]
	UCsim=sim_data[sim_data$CodeUC==codeUC,]
	UCsim2=sim_data2[sim_data2$CodeUC==codeUC,]
	x=c();y=c()
	for (i in 1:length(UCsim$Date)){
		stade=UC$stadeUC[UC$date$yday==UCsim$Date[i]$yday]
		if (length(stade)!=0){
			x=c(x,stade[1])
			y2=c(y,UCsim2$Pheno_stage[i])
			y=c(y,UCsim$Pheno_stage[i])}
	}
x[x<8]=0 ; x[x>7 & x<12]=1 ; x[x>11 & x<15]=2 ; x[x>14]=3
plot(c(1:length(x)),x-y, cex.lab = 1.7, cex.axis = 1.5)
points(c(1:length(x)),x-y2,col=2,pch="+")
}

#Fonction traçant les données simulées en fonction des données estimées, pour les UCs
valid_croiss=function(data,sim_data,codeUC){
	UC=data[data$codeUC==codeUC,]
	UCsim=sim_data[sim_data$CodeUC==codeUC,]
	x=c();y=c()
	long_sim=UCsim$Length
	date=unique(UCsim$Date)
	for (i in 1:length(date)){
		long_obs=UC$longueurUC[UC$date$yday==date[i]$yday & UC$date$year==date[i]$year]
		if (length(long_obs)!=0){
			x=c(x,long_obs[1])
			y=c(y,long_sim[i])}
	}
plot(x,y,ylim=c(0,24),xlim=c(0,24), cex.lab = 1.7, cex.axis = 1.5,lwd=3,main=codeUC,xlab="longueur observée",ylab="longueur estimée")
abline(0,1,lwd=2)
list(obs=x,sim=y)
}

#Fonction traçant les données simulées en fonction des données estimées, pour les inflorescences
valid_croiss_inflo=function(data,sim_data,codeUC){
	UC=data[data$codeUC==codeUC,]
	UCsim=sim_data[sim_data$CodeUC==codeUC,]
	x=c();y=c()
	long_sim=UCsim$Length
	date=unique(UCsim$Date)
	for (i in 1:length(date)){
		long_obs=UC$longueurInflo[UC$date$yday==date[i]$yday & UC$date$year==date[i]$year]
		if (length(long_obs)!=0){
			x=c(x,long_obs[1])
			y=c(y,long_sim[i])}
	}
plot(x,y,ylim=c(0,24),xlim=c(0,24), cex.lab = 1.7, cex.axis = 1.5,lwd=3,main=codeUC,xlab="longueur observée",ylab="longueur estimée")
abline(0,1,lwd=2)
list(obs=x,sim=y)
}

#Fonction traçant les données simulées en fonction des données estimées, pour les feuilles
valid_croiss_feu=function(data,sim_data,codeUC,feuille){
	UC=data[data$codeUC==codeUC,]
	UCsim=sim_data[sim_data$CodeUC==paste(codeUC,feuille,sep=""),]
	x=c();y=c()
	if (length(UCsim[,1])>0){
		long_sim=UCsim$Length
		date=unique(UCsim$Date)
		for (i in 1:length(date)){
			if (feuille=="-Fprox"){long_obs=UC$longFprox[UC$date$yday==date[i]$yday & UC$date$year==date[i]$year]}
			else if (feuille=="-Fdist"){long_obs=UC$longFdist[UC$date$yday==date[i]$yday & UC$date$year==date[i]$year]}
			else {long_obs=UC$longF1[UC$date$yday==date[i]$yday & UC$date$year==date[i]$year]}
			if (length(long_obs)!=0){
				x=c(x,(long_obs[1]^2)*0.18)
				y=c(y,long_sim[i])}
		}
	plot(x,y,ylim=c(0,24),xlim=c(0,24),main=codeUC,xlab="Surface observée",ylab="Surface estimée")
	abline(0,1)
	}
list(obs=x,sim=y)
}

croissFeu=function(verger,feuille,data,sim_data){
par(mfrow=c(4,4))
UCs=unique(as.character(data[data$verger==verger,]$codeUC))
for (i in 1:length(UCs)){
	UC=data[data$codeUC==UCs[i],]
	UCsim=sim_data[sim_data$CodeUC==paste(UCs[i],feuille,sep=""),]
	if (feuille=="-Fprox"){plot(UC$date,(UC$longFprox^2)*0.18,main=UCs[i],xlab="Date",ylab="Surface Feuille")}
	else if (feuille=="-Fdist"){plot(UC$date,(UC$longFdist^2)*0.18,main=UCs[i],xlab="Date",ylab="Surface Feuille")}
	else {plot(UC$date,(UC$longF1^2)*0.18,main=UCs[i],xlab="Date",ylab="Surface Feuille")}
	points(UCsim$Date,UCsim$Length,type='l',col=2)
}
}

comp_croiss_feu=function(verger,feuille,data,sim_data){
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(data[data$verger==verger,]$codeUC))
for (i in 1:length(UCs)){res=valid_croiss_feu(data,sim_data,UCs[i],feuille)
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
plot(x,y,ylim=c(0,24),xlim=c(0,24),main=verger,xlab="Surface observée",ylab="Surface estimée")
abline(lsfit(x,y),col=2)
abline(0,1)
sim=c(sim,y);obs=c(obs,x)
list(sim=sim,obs=obs)
}


####################################################################
#########     Récupération et mise en forme des données     ########
####################################################################

############### Données de longueurs lors de la croissance des UCs et des inflos

data0=read.csv("data/growth_data/BaseDeCroissanceVeg.csv",header=TRUE,sep=";",dec=",")
inflo0=read.csv("data/growth_data/BaseDeCroissanceInflo.csv",header=TRUE,sep=";",dec=",")

# On ne garde que Cogshall
data1=data0[data0$variete=="cog",]
inflo1=inflo0[inflo0$variete=="cog",]

# Mise en forme des données
data1$longueurUC[is.na(data1$longueurUC)]=0
data1$date=strptime(as.character(data1$date), "%d/%m/%Y %H:%M")
data1$stadeUC=as.numeric(data1$stadeUC)

inflo1$longueurInflo[is.na(inflo1$longueurInflo)]=0
inflo1$date=strptime(as.character(inflo1$date), "%d/%m/%y %H:%M")
inflo1$codeUC=as.character(inflo1$codeUC)


# Récupération des températures pour tous les vergers
verger="BM";TBM0=cbind(read.csv("data/temperature_data/TBMmoy.csv",header=TRUE,sep=";",dec="."),verger)
verger="BP";TBP0=cbind(read.csv("data/temperature_data/TBPmoy.csv",header=TRUE,sep=";",dec="."),verger)
verger="GF";TGF0=cbind(read.csv("data/temperature_data/TGFmoy.csv",header=TRUE,sep=";",dec="."),verger)
verger="GH";TGH0=cbind(read.csv("data/temperature_data/TGHmoy.csv",header=TRUE,sep=";",dec="."),verger)
temp=rbind(TBM0,TBP0,TGF0,TGH0)
temp$datesUnik=strptime(as.character(temp$datesUnik), "%d/%m/%Y")


# Ajout des températures aux données
temperature=rep(-999,length(data1$date))
data2=cbind(data1,temperature)
for (i in 1:length(data1$date)){
	day_temp=NA
	day_temp=as.numeric(temp[temp$verger==data2[i,]$verger & temp$datesUnik$yday ==data2[i,]$date$yday 
										 & temp$datesUnik$year ==data2[i,]$date$year,]$tempMoy)
	data2$temperature[i]=day_temp
}

temperature=rep(-999,length(inflo1$date))
inflo2=cbind(inflo1,temperature)
for (i in 1:length(inflo1$date)){
	day_temp=NA
	day_temp=as.numeric(temp[temp$verger==inflo2[i,]$verger & temp$datesUnik$yday ==inflo2[i,]$date$yday 
										 & temp$datesUnik$year ==inflo2[i,]$date$year,]$tempMoy)
	inflo2$temperature[i]=day_temp
}

######## Données simulées 

# Données issues des simulations sur la longueur des UCs (découpage phéno D EF G)
simGF=MEF("data/simulation_results/croissUC_GF.csv")
simBP=MEF("data/simulation_results/croissUC_BP.csv")
simBM=MEF("data/simulation_results/croissUC_BM.csv")

# Données issues des simulations sur la longueur des UCs (découpage phéno DEF G)
#simGF2=MEF("data/simulation_results/croissUC_GF.csv")
#simBP2=MEF("data/simulation_results/croissUC_BP.csv")
#simBM2=MEF("data/simulation_results/croissUC_BM.csv")

# Données issues des simulations sur la longueur des Feuilles (en feuille par feuille)
simFeuInd_GF=MEF("data/simulation_results/croissFeuInd_GF.csv")
simFeuInd_BP=MEF("data/simulation_results/croissFeuInd_BP.csv")
simFeuInd_BM=MEF("data/simulation_results/croissFeuInd_BM.csv")

# Données issues des simulations sur la longueur des inflorescences
simGF_inflo=MEF("data/simulation_results/croissInflo_GF.csv")
simBP_inflo=MEF("data/simulation_results/croissInflo_BP.csv")
simBM_inflo=MEF("data/simulation_results/croissInflo_BM.csv")



####################################################################
#########   Traitement des données sur la longueur des UCs  ########
####################################################################
pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/UCs.pdf")

############## On veut voir les courbes de croissance réelles et simulées pour chaque UC 
# Grand-Fond
par(mfrow=c(4,4))
UCs=unique(as.character(data2[data2$verger=="GF",]$codeUC))
for (i in 1:length(UCs)){
	UC=data2[data2$codeUC==UCs[i],]
	UCsim=simGF[simGF$CodeUC==UCs[i],]
	plot(UC$date,UC$longueurUC,main=UCs[i],xlab="Date",ylab="Longueur axe UC")
	points(unique(UCsim$Date),unlist(aggregate(UCsim$Length,by=list(as.character(UCsim$Date)),FUN=sum)[2]),type='l',col=2)
}

# Bassin Plat
par(mfrow=c(4,4))
UCs=unique(as.character(data2[data2$verger=="BP",]$codeUC))
for (i in 1:length(UCs)){
	UC=data2[data2$codeUC==UCs[i],]
	UCsim=simBP[simBP$CodeUC==UCs[i],]
	plot(UC$date,UC$longueurUC,main=UCs[i],xlab="Date",ylab="Longueur axe UC")
	points(unique(UCsim$Date),unlist(aggregate(UCsim$Length,by=list(as.character(UCsim$Date)),FUN=sum)[2]),type='l',col=2)
}

# Bassin Martin
par(mfrow=c(4,4))
UCs=unique(as.character(data2[data2$verger=="BM",]$codeUC))
for (i in 1:length(UCs)){
	UC=data2[data2$codeUC==UCs[i],]
	UCsim=simBM[simBM$CodeUC==UCs[i],]
	plot(UC$date,UC$longueurUC,main=UCs[i],xlab="Date",ylab="Longueur axe UC")
	points(unique(UCsim$Date),unlist(aggregate(UCsim$Length,by=list(as.character(UCsim$Date)),FUN=sum)[2]),type='l',col=2)
}
dev.off()


# Validation
pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/comp_croiss_UCs.pdf")

sim=c();obs=c()

# Grand-Fond
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(data2[data2$verger=="GF",]$codeUC))
for (i in 1:length(UCs)){res=valid_croiss(data2,simGF,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
plot(x,y,ylim=c(0,24),xlim=c(0,24),main="Grand-Fond",xlab="stade observé",ylab="stade estimé")
abline(lsfit(x,y),col=2)
abline(0,1)
sim=c(sim,y);obs=c(obs,x)

# Bassin Plat
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(data2[data2$verger=="BP",]$codeUC))
for (i in 1:length(UCs)){res=valid_croiss(data2,simBP,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
plot(x,y,ylim=c(0,24),xlim=c(0,24),main="Bassin plat",xlab="stade observé",ylab="stade estimé")
abline(lsfit(x,y),col=2)
abline(0,1)
sim=c(sim,y);obs=c(obs,x)

# Bassin Martin
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(data2[data2$verger=="BM",]$codeUC))
for (i in 1:length(UCs)){res=valid_croiss(data2,simBM,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
plot(x,y,ylim=c(0,24),xlim=c(0,24),main="Bassin Martin",xlab="stade observé",ylab="stade estimé")
abline(lsfit(x,y),col=2)
abline(0,1)
sim=c(sim,y);obs=c(obs,x)


par(mfrow=c(1,1))
plot(obs,sim,ylim=c(0,24),xlim=c(0,24),cex.axis = 1,cex.lab = 1.7,xlab="Longueur observée",ylab="Longueur estimée")
abline(lsfit(obs,sim),col=2,lwd=2)
abline(0,1)
legend("bottomright",c("bissectrice y=x","droite de régression"),cex=1.5,lty=1,col=c(1,2))
lm.stade=lm(sim~obs)
#abline(lm.stade$coeff,col=4)
summary(lm.stade)

lm2.stade=lm(x~y-1)
summary(lm2.stade)

dev.off()






############## On veut comparer les stades phénos simulés et réels
pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/pheno_UCs.pdf")

# Grand-Fond
par(mfrow=c(4,4))
UCs=unique(as.character(data2[data2$verger=="GF",]$codeUC))
for (i in 1:length(UCs)){affich_pheno(data2,simGF,UCs[i])}

# Bassin Plat
par(mfrow=c(4,4))
UCs=unique(as.character(data2[data2$verger=="BP",]$codeUC))
for (i in 1:length(UCs)){affich_pheno(data2,simBP,UCs[i])}

# Bassin Martin
par(mfrow=c(4,4))
UCs=unique(as.character(data2[data2$verger=="BM",]$codeUC))
for (i in 1:length(UCs)){affich_pheno(data2,simBM,UCs[i])}

dev.off()



pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/comp_pheno_UCs.pdf")
ax=c(0,0,1,1,1,2,2,2,3,3,3,4,4)
by=c(0,1,0,1,2,1,2,3,2,3,4,3,4)
sim=c();obs=c()

# Grand-Fond
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(data2[data2$verger=="GF",]$codeUC))
for (i in 1:length(UCs)){res=valid_pheno(data2,simGF,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}

par(mfrow=c(1,1))
s=c(sum(x==0&y==0),sum(x==0&y==1),sum(x==1&y==0),sum(x==1&y==1),sum(x==1&y==2),sum(x==2&y==1),sum(x==2&y==2),sum(x==2&y==3),sum(x==3&y==2),sum(x==3&y==3),sum(x==3&y==4),sum(x==4&y==3),sum(x==4&y==4))
col=rep(1,length(s))
col[s==0]=0
symbols(ax,by,circles=s/length(x),inches=FALSE,fg=col,xlim=c(-0.5,4.5),bg=col,ylim=c(-0.5,4.5),lwd=2.5,main="Grand-Fond",xlab="stade observé",ylab="stade estimé") 
abline(0,1);abline(lsfit(x,y),col=2)

sim=c(sim,y);obs=c(obs,x)

# Bassin Plat
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(data2[data2$verger=="BP",]$codeUC))
for (i in 1:length(UCs)){res=valid_pheno(data2,simBP,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
s=c(sum(x==0&y==0),sum(x==0&y==1),sum(x==1&y==0),sum(x==1&y==1),sum(x==1&y==2),sum(x==2&y==1),sum(x==2&y==2),sum(x==2&y==3),sum(x==3&y==2),sum(x==3&y==3),sum(x==3&y==4),sum(x==4&y==3),sum(x==4&y==4))
col=rep(1,length(s))
col[s==0]=0
symbols(ax,by,circles=s/length(x),inches=FALSE,fg=col,xlim=c(-0.5,4.5),bg=col,ylim=c(-0.5,4.5),lwd=2.5,main="Bassin Plat",xlab="stade observé",ylab="stade estimé") 
abline(0,1);abline(lsfit(x,y),col=2)

sim=c(sim,y);obs=c(obs,x)

# Bassin Martin
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(data2[data2$verger=="BM",]$codeUC))
for (i in 1:length(UCs)){res=valid_pheno(data2,simBM,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
s=c(sum(x==0&y==0),sum(x==0&y==1),sum(x==1&y==0),sum(x==1&y==1),sum(x==1&y==2),sum(x==2&y==1),sum(x==2&y==2),sum(x==2&y==3),sum(x==3&y==2),sum(x==3&y==3),sum(x==3&y==4),sum(x==4&y==3),sum(x==4&y==4))
col=rep(1,length(s))
col[s==0]=0
symbols(ax,by,circles=s/length(x),inches=FALSE,fg=col,xlim=c(-0.5,4.5),bg=col,ylim=c(-0.5,4.5),lwd=2.5,main="Bassin Martin",xlab="stade observé",ylab="stade estimé") 
abline(0,1);abline(lsfit(x,y),col=2)
sim=c(sim,y);obs=c(obs,x)


par(mfrow=c(1,1))
x=obs;y=sim
s=c(sum(x==0&y==0),sum(x==0&y==1),sum(x==1&y==0),sum(x==1&y==1),sum(x==1&y==2),sum(x==2&y==1),sum(x==2&y==2),sum(x==2&y==3),sum(x==3&y==2),sum(x==3&y==3),sum(x==3&y==4),sum(x==4&y==3),sum(x==4&y==4))
col=rep(1,length(s))
col[s==0]=0
symbols(ax,by,circles=s*0.5/length(x),cex.lab=1.7,inches=FALSE,fg=col,xaxt="n",yaxt="n",xlim=c(-0.5,4.5),bg=col,ylim=c(-0.5,4.5),lwd=2.5,main="",xlab="stade observé",ylab="stade estimé")
axis(1, at =0:4, labels = c("D","E","F","G","H"), tick = TRUE, cex.axis = 1)
axis(2, at = 0:4, labels = c("D","E","F","G","H"), tick = TRUE, cex.axis = 1)
abline(lsfit(obs,sim),col=2,lwd=2)
abline(0,1)
legend("bottomright",c("bissectrice y=x","droite de régression"),cex=1.5,lty=1,col=c(1,2))

lm.stade=lm(sim~obs)
#abline(lm.stade$coeff,col=4)
summary(lm.stade)

lm2.stade=lm(obs~sim-1)
summary(lm2.stade)

dev.off()

# Comparaison des stades phénos pour les deux découpages D-EF-G et DEF-G
pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/comp_decoup_pheno.pdf")

# Grand-Fond
par(mfrow=c(4,4))
UCs=unique(as.character(data2[data2$verger=="GF",]$codeUC))
for (i in 1:length(UCs)){comp_pheno(data2,simGF,simGF2,UCs[i])}

# Bassin Plat
par(mfrow=c(4,4))
UCs=unique(as.character(data2[data2$verger=="BP",]$codeUC))
for (i in 1:length(UCs)){comp_pheno(data2,simBP,simBP2,UCs[i])}

# Bassin Martin
par(mfrow=c(4,4))
UCs=unique(as.character(data2[data2$verger=="BM",]$codeUC))
for (i in 1:length(UCs)){comp_pheno(data2,simBM,simBM2,UCs[i])}

dev.off()



####################################################################
#########   Traitement des données sur la surface foliaire  ########
####################################################################
pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/feuilles.pdf")
# On veut voir les courbes de croissance des longueurs de feuilles (totales) simulées pour chaque UC 
# Grand-Fond
par(mfrow=c(4,5))
UCs=unique(simGF$CodeUC)
for (i in 1:length(UCs)){
	Feusim=simGF[simGF$CodeUC==UCs[i],]
	plot(unique(Feusim$Date),Feusim$SFol,type='l',col=2
		,main=UCs[i],xlab="Date",ylab="Surface foliaire totale")
}

# Bassin Plat
par(mfrow=c(4,5))
UCs=unique(simBP$CodeUC)
for (i in 1:length(UCs)){
	Feusim=simBP[simBP$CodeUC==UCs[i],]
	plot(unique(Feusim$Date),Feusim$SFol,type='l',col=2
		,main=UCs[i],xlab="Date",ylab="Surface foliaire totale")
}

# Bassin Martin
par(mfrow=c(4,5))
UCs=unique(simBM$CodeUC)
for (i in 1:length(UCs)){
	Feusim=simBM[simBM$CodeUC==UCs[i],]
	plot(unique(Feusim$Date),Feusim$SFol,type='l',col=2
		,main=UCs[i],xlab="Date",ylab="Surface foliaire totale")
}
dev.off()


###### Etude de la croissance des feuilles (feuille par feuille : F proximale, F distale et première F distale)
pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/FeuillesInd.pdf")

############## On veut voir les courbes de croissance réelles et simulées pour chaque Feuille 
# Grand-Fond
croissFeu("GF","-Fprox",data2,simFeuInd_GF)
croissFeu("GF","-Fdist",data2,simFeuInd_GF)
croissFeu("GF","-F1",data2,simFeuInd_GF)

# Bassin Plat
croissFeu("BP","-Fprox",data2,simFeuInd_BP)
croissFeu("BP","-Fdist",data2,simFeuInd_BP)
croissFeu("BP","-F1",data2,simFeuInd_BP)

# Bassin Martin
croissFeu("BM","-Fprox",data2,simFeuInd_BM)
croissFeu("BM","-Fdist",data2,simFeuInd_BM)
croissFeu("BM","-F1",data2,simFeuInd_BM)

dev.off()


# Validation
pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/comp_croiss_Feuilles.pdf")


# Grand-Fond
comp_croiss_feu("GF","-Fprox",data2,simFeuInd_GF)
comp_croiss_feu("GF","-Fdist",data2,simFeuInd_GF)
comp_croiss_feu("GF","-F1",data2,simFeuInd_GF)

# Bassin Plat
comp_croiss_feu("BP","-Fprox",data2,simFeuInd_BP)
comp_croiss_feu("BP","-Fdist",data2,simFeuInd_BP)
comp_croiss_feu("BP","-F1",data2,simFeuInd_BP)

# Bassin Martin
comp_croiss_feu("BM","-Fprox",data2,simFeuInd_BM)
comp_croiss_feu("BM","-Fdist",data2,simFeuInd_BM)
comp_croiss_feu("BM","-F1",data2,simFeuInd_BM)


#par(mfrow=c(1,1))
#plot(obs,sim,ylim=c(0,24),xlim=c(0,24),xlab="stade observé",ylab="stade estimé")
#abline(lsfit(obs,sim),col=2)
#abline(0,1)
#lm.stade=lm(sim~obs)
#abline(lm.stade$coeff,col=4)
#summary(lm.stade)

#lm2.stade=lm(x~y-1)
#summary(lm2.stade)

dev.off()




####################################################################
######### Traitement des données sur la longueur des inflos ########
####################################################################
pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/inflos.pdf")
# On veut voir les courbes de croissance réelles et simulées pour chaque UC 
# Grand-Fond
par(mfrow=c(4,4))
inflos=unique(as.character(inflo2[inflo2$verger=="GF",]$codeUC))
for (i in 1:length(inflos)){
	inflo=inflo2[inflo2$codeUC==inflos[i],]
	inflosim=simGF_inflo[simGF_inflo$CodeUC==inflos[i],]
	plot(inflo$date,inflo$longueurInflo,main=inflos[i],xlab="Date",ylab="Longueur axe inflo")
	points(unique(inflosim$Date),unlist(aggregate(inflosim$Length,by=list(as.character(inflosim$Date)),FUN=sum)[2]),type='l',col=2)
}

# Bassin Plat
par(mfrow=c(4,4))
inflos=unique(as.character(inflo2[inflo2$verger=="BP",]$codeUC))
for (i in 1:length(inflos)){
	inflo=inflo2[inflo2$codeUC==inflos[i],]
	inflosim=simBP_inflo[simBP_inflo$CodeUC==inflos[i],]
	plot(inflo$date,inflo$longueurInflo,main=inflos[i],xlab="Date",ylab="Longueur axe inflo")
	points(unique(inflosim$Date),unlist(aggregate(inflosim$Length,by=list(as.character(inflosim$Date)),FUN=sum)[2]),type='l',col=2)
}

# Bassin Martin
par(mfrow=c(4,4))
inflos=unique(as.character(inflo2[inflo2$verger=="BM",]$codeUC))
for (i in 1:length(inflos)){
	inflo=inflo2[inflo2$codeUC==inflos[i],]
	inflosim=simBM_inflo[simBM_inflo$CodeUC==inflos[i],]
	plot(inflo$date,inflo$longueurInflo,main=inflos[i],xlab="Date",ylab="Longueur axe inflo")
	points(unique(inflosim$Date),unlist(aggregate(inflosim$Length,by=list(as.character(inflosim$Date)),FUN=sum)[2]),type='l',col=2)
}
dev.off()


# Validation
pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/comp_croiss_inflos.pdf")

sim=c();obs=c()

# Grand-Fond
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(inflo2[inflo2$verger=="GF",]$codeUC))
for (i in 1:length(UCs)){res=valid_croiss_inflo(inflo2,simGF_inflo,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
plot(x,y,ylim=c(0,24),xlim=c(0,24),main="Grand-Fond",xlab="stade observé",ylab="stade estimé")
abline(lsfit(x,y),col=2)
abline(0,1)
sim=c(sim,y);obs=c(obs,x)

# Bassin Plat
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(inflo2[inflo2$verger=="BP",]$codeUC))
for (i in 1:length(UCs)){res=valid_croiss_inflo(inflo2,simBP_inflo,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
plot(x,y,ylim=c(0,24),xlim=c(0,24),main="Bassin plat",xlab="stade observé",ylab="stade estimé")
abline(lsfit(x,y),col=2)
abline(0,1)
sim=c(sim,y);obs=c(obs,x)

# Bassin Martin
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(inflo2[inflo2$verger=="BM",]$codeUC))
for (i in 1:length(UCs)){res=valid_croiss_inflo(inflo2,simBM_inflo,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
plot(x,y,ylim=c(0,24),xlim=c(0,24),main="Bassin Martin",xlab="stade observé",ylab="stade estimé")
abline(lsfit(x,y),col=2)
abline(0,1)
sim=c(sim,y);obs=c(obs,x)


par(mfrow=c(1,1))
plot(obs,sim,ylim=c(0,44),xlim=c(0,44), cex.lab = 1.7, cex.axis = 1,xlab="Longueur observée",ylab="Longueur estimée")
abline(lsfit(obs,sim),col=2,lwd=2)
abline(0,1)
legend("bottomright",c("bissectrice y=x","droite de régression"),cex=1.5,lty=1,col=c(1,2))

lm.stade=lm(sim~obs)
#abline(lm.stade$coeff,col=4)
summary(lm.stade)

lm2.stade=lm(x~y-1)
summary(lm2.stade)

dev.off()



#### Etude des stades phénologiques

pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/validation/graphs/growth_development/comp_pheno_inflos.pdf")
ax=c(0,0,1,1,1,2,2,2,3,3)
by=c(0,1,0,1,2,1,2,3,2,3)

# Grand-Fond
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(inflo2[inflo2$verger=="GF",]$codeUC))
for (i in 1:length(UCs)){res=valid_pheno_inflo(inflo2,simGF_inflo,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
s=c(sum(x==0&y==0),sum(x==0&y==1),sum(x==1&y==0),sum(x==1&y==1),sum(x==1&y==2),sum(x==2&y==1),sum(x==2&y==2),sum(x==2&y==3),sum(x==3&y==2),sum(x==3&y==3))
col=rep(1,length(s))
col[s==0]=0
symbols(ax,by,circles=s/length(x),inches=FALSE,fg=col,xlim=c(-0.5,3.5),bg=col,ylim=c(-0.5,3.5),lwd=2.5,main="Grand-Fond",xlab="stade observé",ylab="stade estimé") 
abline(0,1);abline(lsfit(x,y),col=2)
simuGF=y;obsGF=x

# Bassin Plat
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(inflo2[inflo2$verger=="BP",]$codeUC))
for (i in 1:length(UCs)){res=valid_pheno_inflo(inflo2,simBP_inflo,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
s=c(sum(x==0&y==0),sum(x==0&y==1),sum(x==1&y==0),sum(x==1&y==1),sum(x==1&y==2),sum(x==2&y==1),sum(x==2&y==2),sum(x==2&y==3),sum(x==3&y==2),sum(x==3&y==3))
col=rep(1,length(s))
col[s==0]=0
symbols(ax,by,circles=s/length(x),inches=FALSE,fg=col,xlim=c(-0.5,3.5),bg=col,ylim=c(-0.5,3.5),lwd=2.5,main="Bassin Plat",xlab="stade observé",ylab="stade estimé") 
abline(0,1);abline(lsfit(x,y),col=2)
simuBP=y;obsBP=x

# Bassin Martin
par(mfrow=c(4,4))
x=c();y=c()
UCs=unique(as.character(inflo2[inflo2$verger=="BM",]$codeUC))
for (i in 1:length(UCs)){res=valid_pheno_inflo(inflo2,simBM_inflo,UCs[i])
				x=c(x,res$obs);y=c(y,res$sim);}
par(mfrow=c(1,1))
s=c(sum(x==0&y==0),sum(x==0&y==1),sum(x==1&y==0),sum(x==1&y==1),sum(x==1&y==2),sum(x==2&y==1),sum(x==2&y==2),sum(x==2&y==3),sum(x==3&y==2),sum(x==3&y==3))
col=rep(1,length(s))
col[s==0]=0
symbols(ax,by,circles=s/length(x),inches=FALSE,fg=col,xlim=c(-0.5,3.5),bg=col,ylim=c(-0.5,3.5),lwd=2.5,main="Grand-Fond",xlab="stade observé",ylab="stade estimé") 
abline(0,1);abline(lsfit(x,y),col=2)
simuBM=y;obsBM=x


par(mfrow=c(1,1))
sim=c(simuGF,simuBM,simuBP);obs=c(obsGF,obsBM,obsBP)
par(mfrow=c(1,1))
x=obs;y=sim
s=c(sum(x==0&y==0),sum(x==0&y==1),sum(x==1&y==0),sum(x==1&y==1),sum(x==1&y==2),sum(x==2&y==1),sum(x==2&y==2),sum(x==2&y==3),sum(x==3&y==2),sum(x==3&y==3))
col=rep(1,length(s))
col[s==0]=0
symbols(ax,by,circles=s*0.5/length(x),cex.lab=1.7,inches=FALSE,xaxt="n",yaxt="n",fg=col,xlim=c(-0.5,3.5),bg=col,ylim=c(-0.5,3.5),lwd=2.5,main="",xlab="stade observé",ylab="stade estimé")
axis(1, at = 0:3, labels = c("D","E","F","G"), cex.axis = 1)
axis(2, at = 0:3, labels = c("D","E","F","G"), cex.axis = 1)
abline(0,1);abline(lsfit(obs,sim),col=2,lwd=2)
legend("bottomright",c("bissectrice y=x","droite de régression"),cex=1.5,lty=1,col=c(1,2))


lm.stade=lm(obs~sim)
summary(lm.stade)

lm2.stade=lm(obs~sim-1)
summary(lm2.stade)

dev.off()

###############################################################?
######## Etude pour le rapport
################

##################### UCs

# Grand-Fond
par(mfrow=c(1,6))
UCs=c("1-GF-cog-V-B12-1-A", "2-GF-cog-V-A39-2-A")           #17.2 / 5.6
#unique(as.character(data2[data2$verger=="GF",]$codeUC))
for (i in 1:length(UCs)){
	UC=data2[data2$codeUC==UCs[i],]
	UCsim=simGF[simGF$CodeUC==UCs[i],]
	plot(UC$date, cex.lab = 1.7, cex.axis = 1.5,lwd=3,UC$longueurUC,main=UCs[i],xlab="Date",ylab="Longueur axe UC",ylim=c(0,20))
	points(UCsim$Date,UCsim$Length,type='l',col=2,lwd=2)
}

# Bassin Plat
UCs=c("1-BP-cog-V-A10-1-A", "2-BP-cog-V-B7-2-L")                             #18.6  /  5
for (i in 1:length(UCs)){
	UC=data2[data2$codeUC==UCs[i],]
	UCsim=simBP[simBP$CodeUC==UCs[i],]
	plot(UC$date, cex.lab = 1.7, cex.axis = 1.5,lwd=3,UC$longueurUC,main=UCs[i],xlab="Date",ylab="Longueur axe UC",ylim=c(0,20))
	points(UCsim$Date,UCsim$Length,type='l',col=2,lwd=2)
}

# Bassin Martin
UCs=c("1-BM-cog-V-F4-1-A","2-BM-cog-V-B8-1-A")                                  #18.9  /  5.5
for (i in 1:length(UCs)){
	UC=data2[data2$codeUC==UCs[i],]
	UCsim=simBM[simBM$CodeUC==UCs[i],]
	plot(UC$date, cex.lab = 1.7, cex.axis = 1.5,lwd=3,UC$longueurUC,main=UCs[i],xlab="Date",ylab="Longueur axe UC",ylim=c(0,20))
	points(UCsim$Date,UCsim$Length,type='l',col=2,lwd=2)
}



# Validation croissance
# Grand-Fond
par(mfrow=c(1,6))
UCs=c("1-GF-cog-V-B12-1-A", "2-GF-cog-V-A39-2-A")                               #17.2 / 5.6
for (i in 1:length(UCs)){res=valid_croiss(data2,simGF,UCs[i])}

# Bassin Plat
UCs=c("1-BP-cog-V-A10-1-A", "2-BP-cog-V-B7-2-L")                                #18.6  /  5
for (i in 1:length(UCs)){res=valid_croiss(data2,simBP,UCs[i])}

# Bassin Martin
UCs=c("1-BM-cog-V-F4-1-A","2-BM-cog-V-B8-1-A")                                  #18.9  /  5.5
for (i in 1:length(UCs)){res=valid_croiss(data2,simBM,UCs[i])}



# phéno
# Grand-Fond
par(mfrow=c(1,6))
UCs=c("1-GF-cog-V-B12-1-A", "2-GF-cog-V-A39-2-A")                               #17.2 / 5.6
for (i in 1:length(UCs)){affich_pheno(data2,simGF,UCs[i])}

# Bassin Plat
UCs=c("1-BP-cog-V-A10-1-A", "2-BP-cog-V-B7-2-L")                                #18.6  /  5
for (i in 1:length(UCs)){affich_pheno(data2,simBP,UCs[i])}

# Bassin Martin
UCs=c("1-BM-cog-V-F4-1-A","2-BM-cog-V-B8-1-A")                                  #18.9  /  5.5
for (i in 1:length(UCs)){affich_pheno(data2,simBM,UCs[i])}

# validation phéno
# Grand-Fond
par(mfrow=c(1,6))
UCs=c("1-GF-cog-V-B12-1-A", "2-GF-cog-V-A39-2-A")                               #17.2 / 5.6
for (i in 1:length(UCs)){res=valid_pheno(data2,simGF,UCs[i])}

# Bassin Plat
UCs=c("1-BP-cog-V-A10-1-A", "2-BP-cog-V-B7-2-L")                                #18.6  /  5
for (i in 1:length(UCs)){res=valid_pheno(data2,simBP,UCs[i])}

# Bassin Martin
UCs=c("1-BM-cog-V-F4-1-A","2-BM-cog-V-B8-1-A")                                  #18.9  /  5.5
for (i in 1:length(UCs)){res=valid_pheno(data2,simBM,UCs[i])}




##################### Inflorescences

# On veut voir les courbes de croissance réelles et simulées pour chaque inflo 
# Grand-Fond
par(mfrow=c(1,6))
inflos=c("3-GF-cog-F-A37-1-A","3-GF-cog-F-B25-1-L2")                                                #18.45 / 28
for (i in 1:length(inflos)){
	inflo=inflo2[inflo2$codeUC==inflos[i],]
	inflosim=simGF_inflo[simGF_inflo$CodeUC==inflos[i],]
	plot(inflo$date,inflo$longueurInflo, cex.lab = 1.7, cex.axis = 1.5,lwd=3,main=inflos[i],xlab="Date",ylab="Longueur axe inflo",ylim=c(0,50))
	points(inflosim$Date,inflosim$Length,type='l',col=2,lwd=2)
}

# Bassin Plat
inflos=c("3-BP-cog-F-A3-1-A","3-BP-cog-F-F3-1-A")                                                #16.6 / 35.9
for (i in 1:length(inflos)){
	inflo=inflo2[inflo2$codeUC==inflos[i],]
	inflosim=simBP_inflo[simBP_inflo$CodeUC==inflos[i],]
	plot(inflo$date,inflo$longueurInflo, cex.lab = 1.7, cex.axis = 1.5,lwd=3,main=inflos[i],xlab="Date",ylab="Longueur axe inflo",ylim=c(0,50))
	points(inflosim$Date,inflosim$Length,type='l',col=2,lwd=2)
}

# Bassin Martin
inflos=c("3-BM-cog-F-B14-1-L","3-BM-cog-F-B14-2-A")                                                #16.5 / 44.45
for (i in 1:length(inflos)){
	inflo=inflo2[inflo2$codeUC==inflos[i],]
	inflosim=simBM_inflo[simBM_inflo$CodeUC==inflos[i],]
	plot(inflo$date,inflo$longueurInflo, cex.lab = 1.7, cex.axis = 1.5,lwd=3,main=inflos[i],xlab="Date",ylab="Longueur axe inflo",ylim=c(0,50))
	points(inflosim$Date,inflosim$Length,type='l',col=2,lwd=2)
}



# Validation
# Grand-Fond
par(mfrow=c(1,6))
inflos=c("3-GF-cog-F-A37-1-A","3-GF-cog-F-B25-1-L2")                                                #18.45 / 28
for (i in 1:length(inflos)){res=valid_croiss_inflo(inflo2,simGF_inflo,inflos[i])}

# Bassin Plat
inflos=c("3-BP-cog-F-A3-1-A","3-BP-cog-F-F3-1-A")                                                #16.6 / 35.9
for (i in 1:length(inflos)){res=valid_croiss_inflo(inflo2,simBP_inflo,inflos[i])}

# Bassin Martin
inflos=c("3-BM-cog-F-B14-1-L","3-BM-cog-F-B14-2-A")                                                #16.5 / 44.45
for (i in 1:length(inflos)){res=valid_croiss_inflo(inflo2,simBM_inflo,inflos[i])}


#### Etude des stades phénologiques
# Grand-Fond
par(mfrow=c(1,6))
UCs=c("3-GF-cog-F-A37-1-A","3-GF-cog-F-B25-1-L2")
for (i in 1:length(UCs)){res=valid_pheno_inflo(inflo2,simGF_inflo,UCs[i])}

# Bassin Plat
UCs=c("3-BP-cog-F-A3-1-A","3-BP-cog-F-F3-1-A") 
for (i in 1:length(UCs)){res=valid_pheno_inflo(inflo2,simBP_inflo,UCs[i])}

# Bassin Martin
UCs=c("3-BM-cog-F-B14-1-L","3-BM-cog-F-B14-2-A")
for (i in 1:length(UCs)){res=valid_pheno_inflo(inflo2,simBM_inflo,UCs[i])}











