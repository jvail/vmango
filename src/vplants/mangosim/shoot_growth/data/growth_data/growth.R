setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/data")

# Etude des vitesses de croissances


####### Définitions de fonctions

vitesse_fct=function(data,cond){
V=matrix(rep(0,length(unique(data$codeUC[cond]))*20),nrow=length(unique(data$codeUC[cond])))
D=matrix(rep(0,length(unique(codeUCs))*20),nrow=length(unique(codeUCs)))
plot(unique(data$date[cond]),rep(-100,length(unique(data$date[cond]))),ylim=c(0,0.3),ylab="Vitesse de croissance",xlab="")
UCs=unique(data$codeUC[cond])
for (i in 1:(length(UCs)-1)){
  data1=data[data$codeUC==UCs[i],]
  n1=length(data1$longueurUC)
  vitesse1=(data1$longueurUC[2:n1]-data1$longueurUC[1:(n1-1)])/as.numeric(data1$date[2:n1]-data1$date[1:(n1-1)])
  vitesse=vitesse1[!is.na(vitesse1)]/max(data1$longueurUC,na.rm=T)
  temps=as.numeric(data1$date[2:n1]-data1$date[1:(n1-1)])[!is.na(vitesse1)]
  D[i,2:(length(vitesse)+1)]=temps
  V[i,2:(length(vitesse)+1)]=vitesse
  points(data1$date[-1],vitesse1,type='l',col=i,lwd=2)
}
return(list("V"=V,"D"=D))
}




#### Recherche de la vitesse maximale

# On récupère les infos de la base de croissance
CroissInflo=read.csv("growth_data/BaseDeCroissanceInflo.csv",sep=";",dec=",")
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
CroissUC=read.csv("growth_data/BaseDeCroissanceVeg.csv",sep=";",dec=",")
CroissUC$date=strptime(as.character(CroissUC$date),"%d/%m/%Y %H:%M")
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


# Etudes des itesses pour 2 UCs dans des contextes thermiques différents
UCs=c("2-BM-cog-V-F2-1-A","2-GF-cog-V-A39-1-A")

data1=CroissUC[CroissUC$codeUC==UCs[1],]
data2=CroissUC[CroissUC$codeUC==UCs[2],]
n1=length(data1$longueurUC)
n2=length(data2$longueurUC)
vitesse1=(data1$longueurUC[2:n1]-data1$longueurUC[1:(n1-1)])/as.numeric(data1$date[2:n1]-data1$date[1:(n1-1)])
vitesse2=(data2$longueurUC[2:n2]-data2$longueurUC[1:(n2-1)])/as.numeric(data2$date[2:n2]-data2$date[1:(n2-1)])
vitesse1[is.na(vitesse1)]=0
vitesse2[is.na(vitesse2)]=0
par(mfrow=c(1,1))
plot(unique(c(data1$date,data2$date)),rep(-99,length(unique(c(data1$date,data2$date)))),ylim=c(0,0.1),xlab="date",ylab="Vitesse de croissance")
points(data2$date[-1],vitesse2,type='l',col=4,lwd=2)
#plot(data1$date[-c(1,17,18,19)],vitesse2,type='l',col=4,lwd=2,xlab="Nombre de jours de croissance",ylab="Vitesse de croissance de l'UC",xaxt="n")
#points(data1$date[-c(1,17,18,19)],vitesse1[-c(1,17,18,19)],type='l',col=3,lwd=2)
points(data1$date[-1],vitesse1,type='l',col=3,lwd=2)
legend("topright",c("Bassin Martin, 24.21°C","Grand Fond, 28.18°C"),lty=1,lwd=2,col=3:4)


# Etudes des vitesses pour l'ensemble des UCs
D=matrix(rep(0,length(unique(codeUCs))*20),nrow=length(unique(codeUCs)))
V=matrix(rep(0,length(unique(codeUCs))*20),nrow=length(unique(codeUCs)))
plot(unique(CroissUC$date),rep(-100,length(unique(CroissUC$date))),ylim=c(0,0.5),ylab="Vitesse de croissance",xlab="")
UCs=unique(codeUCs)
for (i in 1:length(UCs)){
  data1=CroissUC[CroissUC$codeUC==UCs[i],]
  n1=length(data1$longueurUC)
  vitesse1=(data1$longueurUC[2:n1]-data1$longueurUC[1:(n1-1)])/as.numeric(data1$date[2:n1]-data1$date[1:(n1-1)])
  vitesse=vitesse1[!is.na(vitesse1)]
  temps=as.numeric(data1$date[2:n1]-data1$date[1:(n1-1)])[!is.na(vitesse1)]
  V[i,2:(length(vitesse)+1)]=vitesse
  D[i,2:(length(vitesse)+1)]=temps
  points(data1$date[-1],vitesse1,type='l',col=i,lwd=2)
}

plot(1:600,rep(-100,600),ylim=c(0,0.5),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(unique(codeUCs))){  points(cumsum(D[i,]),V[i,],type='l',col=i)}

#On scinde les UCs en 3 groupes en fonction de leur vitesse maximale
par(mfrow=c(3,1))
# Premier groupe
V1=rep(-9,20);D1=rep(-9,20)
for (i in 1:length(UCs)){  if (max(V[i,])>=0.15){V1=rbind(V1,V[i,]);D1=rbind(D1,D[i,])}}
#Deuxième groupe
V2=rep(-9,20);D2=rep(-9,20)
for (i in 1:length(UCs)){  if (max(V[i,])<0.15 & max(V[i,])>0.08){V2=rbind(V2,V[i,]);D2=rbind(D2,D[i,])}}
#Troisième groupe
V3=rep(-9,20);D3=rep(-9,20)
for (i in 1:length(UCs)){  if (max(V[i,])<=0.08){V3=rbind(V3,V[i,]);D3=rbind(D3,D[i,])}}


plot(1:600,rep(-100,600),ylim=c(0,0.2),ylab="Vitesse de croissance",xlab="")
for (i in 2 :length(V1[,1])){  points(cumsum(D1[i,]),V1[i,],type='l',col=i)}

plot(1:600,rep(-100,600),ylim=c(0,0.2),ylab="Vitesse de croissance",xlab="")
for (i in 2 :length(V2[,1])){  points(cumsum(D2[i,]),V2[i,],type='l',col=i)}

plot(1:600,rep(-100,600),ylim=c(0,0.2),ylab="Vitesse de croissance",xlab="")
for (i in 2 :length(V3[,1])){  points(cumsum(D3[i,]),V3[i,],type='l',col=i)}


#On scinde les UCs en 3 groupes en fonction de leur durée de croissance
par(mfrow=c(3,1))
# Premier groupe
E1=rep(-9,20);DE1=rep(-9,20)
for (i in 1:length(UCs)){  if (sum(V[i,]!=0)>11){E1=rbind(E1,V[i,]);DE1=rbind(DE1,D[i,])}}
#Deuxième groupe
E2=rep(-9,20);DE2=rep(-9,20)
for (i in 1:length(UCs)){  if (sum(V[i,]!=0)>8 & sum(V[i,]!=0)<=11){E2=rbind(E2,V[i,]);DE2=rbind(DE2,D[i,])}}
#Troisième groupe
E3=rep(-9,20);DE3=rep(-9,20)
for (i in 1:length(UCs)){  if (sum(V[i,]!=0)<=8){E3=rbind(E3,V[i,]);DE3=rbind(DE3,D[i,])}}


plot(1:400,rep(-100,400),ylim=c(0,0.25),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(E1[,1])){  points(cumsum(DE1[i,]),E1[i,],type='l',col=i)}

plot(1:400,rep(-100,400),ylim=c(0,0.25),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(E2[,1])){  points(cumsum(DE2[i,]),E2[i,],type='l',col=i)}

plot(1:400,rep(-100,400),ylim=c(0,0.25),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(E3[,1])){  points(cumsum(DE3[i,]),E3[i,],type='l',col=i)}



########### Etude des vitesses normalisées par la taille de l'UC
# Etudes des vitesses pour l'ensemble des UCs
V_norm=matrix(rep(0,length(unique(codeUCs))*20),nrow=length(unique(codeUCs)))
D_norm=matrix(rep(0,length(unique(codeUCs))*20),nrow=length(unique(codeUCs)))
plot(unique(CroissUC$date),rep(-100,length(unique(CroissUC$date))),ylim=c(0,0.5),ylab="Vitesse de croissance",xlab="")
UCs=unique(codeUCs)
for (i in 1:length(UCs)){
  data1=CroissUC[CroissUC$codeUC==UCs[i],]
  n1=length(data1$longueurUC)
  vitesse1=(data1$longueurUC[2:n1]-data1$longueurUC[1:(n1-1)])/as.numeric(data1$date[2:n1]-data1$date[1:(n1-1)])
  vitesse=vitesse1[!is.na(vitesse1)]/max(data1$longueurUC,na.rm=T)
  temps=as.numeric(data1$date[2:n1]-data1$date[1:(n1-1)])[!is.na(vitesse1)]
  V_norm[i,2:(length(vitesse)+1)]=vitesse
  D_norm[i,2:(length(vitesse)+1)]=temps
  
  points(data1$date[-1],vitesse1,type='l',col=i,lwd=2)
}

plot(1:600,rep(-100,600),ylim=c(0,0.02),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(unique(codeUCs))){  points(cumsum(D_norm[i,]),V_norm[i,],type='l',col=i)}

#On scinde les UCs en 3 groupes en fonction de leur vitesse maximale
par(mfrow=c(3,1))
# Premier groupe
V1_norm=rep(-9,20)
for (i in 1:length(UCs)){  if (max(V_norm[i,])>=0.008){V1_norm=rbind(V1_norm,V_norm[i,])}}

plot(1:20,rep(-100,20),ylim=c(0,0.02),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(V1_norm[,1])){  points(V1_norm[i,],type='l',col=i)}

#Deuxième groupe
V2_norm=rep(-9,20)
for (i in 1:length(UCs)){  if (max(V_norm[i,])<0.008 & max(V_norm[i,])>0.006){V2_norm=rbind(V2_norm,V_norm[i,])}}

plot(1:20,rep(-100,20),ylim=c(0,0.02),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(V2_norm[,1])){  points(V2_norm[i,],type='l',col=i)}

#Troisième groupe
V3_norm=rep(-9,20)
for (i in 1:length(UCs)){  if (max(V_norm[i,])<=0.006){V3_norm=rbind(V3_norm,V_norm[i,])}}

plot(1:20,rep(-100,20),ylim=c(0,0.02),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(V3_norm[,1])){  points(V3_norm[i,],type='l',col=i)}


#On scinde les UCs en 3 groupes en fonction de leur durée de croissance
par(mfrow=c(3,1))
# Premier groupe
E1_norm=rep(-9,20)
for (i in 1:length(UCs)){  if (sum(V_norm[i,]!=0)>11){E1_norm=rbind(E1_norm,V_norm[i,])}}

plot(1:20,rep(-100,20),ylim=c(0,0.02),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(E1_norm[,1])){  points(E1_norm[i,],type='l',col=i)}

#Deuxième groupe
E2_norm=rep(-9,20)
for (i in 1:length(UCs)){  if (sum(V_norm[i,]!=0)>8 & sum(V_norm[i,]!=0)<=11){E2_norm=rbind(E2_norm,V_norm[i,])}}

plot(1:20,rep(-100,20),ylim=c(0,0.02),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(E2_norm[,1])){  points(E2_norm[i,],type='l',col=i)}

#Troisième groupe
E3_norm=rep(-9,20)
for (i in 1:length(UCs)){  if (sum(V_norm[i,]!=0)<=8){E3_norm=rbind(E3_norm,V_norm[i,])}}

plot(1:20,rep(-100,20),ylim=c(0,0.02),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(E3_norm[,1])){  points(E3_norm[i,],type='l',col=i)}


########### Etude des vitesses normalisées par la taille de l'UC en fonction de la date de débourrement
# Etudes des vitesses pour l'ensemble des UCs
# Janvier-février
jan=CroissUC$date$mon<2 #& as.numeric(CroissUC$stadeUC)<4
res_jan=vitesse_fct(CroissUC,jan)
V_jan=res_jan$V;D_jan=res_jan$D

# Avril
avr=CroissUC$date$mon<5 & CroissUC$date$mon>2
res_avr=vitesse_fct(CroissUC,avr)
V_avr=res_avr$V;D_avr=res_avr$D

# Septembre ++
sept=CroissUC$date$mon>8
res_sept=vitesse_fct(CroissUC,sept)
V_sept=res_sept$V;D_sept=res_sept$D



par(mfrow=c(3,1))
plot(1:600,rep(-100,600),ylim=c(0,0.02),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(V_jan[,1])){  points(cumsum(D_jan[i,]),V_jan[i,],type='l',col=i)}

plot(1:600,rep(-100,600),ylim=c(0,0.02),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(V_avr[,1])){  points(cumsum(D_avr[i,]),V_avr[i,],type='l',col=i)}

plot(1:600,rep(-100,600),ylim=c(0,0.02),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(V_sept[,1])){  points(cumsum(D_sept[i,]),V_sept[i,],type='l',col=i)}


#On scinde les UCs en 3 groupes en fonction de leur vitesse maximale
# Premier groupe
V1=rep(-9,20);D1=seq(20)
for (i in 1:length(V_jan[,1])){  if (max(V_jan[i,])>=0.009){V1=rbind(V1,V_jan[i,]);D1=rbind(D1,D_jan[i,])}}
#Deuxième groupe
V2=rep(-9,20);D2=seq(20)
for (i in 1:length(V_jan[,1])){  if (max(V_jan[i,])<0.009 & max(V_jan[i,])>0.007){V2=rbind(V2,V_jan[i,]);D2=rbind(D2,D_jan[i,])}}
#Troisième groupe
V3=rep(-9,20);D3=seq(20)
for (i in 1:length(V_jan[,1])){  if (max(V_jan[i,])<=0.007){V3=rbind(V3,V_jan[i,]);D3=rbind(D3,D_jan[i,])}}

par(mfrow=c(2,3))

plot(1:400,rep(-100,400),ylim=c(0,0.015),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(V1[,1])){  points(cumsum(D1[i,]),V1[i,],type='l',col=i)}

plot(1:400,rep(-100,400),ylim=c(0,0.015),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(V2[,1])){  points(cumsum(D2[i,]),V2[i,],type='l',col=i)}

plot(1:400,rep(-100,400),ylim=c(0,0.015),ylab="Vitesse de croissance",xlab="")
for (i in 1 :length(V3[,1])){  points(cumsum(D3[i,]),V3[i,],type='l',col=i)}





# On va ajuster une fonction gamma aux courbes de vitesse de croissance
fgamma=function(x,alpha,beta){
  (x^(alpha-1))*(beta^alpha)*exp(-beta*x)/gamma(alpha)
}
coeffs1=c();coeffs2=c();coeffs3=c()

par(mfrow=c(1,3))
plot(1:400,rep(-100,400),ylim=c(0,0.015),ylab="Vitesse de croissance",xlab="")
for (i in c(2:17,19 :length(V1[,1]))){
  t=cumsum(D1[i,])
  sim=V1[i,]
  #on ajuste la fonction sur les données simulées en utilisant les moindres carrés
  fit=nls(sim~fgamma(t,a,b),start=list(a=1,b=0.01))
  coeff=summary(fit)$coeff
  coeffs1=c(coeffs1,coeff[1],coeff[2])
  points(t,fgamma(t,coeff[1],coeff[2]),type="l",col=i)
}

plot(1:400,rep(-100,400),ylim=c(0,0.015),ylab="Vitesse de croissance",xlab="")
for (i in c(2:8,10:15,17:length(V2[,1]))){
  t=cumsum(D2[i,])
  sim=V2[i,]
  #on ajuste la fonction sur les données simulées en utilisant les moindres carrés
  fit=nls(sim~fgamma(t,a,b),start=list(a=1,b=0.01))
  coeff=summary(fit)$coeff
  coeffs2=c(coeffs2,coeff[1],coeff[2])
  points(t,fgamma(t,coeff[1],coeff[2]),type="l",col=i)
}

plot(1:400,rep(-100,400),ylim=c(0,0.015),ylab="Vitesse de croissance",xlab="")
for (i in c(2,3,4,7,9,12,14:16,18:22)){
  t=cumsum(D3[i,])
  sim=V3[i,]
  #on ajuste la fonction sur les données simulées en utilisant les moindres carrés
  fit=nls(sim~fgamma(t,a,b),start=list(a=1,b=0.01))
  coeffs3=c(coeffs3,coeff[1],coeff[2])
  coeff=summary(fit)$coeff
  points(t,fgamma(t,coeff[1],coeff[2]),type="l",col=i)
}