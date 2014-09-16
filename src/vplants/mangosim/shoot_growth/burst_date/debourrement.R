# Etude des dates de débourrement des UCs et des inflorescences

setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/data/burst_date")


# Fonction d'affichage des histogrammes 

#UCs apicales en vert et latérales en blanc
arbre_posit=function(data,numarbre){
		hist(data[data$arbre==numarbre ,]$week,breaks=seq(0,52),xlab="semaine",ylab="Nombre de débourrement")
		hist(data[data$arbre==numarbre & data$posit=="A" ,]$week,breaks=seq(0,52),add=T,col=3)}

#UCs en vert et inflorescences en blanc (par semaine)
arbre_nature=function(data,numarbre){
	hist(data[data$arbre==numarbre & !is.na(data$week),]$week,breaks=seq(0,52),main=numarbre,xlab="semaine",ylab="Nombre de débourrement")
	hist(data[data$arbre==numarbre & data$type=="V" & !is.na(data$week) ,]$week,breaks=seq(0,52),add=T,col=3)}

#UCs en vert et inflorescences en blanc (par mois)
arbre_nature_m=function(data,numarbre){
	hist(data[data$arbre==numarbre  & !is.na(data$month),]$month,breaks=seq(0,20),main=numarbre,xlab="mois",ylab="Nombre de débourrement",xlim=c(0,12))
	hist(data[data$arbre==numarbre & data$type=="V" & !is.na(data$month) ,]$month,breaks=seq(0,12),add=T,col=3)}


# Fonction de mise en forme des données 
MEF=function(fichier){
data=read.csv(fichier,header=TRUE,sep=";",dec=",")
data$ddeb=strptime(as.character(data$ddeb), "%d/%m/%Y")
data$year=as.numeric(as.character(data$year))
data$month=as.numeric(as.character(data$month))
data$week=as.numeric(as.character(data$week))
return( data)
}


###### Import des données
cly0=MEF("deb_cly.csv")
cpea0=MEF("deb_cpea.csv")
cirad0=MEF("deb_cirad.csv")
#cly7=cly0[cly0$arbre==7,];cly3=cly0[cly0$arbre==3,];cly20=cly0[cly0$arbre==20,];cly27=cly0[cly0$arbre==27,];cly16=cly0[cly0$arbre==16,];




# Etude des distributions

#UCs végétatives
hist(cpea0[cpea0$type=="V" & !is.na(cpea0$week),]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
hist(cirad0[cirad0$type=="V" & !is.na(cirad0$week),]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
hist(cly0[cly0$type=="V" & !is.na(cly0$week),]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")




# Inflorescences
hist(cly0[cly0$type!="V" & !is.na(cly0$week),]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
  hist(cly0[cly0$type!="V" & !is.na(cly0$week) & cly0$year==2007,]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
  hist(cly0[cly0$type!="V" & !is.na(cly0$week)& cly0$year==2008,]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")

hist(cpea0[cpea0$type!="V" & !is.na(cpea0$week),]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
  hist(cpea0[cpea0$type!="V" & !is.na(cpea0$week)& cpea0$year==2007,]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
  hist(cpea0[cpea0$type!="V" & !is.na(cpea0$week)& cpea0$year==2008,]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")

hist(cirad0[cirad0$type!="V" & !is.na(cirad0$week),]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
  hist(cirad0[cirad0$type!="V" & !is.na(cirad0$week)& cirad0$year==2007,]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
  hist(cirad0[cirad0$type!="V" & !is.na(cirad0$week)& cirad0$year==2008,]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
  hist(cirad0[cirad0$type!="V" & !is.na(cirad0$week)& cirad0$year==2009,]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")

# On ne garde que cpea et cirad, on regroupe les années
tous_vergers=rbind(cpea0[,c(1:5,11:22)],cirad0[,c(1:5,15:26)])

hist(tous_vergers[tous_vergers$type!="V" & !is.na(tous_vergers$week),]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
  hist(tous_vergers[tous_vergers$type!="V" & !is.na(tous_vergers$week)& tous_vergers$year==2007,]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
  hist(tous_vergers[tous_vergers$type!="V" & !is.na(tous_vergers$week)& tous_vergers$year==2008,]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")
  hist(tous_vergers[tous_vergers$type!="V" & !is.na(tous_vergers$week)& tous_vergers$year==2009,]$week,breaks=seq(0,52),main="",xlab="semaine",ylab="Nombre de débourrement")

# On essaye d'ajuster une loi de Poisson sur la distribution
#x=0:52
#semaine_debourrement=tous_vergers[tous_vergers$type!="V" & !is.na(tous_vergers$week),]$week
#hist(semaine_debourrement,breaks=seq(0,52),freq=F,main="",xlab="semaine",ylab="Nombre de débourrement")
#points(dpois(x,mean(semaine_debourrement)))

# On essaye d'ajuster une loi binomiale sur la distribution
semaine_debourrement=tous_vergers[tous_vergers$type!="V" & !is.na(tous_vergers$week),]$week
hist(semaine_debourrement,breaks=seq(0,52),freq=F,main="",xlab="semaine",ylab="Nombre de débourrement")
curve(dnorm(x,mean(semaine_debourrement),sd(semaine_debourrement)),type='l',col=2,add=T)

rnorm(10,mean(semaine_debourrement),sd(semaine_debourrement))

############### Comparaison des découpages semaine / mois

pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/burst_date/week_month.pdf",width=10)
##### Cirad
annee=sort(unique(cirad0$year[!is.na(cirad0$year)]))
for (i in 1:length(annee)){
	par(mfrow=c(2,5))
	data=cirad0[cirad0$year==annee[i],]
	arbre_nature(data,2);arbre_nature(data,14);arbre_nature(data,27);arbre_nature(data,36);arbre_nature(data,47);
	arbre_nature_m(data,2);arbre_nature_m(data,14);arbre_nature_m(data,27);arbre_nature_m(data,36);arbre_nature_m(data,47);	
	title(paste("cirad, ",annee[i]),outer=T,line=-1)
}

#### Cpea
annee=sort(unique(cpea0$year[!is.na(cpea0$year)]))
for (i in 1:length(annee)){
	par(mfrow=c(2,4))
	data=cpea0[cpea0$year==annee[i],]
	arbre_nature(data,13);arbre_nature(data,23);arbre_nature(data,4);arbre_nature(data,37);
	arbre_nature_m(data,13);arbre_nature_m(data,23);arbre_nature_m(data,4);arbre_nature_m(data,37);	
	title(paste("cpea, ",annee[i]),outer=T,line=-1)
}

#### Cly
annee=sort(unique(cly0$year[!is.na(cly0$year) ]))
for (i in 1:length(annee)){
	par(mfrow=c(2,5))
	data=cly0[cly0$year==annee[i],]
	arbre_nature(data,7);arbre_nature(data,20);arbre_nature(data,3);arbre_nature(data,27);arbre_nature(data,16);
	arbre_nature_m(data,7);arbre_nature_m(data,20);arbre_nature_m(data,3);arbre_nature_m(data,27);arbre_nature_m(data,16);	
	title(paste("ly, ",annee[i]),outer=T,line=-1)
}

dev.off()



############## Comparaison par année
pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/burst_date/year.pdf",width=10)
##### Cirad
annee=sort(unique(cirad0$year[!is.na(cirad0$year)]))
par(mfrow=c(length(annee),5))
for (i in 1:length(annee)){
	data=cirad0[cirad0$year==annee[i],]
	arbre_nature(data,2);arbre_nature(data,14);arbre_nature(data,27);arbre_nature(data,36);arbre_nature(data,47);	
}
title("cirad, 2007 2008 et 2009",outer=T,line=-1)

#### Cpea
annee=sort(unique(cpea0$year[!is.na(cpea0$year)]))
par(mfrow=c(length(annee),4))
for (i in 1:length(annee)){
	data=cpea0[cpea0$year==annee[i],]
	arbre_nature(data,13);arbre_nature(data,23);arbre_nature(data,4);arbre_nature(data,37);
}
title("cpea, 2007 et 2008",outer=T,line=-1)

#### Cly
annee=sort(unique(cly0$year[!is.na(cly0$year) ]))
par(mfrow=c(length(annee),5))
for (i in 1:length(annee)){
	data=cly0[cly0$year==annee[i],]
	arbre_nature(data,7);arbre_nature(data,20);arbre_nature(data,3);arbre_nature(data,27);arbre_nature(data,16);	
}
title("ly, 2007 et 2008",outer=T,line=-1)
dev.off()


# On teste l'influence de la nature de l'UC
#par(mfrow=c(2,3))
#arbre_nature(cly0,7);arbre_nature(cly0,3);arbre_nature(cly0,20);arbre_nature(cly0,27);arbre_nature(cly0,16);
#ly_F=cly0[cly0$type=="F",]
#ly_V=cly0[cly0$type=="V",]



# On teste l'influence de la position de l'UC
#par(mfrow=c(2,3))
#arbre_posit(ly_F,7);arbre_posit(ly_F,3);arbre_posit(ly_F,20);arbre_posit(ly_F,27);arbre_posit(ly_F,16);
#par(mfrow=c(2,3))
#arbre_posit(ly_V,7);arbre_posit(ly_V,3);arbre_posit(ly_V,20);arbre_posit(ly_V,27);arbre_posit(ly_V,16);


#data=cirad0

################################################################
# On met toutes les années sur le même Histogramme les unes à la suite des autres
#####
arbre_nature_year=function(data,numarbre){
	data=data[data$arbre==numarbre ,]
	annee=sort(unique(data$year[!is.na(data$year) ]))
	week=c();type=c()
	for (i in 1:length(annee)){week=c(week,data[data$year==annee[i],]$week+(i-1)*52);type=c(type,data[data$year==annee[i],]$type)}
	hist(week,breaks=seq(0,156),main=paste(data$parcelle[1],numarbre),xlab="semaine",ylab="Nombre de débourrement")#,ylim=c(0,210))
	hist(week[type==4],breaks=seq(0,156),add=T,col=3)}

arbre_nature_year_m=function(data,numarbre){
	data=data[data$arbre==numarbre ,]
	annee=sort(unique(data$year[!is.na(data$year) ]))
	month=c();type=c()
	for (i in 1:length(annee)){month=c(month,data[data$year==annee[i],]$month+(i-1)*12);type=c(type,data[data$year==annee[i],]$type)}
	hist(month,breaks=seq(0,36),main=paste(data$parcelle[1],numarbre),xlab="mois",ylab="Nombre de débourrement")#,ylim=c(0,350))
	hist(month[type==4],breaks=seq(0,36),add=T,col=3)}


pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/burst_date/bdate_week.pdf",width=10)

par(mfrow=c(2,1))
arbre_nature_year(cirad0,2);arbre_nature_year_m(cirad0,2);
arbre_nature_year(cirad0,14);arbre_nature_year_m(cirad0,14);
arbre_nature_year(cirad0,27);arbre_nature_year_m(cirad0,27);
arbre_nature_year(cirad0,36);arbre_nature_year_m(cirad0,36);
arbre_nature_year(cirad0,47);arbre_nature_year_m(cirad0,47);	

arbre_nature_year(cly0,7);arbre_nature_year_m(cly0,7);
arbre_nature_year(cly0,20);arbre_nature_year_m(cly0,20);
arbre_nature_year(cly0,3);arbre_nature_year_m(cly0,3);
arbre_nature_year(cly0,27);arbre_nature_year_m(cly0,27);
arbre_nature_year(cly0,16);arbre_nature_year_m(cly0,16);	

#arbre_nature_year(cpea0,13);arbre_nature_year_m(cpea0,13);
#arbre_nature_year(cpea0,23);arbre_nature_year_m(cpea0,23);
#arbre_nature_year(cpea0,4);arbre_nature_year_m(cpea0,4);
#arbre_nature_year(cpea0,37);arbre_nature_year_m(cpea0,37);	

dev.off()





################################################################
# Toutes les années sur le même Histogramme
# On cherche à regrouper les différents mois par 2 ou 3
#####

arbre_nature_year_tri=function(data,numarbre){
	data=data[data$arbre==numarbre ,]
	annee=sort(unique(data$year[!is.na(data$year) ]))
	month=c()
	for (i in 1:length(annee)){month=c(month,data[data$year==annee[i],]$month+(i-1)*12)}
	hist(month,breaks=seq(0,3*12,3),main=paste(data$parcelle[1],numarbre),xlab="3 mois",ylab="Nombre de débourrement")#,ylim=c(0,350))
	hist(month[data$type=="V"],breaks=seq(0,3*12,3),add=T,col=3)}

arbre_nature_year_bi=function(data,numarbre){
	data=data[data$arbre==numarbre ,]
	annee=sort(unique(data$year[!is.na(data$year) ]))
	month=c()
	for (i in 1:length(annee)){month=c(month,data[data$year==annee[i],]$month+(i-1)*12)}
	hist(month,breaks=seq(0,3*12,2),main=paste(data$parcelle[1],numarbre),xlab="2 mois",ylab="Nombre de débourrement")#,ylim=c(0,350))
	hist(month[data$type=="V"],breaks=seq(0,3*12,2),add=T,col=3)}



pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/burst_date/bdate.pdf",width=10)

par(mfrow=c(3,1))
arbre_nature_year(cirad0,2);arbre_nature_year_bi(cirad0,2);arbre_nature_year_tri(cirad0,2);
arbre_nature_year(cirad0,14);arbre_nature_year_bi(cirad0,14);arbre_nature_year_tri(cirad0,14);
arbre_nature_year(cirad0,27);arbre_nature_year_bi(cirad0,27);arbre_nature_year_tri(cirad0,27);
arbre_nature_year(cirad0,36);arbre_nature_year_bi(cirad0,36);arbre_nature_year_tri(cirad0,36);
arbre_nature_year(cirad0,47);arbre_nature_year_bi(cirad0,47);arbre_nature_year_tri(cirad0,47);	

arbre_nature_year(cly0,7);arbre_nature_year_bi(cly0,7);arbre_nature_year_tri(cly0,7);
arbre_nature_year(cly0,20);arbre_nature_year_bi(cly0,20);arbre_nature_year_tri(cly0,20);
arbre_nature_year(cly0,3);arbre_nature_year_bi(cly0,3);arbre_nature_year_tri(cly0,3);
arbre_nature_year(cly0,27);arbre_nature_year_bi(cly0,27);arbre_nature_year_tri(cly0,27);
arbre_nature_year(cly0,16);arbre_nature_year_bi(cly0,16);arbre_nature_year_tri(cly0,16);	

#arbre_nature_year(cpea0,13);arbre_nature_year_m(cpea0,13);
#arbre_nature_year(cpea0,23);arbre_nature_year_m(cpea0,23);
#arbre_nature_year(cpea0,4);arbre_nature_year_m(cpea0,4);
#arbre_nature_year(cpea0,37);arbre_nature_year_m(cpea0,37);	

dev.off()



## On ne s'intéresse qu'au verger Cirad (CPEA valeurs étranges, CLY pas de végétatif)
ciradV=cirad0[cirad0$type=="V" & !is.na(cirad0$week),]
ciradF=cirad0[cirad0$type=="F" & !is.na(cirad0$week),]

pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/burst_date/bdate_V.pdf",width=10)
par(mfrow=c(2,1))
arbre_nature_year(ciradV,2);arbre_nature_year_m(ciradV,2);
arbre_nature_year(ciradV,14);arbre_nature_year_m(ciradV,14);
arbre_nature_year(ciradV,27);arbre_nature_year_m(ciradV,27);
arbre_nature_year(ciradV,36);arbre_nature_year_m(ciradV,36);
arbre_nature_year(ciradV,47);arbre_nature_year_m(ciradV,47);	
dev.off()


pdf(file="C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth/burst_date/bdate_F.pdf",width=10)
par(mfrow=c(2,1))
arbre_nature_year(ciradF,2);arbre_nature_year_m(ciradF,2);
arbre_nature_year(ciradF,14);arbre_nature_year_m(ciradF,14);
arbre_nature_year(ciradF,27);arbre_nature_year_m(ciradF,27);
arbre_nature_year(ciradF,36);arbre_nature_year_m(ciradF,36);
arbre_nature_year(ciradF,47);arbre_nature_year_m(ciradF,47);	
dev.off()





############ On cherce à prédire la distribution des semaines en fonction
############ de celle des mois

cir2007V=ciradV[ciradV$year==2007,]
cir2008V=ciradV[ciradV$year==2008,]
cir2009V=ciradV[ciradV$year==2009,]

cir2007F=ciradF[ciradF$year==2007,]
cir2008F=ciradF[ciradF$year==2008,]
cir2009F=ciradF[ciradF$year==2009,]

hist(cir2007F$week,freq=F)
points( dbinom( 0:45,length(cir2007F$week),mean(cir2007F$week,na.rm=T)/length(cir2007F$week) ) ,col=2)

hist(cir2007V$week,freq=F)
points( dbinom( 0:65,length(cir2007V$week[cir2007V$week>35]),mean(cir2007V$week[cir2007V$week>35],na.rm=T)/length(cir2007V$week[cir2007V$week>35]) ) ,col=2)
points( dbinom( 0:65,length(cir2007V$week[cir2007V$week<35]),mean(cir2007V$week[cir2007V$week<35],na.rm=T)/length(cir2007V$week[cir2007V$week<35]) ) ,col=2)

hist(cir2008V$week,freq=F)
points( dbinom( 0:65,length(cir2008V$week[cir2008V$week>25]),mean(cir2008V$week[cir2008V$week>25],na.rm=T)/length(cir2008V$week[cir2008V$week>25]) ) ,col=2)
points( dbinom( 0:65,length(cir2008V$week[cir2008V$week<25]),mean(cir2008V$week[cir2008V$week<25],na.rm=T)/length(cir2008V$week[cir2008V$week<25]) ) ,col=2)

hist(cir2009V$week,freq=F)
points( dbinom( 0:65,length(cir2009V$week[cir2009V$week<25]),mean(cir2009V$week[cir2009V$week<25],na.rm=T)/length(cir2009V$week[cir2009V$week<25]) ) ,col=2)
points( dbinom( 0:65,length(cir2009V$week[cir2009V$week>25]),mean(cir2009V$week[cir2009V$week>25],na.rm=T)/length(cir2009V$week[cir2009V$week>25]) ) ,col=2)




# Distribution sur les dates d'apparition des inflorescences









