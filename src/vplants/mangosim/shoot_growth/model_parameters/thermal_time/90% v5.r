################################################################################
###############Chargement des fichiers de température###########################
################################################################################

#On charge les fichiers de température:
path= "D:/Mes Donnees/These/Données croissance-temperature/Meteo/VergerBassinPlat/VergerBassinPlat.txt"
TBP<- read.table(path, sep="\t",dec=".", h=T)
TBP[1:10,]
date<-vector()
date<-TBP$date
dates<-vector()
dates= strptime(date,"%d/%m/%y")   #On transforme en date comprise par R
TBP<-data.frame(TBP,dates)
TBP[1:10,]
#On utilise les températures moyennes par jour:
datesUnik<-unique(TBP$dates)
length(datesUnik)
datesUnik[1:10]
#Calcul du premier type de moyenne:
moyBPheure<-tapply(TBP$temp,TBP$dates,mean)
length(moyBPheure)
moyBPheure[1:10]
#Calcul du deuxième type de moyenne:
compt=0
moyBP<-NA
for ( i in datesUnik) {
compt=compt+1
moyBP[compt]<-((min(TBP$temp[TBP$dates==i])+max(TBP$temp[TBP$dates==i]))/2) }
length(moyBP)
moyBP[1:10]
TBPmoy<-data.frame(datesUnik,moyBPheure,moyBP)
colnames(TBPmoy)<-c("datesUnik","tempMoyheure", "tempMoy")
TBPmoy[1:10,]



path= "D:/Mes Donnees/These/Données croissance-temperature/Meteo/VergerBassinMartin/VergerBassinMartin.txt"
TBM<- read.table(path, sep="\t",dec=".", h=T)
TBM[1:10,]
date<-vector()
date<-TBM$date
dates<-vector()
dates= strptime(date,"%d/%m/%y")   #On transforme en date comprise par R. On ne tient pas compte des heures
TBM<-data.frame(TBM,dates)
TBM[1:10,]
#On utilise les températures moyennes par jour: 
datesUnik<-unique(TBM$dates)
length(datesUnik)
datesUnik[1:10]
#On a 2 façon de calculer la moyenne : 
	#Par moyennes de toutes les températures à chaque quart d'heure pendant la journée
	#Par min+max/2 : On préviligiera l'utilisation de cette moyenne
#Calcul du premier type de moyenne:
moyBMheure<-tapply(TBM$temp,TBM$dates,mean)
length(moyBMheure)
moyBMheure[1:10]
#Calcul du deuxième type de moyenne:
compt=0
moyBM<-NA
for ( i in datesUnik) {
compt=compt+1
moyBM[compt]<-((min(TBM$temp[TBM$dates==i])+max(TBM$temp[TBM$dates==i]))/2) }
length(moyBM)
moyBM[1:10]
TBMmoy<-data.frame(datesUnik,moyBMheure,moyBM)
colnames(TBMmoy)<-c("datesUnik","tempMoyheure", "tempMoy")
TBMmoy[1:10,]
#Estimation des températures manquantes en 2009 à partir de Bassin Plat
MATCH<-match(TBMmoy$datesUnik,TBPmoy$datesUnik)
plot(TBMmoy$tempMoy,TBPmoy$tempMoy[MATCH])
summary(lm(TBMmoy$tempMoy~TBPmoy$tempMoy[MATCH]))
tempesti<-TBPmoy$tempMoy*0.98316-0.71727 
tabesti<-data.frame(TBPmoy$datesUnik,NA,tempesti)
colnames(tabesti)<-c("datesUnik","tempMoyheure","tempMoy")
tabesti<-tabesti[which(as.character(tabesti$datesUnik)=="2009-01-01"):which(as.character(tabesti$datesUnik)=="2010-01-29"),]
TBMmoy2<-rbind(tabesti,TBMmoy)


path= "D:/Mes Donnees/These/Données croissance-temperature/Meteo/VergerGrandFond/VergerGrandFond.txt"
TGF<- read.table(path, sep="\t",dec=".", h=T)
TGF[1:10,]
date<-vector()
date<-TGF$date
dates<-vector()
dates= strptime(date,"%d/%m/%y")   #On transforme en date comprise par R
TGF<-data.frame(TGF,dates)
TGF[1:10,]
#On utilise les températures moyennes par jour: 
datesUnik<-unique(TGF$dates)
length(datesUnik)
datesUnik[1:10]
#Calcul du premier type de moyenne:
moyGFheure<-tapply(TGF$temp,TGF$dates,mean)
length(moyGFheure)
moyGFheure[1:10]
#Calcul du deuxième type de moyenne:
compt=0
moyGF<-NA
for ( i in datesUnik) {
compt=compt+1
moyGF[compt]<-((min(TGF$temp[TGF$dates==i])+max(TGF$temp[TGF$dates==i]))/2) }
length(moyGF)
moyGF[1:10]
TGFmoy<-data.frame(datesUnik,moyGFheure,moyGF)
colnames(TGFmoy)<-c("datesUnik","tempMoyheure", "tempMoy")
TGFmoy[1:10,]
#Estimation des températures manquantes en 2009 à partir de Bassin Plat
MATCH<-match(TGFmoy$datesUnik,TBPmoy$datesUnik)
plot(TGFmoy$tempMoy,TBPmoy$tempMoy[MATCH])
summary(lm(TGFmoy$tempMoy~TBPmoy$tempMoy[MATCH]))
tempesti<-TBPmoy$tempMoy*0.79417  +7.05243 
tabesti<-data.frame(TBPmoy$datesUnik,NA,tempesti)
colnames(tabesti)<-c("datesUnik","tempMoyheure","tempMoy")
tabesti<-tabesti[which(as.character(tabesti$datesUnik)=="2009-01-01"):which(as.character(tabesti$datesUnik)=="2010-01-26"),]
TGFmoy2<-rbind(tabesti,TGFmoy)


path= "D:/Mes Donnees/These/Données croissance-temperature/Meteo/VergerSaintGillesHauts/VergerSaintGillesHauts.txt"
TGH<- read.table(path, sep="\t",dec=".", h=T)
TGH[1:10,]
date<-vector()
date<-TGH$date
dates<-vector()
dates= strptime(date,"%d/%m/%y")   #On transforme en date comprise par R
TGH<-data.frame(TGH,dates)
TGH[1:10,]
#On utilise les températures moyennes par jour: 
datesUnik<-unique(TGH$dates)
length(datesUnik)
datesUnik[1:10]
#Calcul du premier type de moyenne:
moyGHheure<-tapply(TGH$temp,TGH$dates,mean)
length(moyGHheure)
moyGHheure[1:10]
#Calcul du deuxième type de moyenne:
compt=0
moyGH<-NA
for ( i in datesUnik) {
compt=compt+1
moyGH[compt]<-((min(TGH$temp[TGH$dates==i])+max(TGH$temp[TGH$dates==i]))/2) }
length(moyGH)
moyGH[1:10]
TGHmoy<-data.frame(datesUnik,moyGHheure,moyGH)
colnames(TGHmoy)<-c("datesUnik","tempMoyheure", "tempMoy")
TGHmoy[1:10,]
#Estimation des températures manquantes en 2009 à partir de Bassin Plat
MATCH<-match(TGHmoy$datesUnik,TBPmoy$datesUnik)
plot(TGHmoy$tempMoy,TBPmoy$tempMoy[MATCH])
summary(lm(TGHmoy$tempMoy~TBPmoy$tempMoy[MATCH]))
tempesti<-TBPmoy$tempMoy* 0.81662  + 2.75823 
tabesti<-data.frame(TBPmoy$datesUnik,NA,tempesti)
colnames(tabesti)<-c("datesUnik","tempMoyheure","tempMoy")
tabesti<-tabesti[which(as.character(tabesti$datesUnik)=="2009-01-01"):which(as.character(tabesti$datesUnik)=="2010-01-26"),]
TGHmoy2<-rbind(tabesti,TGHmoy)



#Plot rapide pour vérifier la cohérence des températures
plot(TBMmoy$datesUnik, TBMmoy$tempMoy,col="green", ylim=c(0,40), type="l" , xlab="dates", ylab="températures (°C)", main="Evolution des températures sur les 4 vergers")
points(TGFmoy$datesUnik, TGFmoy$tempMoy ,col="red", type="l")
points(TGHmoy$datesUnik, TGHmoy$tempMoy ,col="blue", type="l")
points(TBPmoy$datesUnik, TBPmoy$tempMoy ,col="orange", type="l")
legend("topleft", c("BM","GF","GH","BP"),col=c("green","red","blue","orange"),pch=1) 

#Moy par semaine pour chaque verger
TBMmoy$mois<-as.numeric(strftime(TBMmoy$datesUnik,"%m"))
TBMmoy$jour<-as.numeric(strftime(TBMmoy$datesUnik,"%d"))
TBMmoy$annee<-as.numeric(strftime(TBMmoy$datesUnik,"%y"))
compt=0
moySemBM<-NA
for (i in unique(TBMmoy$mois))
{
compt=compt+1
moySemBM[compt]<-mean(TBMmoy$tempMoy[TBMmoy$annee==10 & TBMmoy$mois==i & TBMmoy$jour<8])
compt=compt+1
moySemBM[compt]<-mean(TBMmoy$tempMoy[TBMmoy$annee==10 &TBMmoy$mois==i & TBMmoy$jour>7 & TBMmoy$jour<15])
compt=compt+1
moySemBM[compt]<-mean(TBMmoy$tempMoy[TBMmoy$annee==10 &TBMmoy$mois==i & TBMmoy$jour>14& TBMmoy$jour<22])
compt=compt+1
moySemBM[compt]<-mean(TBMmoy$tempMoy[TBMmoy$annee==10 &TBMmoy$mois==i & TBMmoy$jour>21])
}

TGFmoy$mois<-as.numeric(strftime(TGFmoy$datesUnik,"%m"))
TGFmoy$jour<-as.numeric(strftime(TGFmoy$datesUnik,"%d"))
TGFmoy$annee<-as.numeric(strftime(TGFmoy$datesUnik,"%y"))
compt=0
moySemGF<-NA
for (i in unique(TGFmoy$mois))
{
compt=compt+1
moySemGF[compt]<-mean(TGFmoy$tempMoy[TGFmoy$annee==10 &TGFmoy$mois==i & TGFmoy$jour<8])
compt=compt+1
moySemGF[compt]<-mean(TGFmoy$tempMoy[TGFmoy$annee==10 &TGFmoy$mois==i & TGFmoy$jour>7 & TGFmoy$jour<15])
compt=compt+1
moySemGF[compt]<-mean(TGFmoy$tempMoy[TGFmoy$annee==10 &TGFmoy$mois==i & TGFmoy$jour>14& TGFmoy$jour<22])
compt=compt+1
moySemGF[compt]<-mean(TGFmoy$tempMoy[TGFmoy$annee==10 &TGFmoy$mois==i & TGFmoy$jour>21])
}

TGHmoy$mois<-as.numeric(strftime(TGHmoy$datesUnik,"%m"))
TGHmoy$jour<-as.numeric(strftime(TGHmoy$datesUnik,"%d"))
TGHmoy$annee<-as.numeric(strftime(TGHmoy$datesUnik,"%y"))
compt=0
moySemGH<-NA
for (i in unique(TGHmoy$mois))
{
compt=compt+1
moySemGH[compt]<-mean(TGHmoy$tempMoy[TGHmoy$annee==10 &TGHmoy$mois==i & TGHmoy$jour<8])
compt=compt+1
moySemGH[compt]<-mean(TGHmoy$tempMoy[TGHmoy$annee==10 &TGHmoy$mois==i & TGHmoy$jour>7 & TGHmoy$jour<15])
compt=compt+1
moySemGH[compt]<-mean(TGHmoy$tempMoy[TGHmoy$annee==10 &TGHmoy$mois==i & TGHmoy$jour>14& TGHmoy$jour<22])
compt=compt+1
moySemGH[compt]<-mean(TGHmoy$tempMoy[TGHmoy$annee==10 &TGHmoy$mois==i & TGHmoy$jour>21])
}

TBPmoy$mois<-as.numeric(strftime(TBPmoy$datesUnik,"%m"))
TBPmoy$jour<-as.numeric(strftime(TBPmoy$datesUnik,"%d"))
TBPmoy$annee<-as.numeric(strftime(TBPmoy$datesUnik,"%y"))
compt=0
moySemBP<-NA
for (i in unique(TBPmoy$mois))
{
compt=compt+1
moySemBP[compt]<-mean(TBPmoy$tempMoy[TBPmoy$annee==10 &TBPmoy$mois==i & TBPmoy$jour<8])
compt=compt+1
moySemBP[compt]<-mean(TBPmoy$tempMoy[TBPmoy$annee==10 &TBPmoy$mois==i & TBPmoy$jour>7 & TBPmoy$jour<15])
compt=compt+1
moySemBP[compt]<-mean(TBPmoy$tempMoy[TBPmoy$annee==10 &TBPmoy$mois==i & TBPmoy$jour>14& TBPmoy$jour<22])
compt=compt+1
moySemBP[compt]<-mean(TBPmoy$tempMoy[TBPmoy$annee==10 &TBPmoy$mois==i & TBPmoy$jour>21])
}



#########################################################################################################
#####################FIGURE 1
#########################################################################################################
layout(matrix(c(1,2)),heights=c(4,0.5))
par(oma=c(1,1,1,1),mar=c(4,4,0,0))
plot(moySemBM[1:46],lty="solid", lwd=1.5,ylim=c(15,35),cex.lab=1.5,cex.axis=1.5 ,xlim=c(1,48),type="l" ,bty="n", xlab="Months", ylab="Mean temperature (°C)",xaxt="n")
#Chaque tick: le 1er du mois
axis(1,cex.axis=1.5, at=c(1,5,9,13,17,21,25,29,33,37,41,45),
c("1","2","3","4","5","6","7","8","9","10","11","12"))
#axis(1,cex.axis=1.5, at=c(2.5,6.5,10.5,14.5,18.5,22.5,26.5,30.5,34.5,38.5,42.5,46.5),
#c("1","2","3","4","5","6","7","8","9","10","11","12"))
#On définit les mois (on met les numéros) au milieu du mois: entre semaine 2 et 3
points(moySemGF[1:46],lty="dotted", lwd=1.5, type="l")
points(moySemGH[1:46] ,lty="solid", lwd=2, type="l")
points(moySemBP[1:46],lty="dashed", lwd=1.5, type="l")
legend("bottomleft",lty=c("dotted","dashed","solid","solid"),lwd=c(1.5,1.5,1.5,2),c("65 m","150 m","300 m","450 m"),cex=1.5,bty="n")

unique(strftime(BaseDeCroissance$dates[BaseDeCroissance$variete=="cog"],"%m-%d"))
unique(strftime(BaseDeCroissance$dates[BaseDeCroissance$variete=="jose"],"%m-%d"))
segments(x0=4.5,y0=30,x1=16.5,y1=30,lwd=2,lty="solid")
segments(x0=32.5,y0=30,x1=42.5,y1=30,lwd=2,lty="solid")
text(8,31,"Axis & Leaf of GU",cex=1.5,adj=0.35)
text(35,31,"Axis & Leaf of GU",cex=1.5,adj=0.35)

unique(strftime(BaseDeCroissanceInflo$dates[BaseDeCroissanceInflo$variete=="cog"],"%m-%d"))
unique(strftime(BaseDeCroissanceInflo$dates[BaseDeCroissanceInflo$variete=="jose"],"%m-%d"))
segments(x0=24.5,y0=32,x1=42.5,y1=32,lwd=2,lty="dashed")
text(31,33,"Inflorescence axis",cex=1.5,adj=0.35)

par(mar=c(0,4,0,0))
plot(cbind(c(0,40),c(0,5)) ,axes=FALSE,type="n",ylab="",xlab="")
text( 20, 2.5, "Figure 1",,cex=1.5)

min(baseAJUST$Tm90[baseAJUST$variete=="cog"],na.rm=TRUE)
max(baseAJUST$Tm90[baseAJUST$variete=="cog"],na.rm=TRUE)
min(baseAJUST$Tm90[baseAJUST$variete=="jose"],na.rm=TRUE)
max(baseAJUST$Tm90[baseAJUST$variete=="jose"],na.rm=TRUE)
min(baseAJUSTinflo$Tm90[baseAJUSTinflo$variete=="cog"],na.rm=T)
max(baseAJUSTinflo$Tm90[baseAJUSTinflo$variete=="cog"],na.rm=T)
min(baseAJUSTinflo$Tm90[baseAJUSTinflo$variete=="jose"],na.rm=T)
max(baseAJUSTinflo$Tm90[baseAJUSTinflo$variete=="jose"],na.rm=T)













#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################














#######################################
########## UC
#######################################

#AJUSTEMENT DES COURBES SIGMOIDALES DE CROISSANCE:
#On charge la base de croissance:
source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/calcul_des_surfaces_foliaires.R")

###ON ENLEVE LES LIGNES OU ON A 2 DATES IDENTIQUES SINON PB CAR MM DAB
vec<-NULL
base<-split(BaseDeCroissance,BaseDeCroissance$codeUC)
BaseDeCroissance2<-NULL

for (i in 1:length(base)){
ssbase<-base[[i]]

vec<-NULL
for (j in 2:nrow(ssbase)){
if(strftime(ssbase$dates[j],"%Y-%m-%d")==strftime(ssbase$dates[j-1],"%Y-%m-%d"))
{vec=c(vec,j)}}

ssbase2<-NULL
ifelse(length(vec)!=0,ssbase2<-ssbase[-vec,],ssbase2<-ssbase)
BaseDeCroissance2<-rbind(BaseDeCroissance2,ssbase2)
}
BaseDeCroissance<-BaseDeCroissance2

BaseDeCroissance$codeUC <- as.character(BaseDeCroissance$codeUC)
BaseDeCroissance$codeUC <- as.factor(BaseDeCroissance$codeUC)

############################
## calcul des nb jours apres débourrement
############################

#On défini la date de débourrement de chaque UC qu'on copie à chaque ligne:
vec2<-NULL
base<-split(BaseDeCroissance,BaseDeCroissance$codeUC)
for (i in 1:length(base)){
ssbase<-base[[i]]
dateDeb<-as.character(ssbase$date[ssbase$DFUC=="D"])
vec<-rep(dateDeb,dim(ssbase)[1])
vec2<-c(vec2,vec)
}
BaseDeCroissance<-cbind(BaseDeCroissance,vec2)

#Pour chaque jour on fait la différence avec la date de débourrement : nb de jours depuis débourrement.
BaseDeCroissance$DAB<-difftime(strptime(BaseDeCroissance$date,"%d/%m/%y"),strptime(BaseDeCroissance$vec2,"%d/%m/%y"),
units="days")
#Les 0 correspondent aux jours mm : gain reçu pendant la journée de débourrement.
#On commence les gains à DAB = 0
#les - sont pour les jours avant débourrement


BaseDeCroissance[1:10,]
a<-unique(codeUC)


###############################################################################
#EQUATION D'AJUSTEMENT DE LA COURBE DE CROISSANCE:
#estimation des paramètres de l'équation
# A : y max : valeur du plateau final
#X0 : date du point d'inflection : 50% de la croissance
#B : durée ou la vitesse est maximale


#COeff pour les UC
A0<-vector()
X00<-vector()
B0<-vector()
#Vitesse maximale: ER
ER0<-vector()

#Vecteur d'erreur    #Ajoute 1 quand il y a des pb d'ajustement.
Error0<-vector()
compt=0

#On commence la boucle
for ( i in a) {
compt=compt+1
#On défini x et y
x=as.numeric(as.character(BaseDeCroissance$DAB[BaseDeCroissance$codeUC==i]))
y0=BaseDeCroissance$longueurUC[BaseDeCroissance$codeUC==i]

#Fit pour les UC : estimation des paramètres
fit0<-NULL
try(fit0<-nls(y0 ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = 18, B = 1.5, X0 =7.5)),silent=T)
                               #start : valeurs initiales où commencer puis il cherche des valeurs aux alentours pour ajuster
                                     #Attention il ne fait pas les NA : enlève ces lignes
try(A0[compt]<-summary(fit0)$coef[1],silent=T)
try(B0[compt]<-summary(fit0)$coef[2],silent=T)
try(X00[compt]<-summary(fit0)$coef[3],silent=T)
#calcul de la vitesse max : ER
try(ER0[compt] <- 0.25 * A0[compt] / B0[compt], silent = T) 
}

baseAJUST<-data.frame(a,A0,X00,B0,ER0)


####### Determination de dimension 90%
baseAJUST$dim90<- baseAJUST$A0*90/100

####### Determination de nb jours pour arriver à dimension 90%
baseAJUST$ndim90<- -baseAJUST$B0*log((baseAJUST$A0/baseAJUST$dim90)-1)+baseAJUST$X00 #nb de jours avec jour de deb=jour 0
baseAJUST$ndim90<-round(baseAJUST$ndim90,digits = 0)
head(baseAJUST)

###### On rajoute variete dans base ajust
#Ajout de la position de l'UC mère
a<-baseAJUST$a
variete<-NA
compt=0
for ( i in a) {
compt=compt+1
variete[compt]<-as.character(unique(BaseDeCroissance$variete[BaseDeCroissance$codeUC==i]))   }
baseAJUST$variete<-variete

#### On récupère le nb jour total
compt=0
nUC<-NA
for(i in unique(BaseDeCroissance$codeUC)) 
{
compt=compt+1
try(nUC[compt]<-BaseDeCroissance$DAB[BaseDeCroissance$codeUC==i & BaseDeCroissance$DFUC=="F"],silent=T)
}
baseAJUST$nUC<-nUC

plot(baseAJUST$nUC,baseAJUST$ndim90,col=c("blue","red")[as.factor(baseAJUST$variete)],xlim=c(0,25),pch=16,ylim=c(0,25),bty="n",xlab="nb jours croissance",ylab="nb jours 90% croissance", main="Axe")
abline(a=0,b=1)
legend("topleft",c("Cogshall","José"),pch=16,col=c("blue","red"))


####### Determination de la température pour aller juska n90
a<-baseAJUST$a

Tm90<-NA
compt=0
for (i in a){
compt=compt+1
DATA<-NA
DATA<-BaseDeCroissance[BaseDeCroissance$codeUC==i,]

dataT<-NULL                           #On prend que le 1er car répétitions du mm verger!
if(DATA$verger[1]=="BM") {dataT<-TBMmoy}     #On choisi le fichier de température qu'il faut selon les vergers
if(DATA$verger[1]=="BP") {dataT<-TBPmoy}
if(DATA$verger[1]=="GH") {dataT<-TGHmoy}
if(DATA$verger[1]=="GF") {dataT<-TGFmoy}

D<-(DATA$date[DATA$DFUC=="D"])
D<-strptime(D,"%d/%m/%y")     
                                                             
F90<-D+(baseAJUST$ndim90[baseAJUST$a==i]*86400)

ifelse(is.na(F90)==TRUE, Tm90[compt]<-NA,Tm90[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==D):which(dataT$datesUnik==(F90-86400))]))
}

baseAJUST$Tm90<-Tm90

####### Determination de la température pour aller juska n90 2 mois avant
a<-baseAJUST$a

TmAVANT<-NA
compt=0
for (i in a){
compt=compt+1
DATA<-NA
DATA<-BaseDeCroissance[BaseDeCroissance$codeUC==i,]

dataT<-NULL                           #On prend que le 1er car répétitions du mm verger!
if(DATA$verger[1]=="BM") {dataT<-TBMmoy2}     #On choisi le fichier de température qu'il faut selon les vergers
if(DATA$verger[1]=="BP") {dataT<-TBPmoy}
if(DATA$verger[1]=="GH") {dataT<-TGHmoy2}
if(DATA$verger[1]=="GF") {dataT<-TGFmoy2}

D<-(DATA$date[DATA$DFUC=="D"])
D<-strptime(D,"%d/%m/%y")
D<-D-(86400*60 ) #On enlève 2 mois environ 
                                                             
F90<-D+(baseAJUST$ndim90[baseAJUST$a==i]*86400)
F90<-F90-(86400*60 )

ifelse(is.na(F90)==TRUE, TmAVANT[compt]<-NA,TmAVANT[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==D):which(dataT$datesUnik==(F90-86400))]))
}

baseAJUST$TmAVANT<-TmAVANT

################################
##### Calcul du RGR au  niveau du point d'inflexion à partir de la courbe simulée.

RGR50<-NA
ndim50<-NA

compt<-0
for(i in baseAJUST$a) 
{
	compt=compt+1
	
	A<-baseAJUST$A0[baseAJUST$a==i]
	X0<-baseAJUST$X00[baseAJUST$a==i]
	B<-baseAJUST$B0[baseAJUST$a==i]
	
	dim50<-A*50/100
	n50<- -B*log((A/dim50)-1)+X0
	navant<-n50-0.5 #On prend 1/2 jour avant et 1/2 jour apres: pente sur 1 jour
	napres<-n50+0.5
	dimavant<-A / ( 1 + exp (-((navant - X0) / B)))
	dimapres<-A / ( 1 + exp (-((napres - X0) / B)))
	
	x<-c(navant,n50,napres)
	y<-c(log(dimavant),log(dim50),log(dimapres))
	
	ndim50[compt]<-n50
	
	ifelse(is.na(y)==TRUE,RGR50[compt]<-NA,RGR50[compt]<-lm(y~x)$coefficients[2])
}

baseAJUST$RGR50sim<-RGR50
baseAJUST$ndim50<-round(ndim50)



####### Determination de la température pour aller juska n50
a<-baseAJUST$a

Tm50<-NA
compt=0
for (i in a){
compt=compt+1
DATA<-NA
DATA<-BaseDeCroissance[BaseDeCroissance$codeUC==i,]

dataT<-NULL                           #On prend que le 1er car répétitions du mm verger!
if(DATA$verger[1]=="BM") {dataT<-TBMmoy}     #On choisi le fichier de température qu'il faut selon les vergers
if(DATA$verger[1]=="BP") {dataT<-TBPmoy}
if(DATA$verger[1]=="GH") {dataT<-TGHmoy}
if(DATA$verger[1]=="GF") {dataT<-TGFmoy}

D<-(DATA$date[DATA$DFUC=="D"])
D<-strptime(D,"%d/%m/%y")     
                                                             
F50<-D+(baseAJUST$ndim50[baseAJUST$a==i]*86400)

ifelse(is.na(F50)==TRUE, Tm50[compt]<-NA,Tm50[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==D):which(dataT$datesUnik==(F50-86400))]))
}

baseAJUST$Tm50<-Tm50

### Calcul RGR à 50% à partir AGRmax
baseAJUST$dim50<- baseAJUST$A0*50/100

baseAJUST$RGRip<-baseAJUST$ER0/baseAJUST$dim50
plot(baseAJUST$RGRip~baseAJUST$RGR50sim)



################################################################################
#########Ajout d'autres variables dans baseAJUST#######################################
################################################################################

#Ajout des vergers
verger<-NULL
compt=0
for ( i in a) {
compt=compt+1
verger[compt]<-as.character(unique(BaseDeCroissance$verger[BaseDeCroissance$codeUC==i]))   }

#Ajout des saisons
sai<-NULL
compt=0
for ( i in a) {
compt=compt+1
sai[compt]<-as.character(unique(BaseDeCroissance$saison[BaseDeCroissance$codeUC==i]))   }

#Ajout des arbres
arbre<-NULL
compt=0
for ( i in a) {
compt=compt+1
arbre[compt]<-as.character(unique(BaseDeCroissance$arbre[BaseDeCroissance$codeUC==i]))   }

#Ajout des position
pos<-NULL
compt=0
for ( i in a) {
compt=compt+1
pos[compt]<-as.character(unique(BaseDeCroissance$positionUC[BaseDeCroissance$codeUC==i]))   }

#Ajout du nb apicale UCmere
nbApUCm<-NULL
compt=0
for ( i in a) {
compt=compt+1
nbApUCm[compt]<-as.character(unique(BaseDeCroissance$nombreApicale[BaseDeCroissance$codeUC==i]))   }

#Ajout du nb latérales UCmere
nbLatUCm<-NULL
compt=0
for ( i in a) {
compt=compt+1
nbLatUCm[compt]<-as.character(unique(BaseDeCroissance$nombreLaterale[BaseDeCroissance$codeUC==i]))   }

#Ajout du niveau de croissance de l'arbre (vigeur)
vigArbre<-NULL
compt=0
for ( i in a) {
compt=compt+1
vigArbre[compt]<-as.character(unique(BaseDeCroissance$croissance[BaseDeCroissance$codeUC==i]))   }

#Ajout de l'orientation de l'UC suivie
or<-NULL
compt=0
for ( i in a) {
compt=compt+1
or[compt]<-as.character(unique(BaseDeCroissance$orientation[BaseDeCroissance$codeUC==i]))   }

#Ajout de la hauteur de l'UC suivie
haut<-NULL
compt=0
for ( i in a) {
compt=compt+1
haut[compt]<-as.character(unique(BaseDeCroissance$hauteur[BaseDeCroissance$codeUC==i]))   }

#Ajout de la position de l'UC mère
posUCm<-NULL
compt=0
for ( i in a) {
compt=compt+1
posUCm[compt]<-as.character(unique(BaseDeCroissance$PUCM[BaseDeCroissance$codeUC==i]))   }

#Ajout du nombre de feuille de l'UC
nbF<-NULL
compt=0
for ( i in a) {
compt=compt+1
nbF[compt]<-as.character(unique(BaseDeCroissance$nombreFeuille[BaseDeCroissance$codeUC==i]))   }

#Ajout de la longueur de l'hypopodium
longHypo<-NULL
compt=0
for ( i in a) {
compt=compt+1
longHypo[compt]<-as.character(unique(BaseDeCroissance$LEPF[BaseDeCroissance$codeUC==i]))   }

baseAJUST<-data.frame(baseAJUST,verger,sai,arbre,pos,nbApUCm,nbLatUCm,vigArbre,or,haut,posUCm,nbF,longHypo)
head(baseAJUST)

#write.table(baseAJUST,"baseAJUST.csv",sep="\t")













################################
### PLOTS UC
################################
#path<-"D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/T,n,d,ER/baseAJUST.csv"
#baseAJUST<-read.csv(path,sep="\t",header=TRUE)

########### Moyennes et ecart type des parametres
jose<-baseAJUST[baseAJUST$variete=="jose",]
cog<-baseAJUST[baseAJUST$variete=="cog",]

mean(jose$dim90,na.rm=TRUE)
sd(jose$dim90,na.rm=TRUE)
mean(cog$dim90,na.rm=TRUE)
sd(cog$dim90,na.rm=TRUE)
t.test(jose$dim90,cog$dim90,paired=FALSE)

mean(jose$ndim90,na.rm=TRUE)
sd(jose$ndim90,na.rm=TRUE)
mean(cog$ndim90,na.rm=TRUE)
sd(cog$ndim90,na.rm=TRUE)
t.test(jose$ndim90,cog$ndim90,paired=FALSE)

mean(jose$ER0,na.rm=TRUE)
sd(jose$ER0,na.rm=TRUE)
mean(cog$ER0,na.rm=TRUE)
sd(cog$ER0,na.rm=TRUE)
t.test(jose$ER0,cog$ER0,paired=FALSE)

mean(jose$RGR50sim,na.rm=TRUE)
sd(jose$RGR50sim,na.rm=TRUE)
mean(cog$RGR50sim,na.rm=TRUE)
sd(cog$RGR50sim,na.rm=TRUE)
t.test(jose$RGR50sim,cog$RGR50sim,paired=FALSE)

########### UC : Tm90 sur nb jours
jose<-baseAJUST[baseAJUST$variete=="jose",]
cog<-baseAJUST[baseAJUST$variete=="cog",]

baseAJUST$variete<-as.factor(baseAJUST$variete)
plot(baseAJUST$ndim90[baseAJUST$variete=="cog"]~baseAJUST$Tm90[baseAJUST$variete=="cog"],xlim=c(0,30),ylim=c(0,35),bty="n",col="blue",pch=20,xlab="Mean temperature 90%",ylab="number of days from burst to 90%")
points(baseAJUST$ndim90[baseAJUST$variete=="jose"]~baseAJUST$Tm90[baseAJUST$variete=="jose"],col="red",pch=20)

options(contrasts=c("contr.sum","contr.poly"))          # contrasts = c("fact non ordonnés" , "facteurs ordonnés")
#Test var par var en premier
el<-lm(ndim90~Tm90,data=cog)
summary(el) #Signif

el<-lm(ndim90~Tm90,data=jose)
summary(el)  #Signif

el<-lm(ndim90~Tm90*variete,data=baseAJUST) #interaction NS 0.03775 *  
anova(el)
summary(el)

el<-lm(ndim90~Tm90+variete,data=baseAJUST)
anova(el)
summary(el) # Elevation significative < 2.2e-16 ***


abline(a= 28.08220  -2.01912 , b=-0.74292  , col="blue")
abline(a=28.08220  +2.01912 , b=-0.74292 , col="red")


legend("bottomleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(18,0,"OLS: same slopes P= 0.03775*, signif elevation P< 2e-16 ***")
text(18,3,"Slope: -0.74292 ")



####### UC n vs long finale 90%
jose<-baseAJUST[baseAJUST$variete=="jose",]
cog<-baseAJUST[baseAJUST$variete=="cog",]

plot(ndim90~dim90,data=jose,main="Axe",xlim=c(0,30),ylim=c(0,35),bty="n",col="red",pch=20,xlab=" 90% Final Dimension",ylab="number of days from burst to 90%")
points(ndim90~dim90,data=cog,col="blue",pch=20)

anova(lm(ndim90~dim90*variete,data=baseAJUST))
#On a des pentes différentes, donc on fait 2 modèles

summary(lm(ndim90~dim90,data=jose))
summary(lm(ndim90~dim90,data=cog))
abline(lm(ndim90~dim90,data=cog)$coef,col="blue")
text(15,35,"José: OLS NS / Cog: OLS : Sign P=3.29e-07 *** slope=0.1995")


####### UC temp vs long finale 90%
plot(dim90~Tm90 ,data=jose,main="Axe",xlim=c(0,30),ylim=c(0,35),bty="n",col="red",pch=20,xlab=" Mean Temperature",ylab="90% final dimension")
points(dim90~Tm90 ,data=cog,col="blue",pch=20)

#Pour chaque variete en premier:
summary(lm(dim90~Tm90,data=cog))
summary(lm(dim90~Tm90,data=jose)) #NS pour José.
#on ne peut pas regarder de pente commune

####### UC temp 2 mois avant vs long finale 90%
plot(dim90~TmAVANT ,data=jose,main="Axe",xlim=c(0,30),ylim=c(0,35),bty="n",col="red",pch=20,xlab=" Mean Temperature",ylab="90% final dimension")
points(dim90~TmAVANT ,data=cog,col="blue",pch=20)

#Pour chaque variete en premier:
summary(lm(dim90~TmAVANT,data=cog))
summary(lm(dim90~TmAVANT,data=jose)) #NS pour José.
#on ne peut pas regarder de pente commune


###### Effet verger? sur dimension
par(mfrow=c(2,2))
plot(Tm90~verger,data=jose,bty="n",main="axe jose",ylab="Temp moy 90%")
text(2,27,"GF ***")
text(1,26,"GH ***")
plot(Tm90~verger,data=cog,bty="n",main="axe cogshall",ylab="Temp moy 90%")
text(2,27,"BM ***")
text(3,28,"BP ***")
text(1,26,"GF ***")
#On ne peut pas différencier verger et température
plot(dim90~verger,data=jose,bty="n",main="axe jose",ylab="90% long finale")
text(2.5,22,"GF ***")
plot(dim90~verger,data=cog,bty="n",main="axe cogshall",ylab="90% long finale")
text(1.5,17,"GF ***")
text(2,3,"BM ***")

anova(lm(dim90~Tm90*verger,data=jose))
summary(lm(dim90~Tm90+verger,data=jose))

anova(lm(dim90~Tm90*verger,data=cog))
summary(lm(dim90~Tm90*verger,data=cog))

jose$verger<-relevel(jose$verger,"GF")
cog$verger<-relevel(cog$verger,"BM")

###### Effet verger? sur 1/nb jours (vitesse de développement)
anova(lm(1/ndim90~Tm90*verger,data=jose))
anova(lm(1/ndim90~Tm90+verger,data=jose))

anova(lm(1/ndim90~Tm90*verger,data=cog))
anova(lm(1/ndim90~Tm90+verger,data=cog))
summary(lm(1/ndim90~Tm90+verger,data=cog))






#Distribution des longueurs finales:
par(mfrow=c(4,2))
hist(cog$dim90[order(cog$dim90,na.last=NA)],main="Cogshall axes",breaks=10,xlab="90% length axe (cm)",col="blue",ylab="number of axes",ylim=c(0,30),xlim=c(0,50))
hist(jose$dim90[order(jose$dim90,na.last=NA)],main="José axes",breaks=10,xlab="90% length axe (cm)",ylab="number of axes",col="red",ylim=c(0,30),xlim=c(0,50))

# Distribution des points d'inflexion
hist(cog$X00[order(cog$X00,na.last=NA)],main="Cogshall axes",breaks=10,xlab="inflexion point (nb jour)",col="blue",ylab="number of axes",ylim=c(0,30),xlim=c(0,20))
hist(jose$X00[order(jose$X00,na.last=NA)],main="José axes",breaks=10,xlab="inflexion point (nb jour)",ylab="number of axes",col="red",ylim=c(0,30),xlim=c(0,20))

# Distribution des durées de vitesse max
hist(cog$B0[order(cog$B0,na.last=NA)],main="Cogshall axes",breaks=10,xlab="duration at max ER (nb jour)",col="blue",ylab="number of axes",ylim=c(0,30),xlim=c(0,5))
hist(jose$B0[order(jose$B0,na.last=NA)],main="José axes",breaks=10,xlab="duration at max ER (nb jour)",ylab="number of axes",col="red",ylim=c(0,30),xlim=c(0,5))

# Distribution des vitesse max
hist(cog$ER0[order(cog$ER0,na.last=NA)],main="Cogshall axes",breaks=10,xlab="max ER (cm/j)",col="blue",ylab="number of axes",ylim=c(0,30),xlim=c(0,10))
hist(jose$ER0[order(jose$ER0,na.last=NA)],main="José axes",breaks=10,xlab="max ER (cm/j)",ylab="number of axes",col="red",ylim=c(0,30),xlim=c(0,10))


####### Max ER
########## Dim 90 sur max ER
jose<-baseAJUST[baseAJUST$variete=="jose",]
cog<-baseAJUST[baseAJUST$variete=="cog",]

plot(ER0~dim90,data=jose,main="Axe",xlim=c(0,50),ylim=c(0,10),bty="n",col="red",pch=20,xlab=" 90% Final Dimension",ylab="Max Expansion Rate")
points(ER0~dim90,data=cog,col="blue",pch=20)

summary(lm(ER0~dim90,data=cog))
summary(lm(ER0~dim90,data=jose))

anova(lm(ER0~dim90*variete,data=baseAJUST))
#Même pente  P=0.01946 *  
summary(lm(ER0~dim90+variete,data=baseAJUST))

abline(a= 0.17187+0.36160  , b= 0.14813  , col="blue")
abline(a=0.17187-0.36160 , b=0.14813 , col="red")


legend("topleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(30,0,"OLS: same slopes P= 0.01946 * , signif elevation P< 2e-16 ***")


########## Temp sur max ER
jose<-baseAJUST[baseAJUST$variete=="jose",]
cog<-baseAJUST[baseAJUST$variete=="cog",]

plot(ER0~Tm50,data=jose,main="Axe",xlim=c(0,30),ylim=c(0,10),bty="n",col="red",pch=20,xlab=" Mean Temp 50%",ylab="Max Expansion Rate")
points(ER0~Tm50,data=cog,col="blue",pch=20)

#Var par var
anova(lm(ER0~Tm50,data=jose)) #NS
anova(lm(ER0~Tm50,data=cog)) #NS

legend("topleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(15,10,"OLS: No significant relationship")


########## n sur max ER
jose<-baseAJUST[baseAJUST$variete=="jose",]
cog<-baseAJUST[baseAJUST$variete=="cog",]

plot(ER0~ndim90,data=jose,main="Axe",xlim=c(0,30),ylim=c(0,10),bty="n",col="red",pch=20,xlab=" nb jours 90%",ylab="Max Expansion Rate")
points(ER0~ndim90,data=cog,col="blue",pch=20)

anova(lm(ER0~ndim90*variete,data=baseAJUST))
#NS


legend("topleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(15,10,"OLS: No significant relationship")

###Scatterplot
library(car)
scatterplotMatrix(~Tm90+ndim90+dim90+ER0+X00+B0|variete,data=baseAJUST,main="Axe",diagonal="none",smooth=FALSE,reg.line=FALSE,by.group=TRUE,pch=c(20,20),col=c("blue","red"),var.labels=c("Mean Temp","nb days","Dimension","Max ER","day of inflexion","days at max ER"),cex.labels=2)


####### Calcul du AGR et RGR OBSERVES
#BaseDeCroissance[1:10,]
#a<-unique(BaseDeCroissance$codeUC)
#length(a)

#AGRUC=vector()    
#RGRUC=vector()  

#####Pour chaque UC:     #OUVERTURE DE LA BOUCLE
#for (j in a) {
#
#compt=0
# 
#agr<-NA
#rgr<-NA
#
#t<-NA
#t<-BaseDeCroissance$DAB[BaseDeCroissance$codeUC==j]
#
#for (i in c(1:(length(t) ))) {
#
#compt=compt+1
#                                                       
#Stok des DAB:
#ifelse((i-1)==0,x0<-NA,x0<-t[i-1])      #Car pour i = 1 : i-1 = 0 : problème, on met NA
#x1<-t[i]
#x2<-t[i+1]        #Met NA automatiquement si i+1 >length(t)
#
#On prend les longueurs correcpondant à chaque date:
#ifelse(is.na(x0),y0<-NA,y0<-BaseDeCroissance$longueurUC[BaseDeCroissance$codeUC==j & BaseDeCroissance$DAB==x0])     #NA si x0 = NA
#y1<-BaseDeCroissance$longueurUC[BaseDeCroissance$codeUC==j & BaseDeCroissance$DAB==x1]
#ifelse(is.na(x2),y2<-NA,y2<-BaseDeCroissance$longueurUC[BaseDeCroissance$codeUC==j & BaseDeCroissance$DAB==x2])     #NA si x2 = NA
#
#La regression est faite entre la longueur et le nombre de jour pour avoir le nombre de cm par jour
#x<-NA
#y<-NA
#x<-c(x0, x1, x2)
#y<-c(y0, y1, y2)
#Y<-c(log(y0), log(y1), log(y2))
#
#On fait une régression linéaire pour obtenir la pente  (estimate)  de chaque point                            
#ifelse(is.na(y[1])|is.na(y[2])|is.na(y[3]),agr[compt]<-NA, agr[compt]<-lm(y~x)$coefficients[2] )   
#ifelse(is.na(Y[1])|is.na(Y[2])|is.na(Y[3]),rgr[compt]<-NA, rgr[compt]<-lm(Y~x)$coefficients[2] )  }
#                                      #On ne calcule la pente que pour les cas ou il y a 3 points     
#AGRUC=c(AGRUC,agr) 
#RGRUC=c(RGRUC,rgr)}   

#BaseDeCroissance<-data.frame(BaseDeCroissance,AGRUC,RGRUC)


########################## Estimation RGR a partir SIMULATION
#Calcul longueur UC simulée:
#longFIT<-NA
#compt<-0
#for(i in baseAJUST$a) 
#{
#	A<-baseAJUST$A0[baseAJUST$a==i]
#	X0<-baseAJUST$X00[baseAJUST$a==i]
#	B<-baseAJUST$B0[baseAJUST$a==i]
#	
#	for(x in BaseDeCroissance$DAB[BaseDeCroissance$codeUC==i])
#	{
#		compt<-compt+1
#		longFIT[compt]<- A / ( 1 + exp (-((x - X0) / B)))
#	}
#}
#
#
# Calcul RGR simulé, sur 2 points car courbe simulée est lisse et sans a coup
#
#a<-unique(codeUC)
#length(a)
#RGRUCsim=vector()    

#####Pour chaque UC:     #OUVERTURE DE LA BOUCLE

#for (i in a)
#{
#	DATA<-BaseDeCroissance[BaseDeCroissance$codeUC==i,]
#	rgr<-NA
#
#	for (j in 2:nrow(DATA)) 
#	{
#		x<-c(DATA$DAB[j-1],DATA$DAB[j])
#		y<-c(log(DATA$longFIT[j-1]),log(DATA$longFIT[j]))
#		ifelse(is.na(y)==TRUE,rgr[j]<-NA,rgr[j]<-lm(y~x)$coefficients[2])
#	}
#	RGRUCsim<-c(RGRUCsim,rgr) 
#}

#BaseDeCroissance<-data.frame(BaseDeCroissance,RGRUCsim)



## Plots:
#maxL<-max(BaseDeCroissance$longueurUC,na.rm=TRUE)
#maxAGR<-max(BaseDeCroissance$AGRUC,na.rm=TRUE)
#maxDAB<-max(BaseDeCroissance$DAB,na.rm=TRUE)
#maxRGR<-max(BaseDeCroissance$RGRUC,na.rm=TRUE)

#pdf("essai.pdf", paper="a4",width=7,height=10)
#par(mfrow=c(2,1))
#par(oma=c(1,1,1,1),mar=c(4,4,2,6))
#a<-unique(BaseDeCroissance$codeUC)

#for (i in a) {

#plot(BaseDeCroissance$longueurUC[BaseDeCroissance$codeUC==i]~BaseDeCroissance$DAB[BaseDeCroissance$codeUC==i],bty="n",col="black",type="l",xlab="Days after burst",ylab="length (cm)",ylim=c(0,maxL),pch=20,xlim=c(0,maxDAB),main=i)
#abline(h=baseAJUST$A0[baseAJUST$a==i]*10/100) #trace droite des 10%
#points(BaseDeCroissance$longFIT[BaseDeCroissance$codeUC==i]~BaseDeCroissance$DAB[BaseDeCroissance$codeUC==i],col="black",type="l",lty="dashed",pch=20)
#par(new=T)
#plot(BaseDeCroissance$AGRUC[BaseDeCroissance$codeUC==i]~BaseDeCroissance$DAB[BaseDeCroissance$codeUC==i],bty="n",col="blue",type="l",pch=20,xaxt="n", yaxt="n",xlab="",ylab="",ylim=c(0,maxAGR),xlim=c(0,maxDAB))
#axis(4)
#mtext("Absolute Growth Rate: d(length)/d(DAB)",side=4,line=2,cex=1,col="blue")
#par(new=T)
#plot(BaseDeCroissance$RGRUC[BaseDeCroissance$codeUC==i]~BaseDeCroissance$DAB[BaseDeCroissance$codeUC==i],bty="n",col="red",type="l",pch=20,xaxt="n", yaxt="n",xlab="",ylab="",ylim=c(0,maxRGR),xlim=c(0,maxDAB))
#points(BaseDeCroissance$ RGRUCsim[BaseDeCroissance$codeUC==i]~BaseDeCroissance$DAB[BaseDeCroissance$codeUC==i],col="red",type="l",lty="dashed",pch=20)
#axis(4,line=3.5)
#mtext("Relative Growth Rate: d(ln(length))/d(DAB)",side=4,line=5.5,cex=1,col="red") }

#dev.off()



##### max AGR obs vs max AGR sim
#maxAGR<-NA
#compt<-0

#for (i in unique(BaseDeCroissance$codeUC))
#{
#	compt<-compt+1
#	maxAGR[compt]<-max(BaseDeCroissance$AGRUC[BaseDeCroissance$codeUC==i],na.rm=TRUE)
#}

#baseAJUST$maxAGR<-maxAGR
#head(baseAJUST)
#plot(baseAJUST$maxAGR~baseAJUST$ER0,xlab="max AGR fitted",ylab="max AGR obs",bty="n",main="Axes cogshall & josé")
#anova(lm(baseAJUST$maxAGR~baseAJUST$ER0))
#abline(lm(baseAJUST$maxAGR~baseAJUST$ER0)$coef)
#text(3,0.5,"P< 2.2e-16 ***")


####RGR50sim en fonction de Tm50
baseAJUST$variete<-as.factor(baseAJUST$variete)
plot(baseAJUST$RGR50sim~baseAJUST$Tm50,col=c("blue","red")[baseAJUST$variete],pch=20,main="Axes",bty="n",xlab="Mean Temperature from budburst to inflexion point",ylab="RGR at inflexion point from fitted curve")

summary(lm(RGR50sim~Tm50,data=cog))
summary(lm(RGR50sim~Tm50,data=jose))

summary(lm(RGR50sim~Tm50*variete,data=baseAJUST))

summary(lm(baseAJUST$RGR50sim~baseAJUST$Tm50+baseAJUST$variete))
abline(a=-0.161675 +0.054092 ,b= 0.018327,col="blue")
abline(a=-0.161675 -0.054092 ,b= 0.018327,col="red")
text(24,0.6,"P < 2e-16 ***, slope=0.018")
plot(baseAJUST$ER0~baseAJUST$Tm50,col=c("blue","red")[baseAJUST$variete],pch=20,bty="n",xlab="Mean Temperature from budburst to inflexion point",ylab="AGR at inflexion point from fitted curve")


### RGR & nb jours
jose<-baseAJUST[baseAJUST$variete=="jose",]
cog<-baseAJUST[baseAJUST$variete=="cog",]

plot(rnorm(length(baseAJUST$ndim90),mean=baseAJUST$ndim90,sd=0.2)~baseAJUST$RGR50sim,col=c("blue","red")[baseAJUST$variete],pch=20,main="Axes",bty="n",xlab="RGR at inflexion point from fitted curve",ylab="Number of days")

#Essai avec polynome
baseAJUST2<-baseAJUST[!is.na(baseAJUST$RGR50sim),]
plot(baseAJUST2$ndim90~baseAJUST2$RGR50sim,col=c("blue","red")[baseAJUST2$variete],pch=20,main="Axes",bty="n",xlab="RGR at inflexion point from fitted curve",ylab="Number of days")

mod1<-lm(ndim90~poly(RGR50sim,degree=1),data=baseAJUST2)
anova(mod1)
summary(mod1)

mod2<-lm(ndim90~poly(RGR50sim,degree=2),data=baseAJUST2)
anova(mod2)
summary(mod2)

mod3<-lm(ndim90~poly(RGR50sim,degree=3),data=baseAJUST2)
anova(mod3)
summary(mod3)

anova(mod1,mod2,mod3)
lines(baseAJUST2$RGR50sim,predict(mod2),type="p")

#essai avec smatr. Ne marche pas avec OLS
library(smatr)
mod<-sma(ndim90~RGR50sim+variete,data=baseAJUST,na.action=na.omit,log='xy',method="OLS",type="shift",alpha=0.01)
summary(mod)
mod<-sma(ndim90~RGR50sim,data=baseAJUST,na.action=na.omit,log='xy',method="OLS",type="shift",alpha=0.01)
summary(mod)

#Tab pour SMATR
ndim90<-baseAJUST$ndim90
RGR50sim<-baseAJUST$RGR50sim
variete<-baseAJUST$variete
donnees<-data.frame(variete,ndim90,RGR50sim)
donnees<-donnees[!is.na(donnees$RGR50sim),]

#lm en log
plot(rnorm(length(log(baseAJUST$ndim90)),mean=log(baseAJUST$ndim90),sd=0.2)~log(baseAJUST$RGR50sim),col=c("blue","red")[baseAJUST$variete],pch=20,main="log Axes",bty="n",xlab="log RGR at inflexion point from fitted curve",ylab="log Number of days")
summary(lm(log(baseAJUST$ndim90)~log(baseAJUST$RGR50sim)*variete,data=baseAJUST))
summary(lm(log(baseAJUST$ndim90)~log(baseAJUST$RGR50sim)+variete,data=baseAJUST))
abline(a= 1.03677 -0.04902, b= -0.91645, col="blue")
abline(a= 1.03677 +0.04902, b= -0.91645, col="red")
text(-1,3,"log y ~ log x P< 2e-16 ***")
text(-1,2.8,"Same slope: P=0.1723 / diff intercept P= 0.000225")

##### Calcul du RGR à 25% à partir de la courbe simulée.
## Ne change rien.

#RGR25<-NA
#ndim25<-NA

#compt<-0
#for(i in baseAJUST$a) 
#{
#	compt=compt+1
#	
#	A<-baseAJUST$A0[baseAJUST$a==i]
#	X0<-baseAJUST$X00[baseAJUST$a==i]
#	B<-baseAJUST$B0[baseAJUST$a==i]
#	
#	dim25<-A*25/100
#	n25<- -B*log((A/dim25)-1)+X0
#	navant<-n25-0.5 #On prend 1/2 jour avant et 1/2 jour apres: pente sur 1 jour
#	napres<-n25+0.5
#	dimavant<-A / ( 1 + exp (-((navant - X0) / B)))
#	dimapres<-A / ( 1 + exp (-((napres - X0) / B)))
#	
#	x<-c(navant,n25,napres)
#	y<-c(log(dimavant),log(dim25),log(dimapres))
#	
#	ndim25[compt]<-n25
#	
#	ifelse(is.na(y)==TRUE,RGR25[compt]<-NA,RGR25[compt]<-lm(y~x)$coefficients[2])
#}

#baseAJUST$RGR25sim<-RGR25
#baseAJUST$ndim25<-round(ndim25)

#plot(baseAJUST$RGR25sim~baseAJUST$Tm50,col=c("blue","red")[baseAJUST$variete],pch=20,bty="n",xlab="Mean Temperature from budburst to inflexion point",ylab="RGR at inflexion point from fitted curve")
#anova(lm(baseAJUST$RGR25sim~baseAJUST$Tm50*baseAJUST$variete))




















#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

















###################################
########### FEUILLES
###################################


BaseDeCroissance[1:10,]
a<-unique(BaseDeCroissance$codeUC)
baseUC<-BaseDeCroissance

codeUC0<-c(as.character(baseUC$codeUC),as.character(baseUC$codeUC),as.character(baseUC$codeUC),as.character(baseUC$codeUC))
variete<-c(as.character(baseUC$variete),as.character(baseUC$variete),as.character(baseUC$variete),as.character(baseUC$variete))
surF<-c(baseUC$surFprox,baseUC$surFdist,baseUC$surF1,baseUC$surFplus)
#surF<-c(baseUC$longFprox,baseUC$longFdist,baseUC$longF1,baseUC$longFplus)
FF<-c(as.character(baseUC$FFprox),as.character(baseUC$FFdist),as.character(baseUC$FF1),as.character(baseUC$FFplus))
posFeuille<-c(rep("prox",dim(baseUC)[1]),rep("dist",dim(baseUC)[1]),rep("F1",dim(baseUC)[1]),rep("Fplus",dim(baseUC)[1]))
DAB<-c(baseUC$DAB,baseUC$DAB,baseUC$DAB,baseUC$DAB)
verger<-c(as.character(baseUC$verger),as.character(baseUC$verger),as.character(baseUC$verger),as.character(baseUC$verger))
date1<-c(as.character(baseUC$date),as.character(baseUC$date),as.character(baseUC$date),as.character(baseUC$date))
arbre<-c(as.character(baseUC$arbre),as.character(baseUC$arbre),as.character(baseUC$arbre),as.character(baseUC$arbre))
posUC<-c(as.character(baseUC$positionUC),as.character(baseUC$positionUC),as.character(baseUC$positionUC),as.character(baseUC$positionUC))
posUCm<-c(as.character(baseUC$PUCM),as.character(baseUC$PUCM),as.character(baseUC$PUCM),as.character(baseUC$PUCM))
nombreApicale<-c(baseUC$nombreApicale,baseUC$nombreApicale,baseUC$nombreApicale,baseUC$nombreApicale)
nombreLaterale<-c(baseUC$nombreLaterale,baseUC$nombreLaterale,baseUC$nombreLaterale,baseUC$nombreLaterale)
croissance<-c(as.character(baseUC$croissance),as.character(baseUC$croissance),as.character(baseUC$croissance),as.character(baseUC$croissance))
orientation<-c(as.character(baseUC$orientation),as.character(baseUC$orientation),as.character(baseUC$orientation),as.character(baseUC$orientation))
hauteur<-c(baseUC$hauteur,baseUC$hauteur,baseUC$hauteur,baseUC$hauteur)
LEPF<-c(baseUC$LEPF,baseUC$LEPF,baseUC$LEPF,baseUC$LEPF)
nombreFeuille<-c(baseUC$nombreFeuille,baseUC$nombreFeuille,baseUC$nombreFeuille,baseUC$nombreFeuille)

baseUC2<-data.frame(codeUC0,variete,surF,FF,posFeuille,DAB,verger,date1,arbre,posUC,posUCm,nombreApicale,nombreLaterale,croissance,orientation,hauteur,LEPF,nombreFeuille)
colnames(baseUC2)[8]<-"date"
head(baseUC2)

#ON regroupe ensemble les feuilles d'une même UC
baseUC3<-data.frame()
baseuh<-split(baseUC2,baseUC2$codeUC0)
for (i in 1:length(baseuh)){
baseUC3<<-rbind(baseUC3,baseuh[[i]])}

#On enlève les feuilles ou il n'y avait pas de F
baseUC4<-data.frame()
for(i in levels(baseUC3$codeUC0)) {
for (j in levels(baseUC3$posFeuille)){
donnee<-NA
donnee2<-NA
donnee<-baseUC3[baseUC3$codeUC0==i&baseUC3$posFeuille==j,]
if(nrow(donnee)>0){
if(names(table(donnee$FF==""))[1]=="FALSE"){
donnee2<-donnee
baseUC4<-rbind(baseUC4,donnee2)  }}}}

#On  met la position feuille dans le code UC
baseUC5<-baseUC4
baseUC5$codeUC<-paste(baseUC4$codeUC0,baseUC4$posFeuille)
baseUC5$codeUC<-as.factor(baseUC5$codeUC)
baseF<-baseUC5
baseF[1:100,]

#### CodeUC0
codeUC0<-NA
compt=0
for (i in unique(baseF$codeUC)){
compt=compt+1
codeUC0[compt]<-as.character(unique(baseF$codeUC0[baseF$codeUC==i]))
}

##Pos Feuille
posF<-NA
compt=0
for (i in unique(baseF$codeUC)){
compt=compt+1
posF[compt]<-as.character(unique(baseF$posFeuille[baseF$codeUC==i]))
}



##################################
###### Calcul des paramètres ##
##################################

a<-unique(baseF$codeUC)

###############################################################################
#EQUATION D'AJUSTEMENT DE LA COURBE DE CROISSANCE:
#estimation des paramètres de l'équation
# A : y max : valeur du plateau final
#X0 : date du point d'inflection : 50% de la croissance
#B : durée ou la vitesse est maximale


#COeff pour les UC
A0<-vector()
X00<-vector()
B0<-vector()
#Vitesse maximale: ER
ER0<-vector()

#Vecteur d'erreur    #Ajoute 1 quand il y a des pb d'ajustement.
Error0<-vector()
compt=0

#On commence la boucle
for ( i in a) {
compt=compt+1
#On défini x et y
x=as.numeric(as.character(baseF$DAB[baseF$codeUC==i]))
y0=baseF$surF[baseF$codeUC==i]

#Fit pour les UC : estimation des paramètres
fit0<-NULL
try(fit0<-nls(y0 ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = 45, B = 1.5, X0 =7.5)),silent=T)
                               #start : valeurs initiales où commencer puis il cherche des valeurs aux alentours pour ajuster
                                     #Attention il ne fait pas les NA : enlève ces lignes
try(A0[compt]<-summary(fit0)$coef[1],silent=T)
try(B0[compt]<-summary(fit0)$coef[2],silent=T)
try(X00[compt]<-summary(fit0)$coef[3],silent=T)
#calcul de la vitesse max : ER
try(ER0[compt] <- 0.25 * A0[compt] / B0[compt], silent = T) 
}

baseAJUSTF<-data.frame(codeUC0,posF,a,A0,X00,B0,ER0)


####### Determination de dimension 90%
baseAJUSTF$dim90<- baseAJUSTF$A0*90/100
head(baseAJUSTF)

####### Determination de nb jours pour arriver à dimension 95%
baseAJUSTF$ndim90<- -baseAJUSTF$B0*log((baseAJUSTF$A0/baseAJUSTF$dim90)-1)+baseAJUSTF$X00 #nb de jours avec jour de deb=jour 0
baseAJUSTF$ndim90<-round(baseAJUSTF$ndim90,digits = 0)
head(baseAJUSTF)

####### Determination de nb jours total
compt=0
nF<-NA
for(i in unique(baseF$codeUC)) 
{
compt=compt+1
try(nF[compt]<-baseF$DAB[baseF$codeUC==i & baseF$FF=="f"],silent=T)
}
baseAJUSTF$nF<-nF
head(baseAJUSTF)

#Ajout de variete dans le tableau:
variete<-NULL
compt=0
for ( i in a) {
compt=compt+1
variete[compt]<-as.character(unique(baseF$variete[baseF$codeUC==i]))   }
baseAJUSTF$variete<-as.factor(variete)

plot(baseAJUSTF$nF,baseAJUSTF$ndim90,pch=20,col=c("blue","red")[baseAJUSTF$variete],xlim=c(0,30),ylim=c(0,30),bty="n",xlab="nb jours croissance",ylab="nb jours 90% croissance",main="leaf")
abline(a=0,b=1)
legend("topleft",c("Cogshall","José"),pch=16,col=c("blue","red"))


####### Determination de la température pour aller juska n90
a<-baseAJUSTF$a

Tm90<-NA
compt=0
for (i in a){
compt=compt+1
DATA<-NA
DATA<-baseF[baseF$codeUC==i,]

dataT<-NULL                           #On prend que le 1er car répétitions du mm verger!
if(DATA$verger[1]=="BM") {dataT<-TBMmoy}     #On choisi le fichier de température qu'il faut selon les vergers
if(DATA$verger[1]=="BP") {dataT<-TBPmoy}
if(DATA$verger[1]=="GH") {dataT<-TGHmoy}
if(DATA$verger[1]=="GF") {dataT<-TGFmoy}

D<-(DATA$date[DATA$DAB=="0"])
D<-strptime(D,"%d/%m/%y")     
                                                             
F90<-D+(baseAJUSTF$ndim90[baseAJUSTF$a==i]*86400)

ifelse(is.na(F90)==TRUE, Tm90[compt]<-NA,Tm90[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==D):which(dataT$datesUnik==(F90-86400))]))
}


baseAJUSTF$Tm90<-Tm90
head(baseAJUSTF)

#Fplus est en fait une feuille distale.
levels(baseAJUSTF$posF)[3]<-"dist"

verger<-NULL
compt=0
for ( i in a) {
compt=compt+1
verger[compt]<-as.character(unique(baseF$verger[baseF$codeUC==i]))   }
baseAJUSTF$verger<-as.factor(verger)

#Ajout des arbres
arbre<-NULL
compt=0
for ( i in a) {
compt=compt+1
arbre[compt]<-as.character(unique(baseF$arbre[baseF$codeUC==i]))   }
baseAJUSTF$arbre<-as.factor(arbre)

#Ajout position UC
posUC<-NULL
compt=0
for ( i in a) {
compt=compt+1
posUC[compt]<-as.character(unique(baseF$posUC[baseF$codeUC==i]))   }
baseAJUSTF$posUC<-as.factor(posUC)

#Ajout position UC mère
posUCm<-NULL
compt=0
for ( i in a) {
compt=compt+1
posUCm[compt]<-as.character(unique(baseF$posUCm[baseF$codeUC==i]))   }
baseAJUSTF$posUCm<-as.factor(posUCm)


jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]
cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]

##########################
##### Calcul du RGR au  niveau du point d'inflexion à partir de la courbe simulée.

RGR50<-NA
ndim50<-NA

compt<-0
for(i in baseAJUSTF$a) 
{
	compt=compt+1
	
	A<-baseAJUSTF$A0[baseAJUSTF$a==i]
	X0<-baseAJUSTF$X00[baseAJUSTF$a==i]
	B<-baseAJUSTF$B0[baseAJUSTF$a==i]
	
	dim50<-A*50/100
	n50<- -B*log((A/dim50)-1)+X0
	navant<-n50-0.5 #On prend 1/2 jour avant et 1/2 jour apres: pente sur 1 jour
	napres<-n50+0.5
	dimavant<-A / ( 1 + exp (-((navant - X0) / B)))
	dimapres<-A / ( 1 + exp (-((napres - X0) / B)))
	
	x<-c(navant,n50,napres)
	y<-c(log(dimavant),log(dim50),log(dimapres))
	
	ndim50[compt]<-n50
	
	ifelse(is.na(y)==TRUE,RGR50[compt]<-NA,RGR50[compt]<-lm(y~x)$coefficients[2])
}

baseAJUSTF$RGR50sim<-RGR50
baseAJUSTF$ndim50<-round(ndim50)



####### Determination de la température pour aller juska n50
a<-baseAJUSTF$a

Tm50<-NA
compt=0
for (i in a){
compt=compt+1
DATA<-NA
DATA<-baseF[baseF$codeUC==i,]

dataT<-NULL                           #On prend que le 1er car répétitions du mm verger!
if(DATA$verger[1]=="BM") {dataT<-TBMmoy}     #On choisi le fichier de température qu'il faut selon les vergers
if(DATA$verger[1]=="BP") {dataT<-TBPmoy}
if(DATA$verger[1]=="GH") {dataT<-TGHmoy}
if(DATA$verger[1]=="GF") {dataT<-TGFmoy}

D<-(DATA$date[DATA$DAB=="0"])
D<-strptime(D,"%d/%m/%y")     
                                                             
F50<-D+(baseAJUSTF$ndim50[baseAJUSTF$a==i]*86400)

ifelse(is.na(F50)==TRUE, Tm50[compt]<-NA,Tm50[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==D):which(dataT$datesUnik==(F50-86400))]))
}

baseAJUSTF$Tm50<-Tm50

### Calcul RGR à 50% à partir AGRmax
baseAJUSTF$dim50<- baseAJUSTF$A0*50/100

baseAJUSTF$RGRip<-baseAJUSTF$ER0/baseAJUSTF$dim50
plot(baseAJUSTF$RGRip~baseAJUSTF$RGR50sim)


### Ajout d'autres variables dans la base

#Ajout du nb apicale UCmere
nbApUCm<-NULL
compt=0
for ( i in a) {
compt=compt+1
nbApUCm[compt]<-as.character(unique(baseF$nombreApicale[baseF$codeUC==i]))   }

#Ajout du nb latérales UCmere
nbLatUCm<-NULL
compt=0
for ( i in a) {
compt=compt+1
nbLatUCm[compt]<-as.character(unique(baseF$nombreLaterale[baseF$codeUC==i]))   }

#Ajout du niveau de croissance de l'arbre (vigeur)
vigArbre<-NULL
compt=0
for ( i in a) {
compt=compt+1
vigArbre[compt]<-as.character(unique(baseF$croissance[baseF$codeUC==i]))   }

#Ajout de l'orientation de l'UC suivie
or<-NULL
compt=0
for ( i in a) {
compt=compt+1
or[compt]<-as.character(unique(baseF$orientation[baseF$codeUC==i]))   }

#Ajout de la hauteur de l'UC suivie
haut<-NULL
compt=0
for ( i in a) {
compt=compt+1
haut[compt]<-as.character(unique(baseF$hauteur[baseF$codeUC==i]))   }


#Ajout de la longueur de l'hypopodium
longHypo<-NULL
compt=0
for ( i in a) {
compt=compt+1
longHypo[compt]<-as.character(unique(baseF$LEPF[baseF$codeUC==i]))   }

#Nombre de feuilles
nbF<-NULL
compt=0
for ( i in a) {
compt=compt+1
nbF[compt]<-as.character(unique(baseF$nombreFeuille[baseF$codeUC==i]))   }

baseAJUSTF<-data.frame(baseAJUSTF,nbApUCm,nbLatUCm,vigArbre,or,haut,nbF,longHypo)
head(baseAJUSTF)

#write.table(baseAJUSTF,"baseAJUSTF.csv",sep="\t")



################################
### PLOTS FEUILLES
################################
#path<-"D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/T,n,d,ER/baseAJUSTF.csv"
#baseAJUSTF<-read.csv(path,sep="\t",header=TRUE)

cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]
jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]

########### Moyennes et ecart type des parametres

mean(jose$dim90,na.rm=TRUE)
sd(jose$dim90,na.rm=TRUE)
mean(cog$dim90,na.rm=TRUE)
sd(cog$dim90,na.rm=TRUE)
t.test(cog$dim90,jose$dim90,paired=FALSE)

mean(jose$ndim90,na.rm=TRUE)
sd(jose$ndim90,na.rm=TRUE)
mean(cog$ndim90,na.rm=TRUE)
sd(cog$ndim90,na.rm=TRUE)
t.test(cog$ndim90,jose$ndim90,paired=FALSE)

mean(jose$ER0,na.rm=TRUE)
sd(jose$ER0,na.rm=TRUE)
mean(cog$ER0,na.rm=TRUE)
sd(cog$ER0,na.rm=TRUE)
t.test(cog$ER0,jose$ER0,paired=FALSE)

mean(jose$RGR50sim,na.rm=TRUE)
sd(jose$RGR50sim,na.rm=TRUE)
mean(cog$RGR50sim,na.rm=TRUE)
sd(cog$RGR50sim,na.rm=TRUE)
t.test(cog$RGR50sim,jose$RGR50sim,paired=FALSE)

#Selon position José
prox<-jose[jose$posF=="prox",]
mean(prox$dim90,na.rm=TRUE)
sd(prox$dim90,na.rm=TRUE)
mean(prox$ndim90,na.rm=TRUE)
sd(prox$ndim90,na.rm=TRUE)
mean(prox$ER0,na.rm=TRUE)
sd(prox$ER0,na.rm=TRUE)
mean(prox$RGR50sim,na.rm=TRUE)
sd(prox$RGR50sim,na.rm=TRUE)

dist<-jose[jose$posF=="dist",]
mean(dist$dim90,na.rm=TRUE)
sd(dist$dim90,na.rm=TRUE)
mean(dist$ndim90,na.rm=TRUE)
sd(dist$ndim90,na.rm=TRUE)
mean(dist$ER0,na.rm=TRUE)
sd(dist$ER0,na.rm=TRUE)
mean(dist$RGR50sim,na.rm=TRUE)
sd(dist$RGR50sim,na.rm=TRUE)

first<-jose[jose$posF=="F1",]
mean(first$dim90,na.rm=TRUE)
sd(first$dim90,na.rm=TRUE)
mean(first$ndim90,na.rm=TRUE)
sd(first$ndim90,na.rm=TRUE)
mean(first$ER0,na.rm=TRUE)
sd(first$ER0,na.rm=TRUE)
mean(first$RGR50sim,na.rm=TRUE)
sd(first$RGR50sim,na.rm=TRUE)

library(car)
Anova(lm(jose$dim90~jose$posF),test.statistic="Wald", type="III")
Anova(lm(jose$ndim90~jose$posF),test.statistic="Wald", type="III")
Anova(lm(jose$ER0~jose$posF),test.statistic="Wald", type="III")
Anova(lm(jose$RGR50sim~jose$posF),test.statistic="Wald", type="III")

#Selon position Cogshall
prox<-cog[cog$posF=="prox",]
mean(prox$dim90,na.rm=TRUE)
sd(prox$dim90,na.rm=TRUE)
mean(prox$ndim90,na.rm=TRUE)
sd(prox$ndim90,na.rm=TRUE)
mean(prox$ER0,na.rm=TRUE)
sd(prox$ER0,na.rm=TRUE)
mean(prox$RGR50sim,na.rm=TRUE)
sd(prox$RGR50sim,na.rm=TRUE)

dist<-cog[cog$posF=="dist",]
mean(dist$dim90,na.rm=TRUE)
sd(dist$dim90,na.rm=TRUE)
mean(dist$ndim90,na.rm=TRUE)
sd(dist$ndim90,na.rm=TRUE)
mean(dist$ER0,na.rm=TRUE)
sd(dist$ER0,na.rm=TRUE)
mean(dist$RGR50sim,na.rm=TRUE)
sd(dist$RGR50sim,na.rm=TRUE)

first<-cog[cog$posF=="F1",]
mean(first$dim90,na.rm=TRUE)
sd(first$dim90,na.rm=TRUE)
mean(first$ndim90,na.rm=TRUE)
sd(first$ndim90,na.rm=TRUE)
mean(first$ER0,na.rm=TRUE)
sd(first$ER0,na.rm=TRUE)
mean(first$RGR50sim,na.rm=TRUE)
sd(first$RGR50sim,na.rm=TRUE)

Anova(lm(cog$dim90~cog$posF),test.statistic="Wald", type="III")
Anova(lm(cog$ndim90~cog$posF),test.statistic="Wald", type="III")
Anova(lm(cog$ER0~cog$posF),test.statistic="Wald", type="III")
Anova(lm(cog$RGR50sim~cog$posF),test.statistic="Wald", type="III")

################### NB jour en fonction de la température moyenne
plot(baseAJUSTF$ndim90[baseAJUSTF$variete=="cog"]~baseAJUSTF$Tm90[baseAJUSTF$variete=="cog"],xlim=c(0,30),ylim=c(0,30),bty="n",col="blue",pch=20,xlab="Mean temperature",ylab="number of days from burst to 90%")
points(baseAJUSTF$ndim90[baseAJUSTF$variete=="jose"]~baseAJUSTF$Tm90[baseAJUSTF$variete=="jose"],col="red",pch=20)

#Var par var:
el<-lm(ndim90~Tm90,data=cog)
summary(el)
el<-lm(ndim90~Tm90,data=jose)
summary(el)

el<-lm(ndim90~Tm90*variete,data=baseAJUSTF)
anova(el)
#Mêmes pentes 0.2304   
summary(lm(ndim90~Tm90+variete,data=baseAJUSTF))
#diff var <2e-16 ***
abline(a= 36.05158  -0.69584 , b=-0.94941  , col="blue")
abline(a=36.05158   +0.69584 , b=-0.94941  , col="red")

legend("bottomleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(18,0,"OLS: same slopes P= 0.2304, signif elevation P< 2e-16 ***")
text(18,3,"Slope = -0.94941  ")

#Effet de la position sur nb jour
cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]
jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]

el<-lm(ndim90~posF,data=cog)
anova(el)	#NS
summary(el)
levels(cog$posF)

el<-lm(ndim90~posF,data=jose)
anova(el)	#NS
summary(el)
levels(jose$posF)
jose$posF<-relevel(jose$posF,"prox")

#Effet de la position sur nb jour ~ temperature

prox<-cog[cog$posF=="prox",]
dist<-cog[cog$posF=="dist",]
first<-cog[cog$posF=="F1",]
summary(lm(ndim90~Tm90,data=prox))
summary(lm(ndim90~Tm90,data=dist))
summary(lm(ndim90~Tm90,data=first))

prox<-jose[jose$posF=="prox",]
dist<-jose[jose$posF=="dist",]
first<-jose[jose$posF=="F1",]
summary(lm(ndim90~Tm90,data=prox))
summary(lm(ndim90~Tm90,data=dist))
summary(lm(ndim90~Tm90,data=first))



####### FEUILLES n vs surf finale 90%
jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]
cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]

plot(ndim90~dim90,data=jose,main="leaf",xlim=c(0,150),ylim=c(0,35),bty="n",col="red",pch=20,xlab=" 90% Final Dimension",ylab="number of days from burst to 90%")
points(ndim90~dim90,data=cog,col="blue",pch=20)

anova(lm(ndim90~dim90*variete,data=baseAJUSTF))
#Pentes différentes
summary(lm(ndim90~dim90,data=jose))
summary(lm(ndim90~dim90,data=cog))
abline(lm(ndim90~dim90,data=jose)$coef,col="red")
text(60,35,"José: OLS Sign  P=0.00119 ** / Cog: NS")



####### FEUILLES temp vs surf finale 90%
plot(dim90~Tm90 ,data=jose,main="leaf",xlim=c(0,30),ylim=c(0,150),bty="n",col="red",pch=20,xlab=" Mean Temperature",ylab="90% final dimension")
points(dim90~Tm90 ,data=cog,col="blue",pch=20)

summary(lm(dim90~Tm90,data=jose))
summary(lm(dim90~Tm90,data=cog))



#Effet de la position sur surf finale
cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]
jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]

el<-lm(dim90~posF,data=cog)
anova(el)
summary(el)
levels(cog$posF)
cog$posF<-relevel(cog$posF,"prox")

el<-lm(dim90~posF,data=jose)
anova(el)	
summary(el)
levels(jose$posF)
jose$posF<-relevel(jose$posF,"dist")

#Effet de la position sur dim ~ temperature
prox<-cog[cog$posF=="prox",]
dist<-cog[cog$posF=="dist",]
first<-cog[cog$posF=="F1",]
summary(lm(dim90~Tm90,data=prox))
summary(lm(dim90~Tm90,data=dist))
summary(lm(dim90~Tm90,data=first))

prox<-jose[jose$posF=="prox",]
dist<-jose[jose$posF=="dist",]
first<-jose[jose$posF=="F1",]
summary(lm(dim90~Tm90,data=prox))
summary(lm(dim90~Tm90,data=dist))
summary(lm(dim90~Tm90,data=first))



###### Effet verger?
#Ajout de verger dans le tableau:

par(mfrow=c(2,2))
plot(Tm90~verger,data=jose,bty="n",main="leaf jose",ylab="Temp moy 90%")
text(4,24,"GF ***")
text(1,26,"GH ***")
plot(Tm90~verger,data=cog,bty="n",main="leaf cogshall",ylab="Temp moy 90%")
text(3,25,"GF ***")
text(1,26,"BM ***")
text(2,27,"BP ***")
#On ne peut pas différencier verger et température
plot(dim90~verger,data=jose,bty="n",main="leaf jose",ylab="90% surf finale")
plot(dim90~verger,data=cog,bty="n",main="leaf cogshall",ylab="90% surf finale")
text(1,80,"BM **")
text(2,90,"BP **")

anova(lm(dim90~Tm90*verger,data=jose))

anova(lm(dim90~Tm90*verger,data=cog))
jose$verger<-relevel(jose$verger,"GH")
cog$verger<-relevel(cog$verger,"GF")


###### Effet verger? sur 1/nb jours (vitesse de développement)
anova(lm(1/ndim90~Tm90*verger,data=jose))
summary(lm(1/ndim90~Tm90*verger,data=jose))

anova(lm(1/ndim90~Tm90*verger,data=cog))
summary(lm(1/ndim90~Tm90*verger,data=cog))




#Distribution des surfaces finales:
par(mfrow=c(4,2))
hist(cog$dim90[order(cog$dim90,na.last=NA)],main="Cogshall leaves",breaks=10,xlab="90% Surface of leaves (cm2)",col="blue",ylab="number of leaves",ylim=c(0,100),xlim=c(0,150))
hist(jose$dim90[order(jose$dim90,na.last=NA)],main="José leaves",breaks=10,xlab="90% Surface of leaves (cm2)",ylab="number of leaves",col="red",ylim=c(0,100),xlim=c(0,150))

# Distribution des points d'inflexion
hist(cog$X00[order(cog$X00,na.last=NA)],main="Cogshall leaves",breaks=10,xlab="inflexion point (nb jour)",col="blue",ylab="number of leaves",ylim=c(0,100),xlim=c(0,20))
hist(jose$X00[order(jose$X00,na.last=NA)],main="José leaves",breaks=10,xlab="inflexion point (nb jour)",ylab="number of leaves",col="red",ylim=c(0,100),xlim=c(0,20))

# Distribution des durées de vitesse max
hist(cog$B0[order(cog$B0,na.last=NA)],main="Cogshall leaves",breaks=10,xlab="duration at max ER (nb jour)",col="blue",ylab="number of leaves",ylim=c(0,100),xlim=c(0,3))
hist(jose$B0[order(jose$B0,na.last=NA)],main="José leaves",breaks=10,xlab="duration at max ER (nb jour)",ylab="number of leaves",col="red",ylim=c(0,100),xlim=c(0,3))

# Distribution des vitesse max
hist(cog$ER0[order(cog$ER0,na.last=NA)],main="Cogshall leaves",breaks=10,xlab="max ER (cm2/j)",col="blue",ylab="number of leaves",ylim=c(0,100),xlim=c(0,30))
hist(jose$ER0[order(jose$ER0,na.last=NA)],main="José leaves",breaks=10,xlab="max ER (cm2/j)",ylab="number of leaves",col="red",ylim=c(0,100),xlim=c(0,30))



####### Max ER
########## Dim 90 sur max ER
jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]
cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]

plot(ER0~dim90,data=jose,main="leaf",xlim=c(0,150),ylim=c(0,30),bty="n",col="red",pch=20,xlab=" 90% Final Dimension",ylab="Max Expansion Rate")
points(ER0~dim90,data=cog,col="blue",pch=20)

anova(lm(ER0~dim90*variete,data=baseAJUSTF))
#Pentes différentes 0.001545 ** 
anova(lm(ER0~dim90+variete,data=baseAJUSTF)) #elevation signif 6.986e-11 ***

summary(lm(ER0~dim90,data=jose))
summary(lm(ER0~dim90,data=cog))

abline(a= 0.079938  , b= 0.228036  , col="blue")
abline(a=-0.084926 , b=0.200789  , col="red")


legend("topleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(80,0,"OLS: dim vs ER P<2e-16 ***, diff slopes P= 0.001545 ** ")


########## Temp sur max ER
jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]
cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]

plot(ER0~Tm50,data=jose,main="leaf",xlim=c(0,30),ylim=c(0,30),bty="n",col="red",pch=20,xlab=" Mean Temp 90%",ylab="Max Expansion Rate")
points(ER0~Tm50,data=cog,col="blue",pch=20)

summary(lm(ER0~Tm50,data=cog)) 
summary(lm(ER0~Tm50,data=jose))
#Signif pour les 2

anova(lm(ER0~Tm50*variete,data=baseAJUSTF))
#Même pente 
anova(lm(ER0~Tm50+variete,data=baseAJUSTF))
summary(lm(ER0~Tm50+variete,data=baseAJUSTF))

abline(a=  -3.83511 +1.00759    , b=   0.36770  , col="blue")
abline(a= -3.83511-1.00759     , b=  0.36770    , col="red")

legend("topleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(20,30,"OLS: Same slopes (0.37). Sign elevation P=4.03e-10 ***")
text(20,27,"OLS: Mean T vs Max ER P= 1.53e-08 ***")

#Effet de la position sur ERmax
cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]
jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]

el<-lm(ER0~posF,data=cog)
anova(el)
summary(el)
levels(cog$posF)
cog$posF<-relevel(cog$posF,"dist")

el<-lm(ER0~posF,data=jose)
anova(el)	
summary(el)
levels(jose$posF)
jose$posF<-relevel(jose$posF,"F1")


#Effet de la position sur ERmax ~ temperature
prox<-cog[cog$posF=="prox",]
dist<-cog[cog$posF=="dist",]
first<-cog[cog$posF=="F1",]
summary(lm(ER0~Tm50,data=prox))
summary(lm(ER0~Tm50,data=dist))
summary(lm(ER0~Tm50,data=first))

prox<-jose[jose$posF=="prox",]
dist<-jose[jose$posF=="dist",]
first<-jose[jose$posF=="F1",]
summary(lm(ER0~Tm50,data=prox))
summary(lm(ER0~Tm50,data=dist))
summary(lm(ER0~Tm50,data=first))



########## n sur max ER
jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]
cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]

plot(ER0~ndim90,data=jose,main="leaf",xlim=c(0,30),ylim=c(0,30),bty="n",col="red",pch=20,xlab=" nb jours 90%",ylab="Max Expansion Rate")
points(ER0~ndim90,data=cog,col="blue",pch=20)

anova(lm(ER0~ndim90*variete,data=baseAJUSTF))
#Même pente 
anova(lm(ER0~ndim90+variete,data=baseAJUSTF))
summary(lm(ER0~ndim90+variete,data=baseAJUSTF))

abline(a= 10.47121 +0.71219      , b=  -0.41695   , col="blue")
abline(a= 10.47121 -0.71219      , b=  -0.41695   , col="red")

legend("topleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(18,30,"OLS: Same slopes. Sign elevation P=1.232e-05 *** ")
text(15,27,"OLS: nbj vs Max ER P< 2.2e-16 ***")


###### T, n, ER, dim
#On prend des feuilles de mm dimension:
DATA<-baseAJUSTF[baseAJUSTF$dim90<15 &baseAJUSTF$dim90>10 &baseAJUSTF$variete=="jose",]   
par(mfrow=c(2,1))
plot(ndim90~Tm90, data=DATA,bty="n", pch=20,ylim=c(0,30), xlim=c(0,30),xlab="Temp moy",ylab="nb jour",main="leaf, José, 10<dim<15")
plot(ER0~Tm90, data=DATA,bty="n", pch=20,ylim=c(0,5), xlim=c(0,30),main="Leaf, José, 10<dim<15")

###Scatterplot
levels(baseAJUSTF$pos)[3]<-"dist"
prox<-baseAJUSTF[baseAJUSTF$pos=="prox",]
distal<-baseAJUSTF[baseAJUSTF$pos=="dist",]
F1<-baseAJUSTF[baseAJUSTF$pos=="F1",]

library(car)
scatterplotMatrix(~Tm90+ndim90+dim90+ER0+X00+B0|variete,data=prox,diagonal="none",main="Feuilles proximales",smooth=FALSE,reg.line=FALSE,by.group=TRUE,pch=c(20,20),col=c("blue","red"),var.labels=c("Mean Temp","nb days","Dimension","Max ER","day of inflexion","days at max ER"),cex.labels=2)
scatterplotMatrix(~Tm90+ndim90+dim90+ER0+X00+B0|variete,data=distal,diagonal="none",main="Feuilles distales",smooth=FALSE,reg.line=FALSE,by.group=TRUE,pch=c(20,20),col=c("blue","red"),var.labels=c("Mean Temp","nb days","Dimension","Max ER","day of inflexion","days at max ER"),cex.labels=2)
scatterplotMatrix(~Tm90+ndim90+dim90+ER0+X00+B0|variete,data=F1,diagonal="none",main="Feuilles 1",smooth=FALSE,reg.line=FALSE,by.group=TRUE,pch=c(20,20),col=c("blue","red"),var.labels=c("Mean Temp","nb days","Dimension","Max ER","day of inflexion","days at max ER"),cex.labels=2)

scatterplotMatrix(~Tm90+ndim90+dim90+ER0+X00+B0|variete,data=baseAJUSTF,diagonal="none",main="Feuilles",smooth=FALSE,reg.line=FALSE,by.group=TRUE,pch=c(20,20),col=c("blue","red"),var.labels=c("Mean Temp","nb days","Dimension","Max ER","day of inflexion","days at max ER"),cex.labels=2)



####### Calcul du AGR et RGR OBS
baseF[1:10,]
a<-unique(baseF$codeUC)
length(a)

AGRF=vector()    
RGRF=vector()  

#####Pour chaque Feuille:     #OUVERTURE DE LA BOUCLE
for (j in a) {

compt=0
 
agr<-NA
rgr<-NA

t<-NA
t<-baseF$DAB[baseF$codeUC==j]

for (i in c(1:(length(t) ))) {

compt=compt+1
                                                       
#Stok des DAB:
ifelse((i-1)==0,x0<-NA,x0<-t[i-1])      #Car pour i = 1 : i-1 = 0 : problème, on met NA
x1<-t[i]
x2<-t[i+1]        #Met NA automatiquement si i+1 >length(t)

#On prend les longueurs correcpondant à chaque date:
ifelse(is.na(x0),y0<-NA,y0<-baseF$surF[baseF$codeUC==j & baseF$DAB==x0])     #NA si x0 = NA
y1<-baseF$surF[baseF$codeUC==j & baseF$DAB==x1]
ifelse(is.na(x2),y2<-NA,y2<-baseF$surF[baseF$codeUC==j & baseF$DAB==x2])     #NA si x2 = NA

#La regression est faite entre la longueur et le nombre de jour pour avoir le nombre de cm par jour
x<-NA
y<-NA
x<-c(x0, x1, x2)
y<-c(y0, y1, y2)
Y<-c(log(y0), log(y1), log(y2))

#On fait une régression linéaire pour obtenir la pente  (estimate)  de chaque point                            
ifelse(is.na(y[1])|is.na(y[2])|is.na(y[3]),agr[compt]<-NA, agr[compt]<-lm(y~x)$coefficients[2] )   
ifelse(is.na(Y[1])|is.na(Y[2])|is.na(Y[3]),rgr[compt]<-NA, rgr[compt]<-lm(Y~x)$coefficients[2] )  }
                                      #On ne calcule la pente que pour les cas ou il y a 3 points     
AGRF=c(AGRF,agr) 
RGRF=c(RGRF,rgr)}   

baseF<-data.frame(baseF,AGRF,RGRF)




#### Estimation RGR a partir SIMULATION
#Calcul surF simulée:
surFIT<-NA
compt<-0
for(i in baseAJUSTF$a) 
{
	A<-baseAJUSTF$A0[baseAJUSTF$a==i]
	X0<-baseAJUSTF$X00[baseAJUSTF$a==i]
	B<-baseAJUSTF$B0[baseAJUSTF$a==i]
	
	for(x in baseF$DAB[baseF$codeUC==i])
	{
		compt<-compt+1
		surFIT[compt]<- A / ( 1 + exp (-((x - X0) / B)))
	}
}

baseF<-data.frame(baseF,surFIT)


# Calcul RGR simulé, sur 2 points car courbe simulée est lisse et sans a coup

a<-unique(baseF$codeUC)
length(a)
RGRFsim=vector()    

#####Pour chaque UC:     #OUVERTURE DE LA BOUCLE

for (i in a)
{
	DATA<-baseF[baseF$codeUC==i,]
	rgr<-NA

	for (j in 2:nrow(DATA)) 
	{
		x<-c(DATA$DAB[j-1],DATA$DAB[j])
		y<-c(log(DATA$surFIT[j-1]),log(DATA$surFIT[j]))
		ifelse(is.na(y)==TRUE,rgr[j]<-NA,rgr[j]<-lm(y~x)$coefficients[2])
	}
	RGRFsim<-c(RGRFsim,rgr) 
}

baseF<-data.frame(baseF,RGRFsim)


## Plots:
maxL<-max(baseF$surF,na.rm=TRUE)
maxAGR<-max(baseF$AGRF,na.rm=TRUE)
maxDAB<-max(baseF$DAB,na.rm=TRUE)
maxRGR<-max(baseF$RGRF,na.rm=TRUE)

#pdf("essai.pdf", paper="a4",width=7,height=10)
par(mfrow=c(2,1))
par(oma=c(1,1,1,1),mar=c(4,4,2,6))

for (i in a) {

plot(baseF$surF[baseF$codeUC==i]~baseF$DAB[baseF$codeUC==i],bty="n",col="black",type="l",xlab="Days after burst",ylab="Area (cm2)",ylim=c(0,maxL),pch=20,xlim=c(0,maxDAB),main=i)
abline(h=baseAJUSTF$A0[baseAJUSTF$a==i]*10/100) #trace droite des 10%
points(baseF$surFIT[baseF$codeUC==i]~baseF$DAB[baseF$codeUC==i],col="black",type="l",lty="dashed")
par(new=T)
plot(baseF$AGRF[baseF$codeUC==i]~baseF$DAB[baseF$codeUC==i],bty="n",col="blue",type="l",pch=20,xaxt="n", yaxt="n",xlab="",ylab="",ylim=c(0,maxAGR),xlim=c(0,maxDAB))
axis(4)
mtext("Absolute Growth Rate: d(Area)/d(DAB)",side=4,line=2,cex=1,col="blue")
par(new=T)
plot(baseF$RGRF[baseF$codeUC==i]~baseF$DAB[baseF$codeUC==i],bty="n",col="red",type="l",pch=20,xaxt="n", yaxt="n",xlab="",ylab="",ylim=c(0,maxRGR),xlim=c(0,maxDAB))
points(baseF$RGRFsim[baseF$codeUC==i]~baseF$DAB[baseF$codeUC==i],col="red",type="l",lty="dashed")
axis(4,line=3.5)
mtext("Relative Growth Rate: d(ln(Area))/d(DAB)",side=4,line=5.5,cex=1,col="red") }

#dev.off()


##### max AGR obs vs max AGR sim
maxAGR<-NA
compt<-0

for (i in unique(baseF$codeUC))
{
	compt<-compt+1
	maxAGR[compt]<-max(baseF$AGRF[baseF$codeUC==i],na.rm=TRUE)
}

baseAJUSTF$maxAGR<-maxAGR
head(baseAJUSTF)
plot(baseAJUSTF$maxAGR~baseAJUSTF$ER0,xlab="max AGR fitted",ylab="max AGR obs",bty="n",main="Feuilles cogshall & josé")
anova(lm(baseAJUSTF$maxAGR~baseAJUSTF$ER0))
abline(lm(baseAJUSTF$maxAGR~baseAJUSTF$ER0)$coef)
text(10,1,"P< 2.2e-16 ***")






#Effet de la position des feuilles sur RGR50
cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]
jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]

el<-lm(RGR50sim~posF,data=cog)
anova(el)
summary(el)
levels(cog$posF)
cog$posF<-relevel(cog$posF,"prox")

el<-lm(RGR50sim~posF,data=jose)
anova(el)	
summary(el)
levels(jose$posF)
jose$posF<-relevel(jose$posF,"prox")

#Effet de la position des feuilles sur RGR50 ~ temperature
prox<-cog[cog$posF=="prox",]
dist<-cog[cog$posF=="dist",]
first<-cog[cog$posF=="F1",]
summary(lm(RGR50sim~Tm50,data=prox))
summary(lm(RGR50sim~Tm50,data=dist))
summary(lm(RGR50sim~Tm50,data=first))

prox<-jose[jose$posF=="prox",]
dist<-jose[jose$posF=="dist",]
first<-jose[jose$posF=="F1",]
summary(lm(RGR50sim~Tm50,data=prox))
summary(lm(RGR50sim~Tm50,data=dist))
summary(lm(RGR50sim~Tm50,data=first))


## RGR en fontion Tm50
baseAJUSTF$variete<-as.factor(baseAJUSTF$variete)
plot(baseAJUSTF$RGR50sim~baseAJUSTF$Tm50,col=c("blue","red")[baseAJUSTF$variete],pch=20,main="Feuilles",bty="n",xlab="Mean Temperature from budburst to inflexion point",ylab="RGR at inflexion point from fitted curve")

summary(lm(RGR50sim~Tm50,data=cog))
summary(lm(RGR50sim~Tm50,data=jose))

anova(lm(baseAJUSTF$RGR50sim~baseAJUSTF$Tm50*baseAJUSTF$variete))
summary(lm(baseAJUSTF$RGR50sim~baseAJUSTF$Tm50+baseAJUSTF$variete))
abline(a=-0.2098066 +0.0319860  ,b=0.0233497,col="blue")
abline(a=-0.2098066 -0.0319860  ,b= 0.0233497,col="red")
text(24,0.6,"P < 2e-16 ***, slope=0.023")




### RGR & nb jours
jose<-baseAJUSTF[baseAJUSTF$variete=="jose",]
cog<-baseAJUSTF[baseAJUSTF$variete=="cog",]

plot(rnorm(length(baseAJUSTF$ndim90),mean=baseAJUSTF$ndim90,sd=0.2)~baseAJUSTF$RGR50sim,col=c("blue","red")[baseAJUSTF$variete],pch=20,main="Feuilles",bty="n",xlab="RGR at inflexion point from fitted curve",ylab="Number of days")

summary(lm(ndim90~RGR50sim,data=cog))
summary(lm(ndim90~RGR50sim,data=jose))

summary(lm(ndim90~RGR50sim*variete,data=baseAJUSTF))


#lm en log
plot(rnorm(length(log(baseAJUSTF$ndim90)),mean=log(baseAJUSTF$ndim90),sd=0.2)~log(baseAJUSTF$RGR50sim),col=c("blue","red")[baseAJUSTF$variete],pch=20,main="log Feuilles",bty="n",xlab="log RGR at inflexion point from fitted curve",ylab="log Number of days")
summary(lm(log(baseAJUSTF$ndim90)~log(baseAJUSTF$RGR50sim)*variete,data=baseAJUSTF))
summary(lm(log(baseAJUSTF$ndim90)~log(baseAJUSTF$RGR50sim)+variete,data=baseAJUSTF))
abline(a= 1.614014 +0.017474, b= -0.856708, col="blue")
abline(a= 1.614014 -0.017474, b= -0.856708, col="red")
text(-0.8,3.3,"log y ~ log x P< 2e-16 ***")
text(-0.9,3.2,"Same slope: P=0.529  / diff intercept P=   0.00293")















#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################


















###################
######## INFLO
#####################
source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Preparation Base Inflo.R")
BaseDeCroissanceInflo[1:10,]

############################
## calcul des nb jours apres débourrement
############################

#On défini la date de débourrement de chaque UC qu'on copie à chaque ligne:
vec2<-NULL
base<-split(BaseDeCroissanceInflo,BaseDeCroissanceInflo$codeUC)
for (i in 1:length(base)){
ssbase<-base[[i]]
dateDeb<-as.character(ssbase$date[ssbase$DFInflo=="D"])
vec<-rep(dateDeb,dim(ssbase)[1])
vec2<-c(vec2,vec)
}
BaseDeCroissanceInflo<-cbind(BaseDeCroissanceInflo,vec2)

#Pour chaque jour on fait la différence avec la date de débourrement : nb de jours depuis débourrement.
BaseDeCroissanceInflo$DAB<-difftime(strptime(BaseDeCroissanceInflo$date,"%d/%m/%y"),strptime(BaseDeCroissanceInflo$vec2,"%d/%m/%y"),
units="days")
#Les 0 correspondent aux jours mm : gain reçu pendant la journée de débourrement.
#On commence les gains à DAB = 0
#les - sont pour les jours avant débourrement

BaseDeCroissanceInflo[1:10,]
a<-unique(BaseDeCroissanceInflo$codeUC)


###############################################################################
#EQUATION D'AJUSTEMENT DE LA COURBE DE CROISSANCE:
#estimation des paramètres de l'équation
# A : y max : valeur du plateau final
#X0 : date du point d'inflection : 50% de la croissance
#B : durée ou la vitesse est maximale


#COeff pour les Inflo
A0<-vector()
X00<-vector()
B0<-vector()
#Vitesse maximale: ER
ER0<-vector()

#Vecteur d'erreur    #Ajoute 1 quand il y a des pb d'ajustement.
Error0<-vector()
compt=0

#On commence la boucle
for ( i in a) {
compt=compt+1
#On défini x et y
x=as.numeric(as.character(BaseDeCroissanceInflo$DAB[BaseDeCroissanceInflo$codeUC==i]))
y0=BaseDeCroissanceInflo$longueurInflo[BaseDeCroissanceInflo$codeUC==i]

#Fit pour les UC : estimation des paramètres
fit0<-NULL
try(fit0<-nls(y0 ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = 20.5, B = 5, X0 =10)),silent=T)
                               #start : valeurs initiales où commencer puis il cherche des valeurs aux alentours pour ajuster
                                     #Attention il ne fait pas les NA : enlève ces lignes
try(A0[compt]<-summary(fit0)$coef[1],silent=T)
try(B0[compt]<-summary(fit0)$coef[2],silent=T)
try(X00[compt]<-summary(fit0)$coef[3],silent=T)
#calcul de la vitesse max : ER
try(ER0[compt] <- 0.25 * A0[compt] / B0[compt], silent = T) 
}

baseAJUSTinflo<-data.frame(a,A0,X00,B0,ER0)


####### Determination de dimension 90%
baseAJUSTinflo$dim90<- baseAJUSTinflo$A0*90/100

####### Determination de nb jours pour arriver à dimension 90%
baseAJUSTinflo$ndim90<- -baseAJUSTinflo$B0*log((baseAJUSTinflo$A0/baseAJUSTinflo$dim90)-1)+baseAJUSTinflo$X00 #nb de jours avec jour de deb=jour 0
baseAJUSTinflo$ndim90<-round(baseAJUSTinflo$ndim90,digits = 0)
head(baseAJUSTinflo)

#### On récupère le nb jour total
compt=0
nInflo<-NA
for(i in unique(BaseDeCroissanceInflo$codeUC)) 
{
compt=compt+1
try(nInflo[compt]<-BaseDeCroissanceInflo$DAB[BaseDeCroissanceInflo$codeUC==i & BaseDeCroissanceInflo$DFInflo=="F"],silent=T)
}
baseAJUSTinflo$nInflo<-nInflo

#Ajout de variete dans le tableau:
variete<-NULL
compt=0
for ( i in a) {
compt=compt+1
variete[compt]<-as.character(unique(BaseDeCroissanceInflo$variete[BaseDeCroissanceInflo$codeUC==i]))   }
baseAJUSTinflo$variete<-as.factor(variete)

plot(baseAJUSTinflo$nInflo,baseAJUSTinflo$ndim90,pch=20,col=c("blue","red")[baseAJUSTinflo$variete],xlim=c(0,80),ylim=c(0,80),bty="n",xlab="nb jours croissance",ylab="nb jours 90% croissance",main="inflo")
abline(a=0,b=1)
legend("topleft",c("Cogshall","José"),pch=16,col=c("blue","red"))



####### Determination de la température pour aller juska n90
a<-baseAJUSTinflo$a

Tm90<-NA
compt=0
for (i in a){
compt=compt+1
DATA<-NA
DATA<-BaseDeCroissanceInflo[BaseDeCroissanceInflo$codeUC==i,]

dataT<-NULL                           #On prend que le 1er car répétitions du mm verger!
if(DATA$verger[1]=="BM") {dataT<-TBMmoy}     #On choisi le fichier de température qu'il faut selon les vergers
if(DATA$verger[1]=="BP") {dataT<-TBPmoy}
if(DATA$verger[1]=="GH") {dataT<-TGHmoy}
if(DATA$verger[1]=="GF") {dataT<-TGFmoy}

D<-(DATA$date[DATA$DFInflo=="D"])
D<-strptime(D,"%d/%m/%y")     
                                                             
F90<-D+(baseAJUSTinflo$ndim90[baseAJUSTinflo$a==i]*86400)

ifelse(is.na(F90)==TRUE, Tm90[compt]<-NA,Tm90[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==D):which(dataT$datesUnik==(F90-86400))]))
}

baseAJUSTinflo$Tm90<-Tm90

#Ajout des position
pos<-NULL
compt=0
for ( i in a) {
compt=compt+1
pos[compt]<-as.character(unique(BaseDeCroissanceInflo$positionUC[BaseDeCroissanceInflo$codeUC==i]))   }
baseAJUSTinflo$pos<-pos

#Ajout des position des UC mère
posUCm<-NULL
compt=0
for ( i in a) {
compt=compt+1
posUCm[compt]<-as.character(unique(BaseDeCroissanceInflo$PUCM[BaseDeCroissanceInflo$codeUC==i]))   }
baseAJUSTinflo$posUCm<-posUCm

#Ajout des arbres
arbre<-NULL
compt=0
for ( i in a) {
compt=compt+1
arbre[compt]<-as.character(unique(BaseDeCroissanceInflo$arbre[BaseDeCroissanceInflo$codeUC==i]))   }
baseAJUSTinflo$arbre<-arbre



###########################		
##### Calcul du RGR au  niveau du point d'inflexion à partir de la courbe simulée.

RGR50<-NA
ndim50<-NA

compt<-0
for(i in baseAJUSTinflo$a) 
{
	compt=compt+1
	
	A<-baseAJUSTinflo$A0[baseAJUSTinflo$a==i]
	X0<-baseAJUSTinflo$X00[baseAJUSTinflo$a==i]
	B<-baseAJUSTinflo$B0[baseAJUSTinflo$a==i]
	
	dim50<-A*50/100
	n50<- -B*log((A/dim50)-1)+X0
	navant<-n50-0.5 #On prend 1/2 jour avant et 1/2 jour apres: pente sur 1 jour
	napres<-n50+0.5
	dimavant<-A / ( 1 + exp (-((navant - X0) / B)))
	dimapres<-A / ( 1 + exp (-((napres - X0) / B)))
	
	x<-c(navant,n50,napres)
	y<-c(log(dimavant),log(dim50),log(dimapres))
	
	ndim50[compt]<-n50
	
	ifelse(is.na(y)==TRUE,RGR50[compt]<-NA,RGR50[compt]<-lm(y~x)$coefficients[2])
}

baseAJUSTinflo$RGR50sim<-RGR50
baseAJUSTinflo$ndim50<-round(ndim50)


####### Determination de la température pour aller juska n50
a<-baseAJUSTinflo$a

Tm50<-NA
compt=0
for (i in a){
compt=compt+1
DATA<-NA
DATA<-BaseDeCroissanceInflo[BaseDeCroissanceInflo$codeUC==i,]

dataT<-NULL                           #On prend que le 1er car répétitions du mm verger!
if(DATA$verger[1]=="BM") {dataT<-TBMmoy}     #On choisi le fichier de température qu'il faut selon les vergers
if(DATA$verger[1]=="BP") {dataT<-TBPmoy}
if(DATA$verger[1]=="GH") {dataT<-TGHmoy}
if(DATA$verger[1]=="GF") {dataT<-TGFmoy}

D<-(DATA$date[DATA$DFInflo=="D"])
D<-strptime(D,"%d/%m/%y")     
                                                             
F50<-D+(baseAJUSTinflo$ndim50[baseAJUSTinflo$a==i]*86400)

ifelse(is.na(F50)==TRUE, Tm50[compt]<-NA,Tm50[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==D):which(dataT$datesUnik==(F50-86400))]))
}

baseAJUSTinflo$Tm50<-Tm50


### Calcul RGR à 50% à partir AGRmax
baseAJUSTinflo$dim50<- baseAJUSTinflo$A0*50/100

baseAJUSTinflo$RGRip<-baseAJUSTinflo$ER0/baseAJUSTinflo$dim50
plot(baseAJUSTinflo$RGRip~baseAJUSTinflo$RGR50sim)

#Ajout d'autres variables
#Ajout des vergers
verger<-NULL
compt=0
for ( i in a) {
compt=compt+1
verger[compt]<-as.character(unique(BaseDeCroissanceInflo$verger[BaseDeCroissanceInflo$codeUC==i]))   }

#Ajout des saisons
sai<-NULL
compt=0
for ( i in a) {
compt=compt+1
sai[compt]<-as.character(unique(BaseDeCroissanceInflo$saison[BaseDeCroissanceInflo$codeUC==i]))   }

#Ajout du nb apicales UCmere
nbApUCm<-NULL
compt=0
for ( i in a) {
compt=compt+1
nbApUCm[compt]<-as.character(unique(BaseDeCroissanceInflo$nombreApicale[BaseDeCroissanceInflo$codeUC==i]))   }

#Ajout du nb latérales UCmere
nbLatUCm<-NULL
compt=0
for ( i in a) {
compt=compt+1
nbLatUCm[compt]<-as.character(unique(BaseDeCroissanceInflo$nombreLaterale[BaseDeCroissanceInflo$codeUC==i]))   }

#Ajout du niveau de croissance de l'arbre (vigeur)
vigArbre<-NULL
compt=0
for ( i in a) {
compt=compt+1
vigArbre[compt]<-as.character(unique(BaseDeCroissanceInflo$croissance[BaseDeCroissanceInflo$codeUC==i]))   }

#Ajout de l'orientation de l'UC suivie
or<-NULL
compt=0
for ( i in a) {
compt=compt+1
or[compt]<-as.character(unique(BaseDeCroissanceInflo$orientation[BaseDeCroissanceInflo$codeUC==i]))   }

#Ajout de la hauteur de l'UC suivie
haut<-NULL
compt=0
for ( i in a) {
compt=compt+1
haut[compt]<-as.character(unique(BaseDeCroissanceInflo$hauteur[BaseDeCroissanceInflo$codeUC==i]))   }

#Ajout du diamètre moyen UCmère
diamUCm<-NULL
compt=0
for ( i in a) {
compt=compt+1
diamUCm[compt]<-mean(unique(BaseDeCroissanceInflo$diamUCM1[BaseDeCroissanceInflo$codeUC==i]),unique(BaseDeCroissanceInflo$diamUCM2[BaseDeCroissanceInflo$codeUC==i]))   }

#Ajout du diamètre axe I
diamAxeI<-NULL
compt=0
for ( i in a) {
compt=compt+1
diamAxeI[compt]<-as.character(unique(BaseDeCroissanceInflo$diamAxeI[BaseDeCroissanceInflo$codeUC==i]))   }

#Ajout de la longueur du plus grand axe II
longAxeII<-NULL
compt=0
for ( i in a) {
compt=compt+1
longAxeII[compt]<-as.character(unique(BaseDeCroissanceInflo$longueurAxeSec[BaseDeCroissanceInflo$codeUC==i]))   }

baseAJUSTinflo<-data.frame(baseAJUSTinflo,verger,sai,nbApUCm,nbLatUCm,vigArbre,or,haut,diamUCm,diamAxeI,longAxeII)
head(baseAJUSTinflo)

#write.table(baseAJUSTinflo,"baseAJUSTinflo.csv",sep="\t")

################################
### PLOTS INFLOS
################################
#path<-"D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Exploit/T,n,d,ER/baseAJUSTinflo.csv"
#baseAJUSTinflo<-read.csv(path,sep="\t",header=TRUE)

options(contrasts=c("contr.sum","contr.poly"))          # contrasts = c("fact non ordonnés" , "facteurs ordonnés")
jose<-baseAJUSTinflo[baseAJUSTinflo$variete=="jose",]
cog<-baseAJUSTinflo[baseAJUSTinflo$variete=="cog",]

############# Mean & sd
mean(jose$dim90,na.rm=TRUE)
sd(jose$dim90,na.rm=TRUE)
mean(cog$dim90,na.rm=TRUE)
sd(cog$dim90,na.rm=TRUE)
t.test(cog$dim90,jose$dim90,paired=FALSE)

mean(jose$ndim90,na.rm=TRUE)
sd(jose$ndim90,na.rm=TRUE)
mean(cog$ndim90,na.rm=TRUE)
sd(cog$ndim90,na.rm=TRUE)
t.test(cog$ndim90,jose$ndim90,paired=FALSE)

mean(jose$ER0,na.rm=TRUE)
sd(jose$ER0,na.rm=TRUE)
mean(cog$ER0,na.rm=TRUE)
sd(cog$ER0,na.rm=TRUE)
t.test(cog$ER0,jose$ER0,paired=FALSE)

mean(jose$RGR50sim,na.rm=TRUE)
sd(jose$RGR50sim,na.rm=TRUE)
mean(cog$RGR50sim,na.rm=TRUE)
sd(cog$RGR50sim,na.rm=TRUE)
t.test(cog$RGR50sim,jose$RGR50sim,paired=FALSE)


########### INFLOS
plot(baseAJUSTinflo$ndim90[baseAJUSTinflo$variete=="cog"]~baseAJUSTinflo$Tm90[baseAJUSTinflo$variete=="cog"],xlim=c(0,30),ylim=c(0,60),bty="n",col="blue",pch=20,xlab="Mean temperature",ylab="number of days from burst to 90%")
points(baseAJUSTinflo$ndim90[baseAJUSTinflo$variete=="jose"]~baseAJUSTinflo$Tm90[baseAJUSTinflo$variete=="jose"],col="red",pch=20)

summary(lm(ndim90~Tm90,data=cog))
summary(lm(ndim90~Tm90,data=jose))

el<-lm(ndim90~Tm90*variete,data=baseAJUSTinflo)
anova(el)
#interaction NS 0.7517 

el<-lm(ndim90~Tm90+variete,data=baseAJUSTinflo)
summary(el)
#Pas d'effet variete 0.142  
summary(lm(ndim90~Tm90,data=baseAJUSTinflo))
abline(a= 78.9230  , b=-2.5017  , col="black")

legend("bottomleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(18,0,"OLS: Pas d'effet varietal P= 0.1442 ")


####### INFLOS n vs surf finale 90%
jose<-baseAJUSTinflo[baseAJUSTinflo$variete=="jose",]
cog<-baseAJUSTinflo[baseAJUSTinflo$variete=="cog",]

plot(ndim90~dim90,data=jose,main="inflo",xlim=c(0,50),ylim=c(0,60),bty="n",col="red",pch=20,xlab=" 90% Final Dimension",ylab="number of days from burst to 90%")
points(ndim90~dim90,data=cog,col="blue",pch=20)


anova(lm(ndim90~dim90*variete,data=baseAJUSTinflo))
#Meme pente
anova(lm(ndim90~dim90+variete,data=baseAJUSTinflo))
summary(lm(ndim90~dim90+variete,data=baseAJUSTinflo))

abline(a=13.59497  +1.70498 , b=  0.62395   ,col="red")
abline(a=13.59497  -1.70498  , b=  0.62395  ,col="blue")
legend("bottomleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(30,0,"OLS. Same slopes. Significant elevation. Dim vs n P=2.61e-08 ***")


####### INFLOS temp vs surf finale 90%
plot(dim90~Tm90 ,data=jose,main="inflo",xlim=c(0,30),ylim=c(0,60),bty="n",col="red",pch=20,xlab=" Mean Temperature",ylab="90% final dimension")
points(dim90~Tm90 ,data=cog,col="blue",pch=20)

summary(lm(dim90~Tm90,data=cog))
summary(lm(dim90~Tm90,data=jose))

anova(lm(dim90~Tm90*variete,data=baseAJUSTinflo))
#Même pente
summary(lm(dim90~Tm90+variete,data=baseAJUSTinflo))
abline(a=52.2504   +1.5847   , b=  -1.5449   ,col="blue")
abline(a=52.2504   -1.5847   , b=  -1.5449   ,col="red")
legend("bottomleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(15,60,"OLS. Same slopes. Significant elevation. Dim vs Temp P=4.857e-07 ***")

###### Effet verger?
#Ajout de verger dans le tableau:
verger<-NULL
compt=0
for ( i in a) {
compt=compt+1
verger[compt]<-as.character(unique(BaseDeCroissanceInflo$verger[BaseDeCroissanceInflo$codeUC==i]))   }
baseAJUSTinflo$verger<-as.factor(verger)
jose<-baseAJUSTinflo[baseAJUSTinflo$variete=="jose",]
cog<-baseAJUSTinflo[baseAJUSTinflo$variete=="cog",]

par(mfrow=c(2,2))
plot(Tm90~verger,data=jose,bty="n",main="inflo jose",ylab="Temp moy 90%")
text(2,23,"BM ***")
text(4,23,"GH ***")
text(1,22,"GF ***")
plot(Tm90~verger,data=cog,bty="n",main="inflo cogshall",ylab="Temp moy 90%")
text(2,21,"BM ***")
text(3,22,"BP **")
text(1,24,"GF ***")
#On ne peut pas différencier verger et température
plot(dim90~verger,data=jose,bty="n",main="inflo jose",ylab="90% long finale")
plot(dim90~verger,data=cog,bty="n",main="inflo cogshall",ylab="90% long finale")
text(3,30,"GF **")
anova(lm(dim90~verger,data=jose))
summary(lm(dim90~verger,data=cog))
cog$verger<-relevel(cog$verger,"GF")
jose$verger<-relevel(jose$verger,"GF")

summary(lm(Tm90~verger,data=jose))
summary(lm(Tm90~verger,data=cog))



###### Effet verger? sur 1/nb jours (vitesse de développement)
anova(lm(1/ndim90~Tm90*verger,data=jose))
anova(lm(1/ndim90~Tm90+verger,data=jose))
summary(lm(1/ndim90~Tm90+verger,data=jose))


anova(lm(1/ndim90~Tm90*verger,data=cog))
summary(lm(1/ndim90~Tm90*verger,data=cog))


#Distribution des long finales:
par(mfrow=c(4,2))
hist(cog$dim90[order(cog$dim90,na.last=NA)],main="Cogshall inflo",breaks=10,xlab="90% length of inflo (cm)",col="blue",ylab="number of inflo",ylim=c(0,30),xlim=c(0,50))
hist(jose$dim90[order(jose$dim90,na.last=NA)],main="José inflo",breaks=10,xlab="90% length of inflo (cm)",ylab="number of inflo",col="red",ylim=c(0,30),xlim=c(0,50))

# Distribution des points d'inflexion
hist(cog$X00[order(cog$X00,na.last=NA)],main="Cogshall inflo",breaks=10,xlab="inflexion point (nb jour)",col="blue",ylab="number of inflo",ylim=c(0,30),xlim=c(0,50))
hist(jose$X00[order(jose$X00,na.last=NA)],main="José inflo",breaks=10,xlab="inflexion point (nb jour)",ylab="number of inflo",col="red",ylim=c(0,30),xlim=c(0,50))

# Distribution des durées de vitesse max
hist(cog$B0[order(cog$B0,na.last=NA)],main="Cogshall inflo",breaks=10,xlab="duration at max ER (nb jour)",col="blue",ylab="number of inflo",ylim=c(0,30),xlim=c(0,20))
hist(jose$B0[order(jose$B0,na.last=NA)],main="José inflo",breaks=10,xlab="duration at max ER (nb jour)",ylab="number of inflo",col="red",ylim=c(0,30),xlim=c(0,20))

# Distribution des vitesse max
hist(cog$ER0[order(cog$ER0,na.last=NA)],main="Cogshall inflo",breaks=10,xlab="max ER (cm/j)",col="blue",ylab="number of inflo",ylim=c(0,30),xlim=c(0,5))
hist(jose$ER0[order(jose$ER0,na.last=NA)],main="José inflo",breaks=10,xlab="max ER (cm/j)",ylab="number of inflo",col="red",ylim=c(0,30),xlim=c(0,5))



####### Max ER
########## Dim 90 sur max ER
jose<-baseAJUSTinflo[baseAJUSTinflo$variete=="jose",]
cog<-baseAJUSTinflo[baseAJUSTinflo$variete=="cog",]

plot(ER0~dim90,data=jose,main="inflo",xlim=c(0,50),ylim=c(0,5),bty="n",col="red",pch=20,xlab=" 90% Final Dimension",ylab="Max Expansion Rate")
points(ER0~dim90,data=cog,col="blue",pch=20)

anova(lm(ER0~dim90*variete,data=baseAJUSTinflo))
#Pentes différentes
anova(lm(ER0~dim90+dim90:variete,data=baseAJUSTinflo))
#Pas d'effet variete

summary(lm(ER0~dim90,data=baseAJUSTinflo))

abline(a= 0.312552  , b= 0.039669  , col="black")

legend("topleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(30,5,"OLS: Sign shift P= 0.008738 ** ")
text(30,4.5,"OLS: Dim vs ER: P= 2e-14 *** ")


########## Temp sur max ER
jose<-baseAJUSTinflo[baseAJUSTinflo$variete=="jose",]
cog<-baseAJUSTinflo[baseAJUSTinflo$variete=="cog",]

plot(ER0~Tm50,data=jose,main="inflo",xlim=c(0,30),ylim=c(0,5),bty="n",col="red",pch=20,xlab=" Mean Temp 90%",ylab="Max Expansion Rate")
points(ER0~Tm50,data=cog,col="blue",pch=20)

summary(lm(ER0~Tm50,data=cog))
summary(lm(ER0~Tm50,data=jose))
#NS

legend("topleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(15,5,"OLS: NS ")

########## nb jour sur max ER
jose<-baseAJUSTinflo[baseAJUSTinflo$variete=="jose",]
cog<-baseAJUSTinflo[baseAJUSTinflo$variete=="cog",]

plot(ER0~ndim90,data=jose,main="inflo",xlim=c(0,60),ylim=c(0,5),bty="n",col="red",pch=20,xlab=" nb jours 90%",ylab="Max Expansion Rate")
points(ER0~ndim90,data=cog,col="blue",pch=20)

anova(lm(ER0~ndim90*variete,data=baseAJUSTinflo))
#Rien n'est significatif


legend("topleft",c("Cogshall","Jose"),pch=16,col=c("blue","red"))
text(30,5,"OLS: NS ")

#Scatterplot
library(car)
scatterplotMatrix(~Tm90+ndim90+dim90+ER0+X00+B0|variete,data=baseAJUSTinflo,diagonal="none",main="Inflorescences",smooth=FALSE,reg.line=FALSE,by.group=TRUE,pch=c(20,20),col=c("blue","red"),var.labels=c("Mean Temp","nb days","Dimension","Max ER","day of inflexion","days at max ER"),cex.labels=2)


####### Calcul du AGR et RGR
BaseDeCroissanceInflo[1:10,]
a<-unique(BaseDeCroissanceInflo$codeUC)
length(a)

AGRI=vector()    
RGRI=vector()  

#####Pour chaque INFLO:     #OUVERTURE DE LA BOUCLE
for (j in a) {

compt=0
 
agr<-NA
rgr<-NA

t<-NA
t<-BaseDeCroissanceInflo$DAB[BaseDeCroissanceInflo$codeUC==j]

for (i in c(1:(length(t) ))) {

compt=compt+1
                                                       
#Stok des DAB:
ifelse((i-1)==0,x0<-NA,x0<-t[i-1])      #Car pour i = 1 : i-1 = 0 : problème, on met NA
x1<-t[i]
x2<-t[i+1]        #Met NA automatiquement si i+1 >length(t)

#On prend les longueurs correcpondant à chaque date:
ifelse(is.na(x0),y0<-NA,y0<-BaseDeCroissanceInflo$longueurInflo[BaseDeCroissanceInflo$codeUC==j & BaseDeCroissanceInflo$DAB==x0])     #NA si x0 = NA
y1<-BaseDeCroissanceInflo$longueurInflo[BaseDeCroissanceInflo$codeUC==j & BaseDeCroissanceInflo$DAB==x1]
ifelse(is.na(x2),y2<-NA,y2<-BaseDeCroissanceInflo$longueurInflo[BaseDeCroissanceInflo$codeUC==j & BaseDeCroissanceInflo$DAB==x2])     #NA si x2 = NA

#La regression est faite entre la longueur et le nombre de jour pour avoir le nombre de cm par jour
x<-NA
y<-NA
x<-c(x0, x1, x2)
y<-c(y0, y1, y2)
Y<-c(log(y0), log(y1), log(y2))

#On fait une régression linéaire pour obtenir la pente  (estimate)  de chaque point                            
ifelse(is.na(y[1])|is.na(y[2])|is.na(y[3]),agr[compt]<-NA, agr[compt]<-lm(y~x)$coefficients[2] )   
ifelse(is.na(Y[1])|is.na(Y[2])|is.na(Y[3]),rgr[compt]<-NA, rgr[compt]<-lm(Y~x)$coefficients[2] )  }
                                      #On ne calcule la pente que pour les cas ou il y a 3 points     
AGRI=c(AGRI,agr) 
RGRI=c(RGRI,rgr)}   

BaseDeCroissanceInflo<-data.frame(BaseDeCroissanceInflo,AGRI,RGRI)

		
		
#### Estimation RGR a partir SIMULATION
#Calcul surF simulée:
longFIT<-NA
compt<-0
for(i in baseAJUSTinflo$a) 
{
	A<-baseAJUSTinflo$A0[baseAJUSTinflo$a==i]
	X0<-baseAJUSTinflo$X00[baseAJUSTinflo$a==i]
	B<-baseAJUSTinflo$B0[baseAJUSTinflo$a==i]
	
	for(x in BaseDeCroissanceInflo$DAB[BaseDeCroissanceInflo$codeUC==i])
	{
		compt<-compt+1
		longFIT[compt]<- A / ( 1 + exp (-((x - X0) / B)))
	}
}

BaseDeCroissanceInflo<-data.frame(BaseDeCroissanceInflo,longFIT)


# Calcul RGR simulé, sur 2 points car courbe simulée est lisse et sans a coup

a<-unique(BaseDeCroissanceInflo$codeUC)
length(a)
RGRIsim=vector()    

#####Pour chaque UC:     #OUVERTURE DE LA BOUCLE

for (i in a)
{
	DATA<-BaseDeCroissanceInflo[BaseDeCroissanceInflo$codeUC==i,]
	rgr<-NA

	for (j in 2:nrow(DATA)) 
	{
		x<-c(DATA$DAB[j-1],DATA$DAB[j])
		y<-c(log(DATA$longFIT[j-1]),log(DATA$longFIT[j]))
		ifelse(is.na(y)==TRUE,rgr[j]<-NA,rgr[j]<-lm(y~x)$coefficients[2])
	}
	RGRIsim<-c(RGRIsim,rgr) 
}

BaseDeCroissanceInflo<-data.frame(BaseDeCroissanceInflo,RGRIsim)


## Plots:
maxL<-max(BaseDeCroissanceInflo$longueurInflo,na.rm=TRUE)
maxAGR<-max(BaseDeCroissanceInflo$AGRI,na.rm=TRUE)
maxDAB<-max(BaseDeCroissanceInflo$DAB,na.rm=TRUE)
maxRGR<-max(BaseDeCroissanceInflo$RGRI,na.rm=TRUE)

pdf("essai.pdf", paper="a4",width=7,height=10)
par(mfrow=c(2,1))
par(oma=c(1,1,1,1),mar=c(4,4,2,6))

for (i in a) {

plot(BaseDeCroissanceInflo$longueurInflo[BaseDeCroissanceInflo$codeUC==i]~BaseDeCroissanceInflo$DAB[BaseDeCroissanceInflo$codeUC==i],bty="n",col="black",type="l",xlab="Days after burst",ylab="length (cm)",ylim=c(0,maxL),pch=20,xlim=c(0,maxDAB),main=i)
points(BaseDeCroissanceInflo$longFIT[BaseDeCroissanceInflo$codeUC==i]~BaseDeCroissanceInflo$DAB[BaseDeCroissanceInflo$codeUC==i],lty="dashed",col="black",type="l")
abline(h=baseAJUSTinflo$A0[baseAJUSTinflo$a==i]*10/100) #trace droite des 10%
par(new=T)
plot(BaseDeCroissanceInflo$AGRI[BaseDeCroissanceInflo$codeUC==i]~BaseDeCroissanceInflo$DAB[BaseDeCroissanceInflo$codeUC==i],bty="n",col="blue",type="l",pch=20,xaxt="n", yaxt="n",xlab="",ylab="",ylim=c(0,maxAGR),xlim=c(0,maxDAB))
axis(4)
mtext("Absolute Growth Rate: d(length)/d(DAB)",side=4,line=2,cex=1,col="blue")
par(new=T)
plot(BaseDeCroissanceInflo$RGRI[BaseDeCroissanceInflo$codeUC==i]~BaseDeCroissanceInflo$DAB[BaseDeCroissanceInflo$codeUC==i],bty="n",col="red",type="l",pch=20,xaxt="n", yaxt="n",xlab="",ylab="",ylim=c(0,maxRGR),xlim=c(0,maxDAB))
points(BaseDeCroissanceInflo$RGRIsim[BaseDeCroissanceInflo$codeUC==i]~BaseDeCroissanceInflo$DAB[BaseDeCroissanceInflo$codeUC==i],lty="dashed",col="red",type="l")
axis(4,line=3.5)
mtext("Relative Growth Rate: d(ln(length))/d(DAB)",side=4,line=5.5,cex=1,col="red") }

dev.off()		
		
		
##### max AGR obs vs max AGR sim
maxAGR<-NA
compt<-0

for (i in unique(BaseDeCroissanceInflo$codeUC))
{
	compt<-compt+1
	maxAGR[compt]<-max(BaseDeCroissanceInflo$AGRI[BaseDeCroissanceInflo$codeUC==i],na.rm=TRUE)
}

baseAJUSTinflo$maxAGR<-maxAGR
head(baseAJUSTinflo)
plot(baseAJUSTinflo$maxAGR~baseAJUSTinflo$ER0,xlab="max AGR fitted",ylab="max AGR obs",bty="n",main="Inflorescences cogshall & josé")
anova(lm(baseAJUSTinflo$maxAGR~baseAJUSTinflo$ER0))
abline(lm(baseAJUSTinflo$maxAGR~baseAJUSTinflo$ER0)$coef)
text(1.5,1,"P< 2.2e-16 ***")
	
		
		
#### RGR et température		

baseAJUSTinflo$variete<-as.factor(baseAJUSTinflo$variete)
plot(baseAJUSTinflo$RGR50sim~baseAJUSTinflo$Tm50,col=c("blue","red")[baseAJUSTinflo$variete],pch=20,main="INFLOS",bty="n",xlab="Mean Temperature from budburst to inflexion point",ylab="RGR at inflexion point from fitted curve")

summary(lm(RGR50sim~Tm50,data=cog))
summary(lm(RGR50sim~Tm50,data=jose))

anova(lm(baseAJUSTinflo$RGR50sim~baseAJUSTinflo$Tm50*baseAJUSTinflo$variete))
anova(lm(baseAJUSTinflo$RGR50sim~baseAJUSTinflo$Tm50+baseAJUSTinflo$variete))
summary(lm(baseAJUSTinflo$RGR50sim~baseAJUSTinflo$Tm50))
abline(lm(baseAJUSTinflo$RGR50sim~baseAJUSTinflo$Tm50)$coef)
text(22,0.2,"P < 2e-16 ***, slope=0.00996")
plot(baseAJUSTinflo$ER0~baseAJUSTinflo$Tm50,col=c("blue","red")[baseAJUSTinflo$variete],pch=20,bty="n",xlab="Mean Temperature from budburst to inflexion point",ylab="AGR at inflexion point from fitted curve")
		
		
		
### RGR & nb jours
jose<-baseAJUSTinflo[baseAJUSTinflo$variete=="jose",]
cog<-baseAJUSTinflo[baseAJUSTinflo$variete=="cog",]

plot(rnorm(length(baseAJUSTinflo$ndim90),mean=baseAJUSTinflo$ndim90,sd=0.2)~baseAJUSTinflo$RGR50sim,col=c("blue","red")[baseAJUSTinflo$variete],pch=20,main="Inflo",bty="n",xlab="RGR at inflexion point from fitted curve",ylab="Number of days")

summary(lm(ndim90~RGR50sim,data=cog))
summary(lm(ndim90~RGR50sim,data=jose))

summary(lm(ndim90~RGR50sim*variete,data=baseAJUSTinflo))		
		
		
		
#lm en log
plot(rnorm(length(log(baseAJUSTinflo$ndim90)),mean=log(baseAJUSTinflo$ndim90),sd=0.2)~log(baseAJUSTinflo$RGR50sim),col=c("blue","red")[baseAJUSTinflo$variete],pch=20,main="log Inflo",bty="n",xlab="log RGR at inflexion point from fitted curve",ylab="log Number of days")
summary(lm(log(baseAJUSTinflo$ndim90)~log(baseAJUSTinflo$RGR50sim)*variete,data=baseAJUSTinflo))
summary(lm(log(baseAJUSTinflo$ndim90)~log(baseAJUSTinflo$RGR50sim)+variete,data=baseAJUSTinflo))
abline(a= 0.97723  -0.07320 , b= -0.97404, col="blue")
abline(a= 0.97723  +0.07320 , b= -0.97404, col="red")
text(-1.8,3.5,"log y ~ log x P< 2e-16 ***")
text(-1.8,3.4,"Same slope: P=0.053")
text(-1.8,3.3,"diff intercept P=   1.49e-08")		
		
		
		
		
		
		
		
		


### Tm50~Tm90
Tm50<-c(baseAJUST$Tm50,baseAJUSTF$Tm50,baseAJUSTinflo$Tm50)
Tm90<-c(baseAJUST$Tm90,baseAJUSTF$Tm90,baseAJUSTinflo$Tm90)

plot(Tm90~Tm50,bty="n",xlab="Tm50",ylab="Tm90",xlim=c(15,30),ylim=c(15,30),cex.axis=1.5,cex.lab=1.5)
summary(lm(Tm90~Tm50)) # P < 2 -16, R² = 0.9919
abline(a=lm(Tm90~Tm50)$coeff[1],b=lm(Tm90~Tm50)$coeff[2])
text(x=18,y =29, "R² = 0.99, P < 0.001",cex=1.5)
cor(y=Tm50,x=Tm90,use="complete.obs")









####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

		
		
		
		
		
		
		
		
		
		
		
		
		
		

#########################
###### AXE / FEUILLE
#########################

### Dimensions
correspondance<-match(baseAJUSTF$codeUC0,baseAJUST$a)
pos<-as.factor(as.character(baseAJUSTF$posF))
posUC<-as.factor(as.character(baseAJUSTF$posUC))
posUCm<-as.factor(as.character(baseAJUSTF$posUCm))
variete<-as.factor(as.character(baseAJUSTF$variete))
dimF<-baseAJUSTF$dim90
dimAxe<-baseAJUST$dim90[correspondance]
nF<-baseAJUSTF$ndim90
nAxe<-baseAJUST$ndim90[correspondance]
DATA<-data.frame(variete,pos,posUC,posUCm,dimF,dimAxe,nF,nAxe)

plot(dimF~dimAxe,data=DATA,col=c("blue","red")[variete],main="Dependance dimensions Axe / Feuille",pch=20,bty="n",xlab="90% long axe",ylab="90% surf F")
legend("topleft",c("cogshall" , "jose"),pch=20,col=c("blue","red"))

jose<-DATA[DATA$variete=="jose",]
cog<-DATA[DATA$variete=="cog",]

summary(lm(dimF~dimAxe,data=cog))
summary(lm(dimF~dimAxe,data=jose))
abline(lm(dimF~dimAxe,data=jose)$coef,col="red")
text(20,100,"Cogshall: NS, José: P=0.00154 **")


#Effet de la position sur dimF~dimAxe
prox<-cog[cog$pos=="prox",]
dist<-cog[cog$pos=="dist",]
first<-cog[cog$pos=="F1",]
summary(lm(dimF~dimAxe,data=prox))
summary(lm(dimF~dimAxe,data=dist))
summary(lm(dimF~dimAxe,data=first))

prox<-jose[jose$pos=="prox",]
dist<-jose[jose$pos=="dist",]
first<-jose[jose$pos=="F1",]
summary(lm(dimF~dimAxe,data=prox))
summary(lm(dimF~dimAxe,data=dist))
summary(lm(dimF~dimAxe,data=first))


### nb jours

plot(rnorm(length(nF),mean=nF,sd=0.2)~rnorm(length(nAxe),mean=nAxe,sd=0.2),data=DATA,col=c("blue","red")[variete],main="Dependance temporelle Axe / Feuille",pch=20,bty="n",xlab="nb jour axe",ylab="nb jour F")
legend("topleft",c("cogshall" , "jose"),pch=20,col=c("blue","red"))
abline(a=0,b=1)

jose<-DATA[DATA$variete=="jose",]
cog<-DATA[DATA$variete=="cog",]

summary(lm(nF~nAxe,data=cog))
summary(lm(nF~nAxe,data=jose))

summary(lm(nF~nAxe*variete,data=DATA))
summary(lm(nF~nAxe+variete,data=DATA))

abline(a=2.34845 +1.33534 ,b=1.02959 ,col="blue")
abline(a=2.34845 -1.33534 ,b=1.02959 ,col="red")

#Effet de la position sur nF~nAxe
prox<-cog[cog$pos=="prox",]
dist<-cog[cog$pos=="dist",]
first<-cog[cog$pos=="F1",]
summary(lm(nF~nAxe,data=prox))
summary(lm(nF~nAxe,data=dist))
summary(lm(nF~nAxe,data=first))

prox<-jose[jose$pos=="prox",]
dist<-jose[jose$pos=="dist",]
first<-jose[jose$pos=="F1",]
summary(lm(nF~nAxe,data=prox))
summary(lm(nF~nAxe,data=dist))
summary(lm(nF~nAxe,data=first))







### nmax ER
correspondance<-match(baseAJUSTF$codeUC0,baseAJUST$a)
pos<-as.factor(as.character(baseAJUSTF$posF))
variete<-as.factor(as.character(baseAJUSTF$variete))
ERF<-baseAJUSTF$ER0
ERAxe<-baseAJUST$ER0[correspondance]
DATA<-data.frame(variete,pos,ERF,ERAxe)


plot(ERF~ERAxe,data=DATA,col=c("blue","red")[variete],main="Dependance ER Axe / Feuille",pch=20,bty="n",xlab="max ER axe",ylab="max ER F")
legend("topleft",c("cogshall" , "jose"),pch=20,col=c("blue","red"))

jose<-DATA[DATA$variete=="jose",]
cog<-DATA[DATA$variete=="cog",]

summary(lm(ERF~ERAxe,data=cog))
summary(lm(ERF~ERAxe,data=jose))
abline(lm(ERF~ERAxe,data=jose)$coef,col="red")

text(3.5,20,"Cogshall: NS, José: P=7.39 10-4")

#Effet de la position sur ERF~ERAxe
prox<-cog[cog$pos=="prox",]
dist<-cog[cog$pos=="dist",]
first<-cog[cog$pos=="F1",]
summary(lm(ERF~ERAxe,data=prox))
summary(lm(ERF~ERAxe,data=dist))
summary(lm(ERF~ERAxe,data=first))

prox<-jose[jose$pos=="prox",]
dist<-jose[jose$pos=="dist",]
first<-jose[jose$pos=="F1",]
summary(lm(ERF~ERAxe,data=prox))
summary(lm(ERF~ERAxe,data=dist))
summary(lm(ERF~ERAxe,data=first))


### RGR at inflexion point
correspondance<-match(baseAJUSTF$codeUC0,baseAJUST$a)
pos<-as.factor(as.character(baseAJUSTF$posF))
variete<-as.factor(as.character(baseAJUSTF$variete))
RGRF<-baseAJUSTF$RGR50sim
RGRAxe<-baseAJUST$RGR50sim[correspondance]
DATA<-data.frame(variete,pos,RGRF,RGRAxe)

plot(RGRF~RGRAxe,data=DATA,col=c("blue","red")[variete],main="Dependance RGR Axe / Feuille",pch=20,bty="n",xlab="RGR axe",ylab="RGR F")
legend("topleft",c("cogshall" , "jose"),pch=20,col=c("blue","red"))

jose<-DATA[DATA$variete=="jose",]
cog<-DATA[DATA$variete=="cog",]

summary(lm(RGRF~RGRAxe,data=cog))
summary(lm(RGRF~RGRAxe,data=jose))

summary(lm(RGRF~RGRAxe*variete,data=DATA))
#Pentes différentes


#Effet de la position sur RGRF~RGRAxe
prox<-cog[cog$pos=="prox",]
dist<-cog[cog$pos=="dist",]
first<-cog[cog$pos=="F1",]
summary(lm(RGRF~RGRAxe,data=prox))
summary(lm(RGRF~RGRAxe,data=dist))
summary(lm(RGRF~RGRAxe,data=first))

prox<-jose[jose$pos=="prox",]
dist<-jose[jose$pos=="dist",]
first<-jose[jose$pos=="F1",]
summary(lm(RGRF~RGRAxe,data=prox))
summary(lm(RGRF~RGRAxe,data=dist))
summary(lm(RGRF~RGRAxe,data=first))





#Scatterplot
correspondance<-match(baseAJUSTF$codeUC0,baseAJUST$a)
pos<-as.factor(as.character(baseAJUSTF$posF))
variete<-as.factor(as.character(baseAJUSTF$variete))
ERF<-baseAJUSTF$ER0
ERAxe<-baseAJUST$ER0[correspondance]
nF<-baseAJUSTF$ndim90
nAxe<-baseAJUST$ndim90[correspondance]
dimF<-baseAJUSTF$dim90
dimAxe<-baseAJUST$dim90[correspondance]
DATA<-data.frame(pos,variete,ERF,ERAxe,nF,nAxe,dimF,dimAxe)
levels(DATA$pos)[3]<-"dist"

jose<-DATA[DATA$variete=="jose",]
cog<-DATA[DATA$variete=="cog",]

library(car)

scatterplotMatrix(~ERF+ERAxe+nF+nAxe+dimF+dimAxe|pos,data=jose,diagonal="none",main="JOSE Axe vs. Feuilles",smooth=FALSE,reg.line=FALSE,by.group=TRUE,pch=c(20,20,20),col=c("green","red","blue"),var.labels=c("ER feuilles","ER axe","nb jours F","nb jours axe","dimension F","dimension A"),cex.labels=2)
scatterplotMatrix(~ERF+ERAxe+nF+nAxe+dimF+dimAxe|pos,data=cog,diagonal="none",main="COG Axe vs. Feuilles",smooth=FALSE,reg.line=FALSE,by.group=TRUE,pch=c(20,20,20),col=c("green","red","blue"),var.labels=c("ER feuilles","ER axe","nb jours F","nb jours axe","dimension F","dimension A"),cex.labels=2)


#effet de la position de l'UC
jose<-DATA[DATA$variete=="jose",]
cog<-DATA[DATA$variete=="cog",]

summary(lm(dimF~dimAxe*posUC,data=jose))
summary(lm(dimF~dimAxe+posUC,data=jose))

summary(lm(dimF~dimAxe*posUC,data=cog))
summary(lm(dimF~dimAxe+posUC,data=cog))
summary(lm(dimF~dimAxe,data=cog))

summary(lm(nF~nAxe*posUC,data=jose))
summary(lm(nF~nAxe+posUC,data=jose))

summary(lm(nF~nAxe*posUC,data=cog))
summary(lm(nF~nAxe+posUC,data=cog))
summary(lm(nF~nAxe,data=cog))


#effet de la position de l'UC mère
jose<-DATA[DATA$variete=="jose",]
cog<-DATA[DATA$variete=="cog",]

summary(lm(dimF~dimAxe*posUCm,data=jose))
summary(lm(dimF~dimAxe+posUCm,data=jose))
summary(lm(dimF~dimAxe,data=jose))

summary(lm(dimF~dimAxe*posUCm,data=cog))
summary(lm(dimF~dimAxe+posUCm,data=cog))
summary(lm(dimF~dimAxe,data=cog))

summary(lm(nF~nAxe*posUCm,data=jose))
summary(lm(nF~nAxe+posUCm,data=jose))
summary(lm(nF~nAxe,data=jose))

summary(lm(nF~nAxe*posUCm,data=cog))#0.00845 ** 












####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################













###########################################
################Modèles globaux
##########################################

#dim90~Tm90*variete*type #Axes
#AGR~Tm90*variete*type #Axes
#ndim90~Tm90*variete*type #Axes + Feuilles
#RGR~Tm90*variete*type #Axes + Feuilles

#Création de la  base
dim90A<-baseAJUST$dim90
ndim90A<-baseAJUST$ndim90
ER0A<-baseAJUST$ER0
RGRipA<-baseAJUST$RGRip
Tm90A<-baseAJUST$Tm90
Tm50A<-baseAJUST$Tm50
typeA<-rep("axisGU",length(Tm50A))
varieteA<-as.factor(as.character(baseAJUST$variete))
posA<-rep(NA,length(Tm50A))
codeA<-baseAJUST$a
GUA<-as.numeric(codeA)

dim90L<-baseAJUSTF$dim90
ndim90L<-baseAJUSTF$ndim90
ER0L<-baseAJUSTF$ER0
RGRipL<-baseAJUSTF$RGRip
Tm90L<-baseAJUSTF$Tm90
Tm50L<-baseAJUSTF$Tm50
typeL<-rep("leafGU",length(Tm50L))
varieteL<-as.factor(as.character(baseAJUSTF$variete))
posL<-as.factor(as.character(baseAJUSTF$posF))
codeL<-baseAJUSTF$codeUC0
correspondance<-match(codeL,codeA)
GUL<-GUA[correspondance]

dim90I<-baseAJUSTinflo$dim90
ndim90I<-baseAJUSTinflo$ndim90
ER0I<-baseAJUSTinflo$ER0
RGRipI<-baseAJUSTinflo$RGRip
Tm90I<-baseAJUSTinflo$Tm90
Tm50I<-baseAJUSTinflo$Tm50
typeI<-rep("axisINFLO",length(Tm50I))
varieteI<-as.factor(as.character(baseAJUSTinflo$variete))
posI<-rep(NA,length(Tm50I))
codeI<-baseAJUSTinflo$a
GUI<-rep(NA,length(Tm50I))

dim90<-c(dim90A,dim90L,dim90I)
ndim90<-c(ndim90A,ndim90L,ndim90I)
ER0<-c(ER0A,ER0L,ER0I)
RGRip<-c(RGRipA,RGRipL,RGRipI)
Tm90<-c(Tm90A,Tm90L,Tm90I)
Tm50<-c(Tm50A,Tm50L,Tm50I)
type<-c(typeA,typeL,typeI)
variete<-as.factor(c(as.character(varieteA),as.character(varieteL),as.character(varieteI)))
pos<-as.factor(c(as.character(posA),as.character(posL),as.character(posI)))
code<-as.factor(c(as.character(codeA),as.character(codeL),as.character(codeI)))
GU<-as.factor(c(as.character(GUA),as.character(GUL),as.character(GUI)))
type2<-as.factor(paste(type,pos))
DATA<-data.frame(variete,type,type2,pos,code,GU,dim90,ndim90,ER0,RGRip,Tm90,Tm50)


############
## Dim 90
############
options(contrasts=c("contr.sum","contr.poly"))     
library(car)
subDATA<-DATA[which(DATA$type=="axisINFLO"|DATA$type=="axisGU"),]

dim90<-subDATA$dim90

Tm90<-subDATA$Tm90

cog<-subDATA$variete=="cog"
cog<-ifelse(cog==TRUE,1,0) 
cog<-as.factor(cog)
cog<-relevel(cog,"1")

jose<-subDATA$variete=="jose"
jose<-ifelse(jose==TRUE,1,0)
jose<-as.factor(jose)
jose<-relevel(jose,"1")

axisGU<-subDATA$type=="axisGU"
axisGU<-ifelse(axisGU==TRUE,1,0) 
axisGU<-as.factor(axisGU)
axisGU<-relevel(axisGU,"1")

axisINFLO<-subDATA$type=="axisINFLO"
axisINFLO<-ifelse(axisINFLO==TRUE,1,0) 
axisINFLO<-as.factor(axisINFLO)
axisINFLO<-relevel(axisINFLO,"1")

mod<-lm(dim90~Tm90+cog+jose+axisGU+axisINFLO+
		Tm90:cog+Tm90:jose+Tm90:axisGU+Tm90:axisINFLO+
		cog:axisGU+cog:axisINFLO+
		jose:axisGU+jose:axisINFLO+
		Tm90:cog:axisGU)
Anova(mod,test.statistic="Wald", type="III")
summary(mod)
mod<-lm(dim90~Tm90+cog+axisGU+
		Tm90:cog+Tm90:axisGU+
		cog:axisGU)
Anova(mod,test.statistic="Wald", type="III")
mod<-lm(dim90~Tm90+cog+axisGU+
		Tm90:axisGU+
		cog:axisGU)
Anova(mod,test.statistic="Wald", type="III")
summary(mod)



mod2=lm(dim90~Tm90*variete*type2,data=DATA)
Anova(mod2,test.statistic="Wald", type="III")
mod2=lm(dim90~Tm90+variete+type2+Tm90:variete+Tm90:type2+variete:type2+Tm90:variete:type2,data=DATA)
Anova(mod2,test.statistic="Wald", type="III")
mod2=lm(dim90~variete+Tm90:variete+Tm90:type2+variete:type2+Tm90:variete:type2,data=DATA)
Anova(mod2,test.statistic="Wald", type="III")
mod2=lm(dim90~variete+Tm90:variete+Tm90:type2+variete:type2,data=DATA)
Anova(mod2,test.statistic="Wald", type="III")
summary(mod2)
extractAIC(mod2)

library(lme4)
mod3=lmer(dim90~variete+Tm90:variete+Tm90:type2+variete:type2+(1|GU),REML=FALSE,na.action=na.omit,data=DATA)
Anova(mod3,test.statistic="Wald", type="III")
summary(mod3)
extractAIC(mod3)
mod3=lmer(dim90~variete+Tm90:variete+Tm90:type2+variete:type2+(0+Tm90|GU),REML=FALSE,na.action=na.omit,data=DATA)
Anova(mod3,test.statistic="Wald", type="III")
summary(mod3)
extractAIC(mod3)

cog<-DATA$variete=="cog"
cog<-ifelse(cog==TRUE,1,0) 
jose<-DATA$variete=="jose"
jose<-ifelse(jose==TRUE,1,0) 
axisGU<-DATA$type=="axisGU"
axisGU<-ifelse(axisGU==TRUE,1,0) 
axisINFLO<-DATA$type=="axisINFLO"
axisINFLO<-ifelse(axisINFLO==TRUE,1,0) 
leafGU<-DATA$type=="leafGU"
leafGU<-ifelse(leafGU==TRUE,1,0) 

summary(lm(DATA$dim90~DATA$Tm90+jose+cog+axisGU+axisINFLO+
			DATA$Tm90:jose+DATA$Tm90:cog+DATA$Tm90:axisGU+DATA$Tm90:axisINFLO+
			jose:axisGU+jose:axisINFLO+
			cog:axisGU+cog:axisINFLO+
			Tm90:jose:axisGU
			))

mod2=lm(dim90~Tm90+leafGUdist+leafGUdist+leafGUprox+axisGU+jose+cog+Tm90:jose+Tm90:cog+Tm90:axisGU+Tm90:leafGUdist+Tm90:leafGU1+Tm90:leafGUprox+jose:axisGU+jose:leafGUdist+jose:leafGU1+jose:leafGUprox+cog:axisGU+cog:leafGUdist+cog:leafGU1+cog:leafGUprox)
Anova(mod2,test.statistic="Wald", type="III")
summary(mod2)
mod2=lm(dim90~jose+Tm90:jose+Tm90:cog+Tm90:axisGU+Tm90:leafGUdist+Tm90:leafGU1+jose:axisGU+jose:leafGUdist+jose:leafGU1+cog:axisGU+cog:leafGUdist+cog:leafGU1)
Anova(mod2,test.statistic="Wald", type="III")
mod2=lm(dim90~jose+Tm90:jose+Tm90:cog+Tm90:axisGU+Tm90:leafGUdist+Tm90:leafGU1+jose:leafGUdist+jose:leafGU1+cog:axisGU+cog:leafGUdist+cog:leafGU1)
Anova(mod2,test.statistic="Wald", type="III")
mod2=lm(dim90~jose+Tm90:jose+Tm90:cog+Tm90:axisGU+Tm90:leafGUdist+jose:leafGUdist+jose:leafGU1+cog:axisGU+cog:leafGUdist+cog:leafGU1)
Anova(mod2,test.statistic="Wald", type="III")
mod2=lm(dim90~jose+Tm90:jose+Tm90:cog+Tm90:axisGU+jose:leafGUdist+jose:leafGU1+cog:axisGU+cog:leafGUdist+cog:leafGU1)
Anova(mod2,test.statistic="Wald", type="III")
summary(mod2)


############
## nDim 90
############
options(contrasts=c("contr.sum","contr.poly"))     
library(car)
subDATA<-DATA[which(DATA$type=="axisINFLO"|DATA$type=="axisGU"),]

mod2=lm(ndim90~Tm90*variete*type2,data=DATA)
Anova(mod2,test.statistic="Wald", type="III")
mod2=lm(ndim90~Tm90+variete+type2+Tm90:variete+Tm90:type2+variete:type2+Tm90:variete:type2,data=DATA)
Anova(mod2,test.statistic="Wald", type="III")
mod2=lm(ndim90~Tm90+variete+type2+Tm90:variete+Tm90:type2+variete:type2,data=DATA)
Anova(mod2,test.statistic="Wald", type="III")
mod2=lm(ndim90~Tm90+variete+type2+Tm90:type2+variete:type2,data=DATA)
Anova(mod2,test.statistic="Wald", type="III")
summary(mod2)
summary(lm(ndim90[variete=="cog"&type=="axisGU"]~Tm90[variete=="cog"& type=="axisGU"],data=DATA))
summary(lm(ndim90~Tm90*variete*type,data=DATA))

ndim90<-subDATA$ndim90
Tm90<-subDATA$Tm90
cog<-subDATA$variete=="cog"
cog<-ifelse(cog==TRUE,1,0) 
jose<-subDATA$variete=="jose"
jose<-ifelse(jose==TRUE,1,0) 
axisGU<-subDATA$type=="axisGU"
axisGU<-ifelse(axisGU==TRUE,1,0) 
axisINFLO<-subDATA$type=="axisINFLO"
axisINFLO<-ifelse(axisINFLO==TRUE,1,0) 

mod<-lm(ndim90~Tm90+cog+jose+axisGU+axisINFLO+
		Tm90:cog+Tm90:jose+Tm90:axisGU+Tm90:axisINFLO+
		cog:axisGU+cog:axisINFLO+
		jose:axisGU+jose:axisINFLO)
Anova(mod,test.statistic="Wald", type="III")
summary(mod)
mod<-lm(ndim90~Tm90+cog+axisGU+
		Tm90:cog+Tm90:axisGU+
		cog:axisGU)
Anova(mod,test.statistic="Wald", type="III")
mod<-lm(ndim90~Tm90+cog+axisGU+
		Tm90:axisGU+
		cog:axisGU)
Anova(mod,test.statistic="Wald", type="III")
summary(mod)












