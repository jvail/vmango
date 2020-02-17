##################
#Chargement des fichiers de température
##################
#On charge les fichiers de température:
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

################################################
########### VEGETATIF##########################
################################################

#################### COGSHALL


## ne pas oublier de changer TB en fonction variete et organe considéré


#############Calcul des ST pour cogshall
source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/calcul_des_surfaces_foliaires.R")
BaseDeCroissance<-BaseDeCroissance[BaseDeCroissance$variete=="cog",]
BaseDeCroissance$codeUC <- as.character(BaseDeCroissance$codeUC)
BaseDeCroissance$codeUC <- as.factor(BaseDeCroissance$codeUC)


x <- c(9.52)
# PARAMETRES INITIAUX
valmin <- x[1]      # température de seuil, de base statistique : Le paramètre qu'on veut estimer
#valmin c'est x
print(valmin)


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


#########################################
########### Calcul pour chaque UC des ddj
#########################################

#On sépare les UC:
rep2<-split(BaseDeCroissance,BaseDeCroissance$codeUC)
dj<<-list() #Tableau de nb de jour depuis débourrement associé à la somme des températures
j<-0
m<-NULL  #Temp moy par jour

lapply(rep2,function(x){

#On défini le fichier température à choisir pour chaque UC
dataT<-NULL             #On prend que le 1er car répétitions du mm verger!
if(x$verger[1]=="BM") {dataT<-TBMmoy}    
if(x$verger[1]=="BP") {dataT<-TBPmoy}
if(x$verger[1]=="GH") {dataT<-TGHmoy}
if(x$verger[1]=="GF") {dataT<-TGFmoy}
#vérif OK : il choisi les bon fichier température pour chaque UC

Ddeb<-unique(x$vec2) #Date de débourrement de l'UC
j<<-j+1 # (j + 1)
DAB<-difftime(dataT$datesUnik,strptime(Ddeb,"%d/%m/%y"),units="days")		# Dates en nb de jours après débourrement
#Dans le fichier température on a les dates avant et apres les dates de débourrement
t<-length(which(DAB<0))+1   # 1ere valeur pour calculer la somme ddj
for (i in (t:nrow(dataT)))   #Calcul pour tout le fichier de température
{if  (dataT$tempMoy[i]>valmin)  m[i-t+1]=dataT$tempMoy[i]-valmin  else m[i-t+1]=0  } # Calcul de la Tm-Tb
#[i-t+1] sert à commencer à remplir le m à 1: t: DAB=0
#m temp par jour
calcddj <- as.vector(cumsum(m))	# Calcul de la somme de ddj
#creation de dj
DAB<-DAB[t:length(DAB)]
dj[[j]] <<- cbind(DAB,calcddj)  #dj va de 0 à x; UC l'une après l'autre
})

#On match les dj avec les données de la base en fonction des DAB
#Délimination de dj et creation du fichier d avec les codes UCs associés
DABmax<-max(unlist(lapply(dj,function(x)max(x[,"DAB"]))),na.rm=TRUE)
DABmin<-max(unlist(lapply(dj,function(x)min(x[,"DAB"]))),na.rm=TRUE)
DAB<-DABmin:DABmax
#On prend le min et le max. les sommes de temp sont calculées pour plus que nécessaire
#On selectionnera les bonnes dates ( la fin surtout) ensuite.
DAB<-data.frame(DAB)
colnames(DAB)<-"DAB"
#On fait le 1ère colonne : on mix DAB et dj ( de min à max DAB)
d<-merge(dj[[1]],DAB,by="DAB",sort=TRUE)
#On fait pour les colonnes suivantes:
for (i in 2:length(dj)){
d<-merge(d,dj[[i]],by.x="DAB",by.y="DAB",sort=TRUE)}
#On renomme les colonnes: 
colnames(d)<-c("DAB",names(rep2))
#head(d)
rownames(d)<-d[,1] #d: UC l'une a coté de l'autre en data.frame avec code UC en nom de colonne de 0 à x (DAB)


# préparation jeu de données observé
croiss<-data.frame()
croiss<-lapply(rep2,function(x){
croiss<<-rbind.data.frame(croiss,x)})

croiss<-croiss[[length(croiss)]]
#head(croiss)
#head(d)

BaseFin<-data.frame()
for (i in 1:nlevels(croiss$codeUC)){
don <- croiss[croiss$codeUC==levels(croiss$codeUC)[i],]  #selection des données pour cette UC
DAB <- don$DAB  #Selection des DAB de cette UC
ddj <- d[match(DAB,rownames(d)),which(colnames(d)==levels(croiss$codeUC)[i])] # on calcul les ddj associés aux DAB
#On match ces DAB avec le fichier de somme de temp (d) associé à cet UC
don <- cbind(don,ddj) #On rajoute les ddj dans le fichier don
BaseFin<-rbind(BaseFin,don)
} 


################PLot pour les UC
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$longueurUC[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],type="l",ylim=c(0,30),xlab="Days After Burst", ylab="long UC",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$longueurUC[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$longueurUC[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],type="l",ylim=c(0,30),xlab="ddj °C", ylab="long UC",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$longueurUC[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}


################PLot pour les Feuilles
#Fprox
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFprox[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],type="l",ylim=c(0,150),xlab="Days After Burst", ylab="surf Fprox",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFprox[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFprox[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],type="l",ylim=c(0,150),xlab="ddj °C", ylab="surf Fprox",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFprox[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

#Fdist
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFdist[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],type="l",ylim=c(0,100),xlab="Days After Burst", ylab="surf Fdist",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFdist[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFdist[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],type="l",ylim=c(0,100),xlab="ddj °C", ylab="surf Fdist",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFdist[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

#F1
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surF1[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],type="l",ylim=c(0,100),xlab="Days After Burst", ylab="surf F1",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surF1[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surF1[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],type="l",ylim=c(0,100),xlab="ddj °C", ylab="surf F1",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surF1[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

#Fplus
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFplus[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],type="l",ylim=c(0,100),xlab="Days After Burst", ylab="surf Fplus",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFplus[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFplus[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],type="l",ylim=c(0,100),xlab="ddj °C", ylab="surf Fplus",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFplus[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}






 ##################################
 ########### JOSE
 #################################
### Ne pas oublier de changer la TB


#############Calcul des ST pour jose
source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/calcul_des_surfaces_foliaires.R")
BaseDeCroissance<-BaseDeCroissance[BaseDeCroissance$variete=="jose",]
BaseDeCroissance$codeUC <- as.character(BaseDeCroissance$codeUC)
BaseDeCroissance$codeUC <- as.factor(BaseDeCroissance$codeUC)


x <- c(-0.42)
# PARAMETRES INITIAUX
valmin <- x[1]      # température de seuil, de base statistique : Le paramètre qu'on veut estimer
#valmin c'est x
print(valmin)


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


#########################################
########### Calcul pour chaque UC des ddj
#########################################

#On sépare les UC:
rep2<-split(BaseDeCroissance,BaseDeCroissance$codeUC)
dj<<-list() #Tableau de nb de jour depuis débourrement associé à la somme des températures
j<-0
m<-NULL  #Temp moy par jour

lapply(rep2,function(x){

#On défini le fichier température à choisir pour chaque UC
dataT<-NULL             #On prend que le 1er car répétitions du mm verger!
if(x$verger[1]=="BM") {dataT<-TBMmoy}    
if(x$verger[1]=="BP") {dataT<-TBPmoy}
if(x$verger[1]=="GH") {dataT<-TGHmoy}
if(x$verger[1]=="GF") {dataT<-TGFmoy}
#vérif OK : il choisi les bon fichier température pour chaque UC

Ddeb<-unique(x$vec2) #Date de débourrement de l'UC
j<<-j+1 # (j + 1)
DAB<-difftime(dataT$datesUnik,strptime(Ddeb,"%d/%m/%y"),units="days")		# Dates en nb de jours après débourrement
#Dans le fichier température on a les dates avant et apres les dates de débourrement
t<-length(which(DAB<0))+1   # 1ere valeur pour calculer la somme ddj
for (i in (t:nrow(dataT)))   #Calcul pour tout le fichier de température
{if  (dataT$tempMoy[i]>valmin)  m[i-t+1]=dataT$tempMoy[i]-valmin  else m[i-t+1]=0  } # Calcul de la Tm-Tb
#[i-t+1] sert à commencer à remplir le m à 1: t: DAB=0
#m temp par jour
calcddj <- as.vector(cumsum(m))	# Calcul de la somme de ddj
#creation de dj
DAB<-DAB[t:length(DAB)]
dj[[j]] <<- cbind(DAB,calcddj)  #dj va de 0 à x; UC l'une après l'autre
})

#On match les dj avec les données de la base en fonction des DAB
#Délimination de dj et creation du fichier d avec les codes UCs associés
DABmax<-max(unlist(lapply(dj,function(x)max(x[,"DAB"]))),na.rm=TRUE)
DABmin<-max(unlist(lapply(dj,function(x)min(x[,"DAB"]))),na.rm=TRUE)
DAB<-DABmin:DABmax
#On prend le min et le max. les sommes de temp sont calculées pour plus que nécessaire
#On selectionnera les bonnes dates ( la fin surtout) ensuite.
DAB<-data.frame(DAB)
colnames(DAB)<-"DAB"
#On fait le 1ère colonne : on mix DAB et dj ( de min à max DAB)
d<-merge(dj[[1]],DAB,by="DAB",sort=TRUE)
#On fait pour les colonnes suivantes:
for (i in 2:length(dj)){
d<-merge(d,dj[[i]],by.x="DAB",by.y="DAB",sort=TRUE)}
#On renomme les colonnes: 
colnames(d)<-c("DAB",names(rep2))
#head(d)
rownames(d)<-d[,1] #d: UC l'une a coté de l'autre en data.frame avec code UC en nom de colonne de 0 à x (DAB)


# préparation jeu de données observé
croiss<-data.frame()
croiss<-lapply(rep2,function(x){
croiss<<-rbind.data.frame(croiss,x)})

croiss<-croiss[[length(croiss)]]
#head(croiss)
#head(d)

BaseFin<-data.frame()
for (i in 1:nlevels(croiss$codeUC)){
don <- croiss[croiss$codeUC==levels(croiss$codeUC)[i],]  #selection des données pour cette UC
DAB <- don$DAB  #Selection des DAB de cette UC
ddj <- d[match(DAB,rownames(d)),which(colnames(d)==levels(croiss$codeUC)[i])] # on calcul les ddj associés aux DAB
#On match ces DAB avec le fichier de somme de temp (d) associé à cet UC
don <- cbind(don,ddj) #On rajoute les ddj dans le fichier don
BaseFin<-rbind(BaseFin,don)
} 


################PLot pour les UC
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$longueurUC[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],type="l",ylim=c(0,40),xlab="Days after Burst UC josé",ylab="longueur UC josé",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$longueurUC[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$longueurUC[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],type="l",ylim=c(0,40),xlab="Days after Burst UC josé",ylab="ddj °C",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$longueurUC[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}


################PLot pour les Feuilles
#Fprox
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFprox[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],type="l",ylim=c(0,150),xlab="Days after Burst feuill josé",ylab="surf feuill josé",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFprox[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

x11()
### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFprox[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],type="l",ylim=c(0,150),xlab="ddj °C",ylab="surf feuill josé",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFprox[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

#Fdist
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFdist[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],type="l",ylim=c(0,50),xlab="Days after Burst Fdist josé",ylab="surf Fdist josé",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFdist[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

x11()
### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFdist[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],type="l",ylim=c(0,50),xlab="ddj ° C Fdist josé",ylab="surf Fdist josé",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFdist[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

#F1
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surF1[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],ylim=c(0,50),type="l",xlab="Days after Burst F1 josé",ylab="surf F1 josé",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surF1[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

x11()
### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surF1[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],ylim=c(0,50),type="l",xlab="ddj °C F1 josé",ylab="surf F1 josé",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surF1[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

#Fplus
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFplus[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],ylim=c(0,50),type="l",xlab="Days after Burst Fplus josé",ylab="surf Fplus josé",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFplus[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

x11()
### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$surFplus[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],ylim=c(0,50),type="l",xlab="ddj °C Fplus josé",ylab="surf Fplus josé",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$surFplus[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}




################################################
########### FLORIFERE ##########################
################################################

#################### COGSHALL


## ne pas oublier de changer TB en fonction variete et organe considéré


#############Calcul des ST pour cogshall
source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Preparation Base Inflo.R")
BaseDeCroissance<-BaseDeCroissanceInflo
BaseDeCroissance<-BaseDeCroissance[BaseDeCroissance$variete=="cog",]
BaseDeCroissance$codeUC <- as.character(BaseDeCroissance$codeUC)
BaseDeCroissance$codeUC <- as.factor(BaseDeCroissance$codeUC)


x <- c(10.76)
# PARAMETRES INITIAUX
valmin <- x[1]      # température de seuil, de base statistique : Le paramètre qu'on veut estimer
#valmin c'est x
print(valmin)


############################
## calcul des nb jours apres débourrement
############################

#On défini la date de débourrement de chaque UC qu'on copie à chaque ligne:
vec2<-NULL
base<-split(BaseDeCroissance,BaseDeCroissance$codeUC)
for (i in 1:length(base)){
ssbase<-base[[i]]
dateDeb<-as.character(ssbase$date[ssbase$DFInflo=="D"])
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


#########################################
########### Calcul pour chaque UC des ddj
#########################################

#On sépare les UC:
rep2<-split(BaseDeCroissance,BaseDeCroissance$codeUC)
dj<<-list() #Tableau de nb de jour depuis débourrement associé à la somme des températures
j<-0
m<-NULL  #Temp moy par jour

lapply(rep2,function(x){

#On défini le fichier température à choisir pour chaque UC
dataT<-NULL             #On prend que le 1er car répétitions du mm verger!
if(x$verger[1]=="BM") {dataT<-TBMmoy}    
if(x$verger[1]=="BP") {dataT<-TBPmoy}
if(x$verger[1]=="GH") {dataT<-TGHmoy}
if(x$verger[1]=="GF") {dataT<-TGFmoy}
#vérif OK : il choisi les bon fichier température pour chaque UC

Ddeb<-unique(x$vec2) #Date de débourrement de l'UC
j<<-j+1 # (j + 1)
DAB<-difftime(dataT$datesUnik,strptime(Ddeb,"%d/%m/%y"),units="days")		# Dates en nb de jours après débourrement
#Dans le fichier température on a les dates avant et apres les dates de débourrement
t<-length(which(DAB<0))+1   # 1ere valeur pour calculer la somme ddj
for (i in (t:nrow(dataT)))   #Calcul pour tout le fichier de température
{if  (dataT$tempMoy[i]>valmin)  m[i-t+1]=dataT$tempMoy[i]-valmin  else m[i-t+1]=0  } # Calcul de la Tm-Tb
#[i-t+1] sert à commencer à remplir le m à 1: t: DAB=0
#m temp par jour
calcddj <- as.vector(cumsum(m))	# Calcul de la somme de ddj
#creation de dj
DAB<-DAB[t:length(DAB)]
dj[[j]] <<- cbind(DAB,calcddj)  #dj va de 0 à x; UC l'une après l'autre
})

#On match les dj avec les données de la base en fonction des DAB
#Délimination de dj et creation du fichier d avec les codes UCs associés
DABmax<-max(unlist(lapply(dj,function(x)max(x[,"DAB"]))),na.rm=TRUE)
DABmin<-max(unlist(lapply(dj,function(x)min(x[,"DAB"]))),na.rm=TRUE)
DAB<-DABmin:DABmax
#On prend le min et le max. les sommes de temp sont calculées pour plus que nécessaire
#On selectionnera les bonnes dates ( la fin surtout) ensuite.
DAB<-data.frame(DAB)
colnames(DAB)<-"DAB"
#On fait le 1ère colonne : on mix DAB et dj ( de min à max DAB)
d<-merge(dj[[1]],DAB,by="DAB",sort=TRUE)
#On fait pour les colonnes suivantes:
for (i in 2:length(dj)){
d<-merge(d,dj[[i]],by.x="DAB",by.y="DAB",sort=TRUE)}
#On renomme les colonnes: 
colnames(d)<-c("DAB",names(rep2))
#head(d)
rownames(d)<-d[,1] #d: UC l'une a coté de l'autre en data.frame avec code UC en nom de colonne de 0 à x (DAB)


# préparation jeu de données observé
croiss<-data.frame()
croiss<-lapply(rep2,function(x){
croiss<<-rbind.data.frame(croiss,x)})

croiss<-croiss[[length(croiss)]]
#head(croiss)
#head(d)

BaseFin<-data.frame()
for (i in 1:nlevels(croiss$codeUC)){
don <- croiss[croiss$codeUC==levels(croiss$codeUC)[i],]  #selection des données pour cette UC
DAB <- don$DAB  #Selection des DAB de cette UC
ddj <- d[match(DAB,rownames(d)),which(colnames(d)==levels(croiss$codeUC)[i])] # on calcul les ddj associés aux DAB
#On match ces DAB avec le fichier de somme de temp (d) associé à cet UC
don <- cbind(don,ddj) #On rajoute les ddj dans le fichier don
BaseFin<-rbind(BaseFin,don)
} 


################PLot pour les INFLOS
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$longueurInflo[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],type="l",ylim=c(0,50),xlab="Days After Burst", ylab="long INFLO",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$longueurInflo[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$longueurInflo[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],type="l",ylim=c(0,50),xlab="ddj °C", ylab="long INFLO cog",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$longueurInflo[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}







#################### JOSe


## ne pas oublier de changer TB en fonction variete et organe considéré


#############Calcul des ST pour JOSE
source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Preparation Base Inflo.R")
BaseDeCroissance<-BaseDeCroissanceInflo
BaseDeCroissance<-BaseDeCroissance[BaseDeCroissance$variete=="jose",]
BaseDeCroissance$codeUC <- as.character(BaseDeCroissance$codeUC)
BaseDeCroissance$codeUC <- as.factor(BaseDeCroissance$codeUC)


x <- c(11.93)
# PARAMETRES INITIAUX
valmin <- x[1]      # température de seuil, de base statistique : Le paramètre qu'on veut estimer
#valmin c'est x
print(valmin)


############################
## calcul des nb jours apres débourrement
############################

#On défini la date de débourrement de chaque UC qu'on copie à chaque ligne:
vec2<-NULL
base<-split(BaseDeCroissance,BaseDeCroissance$codeUC)
for (i in 1:length(base)){
ssbase<-base[[i]]
dateDeb<-as.character(ssbase$date[ssbase$DFInflo=="D"])
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


#########################################
########### Calcul pour chaque UC des ddj
#########################################

#On sépare les UC:
rep2<-split(BaseDeCroissance,BaseDeCroissance$codeUC)
dj<<-list() #Tableau de nb de jour depuis débourrement associé à la somme des températures
j<-0
m<-NULL  #Temp moy par jour

lapply(rep2,function(x){

#On défini le fichier température à choisir pour chaque UC
dataT<-NULL             #On prend que le 1er car répétitions du mm verger!
if(x$verger[1]=="BM") {dataT<-TBMmoy}    
if(x$verger[1]=="BP") {dataT<-TBPmoy}
if(x$verger[1]=="GH") {dataT<-TGHmoy}
if(x$verger[1]=="GF") {dataT<-TGFmoy}
#vérif OK : il choisi les bon fichier température pour chaque UC

Ddeb<-unique(x$vec2) #Date de débourrement de l'UC
j<<-j+1 # (j + 1)
DAB<-difftime(dataT$datesUnik,strptime(Ddeb,"%d/%m/%y"),units="days")		# Dates en nb de jours après débourrement
#Dans le fichier température on a les dates avant et apres les dates de débourrement
t<-length(which(DAB<0))+1   # 1ere valeur pour calculer la somme ddj
for (i in (t:nrow(dataT)))   #Calcul pour tout le fichier de température
{if  (dataT$tempMoy[i]>valmin)  m[i-t+1]=dataT$tempMoy[i]-valmin  else m[i-t+1]=0  } # Calcul de la Tm-Tb
#[i-t+1] sert à commencer à remplir le m à 1: t: DAB=0
#m temp par jour
calcddj <- as.vector(cumsum(m))	# Calcul de la somme de ddj
#creation de dj
DAB<-DAB[t:length(DAB)]
dj[[j]] <<- cbind(DAB,calcddj)  #dj va de 0 à x; UC l'une après l'autre
})

#On match les dj avec les données de la base en fonction des DAB
#Délimination de dj et creation du fichier d avec les codes UCs associés
DABmax<-max(unlist(lapply(dj,function(x)max(x[,"DAB"]))),na.rm=TRUE)
DABmin<-max(unlist(lapply(dj,function(x)min(x[,"DAB"]))),na.rm=TRUE)
DAB<-DABmin:DABmax
#On prend le min et le max. les sommes de temp sont calculées pour plus que nécessaire
#On selectionnera les bonnes dates ( la fin surtout) ensuite.
DAB<-data.frame(DAB)
colnames(DAB)<-"DAB"
#On fait le 1ère colonne : on mix DAB et dj ( de min à max DAB)
d<-merge(dj[[1]],DAB,by="DAB",sort=TRUE)
#On fait pour les colonnes suivantes:
for (i in 2:length(dj)){
d<-merge(d,dj[[i]],by.x="DAB",by.y="DAB",sort=TRUE)}
#On renomme les colonnes: 
colnames(d)<-c("DAB",names(rep2))
#head(d)
rownames(d)<-d[,1] #d: UC l'une a coté de l'autre en data.frame avec code UC en nom de colonne de 0 à x (DAB)


# préparation jeu de données observé
croiss<-data.frame()
croiss<-lapply(rep2,function(x){
croiss<<-rbind.data.frame(croiss,x)})

croiss<-croiss[[length(croiss)]]
#head(croiss)
#head(d)

BaseFin<-data.frame()
for (i in 1:nlevels(croiss$codeUC)){
don <- croiss[croiss$codeUC==levels(croiss$codeUC)[i],]  #selection des données pour cette UC
DAB <- don$DAB  #Selection des DAB de cette UC
ddj <- d[match(DAB,rownames(d)),which(colnames(d)==levels(croiss$codeUC)[i])] # on calcul les ddj associés aux DAB
#On match ces DAB avec le fichier de somme de temp (d) associé à cet UC
don <- cbind(don,ddj) #On rajoute les ddj dans le fichier don
BaseFin<-rbind(BaseFin,don)
} 


################PLot pour les INFLOS
### DAB
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$longueurInflo[BaseFin$codeUC==i]~BaseFin$DAB[BaseFin$codeUC==i],type="l",ylim=c(0,50),xlab="Days After Burst", ylab="long INFLO jose",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$longueurInflo[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$DAB[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}

### ST
i<-levels(BaseFin$codeUC)[1]
plot(BaseFin$longueurInflo[BaseFin$codeUC==i]~BaseFin$ddj[BaseFin$codeUC==i],type="l",ylim=c(0,50),xlab="ddj °C", ylab="long INFLO jose",bty="n")

for (i in 2:length(levels(BaseFin$codeUC))){
points(BaseFin$longueurInflo[BaseFin$codeUC==levels(BaseFin$codeUC)[i]]~BaseFin$ddj[BaseFin$codeUC==levels(BaseFin$codeUC)[i]],type="l")}


