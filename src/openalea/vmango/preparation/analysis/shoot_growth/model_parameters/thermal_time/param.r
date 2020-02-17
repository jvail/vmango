############################
####  UC  ##################
############################

############################
## CREATION DE LA BASE  ####
############################

######################################
#On charge les fichiers de température:
#path= "D:/Mes Donnees/These/Données croissance-temperature/Meteo/VergerBassinMartin/VergerBassinMartin.txt"
path= "D:/MesDonnées/mes documents/manguier/thèses/Anaëlle/CD fin de thèse/Données croissance-temperature/Meteo/VergerBassinMartin/VergerBassinMartin.txt"               # pour mon ordi
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



#path= "D:/Mes Donnees/These/Données croissance-temperature/Meteo/VergerGrandFond/VergerGrandFond.txt"
path= "D:/MesDonnées/mes documents/manguier/thèses/Anaëlle/CD fin de thèse/Données croissance-temperature/Meteo/VergerGrandFond/VergerGrandFond.txt"               # pour mon ordi
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


#path= "D:/Mes Donnees/These/Données croissance-temperature/Meteo/VergerSaintGillesHauts/VergerSaintGillesHauts.txt"
path= "D:/MesDonnées/mes documents/manguier/thèses/Anaëlle/CD fin de thèse/Données croissance-temperature/Meteo/VergerSaintGillesHauts/VergerSaintGillesHauts.txt"               # pour mon ordi
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


#path= "D:/Mes Donnees/These/Données croissance-temperature/Meteo/VergerBassinPlat/VergerBassinPlat.txt"
path= "D:/MesDonnées/mes documents/manguier/thèses/Anaëlle/CD fin de thèse/Données croissance-temperature/Meteo/VergerBassinPlat/VergerBassinPlat.txt"               # pour mon ordi
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

############################################################################
#On charge la base de croissance:
source("D:/MesDonnées/mes documents/manguier/modélisation phéno/stage 2014/Anne-Sarah Briand/données/fichiers de données/températures de base (Anaëlle)/TB durée croissance/bases et scripts/calcul_des_surfaces_foliaires.r")         # sur mon ordi

###ON ENLEVE LES LIGNES OU ON A 2 DATES IDENTIQUES SINON PB CAR MM SOMME DE TEMP (BASSIN PLAT COG)
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


###########################################

## UC de Cogshall

##########################################

x <- c(9.20) #Mettre cette TB pour les UC de cogshall pour le calcul des ddj


##########################################
## On calcule les ddj


## ne pas oublier de changer TB en fonction variete et organe considéré
#############Calcul des ST pour cogshall

Basecog<-BaseDeCroissance[BaseDeCroissance$variete=="cog",] #TODO
Basecog$codeUC <- as.character(Basecog$codeUC)
Basecog$codeUC <- as.factor(Basecog$codeUC)

# PARAMETRES INITIAUX
valmin <- x[1]      # température de seuil, de base statistique : Le paramètre qu'on veut estimer
#valmin c'est x
print(valmin)


############################
## calcul des nb jours apres débourrement
############################

#On défini la date de débourrement de chaque UC qu'on copie à chaque ligne:
vec2<-NULL
base<-split(Basecog,Basecog$codeUC)
for (i in 1:length(base)){
ssbase<-base[[i]]
dateDeb<-as.character(ssbase$date[ssbase$DFUC=="D"])
vec<-rep(dateDeb,dim(ssbase)[1])
vec2<-c(vec2,vec)
}
Basecog<-cbind(Basecog,vec2)

#Pour chaque jour on fait la différence avec la date de débourrement : nb de jours depuis débourrement.
Basecog$DAB<-difftime(strptime(Basecog$date,"%d/%m/%y"),strptime(Basecog$vec2,"%d/%m/%y"),
units="days")
#Les 0 correspondent aux jours mm : gain reçu pendant la journée de débourrement.
#On commence les gains à DAB = 0
#les - sont pour les jours avant débourrement


#########################################
########### Calcul pour chaque UC des ddj
#########################################

#On sépare les UC:
rep2<-split(Basecog,Basecog$codeUC)
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

#On met NA au deuxième ddj quand 2 ddj identiques se suivent (à Bassin plat quand 2 mesures par jour)
BaseFin2<-BaseFin
for ( i in 2:nrow(BaseFin2)){
if(is.na(BaseFin2$ddj[i])==FALSE){
if(is.na(BaseFin2$ddj[i-1])==FALSE){
if(BaseFin2$ddj[i]==BaseFin2$ddj[i-1]) {BaseFin2$ddj[i]<-NA}
}
}
}

Basecog<-BaseFin2
codeUC<-Basecog$codeUC
longueurUC<-Basecog$longueurUC
ddj<-Basecog$ddj

##################################
###### Calcul des paramètres UC ##
##################################

Basecog[1:10,]
a<-unique(codeUC)


###############################################################################
#EQUATION D'AJUSTEMENT DE LA COURBE DE CROISSANCE:
#estimation des paramètres de l'équation
# A : y max : valeur du plateau final
#X0 : date du point d'inflection : 50% de la croissance
#B : durée ou la vitesse est maximale

maxL=max(longueurUC,na.rm=T)

#COeff pour les UC
A0<-vector()
X00<-vector()
B0<-vector()
#Vitesse maximale: ER
ER0<-vector()

#Vecteur d'erreur    #Ajoute 1 quand il y a des pb d'ajustement.
Error0<-vector()

compt=0

#On ouvre la fenetre graphique
par(mfrow=c(4,2), ask=T, mar=c(4,4,4,5))

#On commence la boucle
for ( i in a) {
compt=compt+1
#On défini x et y
x=ddj[codeUC==i]
y0=longueurUC[codeUC==i]

#Fit pour les UC : estimation des paramètres
fit0<-NULL
try(fit0<-nls(y0 ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = 20.5, B = 50, X0 =100)),silent=T)
                               #start : valeurs initiales où commencer puis il cherche des valeurs aux alentours pour ajuster
                                     #Attention il ne fait pas les NA : enlève ces lignes
try(A0[compt]<-summary(fit0)$coef[1],silent=T)
try(B0[compt]<-summary(fit0)$coef[2],silent=T)
try(X00[compt]<-summary(fit0)$coef[3],silent=T)
#calcul de la vitesse max : ER
try(ER0[compt] <- 0.25 * A0[compt] / B0[compt], silent = T) 


#Représentation graphique : tracer les ajustements sur chaque courbe de croissance:
#On fait 1 graphe pour chaque codeUC

#UC
plot(ddj[codeUC==i],longueurUC[codeUC==i],type="p", col="black",xlab="ddj",ylab="longueurUC (cm)",ylim=c(0,maxL),main=i)

#Ajustement : il faut qu'on prenne que les lignes ou il n'y a pas NA
data<-NULL
data<-data.frame(ddj=ddj[codeUC==i],longueurUC=longueurUC[codeUC==i])
data<-data[-which(is.na(data$longueurUC)),]   #On enlève les lignes ou NA
ifelse(is.na(A0[compt]),Error0[compt]<-1,Error0[compt]<-0)     #Si pb d'ajustement, Error0 reçoit un 1
if(Error0[compt]==0) (points(data$ddj,fitted(fit0),type="l",col="black"))    #On rajoute l'ajustement t
}

#On obtient une base d'ajustment :
baseAJUSTcog<-data.frame(a,A0,X00,B0,Error0,ER0)
baseAJUSTcog[1:30,]
#TODO

###########################################

## UC de José

##########################################
#TODO
x <- c(0.86) #Mettre cette TB pour les UC de José pour le calcul des ddj#


##########################################
## On calcule les ddj


## ne pas oublier de changer TB en fonction variete et organe considéré
#############Calcul des ST pour José

Basejose<-BaseDeCroissance[BaseDeCroissance$variete=="jose",] #TODO
Basejose$codeUC <- as.character(Basejose$codeUC)
Basejose$codeUC <- as.factor(Basejose$codeUC)

# PARAMETRES INITIAUX
valmin <- x[1]      # température de seuil, de base statistique : Le paramètre qu'on veut estimer
#valmin c'est x
print(valmin)


############################
## calcul des nb jours apres débourrement
############################

#On défini la date de débourrement de chaque UC qu'on copie à chaque ligne:
vec2<-NULL
base<-split(Basejose,Basejose$codeUC)
for (i in 1:length(base)){
ssbase<-base[[i]]
dateDeb<-as.character(ssbase$date[ssbase$DFUC=="D"])
vec<-rep(dateDeb,dim(ssbase)[1])
vec2<-c(vec2,vec)
}
Basejose<-cbind(Basejose,vec2)

#Pour chaque jour on fait la différence avec la date de débourrement : nb de jours depuis débourrement.
Basejose$DAB<-difftime(strptime(Basejose$date,"%d/%m/%y"),strptime(Basejose$vec2,"%d/%m/%y"),
units="days")
#Les 0 correspondent aux jours mm : gain reçu pendant la journée de débourrement.
#On commence les gains à DAB = 0
#les - sont pour les jours avant débourrement


#########################################
########### Calcul pour chaque UC des ddj
#########################################

#On sépare les UC:
rep2<-split(Basejose,Basejose$codeUC)
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

#On met NA au deuxième ddj quand 2 ddj identiques se suivent (à Bassin plat quand 2 mesures par jour)
BaseFin2<-BaseFin
for ( i in 2:nrow(BaseFin2)){
if(is.na(BaseFin2$ddj[i])==FALSE){
if(is.na(BaseFin2$ddj[i-1])==FALSE){
if(BaseFin2$ddj[i]==BaseFin2$ddj[i-1]) {BaseFin2$ddj[i]<-NA}
}
}
}

Basejose<-BaseFin2
codeUC<-Basejose$codeUC
longueurUC<-Basejose$longueurUC
ddj<-Basejose$ddj

##################################
###### Calcul des paramètres UC ##
##################################

Basejose[1:10,]
a<-unique(codeUC)


###############################################################################
#EQUATION D'AJUSTEMENT DE LA COURBE DE CROISSANCE:
#estimation des paramètres de l'équation
# A : y max : valeur du plateau final
#X0 : date du point d'inflection : 50% de la croissance
#B : durée ou la vitesse est maximale

maxL=max(longueurUC,na.rm=T)

#COeff pour les UC
A0<-vector()
X00<-vector()
B0<-vector()
#Vitesse maximale: ER
ER0<-vector()

#Vecteur d'erreur    #Ajoute 1 quand il y a des pb d'ajustement.
Error0<-vector()

compt=0

#On ouvre la fenetre graphique
par(mfrow=c(4,2), ask=T, mar=c(4,4,4,5))

#On commence la boucle
for ( i in a) {
compt=compt+1
#On défini x et y
x=ddj[codeUC==i]
y0=longueurUC[codeUC==i]

#Fit pour les UC : estimation des paramètres
fit0<-NULL
try(fit0<-nls(y0 ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = 20.5, B = 50, X0 =100)),silent=T)
                               #start : valeurs initiales où commencer puis il cherche des valeurs aux alentours pour ajuster
                                     #Attention il ne fait pas les NA : enlève ces lignes
try(A0[compt]<-summary(fit0)$coef[1],silent=T)
try(B0[compt]<-summary(fit0)$coef[2],silent=T)
try(X00[compt]<-summary(fit0)$coef[3],silent=T)
#calcul de la vitesse max : ER
try(ER0[compt] <- 0.25 * A0[compt] / B0[compt], silent = T) 


#Représentation graphique : tracer les ajustements sur chaque courbe de croissance:
#On fait 1 graphe pour chaque codeUC

#UC
plot(ddj[codeUC==i],longueurUC[codeUC==i],type="p", col="black",xlab="ddj",ylab="longueurUC (cm)",ylim=c(0,maxL),main=i)

#Ajustement : il faut qu'on prenne que les lignes ou il n'y a pas NA
data<-NULL
data<-data.frame(ddj=ddj[codeUC==i],longueurUC=longueurUC[codeUC==i])
data<-data[-which(is.na(data$longueurUC)),]   #On enlève les lignes ou NA
ifelse(is.na(A0[compt]),Error0[compt]<-1,Error0[compt]<-0)     #Si pb d'ajustement, Error0 reçoit un 1
if(Error0[compt]==0) (points(data$ddj,fitted(fit0),type="l",col="black"))    #On rajoute l'ajustement t
}

#On obtient une base d'ajustment :
baseAJUSTjose<-data.frame(a,A0,X00,B0,Error0,ER0)
baseAJUSTjose[1:30,]



#Distribution des longueurs finales pour Cogshall et José:
par(mfrow=c(4,2))
hist(baseAJUSTcog$A0[order(baseAJUSTcog$A0,na.last=NA)],main="Cogshall GUs",breaks=10,xlab="Length of GUs (cm)",col="blue",ylab="number of GUs",ylim=c(0,50),xlim=c(0,50))
hist(baseAJUSTjose$A0[order(baseAJUSTjose$A0,na.last=NA)],main="José GUs",breaks=10,xlab="Length of GUs (cm)",ylab="number of GUs",col="red",ylim=c(0,50),xlim=c(0,50))

# Distribution des points d'inflexion
hist(baseAJUSTcog$X00[order(baseAJUSTcog$X00,na.last=NA)],main="Cogshall GUs",breaks=10,xlab="inflexion point (ddj)",col="blue",ylab="number of GUs",ylim=c(0,50),xlim=c(0,300))
hist(baseAJUSTjose$X00[order(baseAJUSTjose$X00,na.last=NA)],main="José GUs",breaks=10,xlab="inflexion point (ddj)",ylab="number of GUs",col="red",ylim=c(0,50),xlim=c(0,300))

# Distribution des durées de vitesse max
hist(baseAJUSTcog$B0[order(baseAJUSTcog$B0,na.last=NA)],main="Cogshall GUs",breaks=10,xlab="duration at max ER (ddj)",col="blue",ylab="number of GUs",ylim=c(0,50),xlim=c(0,100))
hist(baseAJUSTjose$B0[order(baseAJUSTjose$B0,na.last=NA)],main="José GUs",breaks=10,xlab="duration at max ER (ddj)",ylab="number of GUs",col="red",ylim=c(0,50),xlim=c(0,100))

# Distribution des vitesse max
hist(baseAJUSTcog$ER0[order(baseAJUSTcog$ER0,na.last=NA)],main="Cogshall GUs",breaks=10,xlab="max ER (ddj)",col="blue",ylab="number of GUs",ylim=c(0,50),xlim=c(0,0.5))
hist(baseAJUSTjose$ER0[order(baseAJUSTjose$ER0,na.last=NA)],main="José GUs",breaks=10,xlab="max ER (ddj)",ylab="number of GUs",col="red",ylim=c(0,50),xlim=c(0,0.5))

#En fonction de la longueur finale:
# points d'inflexion
par(mfrow=c(3,1))
plot(baseAJUSTcog$X00~baseAJUSTcog$A0,main="GUs",pch=16,xlab="Length of GUs (cm)",col="blue",ylab="Inflexion point",ylim=c(0,300),xlim=c(0,50))
points(baseAJUSTjose$X00~baseAJUSTjose$A0,col="red",pch=16)

# Distribution des durées de vitesse max
plot(baseAJUSTcog$B0~baseAJUSTcog$A0,main="GUs",pch=16,xlab="Length of GUs (cm)",col="blue",ylab="Duration at the max ER (ddj)",ylim=c(0,100),xlim=c(0,50))
points(baseAJUSTjose$B0~baseAJUSTjose$A0,col="red",pch=16)

# Distribution des vitesse max
plot(baseAJUSTcog$ER0~baseAJUSTcog$A0,main="GUs",pch=16,xlab="Length of GUs (cm)",col="blue",ylab="Max ER",ylim=c(0,0.5),xlim=c(0,50))
points(baseAJUSTjose$ER0~baseAJUSTjose$A0,col="red",pch=16)

# Relations entre la longueur finale de l'UC (A0) et la vitesse max de croissance (ER0) 
regcogUC <- lm(baseAJUSTcog$ER0[-which(baseAJUSTcog$Error0==1)]~baseAJUSTcog$A0[-which(baseAJUSTcog$Error0==1)])
summary(regcogUC)           # terme constant non différent de 0 (P = 0.799)
regcogUC <- lm(baseAJUSTcog$ER0[-which(baseAJUSTcog$Error0==1)]~baseAJUSTcog$A0[-which(baseAJUSTcog$Error0==1)] - 1)
summary(regcogUC)           

abline(a=0, b=regcogUC$coef, col="blue")

regjoseUC <- lm(baseAJUSTjose$ER0~baseAJUSTjose$A0)
summary(regjoseUC)           # terme constant non différent de 0 (P = 0.893)
regjoseUC <- lm(baseAJUSTjose$ER0~baseAJUSTjose$A0 - 1)
summary(regjoseUC)           

abline(a=0, b=regjoseUC$coef, col="red")


# Création fichier de sortie
paramAJUSTUC <- rbind(baseAJUSTcog, baseAJUSTjose)
# retrait des 3 UCs de Cogshall pou rlesquelles il n'y avait pas de valeur de longueur mesurée
paramAJUSTUC <- paramAJUSTUC[-which(paramAJUSTUC$Error0==1),]
paramAJUSTUC$a <- as.character(paramAJUSTUC$a)
paramAJUSTUC$a <- as.factor(paramAJUSTUC$a)
# on rajoute des infos de base en couplant avec la base nT
summary(nT)
nT <- nT[-which(is.na(nT$nUC)),]          # retrait des 3 UCs Cogshall pour lesquelles il manque les longueurs
dim(nT)
nT$a <- as.character(nT$a)
nT$a <- as.factor(nT$a)
ind <- match(nT$a, paramAJUSTUC$a)
paramAJUSTUCglob <- cbind(nT, paramAJUSTUC[ind,])

write.table(paramAJUSTUCglob,"paramAJUSTUCglob.csv")



############################
####  FEUILLES #############
############################

############################
## CREATION DE LA BASE  ####
############################


#source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/calcul_des_surfaces_foliaires.R")      # original Anaëlle
source("D:/MesDonnées/mes documents/manguier/modélisation phéno/stage 2014/Anne-Sarah Briand/données/fichiers de données/températures de base (Anaëlle)/TB durée croissance/bases et scripts/calcul_des_surfaces_foliaires.r")         # sur mon ordi

baseUC<-BaseDeCroissance
#On fait un tableau ou on mélange toutes les feuilles
codeUC0<-c(as.character(baseUC$codeUC),as.character(baseUC$codeUC),as.character(baseUC$codeUC),as.character(baseUC$codeUC))
saison<-c(as.character(baseUC$saison),as.character(baseUC$saison),as.character(baseUC$saison),as.character(baseUC$saison))
verger<-c(as.character(baseUC$verger),as.character(baseUC$verger),as.character(baseUC$verger),as.character(baseUC$verger))
variete<-c(as.character(baseUC$variete),as.character(baseUC$variete),as.character(baseUC$variete),as.character(baseUC$variete))
date1<-c(as.character(baseUC$date),as.character(baseUC$date),as.character(baseUC$date),as.character(baseUC$date))
dates<-c(as.character(baseUC$dates),as.character(baseUC$dates),as.character(baseUC$dates),as.character(baseUC$dates))
DFUC<-c(as.character(baseUC$DFUC),as.character(baseUC$DFUC),as.character(baseUC$DFUC),as.character(baseUC$DFUC))
surF<-c(baseUC$surFprox,baseUC$surFdist,baseUC$surF1,baseUC$surFplus)
FF<-c(as.character(baseUC$FFprox),as.character(baseUC$FFdist),as.character(baseUC$FF1),as.character(baseUC$FFplus))
posFeuille<-c(rep("prox",dim(baseUC)[1]),rep("dist",dim(baseUC)[1]),rep("F1",dim(baseUC)[1]),rep("Fplus",dim(baseUC)[1]))
posUC<-c(as.character(baseUC$positionUC),as.character(baseUC$positionUC),as.character(baseUC$positionUC),as.character(baseUC$positionUC))
longUC<-c(baseUC$longueurUC,baseUC$longueurUC,baseUC$longueurUC,baseUC$longueurUC)
DFUC<-c(as.character(baseUC$DFUC),as.character(baseUC$DFUC),as.character(baseUC$DFUC),as.character(baseUC$DFUC))

baseUC2<-data.frame(codeUC0,saison,verger,variete,date1,dates,DFUC,surF,FF,posFeuille,posUC,longUC,DFUC)
colnames(baseUC2)[5]<-"date"

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


Basefeu<-baseUC5


###ON ENLEVE LES LIGNES OU ON A 2 DATES IDENTIQUES SINON PB CAR MM SOMME DE TEMP (BASSIN PLAT cog)
vec<-NULL
base<-split(Basefeu,Basefeu$codeUC)
Basefeu2<-NULL

for (i in 1:length(base)){
ssbase<-base[[i]]

vec<-NULL
for (j in 2:nrow(ssbase)){
if(strftime(ssbase$dates[j],"%Y-%m-%d")==strftime(ssbase$dates[j-1],"%Y-%m-%d"))
{vec=c(vec,j)}}

ssbase2<-NULL
ifelse(length(vec)!=0,ssbase2<-ssbase[-vec,],ssbase2<-ssbase)
Basefeu2<-rbind(Basefeu2,ssbase2)
}

Basefeu<-Basefeu2


###########################################

## FEUILLES de Cogshall

##########################################

x <- c(10.73) #Mettre cette TB pour les feuilles de cogshall pour le calcul des ddj

##########################################
## On calcule les ddj


## ne pas oublier de changer TB en fonction variete et organe considéré
#############Calcul des ST pour cogshall

Basefeucog<-Basefeu[Basefeu$variete=="cog",] 
Basefeucog$codeUC <- as.character(Basefeucog$codeUC)
Basefeucog$codeUC <- as.factor(Basefeucog$codeUC)

# PARAMETRES INITIAUX
valmin <- x[1]      # température de seuil, de base statistique : Le paramètre qu'on veut estimer
#valmin c'est x
print(valmin)


############################
## calcul des nb jours apres débourrement
############################

#On défini la date de débourrement de chaque UC qu'on copie à chaque ligne:
vec2<-NULL
base<-split(Basefeucog,Basefeucog$codeUC)
for (i in 1:length(base)){
ssbase<-base[[i]]
dateDeb<-as.character(ssbase$date[ssbase$DFUC=="D"])
vec<-rep(dateDeb,dim(ssbase)[1])
vec2<-c(vec2,vec)
}
Basefeucog<-cbind(Basefeucog,vec2)

#Pour chaque jour on fait la différence avec la date de débourrement : nb de jours depuis débourrement.
Basefeucog$DAB<-difftime(strptime(Basefeucog$date,"%d/%m/%y"),strptime(Basefeucog$vec2,"%d/%m/%y"),
units="days")
#Les 0 correspondent aux jours mm : gain reçu pendant la journée de débourrement.
#On commence les gains à DAB = 0
#les - sont pour les jours avant débourrement


#########################################
########### Calcul des ddj
#########################################

#On sépare les UC:
rep2<-split(Basefeucog,Basefeucog$codeUC)
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

#On met NA au deuxième ddj quand 2 ddj identiques se suivent (à Bassin plat quand 2 mesures par jour)
BaseFin2<-BaseFin
for ( i in 2:nrow(BaseFin2)){
if(is.na(BaseFin2$ddj[i])==FALSE){
if(is.na(BaseFin2$ddj[i-1])==FALSE){
if(BaseFin2$ddj[i]==BaseFin2$ddj[i-1]) {BaseFin2$ddj[i]<-NA}
}
}
}


Basefeucog<-BaseFin2
codeUC<-Basefeucog$codeUC
surF<-Basefeucog$surF
ddj<-Basefeucog$ddj

     
##################################
###### Calcul des paramètres FEUILLES ##
##################################

Basefeucog[1:10,]
a<-unique(codeUC)


###############################################################################
#EQUATION D'AJUSTEMENT DE LA COURBE DE CROISSANCE:
#estimation des paramètres de l'équation
# A : y max : valeur du plateau final
#X0 : date du point d'inflection : 50% de la croissance
#B : durée ou la vitesse est maximale

maxL=max(surF,na.rm=T)

#COeff pour les UC
A0<-vector()
X00<-vector()
B0<-vector()
#Vitesse maximale: ER
ER0<-vector()

#Vecteur d'erreur    #Ajoute 1 quand il y a des pb d'ajustement.
Error0<-vector()

compt=0

#On ouvre la fenetre graphique
par(mfrow=c(4,2), ask=T, mar=c(4,4,4,5))

#On commence la boucle
for ( i in a) {
compt=compt+1
#On défini x et y
x=ddj[codeUC==i]
y0=surF[codeUC==i]

#Fit pour les UC : estimation des paramètres
fit0<-NULL
try(fit0<-nls(y0 ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = 40, B = 50, X0 =150)),silent=T)
                               #start : valeurs initiales où commencer puis il cherche des valeurs aux alentours pour ajuster
                                     #Attention il ne fait pas les NA : enlève ces lignes
try(A0[compt]<-summary(fit0)$coef[1],silent=T)
try(B0[compt]<-summary(fit0)$coef[2],silent=T)
try(X00[compt]<-summary(fit0)$coef[3],silent=T)
#calcul de la vitesse max : ER
try(ER0[compt] <- 0.25 * A0[compt] / B0[compt], silent = T) 


#Représentation graphique : tracer les ajustements sur chaque courbe de croissance:
#On fait 1 graphe pour chaque codeUC

#UC
plot(ddj[codeUC==i],surF[codeUC==i],type="p", col="black",xlab="ddj",ylab="surF (cm2)",ylim=c(0,maxL),main=i)

#Ajustement : il faut qu'on prenne que les lignes ou il n'y a pas NA
data<-NULL
data<-data.frame(ddj=ddj[codeUC==i],surF=surF[codeUC==i])
data<-data[-which(is.na(data$surF)),]   #On enlève les lignes ou NA
ifelse(is.na(A0[compt]),Error0[compt]<-1,Error0[compt]<-0)     #Si pb d'ajustement, Error0 reçoit un 1
if(Error0[compt]==0) (points(data$ddj,fitted(fit0),type="l",col="black"))    #On rajoute l'ajustement t
}

#On obtient une base d'ajustment :
baseAJUSTFcog<-data.frame(a,A0,X00,B0,Error0,ER0)
baseAJUSTFcog[1:30,]
# retrait de 6 feuilles pour lequelles il y a eu un problème d'ajustement
baseAJUSTFcog <- baseAJUSTFcog[-which(is.na(baseAJUSTFcog$A0)),]



###########################################

## FEUILLES de José

##########################################

x <- c(5.78) #Mettre cette TB pour les feuilles de José pour le calcul des ddj




##########################################
## On calcule les ddj


## ne pas oublier de changer TB en fonction variete et organe considéré
#############Calcul des ST pour José

Basefeujose<-Basefeu[Basefeu$variete=="jose",] #TODO
Basefeujose$codeUC <- as.character(Basefeujose$codeUC)
Basefeujose$codeUC <- as.factor(Basefeujose$codeUC)

# PARAMETRES INITIAUX
valmin <- x[1]      # température de seuil, de base statistique : Le paramètre qu'on veut estimer
#valmin c'est x
print(valmin)


############################
## calcul des nb jours apres débourrement
############################

#On défini la date de débourrement de chaque UC qu'on copie à chaque ligne:
vec2<-NULL
base<-split(Basefeujose,Basefeujose$codeUC)
for (i in 1:length(base)){
ssbase<-base[[i]]
dateDeb<-as.character(ssbase$date[ssbase$DFUC=="D"])
vec<-rep(dateDeb,dim(ssbase)[1])
vec2<-c(vec2,vec)
}
Basefeujose<-cbind(Basefeujose,vec2)

#Pour chaque jour on fait la différence avec la date de débourrement : nb de jours depuis débourrement.
Basefeujose$DAB<-difftime(strptime(Basefeujose$date,"%d/%m/%y"),strptime(Basefeujose$vec2,"%d/%m/%y"),
units="days")
#Les 0 correspondent aux jours mm : gain reçu pendant la journée de débourrement.
#On commence les gains à DAB = 0
#les - sont pour les jours avant débourrement


#########################################
########### Calcul des ddj
#########################################

#On sépare les UC:
rep2<-split(Basefeujose,Basefeujose$codeUC)
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

#On met NA au deuxième ddj quand 2 ddj identiques se suivent (à Bassin plat quand 2 mesures par jour)
BaseFin2<-BaseFin
for ( i in 2:nrow(BaseFin2)){
if(is.na(BaseFin2$ddj[i])==FALSE){
if(is.na(BaseFin2$ddj[i-1])==FALSE){
if(BaseFin2$ddj[i]==BaseFin2$ddj[i-1]) {BaseFin2$ddj[i]<-NA}
}
}
}


Basefeujose<-BaseFin2
codeUC<-Basefeujose$codeUC
surF<-Basefeujose$surF
ddj<-Basefeujose$ddj





##################################
###### Calcul des paramètres FEUILLES ##
##################################

Basefeujose[1:10,]
a<-unique(codeUC)


###############################################################################
#EQUATION D'AJUSTEMENT DE LA COURBE DE CROISSANCE:
#estimation des paramètres de l'équation
# A : y max : valeur du plateau final
#X0 : date du point d'inflection : 50% de la croissance
#B : durée ou la vitesse est maximale

maxL=max(surF,na.rm=T)

#COeff pour les UC
A0<-vector()
X00<-vector()
B0<-vector()
#Vitesse maximale: ER
ER0<-vector()

#Vecteur d'erreur    #Ajoute 1 quand il y a des pb d'ajustement.
Error0<-vector()

compt=0

#On ouvre la fenetre graphique
par(mfrow=c(4,2), ask=T, mar=c(4,4,4,5))

#On commence la boucle
for ( i in a) {
compt=compt+1
#On défini x et y
x=ddj[codeUC==i]
y0=surF[codeUC==i]

#Fit pour les UC : estimation des paramètres
fit0<-NULL
try(fit0<-nls(y0 ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = 40, B = 50, X0 =150)),silent=T)
                               #start : valeurs initiales où commencer puis il cherche des valeurs aux alentours pour ajuster
                                     #Attention il ne fait pas les NA : enlève ces lignes
try(A0[compt]<-summary(fit0)$coef[1],silent=T)
try(B0[compt]<-summary(fit0)$coef[2],silent=T)
try(X00[compt]<-summary(fit0)$coef[3],silent=T)
#calcul de la vitesse max : ER
try(ER0[compt] <- 0.25 * A0[compt] / B0[compt], silent = T) 


#Représentation graphique : tracer les ajustements sur chaque courbe de croissance:
#On fait 1 graphe pour chaque codeUC

#UC
plot(ddj[codeUC==i],surF[codeUC==i],type="p", col="black",xlab="ddj",ylab="surF (cm2)",ylim=c(0,maxL),main=i)

#Ajustement : il faut qu'on prenne que les lignes ou il n'y a pas NA
data<-NULL
data<-data.frame(ddj=ddj[codeUC==i],surF=surF[codeUC==i])
data<-data[-which(is.na(data$surF)),]   #On enlève les lignes ou NA
ifelse(is.na(A0[compt]),Error0[compt]<-1,Error0[compt]<-0)     #Si pb d'ajustement, Error0 reçoit un 1
if(Error0[compt]==0) (points(data$ddj,fitted(fit0),type="l",col="black"))    #On rajoute l'ajustement t
}

#On obtient une base d'ajustment :
baseAJUSTFjose<-data.frame(a,A0,X00,B0,Error0,ER0)
baseAJUSTFjose[1:30,]
# retrait de la feuille pour laquelle il y a eu un problème d'ajustement
baseAJUSTFjose <- baseAJUSTFjose[-which(is.na(baseAJUSTFjose$A0)),]



#Distribution des surfaces finales:
par(mfrow=c(4,2))
hist(baseAJUSTFcog$A0[order(baseAJUSTFcog$A0,na.last=NA)],main="Cogshall leaves",breaks=10,xlab="Surface of leaves (cm)",col="blue",ylab="number of leaves",ylim=c(0,120),xlim=c(0,300))
hist(baseAJUSTFjose$A0[order(baseAJUSTFjose$A0,na.last=NA)],main="José leaves",breaks=10,xlab="Surface of leaves (cm)",ylab="number of leaves",col="red",ylim=c(0,120),xlim=c(0,300))

# Distribution des points d'inflexion
hist(baseAJUSTFcog$X00[order(baseAJUSTFcog$X00,na.last=NA)],main="Cogshall leaves",breaks=10,xlab="inflexion point (ddj)",col="blue",ylab="number of leaves",ylim=c(0,120),xlim=c(0,400))
hist(baseAJUSTFjose$X00[order(baseAJUSTFjose$X00,na.last=NA)],main="José leaves",breaks=10,xlab="inflexion point (ddj)",ylab="number of leaves",col="red",ylim=c(0,120),xlim=c(0,400))

# Distribution des durées de vitesse max
hist(baseAJUSTFcog$B0[order(baseAJUSTFcog$B0,na.last=NA)],main="Cogshall leaves",breaks=10,xlab="duration at max ER (ddj)",col="blue",ylab="number of leaves",ylim=c(0,120),xlim=c(0,100))
hist(baseAJUSTFjose$B0[order(baseAJUSTFjose$B0,na.last=NA)],main="José leaves",breaks=10,xlab="duration at max ER (ddj)",ylab="number of leaves",col="red",ylim=c(0,120),xlim=c(0,100))

# Distribution des vitesse max
hist(baseAJUSTFcog$ER0[order(baseAJUSTFcog$ER0,na.last=NA)],main="Cogshall leaves",breaks=10,xlab="max ER (ddj)",col="blue",ylab="number of leaves",ylim=c(0,120),xlim=c(0,2))
hist(baseAJUSTFjose$ER0[order(baseAJUSTFjose$ER0,na.last=NA)],main="José leaves",breaks=10,xlab="max ER (ddj)",ylab="number of leaves",col="red",ylim=c(0,120),xlim=c(0,2))

#En fonction de la longueur finale:
# points d'inflexion
par(mfrow=c(3,1))
plot(baseAJUSTFcog$X00~baseAJUSTFcog$A0,main="leaves",pch=16,xlab="Surface of leaves (cm)",col="blue",ylab="Inflexion point",ylim=c(0,400),xlim=c(0,150))
points(baseAJUSTFjose$X00~baseAJUSTFjose$A0,col="red",pch=16)

# Distribution des durées de vitesse max
plot(baseAJUSTFcog$B0~baseAJUSTFcog$A0,main="leaves",pch=16,xlab="Surface of leaves (cm)",col="blue",ylab="Duration at the max ER (ddj)",ylim=c(0,100),xlim=c(0,150))
points(baseAJUSTFjose$B0~baseAJUSTFjose$A0,col="red",pch=16)

# Distribution des vitesse max
plot(baseAJUSTFcog$ER0~baseAJUSTFcog$A0,main="leaves",pch=16,xlab="Surface of leaves (cm)",col="blue",ylab="Max ER",ylim=c(0,2),xlim=c(0,150))
points(baseAJUSTFjose$ER0~baseAJUSTFjose$A0,col="red",pch=16)


# Relations entre la surface finale des feuilles (A0) et la vitesse max de croissance (ER0) 
regcogfeu <- lm(baseAJUSTFcog$ER0~baseAJUSTFcog$A0)
summary(regcogfeu)           

abline(regcogfeu$coef, col="blue")

regjosefeu <- lm(baseAJUSTFjose$ER0~baseAJUSTFjose$A0)
summary(regjosefeu)           # terme constant non différent de 0 (P = 0.872)
regjosefeu <- lm(baseAJUSTFjose$ER0~baseAJUSTFjose$A0 - 1)
summary(regjosefeu)           

abline(a=0, b=regjosefeu$coef, col="red")




# Création fichier de sortie
paramAJUSTfeu <- rbind(baseAJUSTFcog, baseAJUSTFjose)
paramAJUSTfeu$a <- as.character(paramAJUSTfeu$a)
paramAJUSTfeu$a <- as.factor(paramAJUSTfeu$a)

write.table(paramAJUSTfeu,"paramAJUSTfeu.csv")


###########################

###   INFLO

###########################

############################
## CREATION DE LA BASE  ####
############################


#source("D:/Mes Donnees/These/Données croissance-temperature/12-09 Manip croissance-température/Grosse manip/Resultats/Preparation Base Inflo.R")
source("D:/MesDonnées/mes documents/manguier/modélisation phéno/stage 2014/Anne-Sarah Briand/données/fichiers de données/températures de base (Anaëlle)/TB durée croissance/bases et scripts/Preparation Base Inflo.r")                     # pour mon ordi

Baseinflo<-BaseDeCroissanceInflo

  
###########################################

## INFLo de Cogshall

##########################################

x <- c(11.12) #Mettre cette TB pour les inflos de cogshall pour le calcul des ddj

Baseinflocog <- Baseinflo[Baseinflo$variete=="cog",]
Baseinflocog$codeUC <- as.character(Baseinflocog$codeUC)
Baseinflocog$codeUC <- as.factor(Baseinflocog$codeUC)

##########################################
## On calcule les ddj


## ne pas oublier de changer TB en fonction variete et organe considéré
#############Calcul des ST pour cogshall
# PARAMETRES INITIAUX
valmin <- x[1]      # température de seuil, de base statistique : Le paramètre qu'on veut estimer
#valmin c'est x
print(valmin)

############################
## calcul des nb jours apres débourrement
############################

#On défini la date de débourrement de chaque UC qu'on copie à chaque ligne:
vec2<-NULL
base<-split(Baseinflocog,Baseinflocog$codeUC)
for (i in 1:length(base)){
ssbase<-base[[i]]
dateDeb<-as.character(ssbase$date[ssbase$DFInflo=="D"])
vec<-rep(dateDeb,dim(ssbase)[1])
vec2<-c(vec2,vec)
}
Baseinflocog<-cbind(Baseinflocog,vec2)

#Pour chaque jour on fait la différence avec la date de débourrement : nb de jours depuis débourrement.
Baseinflocog$DAB<-difftime(strptime(Baseinflocog$date,"%d/%m/%y"),strptime(Baseinflocog$vec2,"%d/%m/%y"),
units="days")
#Les 0 correspondent aux jours mm : gain reçu pendant la journée de débourrement.
#On commence les gains à DAB = 0
#les - sont pour les jours avant débourrement


#########################################
########### Calcul pour chaque UC des ddj
#########################################

#On sépare les UC:
rep2<-split(Baseinflocog,Baseinflocog$codeUC)
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
d<-merge(d,dj[[i]],by.x="DAB",by.y="DAB",sort=TRUE,all.x=TRUE)}
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



Baseinflocog<-BaseFin
codeUC<-Baseinflocog$codeUC
longueurInflo<-Baseinflocog$longueurInflo
ddj<-Baseinflocog$ddj



##################################
###### Calcul des paramètres INFLO ##
##################################

Baseinflocog[1:10,]
a<-unique(codeUC)


###############################################################################
#EQUATION D'AJUSTEMENT DE LA COURBE DE CROISSANCE:
#estimation des paramètres de l'équation
# A : y max : valeur du plateau final
#X0 : date du point d'inflection : 50% de la croissance
#B : durée ou la vitesse est maximale

maxL=max(longueurInflo,na.rm=T)

#COeff pour les UC
A0<-vector()
X00<-vector()
B0<-vector()
#Vitesse maximale: ER
ER0<-vector()

#Vecteur d'erreur    #Ajoute 1 quand il y a des pb d'ajustement.
Error0<-vector()

compt=0

#On ouvre la fenetre graphique
par(mfrow=c(4,2), ask=T, mar=c(4,4,4,5))

#On commence la boucle
for ( i in a) {
compt=compt+1
#On défini x et y
x=ddj[codeUC==i]
y0=longueurInflo[codeUC==i]

#Fit pour les UC : estimation des paramètres
fit0<-NULL
try(fit0<-nls(y0 ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = 40, B = 50, X0 =150)),silent=T)
                               #start : valeurs initiales où commencer puis il cherche des valeurs aux alentours pour ajuster
                                     #Attention il ne fait pas les NA : enlève ces lignes
try(A0[compt]<-summary(fit0)$coef[1],silent=T)
try(B0[compt]<-summary(fit0)$coef[2],silent=T)
try(X00[compt]<-summary(fit0)$coef[3],silent=T)
#calcul de la vitesse max : ER
try(ER0[compt] <- 0.25 * A0[compt] / B0[compt], silent = T) 


#Représentation graphique : tracer les ajustements sur chaque courbe de croissance:
#On fait 1 graphe pour chaque codeUC

#UC
plot(ddj[codeUC==i],longueurInflo[codeUC==i],type="p", col="black",xlab="ddj",ylab="longueurInflo (cm)",ylim=c(0,maxL),main=i)

#Ajustement : il faut qu'on prenne que les lignes ou il n'y a pas NA
data<-NULL
data<-data.frame(ddj=ddj[codeUC==i],longueurInflo=longueurInflo[codeUC==i])
data<-data[-which(is.na(data$longueurInflo)),]   #On enlève les lignes ou NA
ifelse(is.na(A0[compt]),Error0[compt]<-1,Error0[compt]<-0)     #Si pb d'ajustement, Error0 reçoit un 1
if(Error0[compt]==0) (points(data$ddj,fitted(fit0),type="l",col="black"))    #On rajoute l'ajustement t
}

#On obtient une base d'ajustment :
baseAJUSTINFLOcog<-data.frame(a,A0,X00,B0,Error0,ER0)
baseAJUSTINFLOcog[1:30,]



###########################################

## INFLO de José

##########################################

x <- c(12.23) #Mettre cette TB pour les inflos de JOSE pour le calcul des ddj#

Baseinflojose <- Baseinflo[Baseinflo$variete=="jose",]
Baseinflojose$codeUC <- as.character(Baseinflojose$codeUC)
Baseinflojose$codeUC <- as.factor(Baseinflojose$codeUC)


##########################################
## On calcule les ddj


## ne pas oublier de changer TB en fonction variete et organe considéré
#############Calcul des ST pour cogshall
# PARAMETRES INITIAUX
valmin <- x[1]      # température de seuil, de base statistique : Le paramètre qu'on veut estimer
#valmin c'est x
print(valmin)

############################
## calcul des nb jours apres débourrement
############################

#On défini la date de débourrement de chaque UC qu'on copie à chaque ligne:
vec2<-NULL
base<-split(Baseinflojose,Baseinflojose$codeUC)
for (i in 1:length(base)){
ssbase<-base[[i]]
dateDeb<-as.character(ssbase$date[ssbase$DFInflo=="D"])
vec<-rep(dateDeb,dim(ssbase)[1])
vec2<-c(vec2,vec)
}
Baseinflojose<-cbind(Baseinflojose,vec2)

#Pour chaque jour on fait la différence avec la date de débourrement : nb de jours depuis débourrement.
Baseinflojose$DAB<-difftime(strptime(Baseinflojose$date,"%d/%m/%y"),strptime(Baseinflojose$vec2,"%d/%m/%y"),
units="days")
#Les 0 correspondent aux jours mm : gain reçu pendant la journée de débourrement.
#On commence les gains à DAB = 0
#les - sont pour les jours avant débourrement


#########################################
########### Calcul pour chaque UC des ddj
#########################################

#On sépare les UC:
rep2<-split(Baseinflojose,Baseinflojose$codeUC)
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
d<-merge(d,dj[[i]],by.x="DAB",by.y="DAB",sort=TRUE,all.x=TRUE)}
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



Baseinflojose<-BaseFin
codeUC<-Baseinflojose$codeUC
longueurInflo<-Baseinflojose$longueurInflo
ddj<-Baseinflojose$ddj



##################################
###### Calcul des paramètres INFLO ##
##################################

Baseinflojose[1:10,]
a<-unique(codeUC)


###############################################################################
#EQUATION D'AJUSTEMENT DE LA COURBE DE CROISSANCE:
#estimation des paramètres de l'équation
# A : y max : valeur du plateau final
#X0 : date du point d'inflection : 50% de la croissance
#B : durée ou la vitesse est maximale

maxL=max(longueurInflo,na.rm=T)

#COeff pour les UC
A0<-vector()
X00<-vector()
B0<-vector()
#Vitesse maximale: ER
ER0<-vector()

#Vecteur d'erreur    #Ajoute 1 quand il y a des pb d'ajustement.
Error0<-vector()

compt=0

#On ouvre la fenetre graphique
par(mfrow=c(4,2), ask=T, mar=c(4,4,4,5))

#On commence la boucle
for ( i in a) {
compt=compt+1
#On défini x et y
x=ddj[codeUC==i]
y0=longueurInflo[codeUC==i]

#Fit pour les UC : estimation des paramètres
fit0<-NULL
try(fit0<-nls(y0 ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = 40, B = 50, X0 =150)),silent=T)
                               #start : valeurs initiales où commencer puis il cherche des valeurs aux alentours pour ajuster
                                     #Attention il ne fait pas les NA : enlève ces lignes
try(A0[compt]<-summary(fit0)$coef[1],silent=T)
try(B0[compt]<-summary(fit0)$coef[2],silent=T)
try(X00[compt]<-summary(fit0)$coef[3],silent=T)
#calcul de la vitesse max : ER
try(ER0[compt] <- 0.25 * A0[compt] / B0[compt], silent = T) 


#Représentation graphique : tracer les ajustements sur chaque courbe de croissance:
#On fait 1 graphe pour chaque codeUC

#UC
plot(ddj[codeUC==i],longueurInflo[codeUC==i],type="p", col="black",xlab="ddj",ylab="longueurInflo (cm)",ylim=c(0,maxL),main=i)

#Ajustement : il faut qu'on prenne que les lignes ou il n'y a pas NA
data<-NULL
data<-data.frame(ddj=ddj[codeUC==i],longueurInflo=longueurInflo[codeUC==i])
data<-data[-which(is.na(data$longueurInflo)),]   #On enlève les lignes ou NA
ifelse(is.na(A0[compt]),Error0[compt]<-1,Error0[compt]<-0)     #Si pb d'ajustement, Error0 reçoit un 1
if(Error0[compt]==0) (points(data$ddj,fitted(fit0),type="l",col="black"))    #On rajoute l'ajustement t
}

#On obtient une base d'ajustment :
baseAJUSTINFLOjose<-data.frame(a,A0,X00,B0,Error0,ER0)
baseAJUSTINFLOjose[1:30,]




#Distribution des surfaces finales:
par(mfrow=c(4,2))
hist(baseAJUSTINFLOcog$A0[order(baseAJUSTINFLOcog$A0,na.last=NA)],main="Cogshall INFLO",breaks=10,xlab="Length of INFLO (cm)",col="blue",ylab="number of INFLO",ylim=c(0,20),xlim=c(0,50))
hist(baseAJUSTINFLOjose$A0[order(baseAJUSTINFLOjose$A0,na.last=NA)],main="José INFLO",breaks=10,xlab="Length of INFLO (cm)",ylab="number of INFLO",col="red",ylim=c(0,20),xlim=c(0,50))

# Distribution des points d'inflexion
hist(baseAJUSTINFLOcog$X00[order(baseAJUSTINFLOcog$X00,na.last=NA)],main="Cogshall INFLO",breaks=10,xlab="inflexion point (ddj)",col="blue",ylab="number of INFLO",ylim=c(0,20),xlim=c(0,300))
hist(baseAJUSTINFLOjose$X00[order(baseAJUSTINFLOjose$X00,na.last=NA)],main="José INFLO",breaks=10,xlab="inflexion point (ddj)",ylab="number of INFLO",col="red",ylim=c(0,20),xlim=c(0,300))

# Distribution des durées de vitesse max
hist(baseAJUSTINFLOcog$B0[order(baseAJUSTINFLOcog$B0,na.last=NA)],main="Cogshall INFLO",breaks=10,xlab="duration at max ER (ddj)",col="blue",ylab="number of INFLO",ylim=c(0,20),xlim=c(0,100))
hist(baseAJUSTINFLOjose$B0[order(baseAJUSTINFLOjose$B0,na.last=NA)],main="José INFLO",breaks=10,xlab="duration at max ER (ddj)",ylab="number of INFLO",col="red",ylim=c(0,20),xlim=c(0,100))

# Distribution des vitesse max
hist(baseAJUSTINFLOcog$ER0[order(baseAJUSTINFLOcog$ER0,na.last=NA)],main="Cogshall INFLO",breaks=10,xlab="max ER (ddj)",col="blue",ylab="number of INFLO",ylim=c(0,20),xlim=c(0,0.5))
hist(baseAJUSTINFLOjose$ER0[order(baseAJUSTINFLOjose$ER0,na.last=NA)],main="José INFLO",breaks=10,xlab="max ER (ddj)",ylab="number of INFLO",col="red",ylim=c(0,20),xlim=c(0,0.5))

#En fonction de la longueur finale:
# points d'inflexion
par(mfrow=c(3,1))
plot(baseAJUSTINFLOcog$X00~baseAJUSTINFLOcog$A0,main="INFLO",pch=16,xlab="Length of INFLO (cm)",col="blue",ylab="Inflexion point",ylim=c(0,300),xlim=c(0,50))
points(baseAJUSTINFLOjose$X00~baseAJUSTINFLOjose$A0,col="red",pch=16)

# Distribution des durées de vitesse max
plot(baseAJUSTINFLOcog$B0~baseAJUSTINFLOcog$A0,main="INFLO",pch=16,xlab="Length of INFLO (cm)",col="blue",ylab="Duration at the max ER (ddj)",ylim=c(0,100),xlim=c(0,50))
points(baseAJUSTINFLOjose$B0~baseAJUSTINFLOjose$A0,col="red",pch=16)

# Distribution des vitesse max
plot(baseAJUSTINFLOcog$ER0~baseAJUSTINFLOcog$A0,main="INFLO",pch=16,xlab="Length of INFLO (cm)",col="blue",ylab="Max ER",ylim=c(0,0.5),xlim=c(0,50))
points(baseAJUSTINFLOjose$ER0~baseAJUSTINFLOjose$A0,col="red",pch=16)


# Relations entre la longueur finale de l'inflo (A0) et la vitesse max de croissance (ER0) 
regcoginflo <- lm(baseAJUSTINFLOcog$ER0~baseAJUSTINFLOcog$A0)
summary(regcoginflo)            # terme constant non différent de 0 (P = 0.399)
regcoginflo <- lm(baseAJUSTINFLOcog$ER0~baseAJUSTINFLOcog$A0 - 1)
summary(regcoginflo)            

abline(a=0, b=regcoginflo$coef, col="blue")

regjoseinflo <- lm(baseAJUSTINFLOjose$ER0~baseAJUSTINFLOjose$A0)
summary(regjoseinflo)           # terme constant non différent de 0 (P = 0.297)
regjoseinflo <- lm(baseAJUSTINFLOjose$ER0~baseAJUSTINFLOjose$A0 - 1)
summary(regjoseinflo)                

abline(a=0, b=regjoseinflo$coef, col="red")




# Création fichier de sortie
paramAJUSTinflo <- rbind(baseAJUSTINFLOcog, baseAJUSTINFLOjose)
# on rajoute des infos de base en couplant avec la base nT
source("D:/MesDonnées/mes documents/manguier/modélisation phéno/stage 2014/Anne-Sarah Briand/données/fichiers de données/températures de base (Anaëlle)/TB durée croissance/bases et scripts/creation base nTInflo.r")
summary(nTInflo)
ind <- match(nTInflo$a, paramAJUSTinflo$a)
paramAJUSTinfloglob <- cbind(nTInflo, paramAJUSTinflo[ind,])

write.table(paramAJUSTinfloglob,"paramAJUSTinfloglob.csv")

