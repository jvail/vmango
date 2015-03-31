#setwd ("D:/Mes documents/VCAT 2008-2009/Modèle mangue/3- MODELE ARBRE")
#getwd()
#tempstpierre2002 <- read.table("tempstpierre2002.txt", header=T, sep="\t")
#     tempstpierre2002$Date <- as.Date(tempstpierre2002$Date, "%d/%m/%y")
#  "Avis : class discarded from column ‘Date’"
#rayostpierre2002 <- read.table("rayorun02f1bis.txt", header=T, sep="\t")
#      rayostpierre2002$DATE <- as.Date(rayostpierre2002$DATE, "%d/%m/%y")
#k1runquant <- read.table("k1runquant.txt", header=T, sep="\t")
#source("D:/Mes documents/VCAT 2008-2009/Modèle mangue/convertmoyjdj.R")


mangsucreauB202 <- function(x)
{

# x <- 10

nombrefr <- x     	# nb fruits sur arbre B2
nombrefr <<- nombrefr


###----- CREATION D'UN TABLEAU DES TEMPERATURES AVEC DAB MOYEN défini autour de la moyenne de floraison, le 19 août : DABmoy------------

dateflomoy <- as.character ("19/08/02")
dateflomoy <- as.Date(dateflomoy, "%d/%m/%y")

tempP <- tempstpierre2002[tempstpierre2002$Date>=dateflomoy,]
tempP <- cbind(tempP,c(0:(dim(tempP)[1]-1)))
dimnames(tempP)[[2]] [dim(tempP)[2]] <- c("DABmoy")

tempN <- tempstpierre2002[tempstpierre2002$Date<dateflomoy,]
tempN <- cbind(tempN,c(-(dim(tempN)[1]):-1))
dimnames(tempN)[[2]] [dim(tempN)[2]] <- c("DABmoy")

temp <- rbind (tempN, tempP)
###----------------------------------------------------------------------------------------------------


###----- CREATION DES TABLEAUX DE SORTIES----------------------------------------- 

# tableau de sorties : FIN DE CROISSANCE

resultfincroiss <- matrix(nrow=nombrefr,ncol=11)
dimnames(resultfincroiss)[[2]] <- c("DABdeb","ddjdeb","DABfin","ddjfin","LF","Pini","clim","datflo","MS","MF","sucrose")
tour <- 1         # pour pouvoir rempli le tableau final resultdistrib à chaque fruit

# tableau de sorties : enregistrement des VARIABLES AU COURS DE LA CROISSANCE
    
      #taux de photosynthese foliaire maximale     
result.Pmax <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.Pmax[,1] <- temp$DABmoy
dimnames(result.Pmax)[[2]] <- c("DABmoy", 1:nombrefr)              
     #assimilats de la photsynthèse foliaire
result.photo.fol <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.photo.fol[,1] <- temp$DABmoy
dimnames(result.photo.fol)[[2]] <- c("DABmoy", 1:nombrefr)         
     #assimilats utilises par les fruits provenant de la photosynthèse et des remobiisations
result.assutilfruits <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.assutilfruits[,1] <- temp$DABmoy
dimnames(result.assutilfruits)[[2]] <- c("DABmoy", 1:nombrefr)
    #demande du fruit    
result.Dfruit <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.Dfruit[,1] <- temp$DABmoy
dimnames(result.Dfruit)[[2]] <- c("DABmoy", 1:nombrefr)  
    #respiration d'entretien des parties vegetatives
result.RE.veget <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.RE.veget[,1] <- temp$DABmoy
dimnames(result.RE.veget)[[2]] <- c("DABmoy", 1:nombrefr)
    #respi d'entretien des parties fructiferes            
result.RE.fruct <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.RE.fruct[,1] <- temp$DABmoy
dimnames(result.RE.fruct)[[2]] <- c("DABmoy", 1:nombrefr)
    #reserves (feuilles & rameau) facilement utilisables  
result.resfu <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.resfu[,1] <- temp$DABmoy
dimnames(result.resfu)[[2]] <- c("DABmoy", 1:nombrefr)             
    #reserves feuilles difficilement utilisables    
result.resdu.feuil <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.resdu.feuil[,1] <- temp$DABmoy
dimnames(result.resdu.feuil)[[2]] <- c("DABmoy", 1:nombrefr)         
    #reserves feuilles difficilement utilisables    
result.resdu.ram <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.resdu.ram[,1] <- temp$DABmoy
dimnames(result.resdu.ram)[[2]] <- c("DABmoy", 1:nombrefr)           
    # MS du fruit, en g          
result.fruitMS <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.fruitMS[,1] <- temp$DABmoy
dimnames(result.fruitMS)[[2]] <- c("DABmoy", 1:nombrefr) 
    # réserves des feuilles, en g          
result.res.feuil <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.res.feuil[,1] <- temp$DABmoy
dimnames(result.res.feuil)[[2]] <- c("DABmoy", 1:nombrefr) 
    # réserves du rameau, en g          
result.res.ram <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.res.ram[,1] <- temp$DABmoy
dimnames(result.res.ram)[[2]] <- c("DABmoy", 1:nombrefr)

    # potentiel hydrique du rameau en MPa   
result.potram <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.potram[,1] <- temp$DABmoy
dimnames(result.potram)[[2]] <- c("DABmoy", 1:nombrefr)
    # potentiel osmotique du fruit en MPa  
result.potosmo <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.potosmo[,1] <- temp$DABmoy
dimnames(result.potosmo)[[2]] <- c("DABmoy", 1:nombrefr)         
    # pression de turgesence en MPa   
result.turgf <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.turgf[,1] <- temp$DABmoy
dimnames(result.turgf)[[2]] <- c("DABmoy", 1:nombrefr)         
    # transpiration du fruit en g 
result.transpi <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.transpi[,1] <- temp$DABmoy
dimnames(result.transpi)[[2]] <- c("DABmoy", 1:nombrefr)         
    # flux d'eau entrant dans le fruit en g 
result.fludoentr <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.fludoentr[,1] <- temp$DABmoy
dimnames(result.fludoentr)[[2]] <- c("DABmoy", 1:nombrefr)         
    # respiration du mésocarpe du fruit en g
result.respi <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.respi[,1] <- temp$DABmoy
dimnames(result.respi)[[2]] <- c("DABmoy", 1:nombrefr)         
    # poids frais de la pulpe (pulpe + peau) en g   
result.MFpulpe <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.MFpulpe[,1] <- temp$DABmoy
dimnames(result.MFpulpe)[[2]] <- c("DABmoy", 1:nombrefr)         
    # poids frais du fruit en g   
result.MFfruit <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.MFfruit[,1] <- temp$DABmoy
dimnames(result.MFfruit)[[2]] <- c("DABmoy", 1:nombrefr)         
    # poids d'eau du fruit en g  
result.eau <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.eau[,1] <- temp$DABmoy
dimnames(result.eau)[[2]] <- c("DABmoy", 1:nombrefr)         
    # poids sec du fruit, calculé dans "fruits", en g
result.MSfruit <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.MSfruit[,1] <- temp$DABmoy
dimnames(result.MSfruit)[[2]] <- c("DABmoy", 1:nombrefr)
    # concentration en saccharose en g/100gMF (%) 
result.cosa <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.cosa[,1] <- temp$DABmoy
dimnames(result.cosa)[[2]] <- c("DABmoy", 1:nombrefr)   
    # concentration en glucose en g/100gMF (%)   
result.cogl <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.cogl[,1] <- temp$DABmoy
dimnames(result.cogl)[[2]] <- c("DABmoy", 1:nombrefr)   
    # concentration en fructose en g/100gMF (%)   
result.cofr <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.cofr[,1] <- temp$DABmoy
dimnames(result.cofr)[[2]] <- c("DABmoy", 1:nombrefr)   
    # concentration en malate en g/100gMF (%)  
result.comal <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.comal[,1] <- temp$DABmoy
dimnames(result.comal)[[2]] <- c("DABmoy", 1:nombrefr)   
    # concentration en citrate en g/100gMF (%)   
result.cocit <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.cocit[,1] <- temp$DABmoy
dimnames(result.cocit)[[2]] <- c("DABmoy", 1:nombrefr)   
    # concentration en amidon en g/100gMF (%) 
result.coam <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.coam[,1] <- temp$DABmoy
dimnames(result.coam)[[2]] <- c("DABmoy", 1:nombrefr)
    # saveur sucrée en g/100gMF
result.souit <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.souit[,1] <- temp$DABmoy
dimnames(result.souit)[[2]] <- c("DABmoy", 1:nombrefr)   
    # saveur acide en g/100gMF 
result.acid <- matrix(nrow=nrow(temp), ncol=nombrefr+1)
result.acid[,1] <- temp$DABmoy
dimnames(result.acid)[[2]] <- c("DABmoy", 1:nombrefr)


                 
###----------------------------------------------------------------------------------------------------


###----- DETERMINATIONS DES VARIABLES D'ENTREE pour l'ensemble des fruirs----------------------------------------- 

# poids initial de tous les fruits pris dans une loi normale
P0.fruit <- 0.97 * rnorm(nombrefr,mean=13.9,sd=4.1) + 0.03 * rnorm(nombrefr,mean=29.2,sd=0.66)   # bimodale de ML
#P0.fruit <- rnorm(nombrefr,mean=14.55205,sd=5.1438)        # unimodale de ML
#P0.fruit <- 0.75*rnorm(nombrefr,mean=17.71,sd=4.9)+0.25*rnorm(nombrefr,mean=32.82,sd=11.21)       # bimodale de GD (2000-2002-2004)
#P0.fruit <- rnorm(nombrefr,mean=5,sd=3) 
 
# leaf-to-fruit ratio de tous les fruits pris dans une loi uniforme continue
LF <- runif(nombrefr,10,100)
LF <<- LF

# environnement lumineux de tous les fruits pris dans une loi uniforme discrète (5 envir. différents)
envirlum <- cbind(k1runquant[,"q10"],k1runquant[,"q25"],k1runquant[,"q50"],k1runquant[,"q75"],k1runquant[,"q90"])
tourenvirlum <- sample(5,nombrefr,T)    # indice pour choisir un des 5 environnements lumineux à chaque tour 

DABo <- c(rep(NA,nombrefr))
###----------------------------------------------------------------------------------------------------

# ======================================================================================
# HYPOTHSES DE DEPART

# Pmax n'est pas dépendante du potentiel hydrique foliaire
# Feuilles et rameaux ne poussent pas --> masse de la structure de ces organes constante
# Le rameau ne se courbe pas au cours de la croissance des fruits --> ombrage des feuilles constant
# Le fruit ne photosythètise pas
# ===========================================================================================


# ======================================================================================
# DEBUT DE LA BOUCLE SUR LE NOMBRE DE FRUITS
# ===========================================================================================

for (w in c(1:nombrefr))
{

###----- DEGRES JOURS ET DEBUT DE SIMULATION-----------------------------------------

# date de floraison prise dans une loi normale
# les petits fruits ont fleuris plus tard ! 
if (P0.fruit[w]<15) {
    DABo[w] <- rnorm(1,mean=5,sd=4.00)
    } else { DABo[w] <- rnorm(1,mean=-5,sd=4.00) }
#DABo[w] <- rnorm(1,mean=0,sd=11.00)

# tableau qui commence à la date de floraison du fruit considérée DABo[w], comme temp mais interne à la boucle du fruit
# définition du DAB réel pour le fruit considéré : DAB
temp2 <- temp[temp$DABmoy>=round(DABo[w]),]
temp2$DAB <- seq (1:length(temp2$DABmoy))

# calcul des degrés jours à partir de cette date-là
valmin <- 16
valmax <- 40
calcddj <- as.vector(cumsum(apply(temp2[,3:4],1,FUN=function(x,truc,bid)
			convertmoyjdj(Tu=bid,Tl=truc,Tmax=x[2],Tmin=x[1]),truc=valmin,bid=valmax)))
temp2 <- cbind(temp2,calcddj)
dimnames(temp2)[[2]] [dim(temp2)[2]] <- c("DEGJOUR")
temp2 <<- temp2		

ddjy <- 352.72    # la fin de la multiplication cellulaire dans le fruit : recalcul basé sur suivi 2002 f2 et valmin = 20°C 

# aller chercher dans "temp2" la valeur la plus proche et supérieure à ddjy
ddjini <- min(temp2$DEGJOUR[temp2$DEGJOUR>ddjy])
jdebsim <- temp2[match(ddjini,temp2[,"DEGJOUR"]),"DAB"]    # DAB réélle du fruit à laquelle débute la simulation
                         
# fin de simulation fixée à 1100 ddj, moyenne des récoltes thèse Mathieu
ddjfin <- min(temp2$DEGJOUR[temp2$DEGJOUR>1500])
jfinsim <- temp2[match(ddjfin,temp2[,"DEGJOUR"]),"DAB"] 
###----------------------------------------------------------------------------------------------------



###----- DEFINITION DE PARAMETRES ET VARIABLES -----------------------------------------   

valtour <- vector(length=8)     # ligne de résultats du fruit w dans resuldistrib

PAR <- rayostpierre2002
# création d'un tableau qui commence à la date de début de simulation, 
  #qui fini le même jour que temp2 et qui possède la colonne DAB réelle du fruit
PAR$indice <- seq(1:nrow(PAR))
  d <- temp2[jdebsim,"Date"]
  f <- temp2[nrow(temp2),"Date"]
  a1 <- PAR[PAR$DATE==d&PAR$HEURE==1,"indice"]
  a2 <- PAR[PAR$DATE==f&PAR$HEURE==24,"indice"]
PAR <- PAR[PAR$indice>=a1& PAR$indice<=a2,]
PAR$DAB <- match(PAR[,"DATE"],temp2[,"Date"])  
  
if (P0.fruit[w] < 5) { 
  P0.fruit[w] <- 5 
  } else {
      if (P0.fruit[w] > 31) { 
        P0.fruit[w] <- 31 
        } else {
        P0.fruit[w] <- P0.fruit[w]}
      }

poids.rameau <- (41.83 + 77.41) / 2   # moy expé, fixe
poids.feuilles <- 0.8                 # poids sec feuille, fixe
partMS.reserves0.rameau <- 0.1        # moy expé
partMS.reserves0.feuilles <- 0.074    # moy expé

croissance <- jdebsim:jfinsim   #numeros des jours de croissance en DAB réelle du fruit
durcroiss <- length(croissance) #nombre de jours de croissance

# PARAMETRES POUR ASSIMILATION
# k1 et k2 : coefficients de pondération du rayonnement inter et intra-rameau
k1.fin <- envirlum[,tourenvirlum[w]]
k2.fin <- rep(0.88,24)
k1 <- matrix(rep(k1.fin,durcroiss), nrow=durcroiss,ncol=24,byrow=T)
k2 <- matrix(rep(k2.fin,durcroiss*24), nrow=durcroiss,ncol=24)
 
p1 <- 3.85
p2 <- 33.23
p3 <- 0.483    # diagn 121108 --> param utilisée dans ML 0.786 - param utlisé dans GD (=thèse) 0.483 
p4 <- 0.034

r3 <- 0.0529

# PARAMETRES POUR RESPIRATIONS D'ENTRETIEN
MRR.rameau <- 0.000858
MRR.feuilles <- 0.000156
MRR.fruits <- 0.0015
Q10.rameau <- 1.96
Q10.feuilles <- 2.11
Q10.fruits <- 1.9

# PARAMETRES POUR DEMANDE DU FRUIT 

gamma.feuilles <- 0.0162         # recalibration ML
gamma.rameau <- 0.0164           # recalibration ML
cram <- 0.4387
cfeuil <- 0.4051
cfruit <- 0.407       # mettre le 0.424 de la thèse ?
GRCfruit <- 0.04
RGRini.fruit <- 0.0105          # reparamétrisation ML
a.fruit <- 16.736             # reparamétrisation ML
b.fruit <- 0.624             # reparamétrisation ML
psi <- 0.3
psi <- 0.3

# PARAMETRES POUR LE MODULE EAU 

citratedep <- 0.000055
sucrosedep <- 0.0047
ro <- 5544         # 230.993
R <- 83
# relations allométriques
A1peau <- 0.4086
B1peau <- 0.7641
A1pulpe <- 0.5219
B1pulpe <- 1.0584
A2peau <- 0.4086
B2peau <- 0.7428
A2pulpe <- 0.5874
B2pulpe <- 1.0584
A3peau <- 7.0330
B3peau <- 0.5769
A3pulpe <- 20.5617
B3pulpe <- 0.6336
A3noyau <- 9.9096
B3noyau <- 0.3627
Anoyau <- 8.5353
Bnoyau <- 0.5883
# modèle sucrose
#mu.sucrose <- 0.0282784    # en DAB  (script ML)
mu.sucrose <- 0.0031662     # en ddj 
# propriétés des parois des cellules
ddini <- 20.769 * P0.fruit[w] + 518.87     # ddini variable en fonction des années (peut être effet de la [N]
tau <- 0.966
phimax <- 0.414
acnst <- 0.3732
Y0 <- 0.0
hcst <- 0.002027
# modèle citrate
qm <- 9.58e-5	
qg <- 0.0033
Q10 <- 1.96
Temp0 <- 296									# en kelvin   : mod ML --> 298    papier ISHS  --> 296
c1 <- 1.05e-5		    #mod ML  -2.5825e-5    #papier ISHS 1.05e-5	
c2 <- -1.275     #mod ML  0.8225     #papier ISHS -1.275
c3 <- 0.2927       #mod ML  0.18477    #papier ISHS 0.2927
c4 <- 7968        #mod ML  5028.4   #papier ISHS 7968

###----------------------------------------------------------------------------------------------------


###----- SORTIES DE CONTROLE -----------------------------------------  

# les dimensions sont en durcroiss-1 car le premier jour n'est pas considéré comme un jour de croissance
assutilfruits <- rep(NA,durcroiss-1)  #assimilats utilises par les fruits provenant de la photosynthèse et des remobiisations
RE.veget <- rep(NA,durcroiss-1)            #respi d'entretien des parties vegetatives
RE.fruct <- rep(NA,durcroiss-1)            #respi d'entretien des parties fructiferes
photo.fol <- rep(NA,durcroiss-1)             #assimilats de la photsy. foliaire
resfu <- rep(NA,durcroiss-1)               #reserves (feuilles & rameau) facilement utilisables
resdu.feuil <- rep(NA,durcroiss-1)          #reserves feuilles difficilement utilisables
resdu.ram <- rep(NA,durcroiss-1)           #reserves feuilles difficilement utilisables
Dfruit <- rep(NA,durcroiss-1)               #demande du fruit
Pmax <- rep(NA,durcroiss-1)                 #taux de photosynthese fol. max

# module eau
potram <- rep(NA,durcroiss-1)             # potentiel hydrique du rameau (MPa)
potosmo <- rep(NA,durcroiss-1)            # potentiel osmotique (MPa)
turgf <- rep(NA,durcroiss-1)              # pression de turgescence du fruit
transpi <- rep(NA,durcroiss-1)            # transpiration du fruit (g d'eau/j)
fludoentr <- rep(NA,durcroiss-1)          # flux d'eau entrant dans le fruit (g/j)
respi <- rep(NA,durcroiss-1)              # respiration du mésocrape du fruit (mol citrate/jour)
sucrose <- rep(NA,durcroiss-1)				    # conc. en sacc (g/g MFpulpe)
propsacc <- rep(NA,durcroiss)					    # conc. de sacc (g/gMSpulpe)
citrate <- rep(NA,durcroiss)				      # conc. en citrate (mol/g MFpulpe)  M=192.12 g/mol
    citrate[1] <- citratedep
varcitrate <- rep(NA,durcroiss-1)         # variation de la conc. en citrate
ncompsol <- rep(NA,durcroiss)             # nb de mol de composés solubles
Cssm <- rep(NA,durcroiss)                 
Y <- rep(NA,durcroiss-1) 
potcat <- rep(NA,durcroiss-1)
potaci <- rep(NA,durcroiss-1)
potsuc <- rep(NA,durcroiss-1)

###----------------------------------------------------------------------------------------------------



###----- DEFINITION ET INITIALISATION DES OBJETS DU SYSTEME -----------------------------------------  

rameau <- list(poids.rameau*(1-partMS.reserves0.rameau), 
                rep(NA,durcroiss),
                cram,
                gamma.rameau)
names(rameau) <- c("struc","res","c","gamma")
rameau$res[1] <- partMS.reserves0.rameau*cram*poids.rameau
#struc: partie structure du rameau (g MS) - CONSTANTE
#res: partie reserves du rameau (g de carbone)
#c: coeff de conversion en nbre de g de carbone/g de MS
#gamma: part des reserves du rameau qui peut etre mobilisee - CONSTANTE
#poids.rameau: poids total initial du rameau (g MS) - reste CONSTANT
#partMS.reserves0: part de la matiere seche totale dans les reserves

feuilles <- list(poids.feuilles * (1-partMS.reserves0.feuilles) * LF[w],
                  rep(NA,length=durcroiss),
                  cfeuil,
                  gamma.feuilles,
                  psi,
                  LF[w])
names(feuilles) <- c("struc","res","c","gamma","psi","LF")
feuilles$res[1] <- poids.feuilles * partMS.reserves0.feuilles * LF[w] * cfeuil  
#struc: partie structure des feuilles (g MS) - CONSTANTE
#res: partie reserves des feuilles (g de carbone)
#c: coeff de conversion en g de carbone/g de MS
#gamma: part des reserves des feuilles qui peut etre mobilisee - CONSTANTE
#psi: part limite du poids des reserves des feuilles / poids total des feuilles - CONSTANTE
#LF: nombre de feuilles

DMfmax <- a.fruit * (P0.fruit[w] ^ b.fruit) 
fruits <- list(rep(NA,length=durcroiss),
                GRCfruit,
                cfruit,
                RGRini.fruit,
                DMfmax)
names(fruits) <- c("gMS","GRC","c","RGRini","DMfmax")
fruits$gMS[1] <- P0.fruit[w]
#GRC: coefficient de conversion en g de carbone / g de MS
#c: coeff de conversion en nbre de g de carbone/g de MS
#RGRini, DMfmax : parametres de la genard3 (en fonction des degres jours)

sucres <- vector(mode="list",length=14)
for (i in 1:14) sucres[[i]] <- rep(NA,length=durcroiss)
names(sucres) <- c("pf.fruit","pf.pulpe","ps.fruit","ps.pulpe","tms.pulpe","eau.fruit",
						        "cosa","cogl","cofr","comal","cocit","coam","souit","acid")
# pf.fruit: poids frais du fruit individuel (g)
# pf.pulpe: poids frais de la pulpe (+ peau) du fruit individuel (g)
# ps.fruit: poids sec du fruit individuel (g)
# ps.pulpe: poids sec de la pulpe (+peau) du fruit individuel (g)
# tms.pulpe: teneur en matiere seche de la pulpe(+ peau) (ratio sans dim, entre 0 et 1)
# eau.fruit: poids d'eau du fruit individuel (g)
# cosa: concentration en saccharose dans le fruit individuel(g de sac/100 g de masse fraiche, en%)
# cogl, cofr, comal, cocit, coam : idem
# souit: indice de caractere sucré, calcule avec le pouvoir sucrant des différents sucres
# acid: indice de caractere acide, calcule avec le pouvoir acide des différents acides

compart <- vector(mode="list",length=6)
for (i in 1:6) compart[[i]] <- rep(NA,length=durcroiss)
names(compart) <- c("pf.peau","pf.pulpe","pf.noyau","ps.peau","ps.pulpe","ps.noyau")
# pf.peau: poids frais de la peau du fruit individuel (g)
# pf.pulpe: poids frais de la pulpe du fruit individuel (g)
# pf.noyau: poids frais du noyau du fruit individuel (g)
# ps.peau: poids sec de la peau du fruit individuel (g)
# ps.pulpe: poids sec de la pulpe du fruit individuel (g)
# ps.noyau: poids sec du noyau du fruit individuel (g)


# ======================================================================================
# DEBUT DE LA BOUCLE DE CROISSANCE DU FRUIT INDIVIDUEL
# ===========================================================================================

for (i in 1:(durcroiss-1))
 {

#calcul surface foliaire (m2)    
surfol <- 0.0051 * LF[w]^0.937   #  voir meilleure relation ek diametre à la base 
  

# ======================================================================================
# ASSIMILATION DE CARBONE
# ===========================================================================================

###----- CALCUL DE Pmax ----------------------------------------------------------------------------------------  

temper <- temp2[temp2[,"DAB"]==croissance[i],"TM"]
a <- temp2[temp2[,"DAB"]==croissance[i+1],"DEGJOUR"] - temp2[temp2[,"DAB"]==croissance[i],"DEGJOUR"]
    
# demande de croissance du fruit (gC/j)
Dfruit[i] <- fruits$gMS[i] * fruits$RGRini * a * (1-(fruits$gMS[i]/fruits$DMfmax)) * (fruits$c + fruits$GRC)
      # la demande tient compte des coûts (en C) de construction 
if (i==1) { Pmax[i] <- 9.63} else {Pmax[i] <- (p1*(Dfruit[i]/surfol)*p2) / (p1*(Dfruit[i]/surfol)+p2)}
# diagn 121108 (script ML) : 
#if (i==1) { Pmax[i] <- 9.63} else {Pmax[i] <- (p1*((Dfruit[i]-RE.fruct[i-1])/surfol)*p2) / (p1*((Dfruit[i]-RE.fruct[i-1])/surfol)+p2)}

if (Pmax[i]>=15) {Pmax[i] <- 15} else {Pmax[i] <- Pmax[i]}

# diagn 121108 (script ML): altération du Pmax par psyfol
#Pmax[i] <- Pmax[i]*(1-exp(4.43*(1/-0.01+0.22)))

###----------------------------------------------------------------------------------------------------

###----- CALCUL DES ASSIMILATS SUR LA JOURNEE -------------------------------------------------------------  

rayo <- PAR[PAR[,"DAB"]==croissance[i],"PAR"]
ss <- k1[i,][rayo>0]*k2[i,][rayo>0]*surfol
so <- surfol-ss

#calcul du rayonnement a l'ombre a l'aide d'une fonction de ponderation du PPFD
rayo.omb <- r3*rayo
photomb <- ((Pmax[i] + p3)* (1 - exp(-p4 * rayo.omb[rayo.omb>0] / (Pmax[i] + p3))))-p3
assiomb <- 3600*sum(photomb*so)*12/10^6

photsol <- ((Pmax[i] + p3)* (1 - exp(-p4*rayo[rayo>0] / (Pmax[i] + p3)))) - p3
assisol <- 3600*sum(photsol*ss)*12/10^6

# assimilation sur la journée (gC/j)
photo.fol[i] <- assiomb + assisol  
# réserves facilement utilisables (gC)
resfu[i] <- (feuilles$res[i]*feuilles$gamma) + (rameau$res[i]*rameau$gamma)
# assimilats disponibles totaux
assimilats <- photo.fol[i] + resfu[i]
assutilfruits[i] <- assimilats
# réserves difficilement utilisables (gC)
resdu.feuil[i] <- feuilles$res[i]*(1-feuilles$gamma)
resdu.ram [i] <- rameau$res[i]*(1-rameau$gamma)
###----------------------------------------------------------------------------------------------------

 
 
# ======================================================================================
# MAINTENANCE ET CROISSANCE DU FRUIT
# ===========================================================================================
 
temper <- temp2[temp2[,"DAB"]==croissance[i],"TM"]

# respirartions d'entretien en gC

RE.rameau <- MRR.rameau * (Q10.rameau^((temper-20)/10)) * (rameau$struc + (rameau$res[i]/rameau$c))              
RE.feuilles <- MRR.feuilles * (Q10.feuilles^((temper-20)/10)) * (feuilles$struc + feuilles$res[i]/feuilles$c)
RE.fruits <- MRR.fruits * (Q10.fruits^((temper-20)/10)) * fruits$gMS[i]

RE.fruct[i] <- RE.fruits                
RE.veget[i] <- RE.rameau + RE.feuilles

      # RAPPEL : règles de priorité d'utiolisation des assimilats : 
      #1- maitenance 2- croissance reproductive 3- mise en réserve dans rameau et feuilles  
       
# 1° utilisation des assimilats disponibles pour la respiration d'entretien
 
if (assimilats >= RE.veget[i]) 
  { Reste.RE <- assimilats - RE.veget[i] 
  } else {
          if (assimilats + resdu.feuil[i] >= RE.veget[i]) { 
            Reste.RE <- 0
            resdu.feuil[i] <- assimilats + resdu.feuil[i] - RE.veget[i] 
            } else {
                if (assimilats + resdu.feuil[i] + resdu.ram[i] >= RE.veget[i]) { 
                  Reste.RE <- 0
                  resdu.ram[i] <- assimilats + resdu.feuil[i] + resdu.ram[i] - RE.veget[i]
                  resdu.feuil[i] <- 0 
                  } else {
                      cat("Les parties vegetatives s'etouffent: le systeme meurt ...\n")
                      break
                      }
            }  
  }

if (Reste.RE < RE.fruct[i]) {
    besoin.fruit <- (RE.fruits-Reste.RE)/fruits$c 
        if (besoin.fruit>=fruits$gMS[i]) {
            cat("Les parties reproductrices s'etouffent: le systeme meurt ...\n")
            break
            } else {
                fruits$gMS[i] <- fruits$gMS[i]-besoin.fruit     # le fruit pompe sur ses réserves
                # diagn 121108 (script ML): 
                #fruits$gMS[i+1] <- fruits$gMS[i]-besoin.fruit 
            }
    } 
 
Reste1 <- max(0,Reste.RE-RE.fruct[i])

           
# 2° utilisation de ce qui reste pour la croissance du fruit

fruits$gMS[i+1] <- fruits$gMS[i] + (min(Dfruit[i],Reste1)/(fruits$c + fruits$GRC))
      # les coût de construction (en C) sont retranchés de la croisance



# ======================================================================================
# ELABORATION DES SUCRES ET DU POIDS FRAIS (FRUIT INDIVIDUEL,SEPARATION PULPE/NOYAU)
# ===========================================================================================

temper <- temp2[temp2[,"DAB"]==croissance[i],"TM"]
ddj <- temp2[temp2[,"DAB"]==croissance[i],"DEGJOUR"]
ha <- mean(PAR[PAR[,"DAB"]==croissance[i],"HR"])
humihor <- PAR[PAR[,"DAB"]==croissance[i],"HR"]       # vecteur horaire
temphor <- PAR[PAR[,"DAB"]==croissance[i],"TM"]       # vecteur horaire
rayohor <- PAR[PAR[,"DAB"]==croissance[i],"RG"]       # vecteur horaire

###----- INITIALISATION ---------------------------------------------------------------------

sucres$ps.fruit[1] <- fruits$gMS[1]
compart$ps.peau[1] <- A1peau * (sucres$ps.fruit[1])^(B1peau)
compart$ps.pulpe[1] <- A1pulpe * (sucres$ps.fruit[1])^(B1pulpe)
sucres$ps.pulpe[1] <- compart$ps.peau[1] + compart$ps.pulpe[1]
compart$ps.noyau[1] <- sucres$ps.fruit[1] - sucres$ps.pulpe[1]

compart$pf.peau[1] <- compart$ps.peau[1] + A3peau * (compart$ps.peau[1]^B3peau)
compart$pf.pulpe[1] <- compart$ps.pulpe[1] + A3pulpe * (compart$ps.pulpe[1]^B3pulpe)
compart$pf.noyau[1] <- compart$ps.noyau[1] + A3noyau * (compart$ps.noyau[1]^B3noyau)
sucres$pf.pulpe[1] <- compart$pf.pulpe[1] + compart$pf.peau[1]
sucres$pf.fruit[1] <- sucres$pf.pulpe[1] + compart$pf.noyau[1]
sucres$eau.fruit[1] <- sucres$pf.pulpe[1] - sucres$ps.pulpe[1]
         

###----- CALCUL DES ETATS CARBONES POUR LA CROISSANCE DE LA PULPE ET DU NOYAU-----------------        

# partage de la croissance sèche pour les compartiments 
          # (dérivée de la rel. allométrique)
fPE <- A1peau * B1peau * (sucres$ps.fruit[i])^(B1peau - 1)
fPU <- A1pulpe * B1pulpe * (sucres$ps.fruit[i])^(B1pulpe - 1)
fNO <- 1 - fPE - fPU

# carbone disponible pour la croissance des compartiments
Fpulpe <- min(Dfruit[i],Reste1)*(fPU)
Fpeau <- min(Dfruit[i],Reste1)*(fPE)
Fnoyau <- min(Dfruit[i],Reste1)*(fNO)

# respiration de la pulpe (+ peau)
RE.pulpefruits <- RE.fruits * sucres$ps.pulpe[i] / sucres$ps.fruit[i]
FA <- Fpulpe + Fpeau            # carbone dispo pour croiss de pulpe + peau 
respi.fruit <- RE.pulpefruits + ( FA*fruits$GRC/(fruits$GRC+fruits$c) )
        # respiration totale de la pulpe + peau d'un fruit (dont couts de construction)
assimCR.pulpe <- FA + RE.pulpefruits
        # assimilats utilsés pour la croissance et la respiration de la pulpe 
            
#croissance de la matiere seche des diff. compartiments (qui prend en compte les CC)
deltaFDB <-  min(Dfruit[i],Reste1) * fPU / (fruits$GRC+fruits$c)     # delta de Flesh Dry Biomass
deltaPDB <-  min(Dfruit[i],Reste1) * fPE / (fruits$GRC+fruits$c)     # delta de Peel Dry Biomass
deltaSDB <- min(Dfruit[i],Reste1) * fNO / (fruits$GRC+fruits$c)     # delta de Stone Dry Biomass
deltacompDB <- deltaFDB + deltaPDB               # delta de peau + pulpe

sucres$ps.pulpe[i+1] <- sucres$ps.pulpe[i] + deltacompDB
compart$ps.peau[i+1] <- compart$ps.peau[i] + deltaPDB
compart$ps.pulpe[i+1] <- compart$ps.pulpe[i] + deltaFDB
compart$ps.noyau[i+1] <- compart$ps.noyau[i] + deltaSDB
sucres$ps.fruit[i+1] <- fruits$gMS[i] + deltacompDB + deltaSDB
        # on incrémente fruits$gMS[i] et pas sucres$ps.fruit[i], 
        # sinon on ne tient pas compte du fait que le fruit puise sur ses réserves lorsque assimilats < RE  
        

###----- CALCUL DE LA TRANSPIRATION DU FRUIT et DU POTENTIEL HYDRIQUE DU RAMEAU -----------------        

# calcul de la surface du fruit
surfruit <- 3.65 * (sucres$pf.fruit[i])^0.73        # expérimental (en cm²)

Petoile <- 0.008048*exp(0.0547*temper)              # pression de vapeur saturante (Nobel, 1974)
transpi.alpha <- 18*Petoile/(83*(temper+273.15))    # concentration de vapeur d'eau à saturation

# calcul de la transpiration du fruit (g/j)
transpi[i] <- surfruit*transpi.alpha*ro*(0.996-ha/100)      
        # 0.996 = humidité relative dans les espaces amplis d'air à l'intérieur du fruit
     

# calcul du potentiel hydrique de tige

# voir ce que ça fait pour les 100 feuilles --> on verra que faire pour arranger
#         à améliorer pour ne pas devoir avoir les paramètres horaires
#          voir meme à ne pas se servir de Hr que nous n'aurons peut etre pas selon les parcelles
#potram[i] <- mean (-0.6617105 + (-0.006940179*temphor) + (0.007888208*humihor) + (0.0000198265*rayohor))
# diagn 121108 (script ML): 
if (LF>=75) {
  potram[i] <- 1.5*mean (-0.6617105 + (-0.006940179*temphor) + (0.007888208*humihor) + (0.0000198265*rayohor))
  } else {
      potram[i] <- mean (-0.6617105 + (-0.006940179*temphor) + (0.007888208*humihor) + (0.0000198265*rayohor))}

###----- PRESSION OSMOTIQUE DU FRUIT  (potosmo[i]) -----------------   

#Calcul des différentes concentrations en composés solubles
#--------------------------------------------------------------------------------------------

MSpu <- compart$ps.pulpe[i]
eaupu <- compart$pf.pulpe[i] - compart$ps.pulpe[i]         #éq. au volume d'eau

# 1° proportion en masse de ces composés

#       vec = vecteur permettant le calcul de la proportion du composé X par g MS pulpe
vec <- c(0.06620651, -0.0000538797, -0.002464413, 2.406565e-006)
  propmal <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (propmal < 0) {    	propmal <- 0.0   	}
vec <- c(0.0006896104, 1.613387e-006, 0.00005063595, -6.912509e-008)
	proppyr <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (proppyr < 0) {    	proppyr <- 0.0   	}
vec <- c(0.004750718, -2.113094e-006, -0.00002965687, 0.0)
	propoxa <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (propoxa < 0) {    	propoxa <- 0.0   	}
vec <- c(0.01394964, -5.234608e-006, -0.000288464, 2.682089e-007)
	propK <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (propK < 0) {    	propK <- 0.0   	}
vec <- c(0.00115595, -7.937479e-007, -0.00002320017, 2.344528e-008)
	propMg <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (propMg < 0) {    	propMg <- 0.0   	}
vec <- c(0.001588606, -6.625787e-007, -0.0000228527, 1.514343e-008)
	propCa <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (propCa < 0) {    	propCa <- 0.0   	}
vec <- c(0.000246011, 3.741743e-007, 0.00002495255, -3.010081e-008)
	propNH4 <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (propNH4 < 0) {    	propNH4 <- 0.0   	}
vec <- c(0.0001279568, 8.15203e-008, -1.468235e-006, 0.0)
	propNa <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (propNa < 0) {    	propNa <- 0.0   	}
vec <- c(0.08074145, -0.00006325543, -0.001161846, 1.161344e-006)
	propglc <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (propglc < 0) {    	propglc <- 0.0   	}
vec <- c(0.04972199, 0.0000966001, -0.001078579, 0.0)
	propfrc <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (propfrc < 0) {    	propfrc <- 0.0   	}
vec <- c(-0.1708815, 0.0004380411, 0.01923022, -0.00002059459)
	propami <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
	if (propami < 0) {    	propami <- 0.0   	}
# essai en proportion pour module citrate
vec <- c(0.1625024, -0.0000640754, 0.003906348, -4.784292e-006)
  propcit <- vec[1] + vec[2] * ddj + vec[3] * MSpu + vec[4] * MSpu*ddj
  if (propcit < 0) {    	propcit <- 0.0   	} 

# modèle citrate  
#propcit <- citrate[i] * 192 * compart$pf.pulpe[i] / compart$ps.pulpe[i]

# modèle sucrose  (Léchaudel et al. 2005, ISHS)
#sucrose[i] <- sucrosedep + sucrosedep * exp(mu.sucrose * (temp2[temp2[,"DAB"]==croissance[i],"DAB"] - jdebsim)) *
#		(sucres$ps.fruit[i] - P0.fruit[w]) / (DMfmax - P0.fruit[w])
sucrose[i] <- sucrosedep + sucrosedep * exp(mu.sucrose * (temp2[temp2[,"DAB"]==croissance[i],"DEGJOUR"] - ddjy)) *
		(sucres$ps.fruit[i] - P0.fruit[w]) / (DMfmax - P0.fruit[w])
if (sucrose[i] < 0.1) sucrose[i] <- sucrose[i]
if (sucrose[i] > 0.1) sucrose[i] <- 0.1

  propsacc[i] <- sucrose[i] * compart$pf.pulpe[i] / compart$ps.pulpe[i]

if (sucrose[i]>=0.04)
{
	cat("Le fruit est mûr \n")
	break                         # sort de la boucle et arrête les simulations 
}

# 2° masse de ces composés 

mmal <- propmal * MSpu
mcit <- propcit * MSpu
mpyr <- proppyr * MSpu
moxa <- propoxa * MSpu
mK <- propK * MSpu
mMg <- propMg * MSpu
mCa <- propCa * MSpu
mNH4 <- propNH4 * MSpu
mNa <- propNa * MSpu
mg <- propglc * MSpu
mf <- propfrc * MSpu
msa <- propsacc[i] * MSpu
mami <- propami * MSpu  

# 3° nb de mol de ces composés
 
nmal <- mmal / 134
ncit <- mcit / 192
npyr <- mpyr / 88
noxa <- moxa / 90
nK <- mK / 39
nMg <- mMg / 24
nCa <- mCa / 40
nNH4 <- mNH4 / 18
nNa <- mNa / 23
nglc <- mg / 180
nfrc <- mf / 180
nsac <- msa / 342

ncompsol[i] <- nmal + ncit + npyr + noxa + nK + nMg + nCa + nNH4 + nNa + nglc + nfrc + nsac
ncations <- nK + nMg + nCa + nNH4 + nNa
nacides <- nmal + ncit + npyr + noxa
nsucres <- nglc + nfrc + nsac

# 3° concentration en composés solubles totaux 

Cssm[i] <- ncompsol[i] / eaupu


#Calcul du potentiel osmotique en MPa
#--------------------------------------------------------------------------------------------
potcat[i] <- (R/10) * (temper+273.15) * ncations / eaupu
potaci[i] <- (R/10) * (temper+273.15) * nacides / eaupu
potsuc[i] <- (R/10) * (temper+273.15) * nsucres / eaupu
potosmo[i] <- (83 /10) * (temper+273.15) * Cssm[i] + 0.2  
      #0.2 étant le nb de moles d'acides aminés, constant
#potosmo[i] <- 0.8*potosmo[i]     # pour voir si le pb de croissance vient bien du potentiel osmotique 


#----- PRESSION DE TURGESCENCE DU FRUIT  (turgf[i]) -----------------   

#Variation de phi en fct des ddj (ddj cst durant 1 journée)
if (ddj > ddini)  { 	
  phi <- phimax * (tau^(ddj-ddini))
	}	else 	{
		phi <- phimax    }

#régulation de la turgescence
AwLf <- surfruit * acnst       
    # produit de la surface, du ratio (membrane composite/surface du fruit du fruit) 
    #     et de la conductivité hydraulique entre la tige et le fruit
Y[i] <- Y0 + hcst * (sucres$eau.fruit[i] - 0)
croissMS <- sucres$ps.pulpe[i+1] - sucres$ps.pulpe[i]

BIDA <- (sucres$eau.fruit[i] * phi * Y[i]) + (AwLf * (potram[i] + potosmo[i]) - transpi[i]) + croissMS / 1.6
BIDC <- (sucres$eau.fruit[i] * phi) + AwLf
turgf[i] <- BIDA / BIDC

if (turgf[i] < Y[i])  turgf[i] <- (potram[i] + potosmo[i]) + (- transpi[i] + (croissMS/1.6)) / (AwLf)
if (turgf[i] < Y0)    turgf[i] <- Y0


#----- BILAN D'EAU DANS LE FRUIT   -----------------   

# Flux d'eau entrant
fludoentr[i] <- AwLf * (potram[i] - turgf[i] + potosmo[i])
delta.eau <- fludoentr[i] - transpi[i] + croissMS / 1.6


#----- MODELE SUCRES PROPREMENT DIT  -----------------   

# concentrations en sucres et autres
sucres$cosa[i] <- (100 * msa) / compart$pf.pulpe[i]
sucres$cofr[i] <- (100 * mf) / compart$pf.pulpe[i]
sucres$cogl[i] <- (100 * mg) / compart$pf.pulpe[i]
sucres$comal[i] <- (100 * mmal) / compart$pf.pulpe[i]
sucres$cocit[i] <- (100 * mcit) / compart$pf.pulpe[i]
sucres$coam[i] <- (100 * mami) / compart$pf.pulpe[i]
sucres$souit[i] <- 1 * sucres$cosa[i] + 1.75 * sucres$cofr[i] + 0.77 * sucres$cogl[i]
# saveur acide (ajouter éventuellement oxalate et pyruvate
sucres$acid[i] <- 100 * (mcit + 1.33*mmal) / compart$pf.pulpe[i]


# partage de l'eau du "fruit" entre peau + et pulpe et croissance en eau des 2 compartiments
fPUwater <- A2pulpe * B2pulpe * sucres$eau.fruit[i] ^ (B2pulpe - 1)
  delta.eau.pulpe <- fPUwater * delta.eau
fPEwater <- A2peau * B2peau * sucres$eau.fruit[i] ^ (B2peau -1)
  delta.eau.peau <- fPEwater * delta.eau

# part de la croiss en MF du noyau et croissance
fNOfresh <- Anoyau * Bnoyau * compart$ps.noyau[i] ^ (Bnoyau - 1)
  delta.fresh.noyau <- fNOfresh * deltaSDB

# listes sucres et compart complétés  
sucres$pf.pulpe[i+1] <- sucres$pf.pulpe[i] + delta.eau + deltacompDB
sucres$tms.pulpe[i+1] <- sucres$ps.pulpe[i+1]/sucres$pf.pulpe[i+1]
compart$pf.pulpe[i+1] <- compart$pf.pulpe[i] + delta.eau.pulpe + deltaFDB
compart$pf.peau[i+1] <- compart$pf.peau[i] + delta.eau.peau + deltaPDB
compart$pf.noyau[i+1] <- compart$pf.noyau[i] + delta.fresh.noyau
sucres$pf.fruit[i+1] <- sucres$pf.pulpe[i+1] + compart$pf.noyau[i+1]
sucres$eau.fruit[i+1] <- sucres$pf.pulpe[i+1] - sucres$ps.pulpe[i+1]

#----- MODELE CITRATE  (Lobit et al. 2003) -----------------   

TempenK <- temper + 273						# température en K

# respi du mésocrape du fruit en mol citrate / jour
dMSpulpe <- compart$ps.pulpe[i+1] - compart$ps.pulpe[i]
Resp <- qm * compart$ps.pulpe[i] * Q10^((TempenK-293)/10) + qg * dMSpulpe     # en mol de citrate par jour
respi[i] <- Resp * (44/192)						            # en mol de CO2 par jour

# calcul de la vitesse de production de citrate (/ jour)
#varcitrate[i] <- c1 * compart$ps.pulpe[1] * (1 + c2*(TempenK - Temp0)) *
#				(1 + c3*(TempenK - Temp0) + c4 * (respi[i] / compart$ps.pulpe[1]))
#citrate[i+1] <- citrate[i] + (1/compart$pf.pulpe[i]) * varcitrate[i]
#if (citrate[i+1]<0) {citrate[i+1]<-0 }

citrate[i] <-  ncit/(compart$pf.pulpe[i])          # quand param du modèle citrate pas estimables
if (citrate[i] < citratedep) {citrate[i] <- citratedep }


# ======================================================================================
# MISE EN RESERVE de ce qui reste
# ===========================================================================================

Reste2 <- Reste1 - min(Dfruit[i],Reste1)

Res.rameau.provi <- resdu.ram[i] + min(Reste2, rameau$res[i]*rameau$gamma)
Res.feuilles.provi <- resdu.feuil[i] + max(0, Reste2 - rameau$res[i]*rameau$gamma)

# création d'un seuil de réserves qui peuvent être stockées chaque jour :
          # part des réserves/unité de struc * nb de strctures (ie nb de feuilles) 
seuil <- (feuilles$psi/(1-feuilles$psi)) * feuilles$struc * feuilles$c  

if (Res.feuilles.provi > seuil){
      feuilles$res[i+1] <- seuil
      rameau$res[i+1] <- Res.feuilles.provi - seuil + Res.rameau.provi
} else {
      feuilles$res[i+1] <- Res.feuilles.provi
      rameau$res[i+1] <- Res.rameau.provi }

# ======================================================================================
# FIN DE LA BOUCLE DE CROISSANCE DU FRUIT INDIVIDUEL
# ===========================================================================================

}

# ======================================================================================
# ======================================================================================

###----------------------------------------------------------------------------------------------------
# inscription dans les matrices result. des valeurs observées au cours de la ctoissance pour le fruit w
###----------------------------------------------------------------------------------------------------

convertDAB <- jdebsim + round(DABo[w])    # traduit le jour de début de simulatio (en DAB réelle du fruit) sur l'échelle des DAB moyennes

assutilfruits <- cbind(1+convertDAB+(0:(durcroiss-2)), assutilfruits)   # durcroiss-2 car le premier jour (DABo) n'est pas considéré comme                                                                        un jour de croissance
  ind <-  pmatch(assutilfruits[,1],result.assutilfruits[,"DABmoy"])      # indice qui permet de vérifier les indices qu'il va matcher.
  result.assutilfruits[ind,w+1] <- assutilfruits[,2]
Pmax <- cbind(1+convertDAB+(0:(durcroiss-2)), Pmax)   
  ind <-  pmatch(Pmax[,1],result.Pmax[,"DABmoy"])      
  result.Pmax[ind,w+1] <- Pmax[,2]
photo.fol <- cbind(1+convertDAB+(0:(durcroiss-2)), photo.fol)   
  ind <-  pmatch(photo.fol[,1],result.photo.fol[,"DABmoy"])     
  result.photo.fol[ind,w+1] <- photo.fol[,2]
Dfruit <- cbind(1+convertDAB+(0:(durcroiss-2)), Dfruit)  
  ind <-  pmatch(Dfruit[,1],result.Dfruit[,"DABmoy"])      
  result.Dfruit[ind,w+1] <- Dfruit[,2]
RE.veget <- cbind(1+convertDAB+(0:(durcroiss-2)), RE.veget) 
  ind <-  pmatch(RE.veget[,1],result.RE.veget[,"DABmoy"])      
  result.RE.veget[ind,w+1] <- RE.veget[,2]
RE.fruct <- cbind(1+convertDAB+(0:(durcroiss-2)), RE.fruct) 
  ind <-  pmatch(RE.fruct[,1],result.RE.fruct[,"DABmoy"]) 
  result.RE.fruct[ind,w+1] <- RE.fruct[,2]
resfu <- cbind(1+convertDAB+(0:(durcroiss-2)), resfu) 
  ind <-  pmatch(resfu[,1],result.resfu[,"DABmoy"])
  result.resfu[ind,w+1] <- resfu[,2]
resdu.feuil <- cbind(1+convertDAB+(0:(durcroiss-2)), resdu.feuil) 
  ind <-  pmatch(resdu.feuil[,1],result.resdu.feuil[,"DABmoy"])
  result.resdu.feuil[ind,w+1] <- resdu.feuil[,2]
resdu.ram <- cbind(1+convertDAB+(0:(durcroiss-2)), resdu.ram) 
  ind <-  pmatch(resdu.ram[,1],result.resdu.ram[,"DABmoy"])
  result.resdu.ram[ind,w+1] <- resdu.ram[,2]
fruits$gMS <- cbind(convertDAB+(0:(durcroiss-1)), fruits$gMS) 
  ind <-  pmatch(fruits$gMS[,1],result.fruitMS[,"DABmoy"]) 
  result.fruitMS[ind,w+1] <- fruits$gMS[,2]
feuilles$res <- cbind(convertDAB+(0:(durcroiss-1)), feuilles$res) 
  ind <-  pmatch(feuilles$res[,1],result.res.feuil[,"DABmoy"]) 
  result.res.feuil[ind,w+1] <- feuilles$res[,2]
rameau$res <- cbind(convertDAB+(0:(durcroiss-1)), rameau$res) 
  ind <-  pmatch(rameau$res[,1],result.res.ram[,"DABmoy"]) 
  result.res.ram[ind,w+1] <- rameau$res[,2] 
  
potram <- cbind(convertDAB+(0:(durcroiss-2)), potram) 
  ind <-  pmatch(potram[,1],result.potram[,"DABmoy"]) 
  result.potram[ind,w+1] <- potram[,2]
potosmo <- cbind(convertDAB+(0:(durcroiss-2)), potosmo) 
  ind <-  pmatch(potosmo[,1],result.potosmo[,"DABmoy"]) 
  result.potosmo[ind,w+1] <- potosmo[,2]
turgf <- cbind(convertDAB+(0:(durcroiss-2)), turgf) 
  ind <-  pmatch(turgf[,1],result.turgf[,"DABmoy"]) 
  result.turgf[ind,w+1] <- turgf[,2]
transpi <- cbind(convertDAB+(0:(durcroiss-2)), transpi) 
  ind <-  pmatch(transpi[,1],result.transpi[,"DABmoy"]) 
  result.transpi[ind,w+1] <- transpi[,2]
fludoentr <- cbind(convertDAB+(0:(durcroiss-2)), fludoentr) 
  ind <-  pmatch(fludoentr[,1],result.fludoentr[,"DABmoy"]) 
  result.fludoentr[ind,w+1] <- fludoentr[,2]
respi <- cbind(convertDAB+(0:(durcroiss-2)), respi) 
  ind <-  pmatch(respi[,1],result.respi[,"DABmoy"]) 
  result.respi[ind,w+1] <- respi[,2]
  
sucres$pf.pulpe <- cbind(convertDAB+(0:(durcroiss-1)), sucres$pf.pulpe) 
  ind <-  pmatch(sucres$pf.pulpe[,1],result.MFpulpe[,"DABmoy"]) 
  result.MFpulpe[ind,w+1] <- sucres$pf.pulpe[,2]
sucres$pf.fruit <- cbind(convertDAB+(0:(durcroiss-1)), sucres$pf.fruit) 
  ind <-  pmatch(sucres$pf.fruit[,1],result.MFfruit[,"DABmoy"]) 
  result.MFfruit[ind,w+1] <- sucres$pf.fruit[,2]    
sucres$eau.fruit <- cbind(convertDAB+(0:(durcroiss-1)), sucres$eau.fruit) 
  ind <-  pmatch(sucres$eau.fruit[,1],result.eau[,"DABmoy"]) 
  result.eau[ind,w+1] <- sucres$eau.fruit[,2]
sucres$ps.fruit <- cbind(convertDAB+(0:(durcroiss-1)), sucres$ps.fruit) 
  ind <-  pmatch(sucres$ps.fruit[,1],result.MSfruit[,"DABmoy"]) 
  result.MSfruit[ind,w+1] <- sucres$ps.fruit[,2]    
sucres$cosa <- cbind(convertDAB+(0:(durcroiss-1)), sucres$cosa) 
  ind <-  pmatch(sucres$cosa[,1],result.cosa[,"DABmoy"]) 
  result.cosa[ind,w+1] <- sucres$cosa[,2] 
sucres$cogl <- cbind(convertDAB+(0:(durcroiss-1)), sucres$cogl) 
  ind <-  pmatch(sucres$cogl[,1],result.cogl[,"DABmoy"]) 
  result.cogl[ind,w+1] <- sucres$cogl[,2]   
sucres$cofr <- cbind(convertDAB+(0:(durcroiss-1)), sucres$cofr) 
  ind <-  pmatch(sucres$cofr[,1],result.cofr[,"DABmoy"]) 
  result.cofr[ind,w+1] <- sucres$cofr[,2]
sucres$comal <- cbind(convertDAB+(0:(durcroiss-1)), sucres$comal) 
  ind <-  pmatch(sucres$comal[,1],result.comal[,"DABmoy"]) 
  result.comal[ind,w+1] <- sucres$comal[,2]
sucres$cocit <- cbind(convertDAB+(0:(durcroiss-1)), sucres$cocit) 
  ind <-  pmatch(sucres$cocit[,1],result.cocit[,"DABmoy"]) 
  result.cocit[ind,w+1] <- sucres$cocit[,2]
sucres$coam <- cbind(convertDAB+(0:(durcroiss-1)), sucres$coam) 
  ind <-  pmatch(sucres$coam[,1],result.coam[,"DABmoy"]) 
  result.coam[ind,w+1] <- sucres$coam[,2]
sucres$souit <- cbind(convertDAB+(0:(durcroiss-1)), sucres$souit) 
  ind <-  pmatch(sucres$souit[,1],result.souit[,"DABmoy"]) 
  result.souit[ind,w+1] <- sucres$souit[,2]
sucres$acid <- cbind(convertDAB+(0:(durcroiss-1)), sucres$acid) 
  ind <-  pmatch(sucres$acid[,1],result.acid[,"DABmoy"]) 
  result.acid[ind,w+1] <- sucres$acid[,2]

###----------------------------------------------------------------------------------------------------
# inscription dans la matrice de résultat de la croissance les valeurs observées en fin de croissance
###----------------------------------------------------------------------------------------------------

c("DABdeb","ddjdeb","DABfin","ddjfin","LF","Pini","clim","datflo","MS","MF","sucrose")

valtour <- c(jdebsim,ddjini,temp2[temp2[,"DAB"]==croissance[i],"DAB"],temp2[temp2[,"DAB"]==croissance[i],"DEGJOUR"],
			LF[w],P0.fruit[w],tourenvirlum[w],round(DABo[w]),fruits$gMS[i,2],sucres$pf.fruit[i,2],sucres$cosa[i-1,2])
resultfincroiss[tour,] <- valtour
tour <- tour + 1
valtour <<- valtour



# ======================================================================================
# FIN DE LA BOUCLE SUR LE NOMBRE DE FRUITS
# ===========================================================================================

}

# ======================================================================================
# ======================================================================================

resultfincroiss <<- resultfincroiss
# write.table(resultfincroiss, file="resultfincroiss.txt", quote=F, col=NA, row=T, sep="\t")

result.assutilfruits <<- result.assutilfruits
result.Pmax <<- result.Pmax   
result.photo.fol <<- result.photo.fol  
result.Dfruit <<- result.Dfruit     
result.RE.veget <<- result.RE.veget
result.RE.fruct <<- result.RE.fruct
result.resfu <<- result.resfu
result.resdu.feuil <<- result.resdu.feuil
result.resdu.ram <<- result.resdu.ram
result.fruitMS <<- result.fruitMS
result.res.feuil <<- result.res.feuil
result.res.ram <<- result.res.ram
result.potram <<- result.potram
result.potosmo <<- result.potosmo
result.turgf <<- result.turgf
result.transpi <<- result.transpi
result.fludoentr <<- result.fludoentr
result.respi <<- result.respi
result.MFpulpe <<- result.MFpulpe
result.MFfruit <<- result.MFfruit   
result.eau <<- result.eau
result.MSfruit <<- result.MSfruit  
result.cosa <<- result.cosa
result.cogl <<- result.cogl
result.cofr <<- result.cofr
result.comal <<- result.comal
result.cocit <<- result.cocit
result.coam <<- result.coam
result.souit <<- result.souit
result.acid <<- result.acid



invisible()
}

