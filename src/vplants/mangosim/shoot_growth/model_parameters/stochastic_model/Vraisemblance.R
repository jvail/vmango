# ModÃ¨le stochastique : calcul de la vraisemblance
# Ce scripte nous permete, au travers d'un calcul de vraisemblance, de calculer les paramètres de variance 
# du modèle stochastique pour les stades phénologiques (développement)
# Section 3.3.2 "Modélisation du développement" du rapport
setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth")

#################################
########  UCs
#############

nT =read.csv( "model_parameters/stochastic_model/nT.csv", sep=";", dec=".", header=T)
colnames(nT) <- c( "dateTh", "dateThMin1","nbUC","stade")
head(nT)
tTh=c(38.50,86.11,133.5,449.876)
x= c(38.50,1,86.11,1,133.5,1,449.876,1)

modpopulation <- function(x){
#fonction qui calcule la log-vraisemblance d'une population de fleurs
#avec le modÃ¨le de floraison en fonction des 2 paramÃ¨tres x[1]=seuil
#et x[2]=paramÃ¨tre de variance de la floraison.
data = nT
z=0
for (i in 1:4){
data2=data[data$stade==(i+1),]
if (i==4){data2$dateThMin1=data2$dateTh-16}
z =z +(-1 * sum(data2$nbUC*log(pnorm((data2$dateTh - x[(i-1)*2+1])/(x[(i-1)*2+2]*sqrt(data2$dateTh )))
- pnorm((data2$dateThMin1 - x[(i-1)*2+1])/(x[(i-1)*2+2]*sqrt(data2$dateThMin1))) )))
}
return(z)
}

nlm(modpopulation, c(38,1,86,1,133,1,449,1), iterlim=10000)
#$minimum
#[1] 609.6695
#$estimate
#[1]  40.646696   1.359737  89.271718   1.366522 145.555593   1.488442 449.147388
#[8]   3.274001


### On va faire une bootstrap pour dÃ©terminer la variance et l'IC
croissUC =read.csv( "model_parameters/stochastic_model/BasedeCroissanceVeg_temp.csv", sep=";", dec=".", header=T)
croissUC$CodeUC=as.character(croissUC$CodeUC)
UCs=unique(croissUC$CodeUC)
est=matrix(rep(0,100*8),ncol=8)
for (i in 1:100){
  new_UCs=sample(1:length(UCs), length(UCs), replace = TRUE)
  new_croissUC=croissUC[croissUC$CodeUC==UCs[new_UCs[1]],]
  for (j in 2:length(UCs)){
    new_croissUC=rbind(new_croissUC,croissUC[croissUC$CodeUC==UCs[new_UCs[j]],])
  }
  nT=MEF_data(new_croissUC)
  colnames(nT) = c( "dateTh", "dateThMin1","nbUC","stade")
  est[i,]=nlm(modpopulation, c(38.50,1,86.11,1,133.5,1,449.876,1), iterlim=10000)$estimate
}
var_est=rep(1,8)
for (i in 1:8){
  var_est[i]=var(est[,i])
}
IC=matrix(c(var_est*99/qchisq(1-0.025,99),var_est*99/qchisq(0.025,99)),ncol=8)


#################################
########  Inflos
#############
nTF =read.csv( "model_parameters/stochastic_model/nTF.csv", sep=";", dec=".", header=T)
colnames(nTF) <- c( "dateTh", "dateThMin1","nbUC","stade")
head(nTF)

modpopulationF <- function(x){
#fonction qui calcule la log-vraisemblance d'une population de fleurs
#avec le modÃ¨le de floraison en fonction des 2 paramÃ¨tres x[1]=seuil
#et x[2]=paramÃ¨tre de variance de la floraison.
data = nTF
z=0
for (i in 1:3){
  data2=data[data$stade==(i+1),]
  #if (i==4){data2$dateThMin1=data2$dateTh-16}
  z =z +(-1 * sum(data2$nbUC*log(pnorm((data2$dateTh - x[(i-1)*2+1])/(x[(i-1)*2+2]*sqrt(data2$dateTh )))
      - pnorm((data2$dateThMin1 - x[(i-1)*2+1])/(x[(i-1)*2+2]*sqrt(data2$dateThMin1))) )))
  }
return(z)
}


nlm(modpopulationF, c(70,1,240,1,470,1), iterlim=10000)
#$minimum
#[1] 259.5822
#$estimate
#[1]  73.355909   1.900164 251.337537   2.097271 500.917358   2.913480

### On va faire une bootstrap pour dÃ©terminer la variance et l'IC
croissInflo =read.csv( "model_parameters/stochastic_model/BasedeCroissanceInflo_temp.csv", sep=";", dec=".", header=T)
croissInflo$CodeUC=as.character(croissInflo$CodeUC)
UCs=unique(croissInflo$CodeUC)
est=matrix(rep(0,100*6),ncol=6)

for (i in 1:100){
  new_UCs=sample(1:length(UCs), length(UCs), replace = TRUE)
  new_croissInflo=croissInflo[croissInflo$CodeUC==UCs[new_UCs[1]],]
  for (j in 2:length(UCs)){
    new_croissInflo=rbind(new_croissInflo,croissInflo[croissInflo$CodeUC==UCs[new_UCs[j]],])
  }
  nTF=MEF_data(new_croissInflo)
  colnames(nTF) = c( "dateTh", "dateThMin1","nbUC","stade")
  est[i,]=nlm(modpopulationF, c(70,1,240,1,470,1), iterlim=10000)$estimate
}

var_estF=rep(1,6)
for (i in 1:6){
var_estF[i]=var(est[,i])
}
ICF=matrix(c(var_estF*99/qchisq(1-0.025,99),var_estF*99/qchisq(0.025,99)),ncol=6)
#          [,1]       [,2]        [,3]      [,4]       [,5]       [,6]
#[1,] 4.2455770 31.6792608 218.9586028 7.4320803 55.4560219 383.297235
#[2,] 0.1805843  0.2347993   0.7043202 0.3161213  0.4110271   1.232945
