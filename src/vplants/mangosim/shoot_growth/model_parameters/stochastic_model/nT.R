# Modèle stochastique : mise en forme des données
setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth")


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
inflo1$stadeInflo=as.numeric(inflo1$stadeInflo)

# Récupération des températures pour tous les vergers
verger="BM";TBM0=cbind(read.csv("data/temperature_data/TBMmoy.csv",header=TRUE,sep=";",dec="."),verger)
verger="BP";TBP0=cbind(read.csv("data/temperature_data/TBPmoy.csv",header=TRUE,sep=";",dec="."),verger)
verger="GF";TGF0=cbind(read.csv("data/temperature_data/TGFmoy.csv",header=TRUE,sep=";",dec="."),verger)
verger="GH";TGH0=cbind(read.csv("data/temperature_data/TGHmoy.csv",header=TRUE,sep=";",dec="."),verger)
temp=rbind(TBM0,TBP0,TGF0,TGH0)
temp$datesUnik=strptime(as.character(temp$datesUnik), "%d/%m/%Y")

# Ajout des températures aux données
temperature=rep(-999,length(data1$date))
cumtemp=rep(-999,length(data1$date));cumtemp_1=rep(-999,length(data1$date))
data2=cbind(data1,temperature,cumtemp,cumtemp_1)
data2$temperature[1]=as.numeric(temp[temp$verger==data2[1,]$verger & temp$datesUnik$yday ==data2[1,]$date$yday
                                  & temp$datesUnik$year ==data2[1,]$date$year,]$tempMoy)
data2$codeUC=as.character(data2$codeUC)

data2$stade[data2$stade<8]=1
data2$stade[data2$stade==8 | data2$stade==9]=2
data2$stade[data2$stade==10 | data2$stade==11]=3
data2$stade[data2$stade==12 | data2$stade==13]=4
data2$stade[data2$stade==14 | data2$stade==15]=5

UC0=data2[1,1];data2$cumtemp[1]=max(0,data2$temperature[1]-13.37);data2$cumtemp_1[1]=0

for (i in 2:length(data1$date)){
# Choix de la température de base
  if (data2[i,]$stade<3){base_temp=13.37}else{base_temp=9.78}
  # ajout de la température du jour
  day_temp=NA
  day_temp=as.numeric(temp[temp$verger==data2[i,]$verger & temp$datesUnik$yday ==data2[i,]$date$yday
                      & temp$datesUnik$year ==data2[i,]$date$year,]$tempMoy)
  data2$temperature[i]=day_temp
  # calcul du cumul de température
  if(data2[i,]$codeUC==UC0){
    nb.jours=data2[i,]$date$yday-data2[i-1,]$date$yday
    if (nb.jours<0){nb.jours=data2[i,]$date$yday-data2[i-1,]$date$yday+365}
    data2$cumtemp[i]=data2$cumtemp[i-1]+max(0,day_temp-base_temp)*nb.jours
    if (nb.jours==0){data2$cumtemp_1[i]=data2$cumtemp[i-2]}else{data2$cumtemp_1[i]=data2$cumtemp[i-1]}
  }else if ( data2$DFUC[i]=="D"){
  data2$cumtemp[i]=0#max(0,day_temp-base_temp)
  data2$cumtemp_1[i]=0
  UC0=data2[i,1]
  }else{data2$cumtemp[i]=0;data2$cumtemp_1[i]=0}
}


### On ne s'intéresse qu'aux changements de stade
UCs=unique(data2$codeUC)
temp_stade=cbind(CodeUC=0,stade=0,STT=0,STT_1=0)
for (i in 1:length(UCs)){
  for (j in 2:5){
    UC_stade=data2[data2$stade==j & data2$codeUC==UCs[i],]
    UC=UC_stade[UC_stade$date==min(UC_stade$date),]
    temp_stade=rbind(temp_stade,c(as.character(UCs[i]),j,as.numeric(as.character(UC$cumtemp)),UC$cumtemp_1))
  }
}

temp_stade=as.data.frame(temp_stade)
temp_stade$STT=as.numeric(as.character(temp_stade$STT))
temp_stade$STT_1=as.numeric(as.character(temp_stade$STT_1))

write.table(temp_stade, file = "model_parameters/stochastic_model/BasedeCroissanceVeg_temp.csv", append = FALSE, quote = FALSE, sep = ";", row.names = FALSE)

stade2=temp_stade[temp_stade$stade==2,]
stade3=temp_stade[temp_stade$stade==3,]
stade4=temp_stade[temp_stade$stade==4,]
stade5=temp_stade[temp_stade$stade==5,]

# On cherche les UCs avec les mêmes températures et on les regroupe
a=table(as.data.frame(cbind(stade2$STT,stade2$STT_1)))
cumtemp_1=c();for (i in 1:length(unique(sort(stade2$STT))) ){cumtemp_1=c(cumtemp_1,unique(stade2[stade2$STT==unique(sort(stade2$STT))[i],4])[1])}
Nb2=cbind(as.data.frame(table(as.numeric(as.character(stade2$STT)))),cumtemp_1)
cumtemp_1=c();for (i in 1:length(unique(sort(stade3$STT))) ){cumtemp_1=c(cumtemp_1,unique(stade3[stade3$STT==unique(sort(stade3$STT))[i],4])[1])}
Nb3=cbind(as.data.frame(table(as.numeric(as.character(stade3$STT)))),cumtemp_1)
cumtemp_1=c();for (i in 1:length(unique(sort(stade4$STT))) ){cumtemp_1=c(cumtemp_1,unique(stade4[stade4$STT==unique(sort(stade4$STT))[i],4])[1])}
Nb4=cbind(as.data.frame(table(as.numeric(as.character(stade4$STT)))),cumtemp_1)
cumtemp_1=c();for (i in 1:length(unique(sort(stade5$STT))) ){cumtemp_1=c(cumtemp_1,unique(stade5[stade5$STT==unique(sort(stade5$STT))[i],4])[1])}
Nb5=cbind(as.data.frame(table(as.numeric(as.character(stade5$STT)))),cumtemp_1)

nT2=as.data.frame(cbind(cumtemp=as.numeric(as.character(Nb2[,1])),cumtemp_1=Nb2[,3],NbUC=Nb2[,2]))
nT3=as.data.frame(cbind(cumtemp=as.numeric(as.character(Nb3[,1])),cumtemp_1=Nb3[,3],NbUC=Nb3[,2]))
nT4=as.data.frame(cbind(cumtemp=as.numeric(as.character(Nb4[,1])),cumtemp_1=Nb4[,3],NbUC=Nb4[,2]))
nT5=as.data.frame(cbind(cumtemp=as.numeric(as.character(Nb5[,1])),cumtemp_1=Nb5[,3],NbUC=Nb5[,2]))
nT=rbind(cbind(nT2,stade=2),cbind(nT3,stade=3),cbind(nT4,stade=4),cbind(nT5,stade=5))
write.table(nT, file = "model_parameters/stochastic_model/nT.csv", append = FALSE, quote = FALSE, sep = ";", row.names = FALSE)


#########################################
###### Inflos
################
# Ajout des températures aux données
temperature=rep(-999,length(inflo1$date))
cumtemp=rep(-999,length(inflo1$date));cumtemp_1=rep(-999,length(inflo1$date))
inflo2=cbind(inflo1,temperature,cumtemp,cumtemp_1)
inflo2$temperature[1]=as.numeric(temp[temp$verger==inflo2[1,]$verger & temp$datesUnik$yday ==inflo2[1,]$date$yday
                                  & temp$datesUnik$year ==inflo2[1,]$date$year,]$tempMoy)
inflo2$codeUC=as.character(inflo2$codeUC)

inflo2$stade[inflo2$stade<11]=1
inflo2$stade[inflo2$stade==11 | inflo2$stade==12]=2
inflo2$stade[inflo2$stade==13 | inflo2$stade==14]=3
inflo2$stade[inflo2$stade==15 | inflo2$stade==16]=4
base_temps=c(11.10,5.38,8.67,15.11)
UC0=inflo2[1,1];inflo2$cumtemp[1]=max(0,inflo2$temperature[1]-9.2);inflo2$cumtemp_1[1]=0

for (i in 2:length(inflo1$date)){
  # Choix de la température de base
  base_temp=base_temps[inflo2[i,]$stade]
  # ajout de la température du jour
  day_temp=NA
  day_temp=as.numeric(temp[temp$verger==inflo2[i,]$verger & temp$datesUnik$yday ==inflo2[i,]$date$yday
                      & temp$datesUnik$year ==inflo2[i,]$date$year,]$tempMoy)
  inflo2$temperature[i]=day_temp
  # calcul du cumul de température
  if(inflo2[i,]$codeUC==UC0){
    nb.jours=inflo2[i,]$date$yday-inflo2[i-1,]$date$yday
    if (nb.jours<0){nb.jours=inflo2[i,]$date$yday-inflo2[i-1,]$date$yday+365}
    inflo2$cumtemp[i]=inflo2$cumtemp[i-1]+max(0,day_temp-base_temp)*nb.jours
    if (nb.jours==0){inflo2$cumtemp_1[i]=inflo2$cumtemp[i-2]}else{inflo2$cumtemp_1[i]=inflo2$cumtemp[i-1]}
  }else if ( inflo2$DFInflo[i]=="D"){
    inflo2$cumtemp[i]=0
    inflo2$cumtemp_1[i]=0
    UC0=inflo2[i,]$codeUC
  }else{inflo2$cumtemp[i]=0;inflo2$cumtemp_1[i]=0}
}

### On ne s'intéresse qu'aux changements de stade
inflos=unique(inflo2$codeUC)
temp_stade=cbind(CodeUC=0,stade=0,STT=0,STT_1=0)

for (i in 1:length(inflos)){
  for (j in 2:4){
    UC_stade=inflo2[inflo2$stade==j & inflo2$codeUC==inflos[i],]
    UC=UC_stade[UC_stade$date==min(UC_stade$date),]
    temp_stade=rbind(temp_stade,c(as.character(inflos[i]),j,UC$cumtemp,UC$cumtemp_1))
  }
}

temp_stade=as.data.frame(temp_stade)
temp_stade$STT=as.numeric(as.character(temp_stade$STT))
temp_stade$STT_1=as.numeric(as.character(temp_stade$STT_1))
write.table(temp_stade, file = "model_parameters/stochastic_model/BasedeCroissanceInflo_temp.csv", append = FALSE, quote = FALSE, sep = ";", row.names = FALSE)

stadeF2=temp_stade[temp_stade$stade==2,]
stadeF3=temp_stade[temp_stade$stade==3,]
stadeF4=temp_stade[temp_stade$stade==4 & !is.na(temp_stade$STT),]

# On cherche les UCs avec les mêmes températures et on les regroupe
cumtemp_1=c();for (i in 1:length(unique(sort(stadeF2$STT))) ){cumtemp_1=c(cumtemp_1,unique(stadeF2[stadeF2$STT==unique(sort(stadeF2$STT))[i],4])[1])}
NbF2=cbind(as.data.frame(table(as.numeric(as.character(stadeF2$STT)))),cumtemp_1)
cumtemp_1=c();for (i in 1:length(unique(sort(stadeF3$STT))) ){cumtemp_1=c(cumtemp_1,unique(stadeF3[stadeF3$STT==unique(sort(stadeF3$STT))[i],4])[1])}
NbF3=cbind(as.data.frame(table(as.numeric(as.character(stadeF3$STT)))),cumtemp_1)
cumtemp_1=c();for (i in 1:length(unique(sort(stadeF4$STT))) ){cumtemp_1=c(cumtemp_1,unique(stadeF4[stadeF4$STT==unique(sort(stadeF4$STT))[i],4])[1])}
NbF4=cbind(as.data.frame(table(as.numeric(as.character(stadeF4$STT)))),cumtemp_1)

nTF2=as.data.frame(cbind(cumtemp=as.numeric(as.character(NbF2[,1])),cumtemp_1=NbF2[,3],NbUC=NbF2[,2]))
nTF3=as.data.frame(cbind(cumtemp=as.numeric(as.character(NbF3[,1])),cumtemp_1=NbF3[,3],NbUC=NbF3[,2]))
nTF4=as.data.frame(cbind(cumtemp=as.numeric(as.character(NbF4[,1])),cumtemp_1=NbF4[,3],NbUC=NbF4[,2]))
nTF=rbind(cbind(nTF2,stade=2),cbind(nTF3,stade=3),cbind(nTF4,stade=4))

write.table(nTF, file = "model_parameters/stochastic_model/nTF.csv", append = FALSE, quote = FALSE, sep = ";", row.names = FALSE)

temp_stade=new_croissInflo
MEF_data=function(temp_stade){
  stade2=temp_stade[temp_stade$stade==2,]
  stade3=temp_stade[temp_stade$stade==3,]
  stade4=temp_stade[temp_stade$stade==4,]

  # On cherche les UCs avec les mêmes températures et on les regroupe
  cumtemp_1=c();for (i in 1:length(unique(sort(stade2$STT))) ){cumtemp_1=c(cumtemp_1,unique(stade2[stade2$STT==unique(sort(stade2$STT))[i],]$STT_1))}
  Nb2=cbind(as.data.frame(table(stade2$STT)),cumtemp_1=sort(unique(stade2$STT_1)))
  cumtemp_1=c();for (i in 1:length(unique(sort(stade3$STT))) ){cumtemp_1=c(cumtemp_1,unique(stade3[stade3$STT==unique(sort(stade3$STT))[i],]$STT_1))}
  Nb3=cbind(as.data.frame(table(stade3$STT)),cumtemp_1=sort(unique(stade3$STT_1)))
  cumtemp_1=c();for (i in 1:length(unique(sort(stade4$STT))) ){cumtemp_1=c(cumtemp_1,unique(stade4[stade4$STT==unique(sort(stade4$STT))[i],]$STT_1))}
  Nb4=cbind(as.data.frame(table(stade4$STT)),cumtemp_1)

  nT2=as.data.frame(cbind(cumtemp=as.numeric(as.character(Nb2[,1])),cumtemp_1=Nb2[,3],NbUC=Nb2[,2]))
  nT3=as.data.frame(cbind(cumtemp=as.numeric(as.character(Nb3[,1])),cumtemp_1=Nb3[,3],NbUC=Nb3[,2]))
  nT4=as.data.frame(cbind(cumtemp=as.numeric(as.character(Nb4[,1])),cumtemp_1=Nb4[,3],NbUC=Nb4[,2]))

  if (length(temp_stade[temp_stade$stade==5,1])!=0){
    stade5=temp_stade[temp_stade$stade==5,]
    cumtemp_1=c();for (i in 1:length(unique(sort(stade5$STT))) ){cumtemp_1=c(cumtemp_1,unique(stade5[stade5$STT==unique(sort(stade5$STT))[i],4]))}
    Nb5=cbind(as.data.frame(table(stade5$STT)),cumtemp_1)
    nT5=as.data.frame(cbind(cumtemp=as.numeric(as.character(Nb5[,1])),cumtemp_1=Nb5[,3],NbUC=Nb5[,2]))
    nT=rbind(cbind(nT2,stade=2),cbind(nT3,stade=3),cbind(nT4,stade=4),cbind(nT5,stade=5))
  }else{nT=rbind(cbind(nT2,stade=2),cbind(nT3,stade=3),cbind(nT4,stade=4))
  }
}
