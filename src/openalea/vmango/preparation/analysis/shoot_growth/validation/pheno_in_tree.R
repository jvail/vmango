setwd("C:/Users/Anne-Sarah/Desktop/stage/mangosim/src/vplants/mangosim/shoot_growth")
# On sa suivi les stades phénologiques dans lequel se trouvait tout un groupe d'UCs afin de voir si l'ajout de variabilité 
# dans la somme journalière de temps thermique avait bien un effet sur les stades phénologiques
# Section 7.1 "Distribution des stades phénologiques sur un arbre" dans le rapport

# Suivi des différents stades phénologiques au sein d'un même arbre

data=read.csv("data/simulation_results/tree_growth_f.csv",header=TRUE,sep=";",dec=".")
attach(data)
Date=strptime(as.character(data$Date), "%d/%m/%Y")#"%Y-%m-%d")


# Un groupe d'UCs uniquement
date_lim=strptime("2003-10-15","%Y-%m-%d")

UCs=unique(CodeUC[Date<date_lim & Pheno_stage==0])
NB0=rep(0,length(dates))
NB1=rep(0,length(dates))
NB2=rep(0,length(dates))
NB3=rep(0,length(dates))
NB4=rep(0,length(dates))
for (j in 1:length(UCs)){
  data2=data[CodeUC==UCs[j],]
  data2$Date=strptime(as.character(data2$Date), "%d/%m/%Y")
  for (i in 1:length(dates)){
    NB0[i]=NB0[i]+length(data2$Pheno_stage[data2$Pheno_stage==0 & data2$Date==dates[i]])
    NB1[i]=NB1[i]+length(data2$Pheno_stage[data2$Pheno_stage==1 & data2$Date==dates[i]])
    NB2[i]=NB2[i]+length(data2$Pheno_stage[data2$Pheno_stage==2 & data2$Date==dates[i]])
    NB3[i]=NB3[i]+length(data2$Pheno_stage[data2$Pheno_stage==3 & data2$Date==dates[i]])
    NB4[i]=NB4[i]+length(data2$Pheno_stage[data2$Pheno_stage==4 & data2$Date==dates[i]]) 
  }
}

plot(dates,NB0,col=2,type='l',xlab="Date",ylab="Nombre d'UCs")
points(dates,NB1,col=3,type='l')
points(dates,NB2,col=4,type='l')
points(dates,NB3,col=5,type='l')
points(dates,cumsum(NB4),col=6,type='l')
legend("topright",col=c(2,3,4,5,6),c("D","E","F","G","H"),lty=1,title="Stades")


# Toutes les UCs
data_tot=read.csv("data/simulation_results/tree_growth.csv",header=TRUE,sep=",",dec=".")
detach(data);
attach(data_tot)
Date=strptime(as.character(data_tot$Date), "%Y-%m-%d")
date_lim=strptime("2003-09-01","%Y-%m-%d")
dates=unique(Date)

UCs=unique(CodeUC[Date>date_lim & data_tot$Pheno_stage==0])
NB0=rep(0,length(dates))
NB1=rep(0,length(dates))
NB2=rep(0,length(dates))
NB3=rep(0,length(dates))
NB4=rep(0,length(dates))
for (j in 1:length(UCs)){
  data2=data_tot[CodeUC==UCs[j],]
  data2$Date=strptime(as.character(data2$Date), "%Y-%m-%d")
  for (i in 1:length(dates)){
    NB0[i]=NB0[i]+length(data2$Pheno_stage[data2$Pheno_stage==0 & data2$Date==dates[i]])
    NB1[i]=NB1[i]+length(data2$Pheno_stage[data2$Pheno_stage==1 & data2$Date==dates[i]])
    NB2[i]=NB2[i]+length(data2$Pheno_stage[data2$Pheno_stage==2 & data2$Date==dates[i]])
    NB3[i]=NB3[i]+length(data2$Pheno_stage[data2$Pheno_stage==3 & data2$Date==dates[i]])
    NB4[i]=NB4[i]+length(data2$Pheno_stage[data2$Pheno_stage==4 & data2$Date==dates[i]]) 
  }
}

par(mfrow=c(1,1))
plot(dates[-c(1:33)],NB0[-c(1:33)],col=2,type='l',lwd=2,xlab="Date",ylab="Nombre d'UCs")
points(dates[-c(1:33)],NB1[-c(1:33)],col=3,lwd=2,type='l')
points(dates[-c(1:33)],NB2[-c(1:33)],col=4,lwd=2,type='l')
points(dates[-c(1:33)],NB3[-c(1:33)],col=5,lwd=2,type='l')
points(dates[-c(1:33)],cumsum(NB4)[-c(1:33)],lwd=2,col=6,type='l')
legend("topleft",col=c(2,3,4,5,6),c("D","E","F","G","H"),lwd=2,lty=1,title="Stades")

