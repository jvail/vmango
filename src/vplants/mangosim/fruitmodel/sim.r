setwd ("D:/Mes Données/doc mathieu/fichier R/modèle-mangue")
getwd()
tempstpierre2002 <- read.table("tempstpierre2002.txt", header=T, sep="\t")
tempstpierre2002$Date <- as.Date(tempstpierre2002$Date, "%d/%m/%Y")
#  "Avis : class discarded from column ‘Date’"
rayostpierre2002 <- read.table("rayorun02f1bis.txt", header=T, sep="\t")
      rayostpierre2002$DATE <- as.Date(rayostpierre2002$DATE, "%d/%m/%y")
k1runquant <- read.table("k1runquant.txt", header=T, sep="\t")
source("D:/Mes Données/doc mathieu/fichier R/modèle-mangue/convertmoyjdj.R")
source("modèle-complet-arbreB2-paramML.R")



rec02 <- read.table("D:/Mes Données/doc mathieu/fichier R/modèle-mangue/MFrec02.csv", header=T, sep=";")
rec02split <- split(rec02, rec02$arbre)


# fonction permettant d'estimer les paramètres de la distribution
logvraineg <- function(param, obs) {
m1 <- param[1]
sd1 <- param[2]
-sum(log(dnorm(x = obs, mean = m1, sd = sd1)))
}


# ++++++++++++++++++++----------------------------------------------++++++++++++++++++++++++++++++++++++++++
# essai sur B2 uniquement --> méthode de Monte-Carlo
# ++++++++++++++++++++----------------------------------------------++++++++++++++++++++++++++++++++++++++++


# --------------------------------------------------------------------------------------------------------------
# CALIBRE + DATE RECOLTE
# --------------------------------------------------------------------------------------------------------------


# SIMULATIONS DE MC

nbfruits <- dim(rec02split$B2)[1]
paramloi <- matrix(nrow=20,ncol=3)  
paramloi2 <- matrix(nrow=20,ncol=3)
paramloi3 <- matrix(nrow=20,ncol=3)
dimnames(paramloi)[[2]] <- c("MFmoy","MFsd","nbfruits") 
dimnames(paramloi2)[[2]] <- c("DABmoy","DABsd","nbfruits")
dimnames(paramloi3)[[2]] <- c("MSmoy","MSsd","nbfruits") 
distribdataC <- NULL
distribdataD <- NULL
distribdataMS <- NULL

for (i in 1:20){

mangsucreauB202(nbfruits)

breaks <- seq(0,800,50)                               # classes de poids        
dist <- table(cut(resultfincroiss[,"MF"], breaks))    # nb de fruits dans chaque classe   
distribdata <- dist/dim(resultfincroiss)[1]           # fréquence des poids 
fit <- optim(f = logvraineg, p = c(350,50), obs = resultfincroiss[,"MF"])
est <- fit$par
paramloi[i,1:3] <- c(round(fit$par[1],0),round(fit$par[2],0),nbfruits)
distribdataC <- rbind(distribdataC, distribdata)

breaks2 <- seq(90,200,10)                                       
dist2 <- table(cut(resultfincroiss[,"DABfin"], breaks2))      
distribdata2 <- dist2/dim(resultfincroiss)[1]           
fit2 <- optim(f = logvraineg, p = c(160,20), obs = resultfincroiss[,"DABfin"])
est2 <- fit2$par
paramloi2[i,1:3] <- c(round(fit2$par[1],0),round(fit2$par[2],0),nbfruits)
distribdataD <- rbind(distribdataD, distribdata2)

breaks3 <- seq(0,150,10)                                     
dist <- table(cut(resultfincroiss[,"MS"], breaks3))      
distribdata3 <- dist/dim(resultfincroiss)[1]           
fit3 <- optim(f = logvraineg, p = c(75,10), obs = resultfincroiss[,"MS"])
est3 <- fit3$par
paramloi3[i,1:3] <- c(round(fit3$par[1],0),round(fit3$par[2],0),nbfruits)
distribdataMS <- rbind(distribdataMS, distribdata3)

}
paramloi <<- paramloi
paramloi2 <<- paramloi2
paramloi3 <<- paramloi3

distribmoy <- apply(distribdataC,2,FUN=mean)
distribsd <- apply(distribdataC,2,FUN=sd)
distribqt <- apply(distribdataC,2,FUN=quantile)
rsltdistribC <- data.frame(seq(25,775,by=50),distribmoy, distribsd,distribqt["25%",],distribqt["75%",])
dimnames(rsltdistribC)[[2]] <- c("x","moy","sd","qt25","qt75")

distribmoy2 <- apply(distribdataD,2,FUN=mean)
distribsd2 <- apply(distribdataD,2,FUN=sd)
distribqt2 <- apply(distribdataD,2,FUN=quantile)
rsltdistribD <- data.frame(seq(95,195,by=10),distribmoy2, distribsd2,distribqt2["25%",],distribqt2["75%",])
dimnames(rsltdistribD)[[2]] <- c("x","moy","sd","qt25","qt75")

distribmoy3 <- apply(distribdataMS,2,FUN=mean)
distribsd3 <- apply(distribdataMS,2,FUN=sd)
distribqt3 <- apply(distribdataMS,2,FUN=quantile)
rsltdistribMS <- data.frame(seq(5,145.5,by=10),distribmoy3, distribsd3,distribqt3["25%",],distribqt3["75%",])
dimnames(rsltdistribMS)[[2]] <- c("x","moy","sd","qt25","qt75")


par(mfrow=c(1,2))

# graphique calibre à la récolte
# -----------------------------------------------------------------------------------------

# OBSERVATIONS --> graphe de la fréquence !
breaks <- seq(0,800,50)                               # classes de poids        
dist <- table(cut(rec02split$B2[,"poids"], breaks))    # nb de fruits dans chaque classe   
distribdataOBS <- dist/dim(rec02split$B2)[1]           # fréquence des poids
plot(seq(25,775,by=50),distribdataOBS,type="l",lwd=2,ylim=c(0,0.5),main="Calibres")
# définition de la loi normale   
paramloiOBS <- vector(length=3)     # vecteur des paramètres de la loi ajustée
names(paramloiOBS) <- c("MFmoy","MFsd","nbfruits")
fit <- optim(f = logvraineg, p = c(350,50), obs = rec02split$B2[,"poids"])
est <- fit$par
paramloiOBS[1:3] <- c(round(fit$par[1],0),round(fit$par[2],0),nrow(rec02split$B2))

# SIMULATIONS --> graphe de la distribution moyenne avec quantiles à 25% et 75%
lines(rsltdistribC$x,rsltdistribC$moy)
lines(rsltdistribC$x,rsltdistribC$qt75,lty="dashed")
lines(rsltdistribC$x,rsltdistribC$qt25,lty="dashed")


# graphique date de récolte
# -----------------------------------------------------------------------------------------

# OBSERVATIONS --> graphe de la fréquence !
breaks <- seq(90,200,10)                               # classes de poids        
dist <- table(cut(rec02split$B2[,"DAB"], breaks))    # nb de fruits dans chaque classe   
distribdataOBS <- dist/dim(rec02split$B2)[1]           # fréquence des poids
plot(seq(95,195,by=10),distribdataOBS,type="l",lwd=2,ylim=c(0,0.5),main="Date récolte")
# définition de la loi normale   
paramloiOBS <- vector(length=3)     # vecteur des paramètres de la loi ajustée
names(paramloiOBS) <- c("DABmoy","DABsd","nbfruits")
fit <- optim(f = logvraineg, p = c(160,20), obs = rec02split$B2[,"DAB"])
est <- fit$par
paramloiOBS[1:3] <- c(round(fit$par[1],0),round(fit$par[2],0),nrow(rec02split$B2))

# SIMULATIONS --> graphe de la distribution moyenne avec quantiles à 25% et 75%
lines(rsltdistribD$x,rsltdistribD$moy)
lines(rsltdistribD$x,rsltdistribD$qt75,lty="dashed")
lines(rsltdistribD$x,rsltdistribD$qt25,lty="dashed")
# --------------------------------------------------------------------------------------------------------------

# graphique de la MS à la récolte
# -----------------------------------------------------------------------------------------

# teneur en MS moyenne à la récolte: 20.75%

# OBSERVATIONS --> graphe de la fréquence !
x11()
breaks <- seq(0,150,10) 
MS <- rec02split$B2[,"poids"]*0.2075                              # classes de poids        
dist <- table(cut(MS, breaks))          # nb de fruits dans chaque classe   
distribdataOBS <- dist/length(MS)           # fréquence des poids
plot(seq(5,145,by=10),distribdataOBS,type="l",lwd=2,ylim=c(0,0.5),main="MS")
# définition de la loi normale   
paramloiOBS <- vector(length=3)     # vecteur des paramètres de la loi ajustée
names(paramloiOBS) <- c("MSmoy","MSFsd","nbfruits")
fit <- optim(f = logvraineg, p = c(75,20), obs = MS)
est <- fit$par
paramloiOBS[1:3] <- c(round(fit$par[1],0),round(fit$par[2],0),nrow(rec02split$B2))

# SIMULATIONS --> graphe de la distribution moyenne avec quantiles à 25% et 75%
lines(rsltdistribMS$x,rsltdistribMS$moy)
lines(rsltdistribMS$x,rsltdistribMS$qt75,lty="dashed")
lines(rsltdistribMS$x,rsltdistribMS$qt25,lty="dashed")

