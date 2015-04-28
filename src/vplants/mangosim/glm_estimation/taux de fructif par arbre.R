setwd("C:/Users/install/Desktop/stage 3A/analyses glm")
table_within_04 <- read.csv("table_within_cycle_04.csv", sep=",")
table_within_05 <- read.csv("table_within_cycle_05.csv", sep=",")

chgt_factor <- function(mydata){					#fonction qui transforme les variables binomiales et multinomiales en facteurs
     noms.var <- c("Burst_Date","Burst_Date_Children","Flowering","Flowering_Week","Fruiting","Has_Apical_GU_Child","Has_Lateral_GU_Children","Nature_Ancestor_V","Position_A","Position_Ancestor_A","Tree_Fruit_Load","Vegetative_Burst","is_terminal")                   # sélection de variables a convertir au format facteur.
     mydata[,noms.var] <- lapply(mydata[,noms.var], function(x) {             # pour chaque variable sélectionnée
                           x <- factor(x)                                     # application de la fonction factor
                           x
                                                                 })
      mydata
                               }

table_within_04 <- chgt_factor(table_within_04)
table_within_05 <- chgt_factor(table_within_05)

data04 <- table_within_04
data05 <- table_within_05

# calcul du taux de fructification de chaque arbre
index_Flowering.loaded <- which(data04$Tree_Fruit_Load == 1 & data04$Flowering == 1 & data04$tree == "F2")
table(data04$Fruiting[index_Flowering.loaded])
length(index_Flowering.loaded)

# nb UC qui ont fructifié B10, B12 et F2 respectivement
nbF <- c(70, 73, 83)
#nb UC total (qui ont fleuri) B10, B12 et F2 respectivement
nbT <- c(166, 194, 210)
#comparaison des proportions
prop.test(nbF,nbT)