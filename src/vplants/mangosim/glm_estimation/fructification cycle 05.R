## Calcul des GLM pour les différents processus du développement végétatif et de la reproduction
## Script dérivé du script global 'estimate_glm_order_1_proba.r'

setwd("C:/Users/install/Desktop/stage 3A/analyses glm")

table_within_05 <- read.csv("table_within_cycle_05.csv", sep=",")
#table_between_03to0405 <- read.csv("table_between_cycle_03to0405.csv", sep=",")
#table_between_04to05 <- read.csv("table_between_cycle_04to05.csv", sep=",")

chgt_factor <- function(mydata){					#fonction qui transforme les variables binomiales et multinomiales en facteurs
     noms.var <- c("Burst_Date","Burst_Date_Children","Flowering","Flowering_Week","Fruiting","Has_Apical_GU_Child","Has_Lateral_GU_Children","Nature_Ancestor_V","Position_A","Position_Ancestor_A","Tree_Fruit_Load","Vegetative_Burst","is_terminal")                   # sélection de variables a convertir au format facteur.
     mydata[,noms.var] <- lapply(mydata[,noms.var], function(x) {             # pour chaque variable sélectionnée
                           x <- factor(x)                                     # application de la fonction factor
                           x
                                                                 })
      mydata
                               }


table_within_05 <- chgt_factor(table_within_05)
#table_between_03to0405 <- chgt_factor(table_between_03to0405)
#table_between_04to05 <- chgt_factor(table_between_04to05)

summary(table_within_05)
#summary(table_between_03to0405)
#summary(table_between_04to05)


#############################################################
######## FONCTIONS
#############################################################

# Fonction d'extraction des tableaux de proba à partir des glm
determine_table_prob_from_glm = function(myglm){

    level_Tree_Fruit_Load = as.factor(1:0)
    level_Position_A = as.factor(1:0)
    level_Position_Ancestor_A = as.factor(1:0)
    level_Nature_Ancestor_V = as.factor(1:0)
    level_Nature_V = as.factor(0:1)
    level_all_Burst_Date = as.factor(1:12)

    variables = NULL # in case of selected glm, contains only influent variables
    level_Burst_Date = level_all_Burst_Date # contains all the observed date mother and used by the glm

    is_vglm = (class(myglm)[1]=="vglm")

    if (is_vglm)             # si le GLM est un vglm (multinomial, variable date)
    {
        if(length(myglm@xlevels)>0){
            level_Burst_Date = myglm@xlevels$Burst_Date
            if(is.null(level_Burst_Date)){
                level_Burst_Date = level_all_Burst_Date
            }
            variables = slot(myglm@terms[[1]],"term.labels")
        }
    }
    else                     # si le GLM est un glm (Poisson ou binomial)
    {
        if(!is.null(myglm$xlevels)){
            level_Burst_Date = myglm$xlevels$Burst_Date
            if(is.null(level_Burst_Date)){
                level_Burst_Date = level_all_Burst_Date
            }
            variables = colnames(myglm$model)[2:length(colnames(myglm$model))]     # crée le vecteur des noms des variables explicatives sélectionnées du glm
        }
    }

    produit_cartesien = expand.grid(level_Tree_Fruit_Load,                 # prépare un tableau de toutes les combinaisons possibles des modalités de tous
                                    level_Burst_Date,                      # les facteurs explicatifs
                                    level_Position_A,
                                    level_Position_Ancestor_A,
                                    level_Nature_Ancestor_V,
                                    level_Nature_V)

    names(produit_cartesien) = c("Tree_Fruit_Load",
                                 "Burst_Date",
                                 "Position_A",
                                 "Position_Ancestor_A",
                                 "Nature_Ancestor_V",
                                 "Nature_V")

    data_probs = unique(produit_cartesien[variables])                      # ne garde que les combinaisons des facteurs retenus par le glm (et dont les noms
                                                                           # sont dans le vecetur 'variables'

    if (is_vglm){
        if(!is.null(variables)){                                               # remplissage du tableau en cas de vglm non nul (= au moins un facteur significatif)
            probability = predictvglm(myglm, newdata= data_probs,type="response")
            for(i in 1:length(colnames(probability)) ){
                data_probs[colnames(probability)[i] ] = probability[,i]
            }
        }
        else{
            # cas du glm null
            probability = predictvglm(myglm,type="response")[1,] # on predit sans valeur de x
            months_p = colnames(myglm@y) # on recupere le nom des mois (valeur possible de la variable y du glm)
            # on veut transformer probs en dataframe. Mais plutot en colonne qu'en ligne (du coup on fait une transposé)
            data_probs = t(data.frame(probability))
            colnames(data_probs)= months_p
        }
    }
    else {                                                                   # remplissage du tableau en cas de glm non nul (= au moins un facteur significatif)
        if(myglm$family[1]=="binomial" || myglm$family[1]=="poisson"){
            if (!is.null(variables)) {
                probability = predict(myglm, newdata=data_probs, type="response")
                data_probs["probability"]=probability
            }
            else{
                probability = predict(myglm,type="response")[1]
                data_probs = data.frame(probability)
            }
        }
    }

   
    return(data_probs)
}



# Fonction qui donne les effectifs par combinaison de modalités de facteurs dans les données initiales
 effectifs <- function(myglm, data, subset){
           factor_signif <- colnames(myglm$model)[2:length(colnames(myglm$model))]      # facteurs significatifs retenus
           combin <- data[subset, factor_signif]                            # fichier des combinaisons de modalités des facteurs signifs pour les observations
           nb <- as.data.frame(ftable(combin))
           nb
                                            }
    
 
# Fonction qui change les valeurs des modalités des facteurs dans un tableau d'effectifs en caractères 
# et construit une variable code (combcode) qui est la combinaison des modalités des facteurs pour chaque ligne
 chgt_character <- function(mydata){
     mydata[,1:(ncol(mydata)-1)] <- sapply(mydata[,1:(ncol(mydata)-1)], function(x) {             
                           x <- as.character(x)              # transformation des valeurs en caractères (sinon il prend les niveaux de facteurs et non les valeurs)
                           x
                                                                                    })
                           
     if(ncol(mydata)-1 == 1){
                       mydata$codecomb <- mydata[,1]
                            }
     else if(ncol(mydata)-1 == 2){
                       mydata$codecomb <- paste(mydata[,1], mydata[,2], sep="-")
                            }
     else if(ncol(mydata)-1 == 3){
                       mydata$codecomb <- paste(mydata[,1], mydata[,2], mydata[,3], sep="-")
                            }
     else if(ncol(mydata)-1 == 4){
                       mydata$codecomb <- paste(mydata[,1], mydata[,2], mydata[,3], mydata[,4], sep="-")
                            }
     else if(ncol(mydata)-1 == 5){
                       mydata$codecomb <- paste(mydata[,1], mydata[,2], mydata[,3], mydata[,4], mydata[,5], sep="-")
                            }
     else if(ncol(mydata)-1 == 6){
                       mydata$codecomb <- paste(mydata[,1], mydata[,2], mydata[,3], mydata[,4], mydata[,5], mydata[,6], sep="-")
                            }
     
     mydata
                                        }





#################################
###  WITHIN 05
#################################
data <- table_within_05

# FRUITING

 index_fruiting.loaded <- which(data$Flowering == 1 & data$Tree_Fruit_Load == 1)                # sélection des UCs terminales florifères sur les arbres chargés

# Modèle complet
 complete_glm.fruiting.loaded = glm( Fruiting ~ Burst_Date +
                                                  Position_A +
                                                  Position_Ancestor_A +
                                                  Nature_Ancestor_V,
                                family = binomial, data=data, subset = index_fruiting.loaded)
 summary(complete_glm.fruiting.loaded)
 anova(complete_glm.fruiting.loaded)


 # sélection automatique des facteurs (fonction step)
 selected_glm.fruiting.loaded = step(complete_glm.fruiting.loaded, trace = 1)
 summary(selected_glm.fruiting.loaded)        

   # probas estimées par combinaison de modalités des facteurs retenus
 fruiting_within_05 <- determine_table_prob_from_glm(selected_glm.fruiting.loaded)
 fruiting_within_05 <- chgt_character(fruiting_within_05)
 
  # effectifs des données dans chaque modalité croisée des facteurs retenus
 size_fruiting_within_05 <- effectifs(selected_glm.fruiting.loaded, data=data, subset=index_fruiting.loaded)
 size_fruiting_within_05 <- chgt_character(size_fruiting_within_05)
 
 # rajout des effectifs dans la table des probas estimées par le glm
  ind <- match(fruiting_within_05$codecomb, size_fruiting_within_05$codecomb, nomatch=NA)
  fruiting_within_05$nb <- size_fruiting_within_05$Freq[ind]
   
 # Exportation du fichier
 write.csv(fruiting_within_05, file="fruiting_within_05.csv", row.names = FALSE)

#####################################################################################################################################
# sélection manuelle des facteurs pour analtser les proba en fonction des dates de débourrement et de la position de l'UC mère
 manual_glm.fruiting.loaded <- glm(Fruiting ~ Burst_Date + Position_A, family = binomial, data=data, subset = index_fruiting.loaded)
 summary(manual_glm.fruiting.loaded)        

   # probas estimées par combinaison de modalités des facteurs retenus
 fruiting_within_05_position.mere <- determine_table_prob_from_glm(manual_glm.fruiting.loaded)
 fruiting_within_05_position.mere <- chgt_character(fruiting_within_05_position.mere)
 
  # effectifs des données dans chaque modalité croisée des facteurs retenus
 size_fruiting_within_05_position.mere <- effectifs(manual_glm.fruiting.loaded, data=data, subset=index_fruiting.loaded)
 size_fruiting_within_05_position.mere <- chgt_character(size_fruiting_within_05_position.mere)
 
 # rajout des effectifs dans la table des probas estimées par le glm
  ind <- match(fruiting_within_05_position.mere$codecomb, size_fruiting_within_05_position.mere$codecomb, nomatch=NA)
  fruiting_within_05_position.mere$nb <- size_fruiting_within_05_position.mere$Freq[ind]
   
 # Exportation du fichier
 write.csv(fruiting_within_05_position.mere, file="fruiting_within_05_position.mere.csv", row.names = FALSE)
       
# sélection manuelle des facteurs pour analyser les proba en fonction des dates de débourrement et de la position de l'ancêtre
 manual_glm.fruiting.loaded <- glm(Fruiting ~ Burst_Date + Position_Ancestor_A, family = binomial, data=data, subset = index_fruiting.loaded)
 summary(manual_glm.fruiting.loaded)        

   # probas estimées par combinaison de modalités des facteurs retenus
 fruiting_within_05_position.ancetre <- determine_table_prob_from_glm(manual_glm.fruiting.loaded)
 fruiting_within_05_position.ancetre <- chgt_character(fruiting_within_05_position.ancetre)
 
  # effectifs des données dans chaque modalité croisée des facteurs retenus
 size_fruiting_within_05_position.ancetre <- effectifs(manual_glm.fruiting.loaded, data=data, subset=index_fruiting.loaded)
 size_fruiting_within_05_position.ancetre <- chgt_character(size_fruiting_within_05_position.ancetre)
 
 # rajout des effectifs dans la table des probas estimées par le glm
  ind <- match(fruiting_within_05_position.ancetre$codecomb, size_fruiting_within_05_position.ancetre$codecomb, nomatch=NA)
  fruiting_within_05_position.ancetre$nb <- size_fruiting_within_05_position.ancetre$Freq[ind]
   
 # Exportation du fichier
 write.csv(fruiting_within_05_position.ancetre, file="fruiting_within_05_position.ancetre.csv", row.names = FALSE)

#################################
###  AUTRES ANALYSES
#################################
index <- which(data$Tree_Fruit_Load == 1 & data$Nb_Inflorescence == 6)
table(data$Fruiting[index])
 
index_tx.fructif <- which(data$Tree_Fruit_Load == 1 & data$Flowering == 1 & data$Position_Ancestor_A == 0 & data$Burst_Date == 3)
table(data$Fruiting[index_tx.fructif])
