# setwd("~/Develop/vplants/branches/mangosim/src/vplants/mangosim/glm_estimation")
#
#
### Import of data :

share_dir = '../../../../share/'
input_dir = paste(share_dir,'glm_estimate_input/cogshall/', sep="")
output_dir = paste(share_dir,'glm_output_proba/cogshall/', sep="")
if (file.exists(output_dir)){
    dir.create(output_dir,recursive=TRUE)
}



library(VGAM)

get_vglm_AIC = function(myglm){
    k = myglm@rank
    logL = myglm@criterion$loglikelihood
    AIC = 2*k - 2*logL
    return(AIC)
}

# Fonction qui donne les effectifs par combinaison de modalités de facteurs dans les données initiales
 effectifs = function(myglm, data, subset){
    factor_signif <- colnames(myglm$model)[2:length(colnames(myglm$model))]      # facteurs significatifs retenus
    combin <- data[subset, factor_signif]                            # fichier des combinaisons de modalités des facteurs signifs pour les observations
    nb <- as.data.frame(ftable(combin))
    return (nb)
}
    
 
# Fonction qui change les valeurs des modalités des facteurs dans un tableau d'effectifs en caractères 
# et construit une variable code (combcode) qui est la combinaison des modalités des facteurs pour chaque ligne
 chgt_character = function(mydata){
     mydata[,1:(ncol(mydata)-1)] <- lapply(mydata[,1:(ncol(mydata)-1)], function(x) {             
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
     
    return (mydata)
}


determine_proba_and_effectifs = function(myglm, data, subset){
   # probas estimées par combinaison de modalités des facteurs retenus
   probtable <- determine_table_prob_from_glm(myglm)
   probtable <- chgt_character(probtable)
 
   # effectifs des données dans chaque modalité croisée des facteurs retenus
   nbelements <- effectifs(myglm, data=data, subset=subset)
   nbelements <- chgt_character(nbelements)
 
   # rajout des effectifs dans la table des probas estimées par le glm
   ind <- match(probtable$codecomb, nbelements$codecomb, nomatch=NA)
   probtable$nb <- nbelements$Freq[ind]

   return (probtable)
}

determine_table_prob_from_glm = function(myglm){

    level_Tree_Fruit_Load = as.factor(1:0)
    level_Position_A = as.factor(1:0)
    level_Position_Ancestor_A = as.factor(1:0)
    level_Nature_Ancestor_F = as.factor(0:1)
    level_Nature_F = as.factor(0:1)
    level_all_Burst_Date = as.factor(1:12)

    variables = NULL # in case of selected glm, contains only influent variables
    level_Burst_Date = level_all_Burst_Date # contains all the observed date mother and used by the glm

    is_vglm = (class(myglm)[1]=="vglm")

    if (is_vglm)
    {
        if(length(myglm@xlevels)>0){
            level_Burst_Date = myglm@xlevels$Burst_Date
            if(is.null(level_Burst_Date)){
                level_Burst_Date = level_all_Burst_Date
            }
            variables = slot(myglm@terms[[1]],"term.labels")
        }
    }
    else
    {
        if(!is.null(myglm$xlevels)){
            level_Burst_Date = myglm$xlevels$Burst_Date
            if(is.null(level_Burst_Date)){
                level_Burst_Date = level_all_Burst_Date
            }
            variables = colnames(myglm$model)[2:length(colnames(myglm$model))]
        }
    }

    produit_cartesien = expand.grid(level_Tree_Fruit_Load,
                                    level_Burst_Date,
                                    level_Position_A,
                                    level_Position_Ancestor_A,
                                    level_Nature_Ancestor_F,
                                    level_Nature_F)

    names(produit_cartesien) = c("Tree_Fruit_Load",
                                 "Burst_Date",
                                 "Position_A",
                                 "Position_Ancestor_A",
                                 "Nature_Ancestor_F",
                                 "Nature_F")
 
    data_probs = unique(produit_cartesien[variables])
    #print(data_probs)

    if (is_vglm){
        if(!is.null(variables)){
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
    else {
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



writing_glm_tables = function(vegetative_burst,
                              has_apical_gu, 
                              has_lateral_gu, 
                              nb_lateral_gu, 
                              burst_date_children, 
                              burst_delta_date_children, 
                              burst_delta_date_children_poisson,
                              flowering, 
                              nb_inflo, 
                              flowering_week, 
                              fruiting,
                              nb_fruits,
                              output_dir, 
                              year, 
                              verbose = TRUE){

    if (file.exists(output_dir) == FALSE){
        dir.create(output_dir,recursive=TRUE)
    }
    if (verbose) print("Write probas of vegetative_burst")
    table_prob_vegetative_burst = determine_table_prob_from_glm(vegetative_burst)
    write.csv(table_prob_vegetative_burst,file=paste(output_dir,"vegetative_burst_",year,".csv", sep=""), row.names = FALSE)

    if (verbose) print("Write probas of has_apical_gu_child")
    table_prob_has_apical_gu = determine_table_prob_from_glm(has_apical_gu)
    write.csv(table_prob_has_apical_gu,file=paste(output_dir,"has_apical_gu_child_",year,".csv",sep=""), row.names = FALSE)

     if (verbose) print("Write probas of has_lateral_gu_children")
    table_prob_has_lateral_gu = determine_table_prob_from_glm(has_lateral_gu)
    write.csv(table_prob_has_lateral_gu,file=paste(output_dir,"has_lateral_gu_children_",year,".csv",sep=""), row.names = FALSE)

    if (verbose) print("Write probas of nb_lateral_gu_children")
    table_prob_nb_lateral_gu = determine_table_prob_from_glm(nb_lateral_gu)
    write.csv(table_prob_nb_lateral_gu,file=paste(output_dir,"nb_lateral_gu_children_",year,".csv",sep=""), row.names = FALSE)

    if( !is.null(burst_date_children) ){
        if (verbose) print("Write probas of burst_date_children")
        table_prob_burst_date_children = determine_table_prob_from_glm(burst_date_children)
        write.csv(table_prob_burst_date_children,file=paste(output_dir,"burst_date_children_",year,".csv",sep=""), row.names = FALSE)
    }

    if( !is.null(burst_delta_date_children) ){
        if (verbose) print("Write probas of burst_delta_date_children")
        table_prob_burst_delta_date_children = determine_table_prob_from_glm(burst_delta_date_children)
        write.csv(table_prob_burst_delta_date_children,file=paste(output_dir,"burst_delta_date_children_",year,".csv",sep=""), row.names = FALSE)
    }

    if( !is.null(burst_delta_date_children_poisson) ){
        if (verbose) print("Write probas of burst_delta_date_children_poisson")
        table_prob_burst_delta_date_children_poisson = determine_table_prob_from_glm(burst_delta_date_children_poisson)
        write.csv(table_prob_burst_delta_date_children_poisson,file=paste(output_dir,"burst_delta_date_children_poisson_",year,".csv",sep=""), row.names = FALSE)
    }

    if( !is.null(flowering) ){
        if (verbose) print("Write probas of flowering")
        table_prob_flowering = determine_table_prob_from_glm(flowering)
        write.csv(table_prob_flowering,file=paste(output_dir,"flowering_",year,".csv",sep=""), row.names = FALSE)
    }

    if( !is.null(nb_inflo) ){
        if (verbose) print("Write probas of nb_inflorescences")
        table_prob_nb_inflo = determine_table_prob_from_glm(nb_inflo)
        write.csv(table_prob_nb_inflo,file=paste(output_dir,"nb_inflorescences_",year,".csv",sep=""), row.names = FALSE)
    }
    if( !is.null(flowering_week) ){
        if (verbose) print("Write probas of flowering_week")
        table_prob_flowering_week = determine_table_prob_from_glm(flowering_week)
        write.csv(table_prob_flowering_week,file=paste(output_dir,"flowering_week_",year,".csv",sep=""), row.names = FALSE)
    }

    if( !is.null(fruiting) ){
        if (verbose) print("Write probas of fruiting")
        table_prob_fruiting = determine_table_prob_from_glm(fruiting)
        write.csv(table_prob_fruiting,file=paste(output_dir,"fruiting_",year,".csv",sep=""), row.names = FALSE)
    }

    if( !is.null(nb_fruits) ){
        if (verbose) print("Write probas of nb_fruits")
        table_prob_nb_fruits = determine_table_prob_from_glm(nb_fruits)
        write.csv(table_prob_nb_fruits,file=paste(output_dir,"nb_fruits_",year,".csv",sep=""), row.names = FALSE)
    }


}


determining_glm_tables_within_cycle = function(data, year, verbose = 0) {

    # Removing of GU born in a month with less than 4 GUs born in this month
    # for( month in 1:12){
    #     nb_month = which(data$Burst_Date==month)
    #     if(0 < length(nb_month) & length(nb_month) <= 4){
    #         data = data[-nb_month,]
    #     }
    # }
    MinNbGUForGLM = 30

    # Assign covariables as factors
    data$Burst_Date=as.factor(data$Burst_Date)
    data$Tree_Fruit_Load = as.factor(data$Tree_Fruit_Load)
    data$Position_Ancestor_A = as.factor(data$Position_Ancestor_A)
    data$Position_A = as.factor(data$Position_A)
    data$Nature_Ancestor_F = as.factor(data$Nature_Ancestor_F)

    ## Assign delta date as ordered factor
    # Delta_Burst_date_child = as.factor(data$Delta_Burst_date_child)
    # Delta_burst_date_child = ordered(Delta_Burst_date_child)

    # Assign date as ordered factor
    level_order = c("6","7","8","9","10","11","12","1","2","3","4","5")
    data$Burst_Date_Children = ordered(data$Burst_Date_Children, levels = level_order)
    data$Burst_Date_Children = factor(data$Burst_Date_Children)

    data$Burst_Delta_Date_ChildrenP = data$Burst_Delta_Date_Children - 1
    
    attach(data)
    summary(data)

    # To make glm for each tree :
    trees = levels(tree)
    trees = c(trees, "loaded", "notloaded")

    tracestep = 0
    if (verbose >= 3) tracestep = 0


    ##############################################
    #### Vegetative Burst
    ##############################################
    if (verbose >= 1) print("Estimate Vegetative Burst") 
    index_trees = list()
    for(tree_name in trees){
        if(tree_name=="loaded"){
            index_trees[[tree_name]] = which(Tree_Fruit_Load == 1)
        }
        else if(tree_name=="notloaded"){
            index_trees[[tree_name]] = which(Tree_Fruit_Load == 0)
        }
        else{
            index_trees[[tree_name]] = which(tree == tree_name)
        }
    }



    ### complete GLM ###
    # For all trees
    complete_glm.vegetative_burst.all = glm( Vegetative_Burst ~ Tree_Fruit_Load + Burst_Date + Position_A + Position_Ancestor_A +  Nature_Ancestor_F,
                                             family = binomial, data=data)
    if (verbose >= 3) summary(complete_glm.vegetative_burst.all) # AIC : 1711
    
    #nbgus= ftable(data$Tree_Fruit_Load,data$Position_A,data$Position_Ancestor_A,data$Nature_Ancestor_F,data$Burst_Date)
    #print(nbgus)
    
    # For each tree, loaded trees and not loaded trees
    complete_glm.vegetative_burst.trees = list()
    for(tree_name in trees){
        complete_glm.vegetative_burst.trees[[tree_name]] = glm(Vegetative_Burst ~ Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_F,
                                                               family = binomial, data=data, subset=index_trees[[tree_name]])
    }


    ### selected GLM ###
    # For all trees
    selected_glm.vegetative_burst.all = step(complete_glm.vegetative_burst.all, trace = tracestep)   
    if (verbose >= 3) summary(selected_glm.vegetative_burst.all) # AIC : 
    sglm = selected_glm.vegetative_burst.all
    #factors = sglm$x
    #print(factors)
    #nbgus= ftable(factors)

    # For each tree, loaded trees and not loaded trees
    selected_glm.vegetative_burst.trees = list()
    for(tree_name in trees){
        selected_glm.vegetative_burst.trees[[tree_name]] = step(complete_glm.vegetative_burst.trees[[tree_name]], trace = tracestep)
    }


    index_bursted.all = which(Vegetative_Burst == 1)

    index_bursted.trees = list()
    for(tree_name in trees){
        if(tree_name=="loaded"){
            index_bursted.trees[[tree_name]] = which(Tree_Fruit_Load == 1 & Vegetative_Burst == 1)
        }
        else if(tree_name=="notloaded"){
            index_bursted.trees[[tree_name]] = which(Tree_Fruit_Load == 0 & Vegetative_Burst == 1)
        }
        else{
            index_bursted.trees[[tree_name]] = which(tree == tree_name & Vegetative_Burst == 1)
        }
    }


    ##############################################
    #### Has_Apical_GU_Child
    ##############################################
    if (verbose >= 1) print("Estimate Has_Apical_GU_Child") 
    ### complete GLM ###
    # For all trees
    complete_glm.has_apical_gu_child.all = glm( Has_Apical_GU_Child ~ Tree_Fruit_Load + 
                                                                      Burst_Date + 
                                                                      Position_A + 
                                                                      Position_Ancestor_A + 
                                                                      Nature_Ancestor_F,
                                                    family = binomial, data=data, subset = index_bursted.all)
    if (verbose >= 3) summary(complete_glm.has_apical_gu_child.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.has_apical_gu_child.trees = list()
    for(tree_name in trees){
        complete_glm.has_apical_gu_child.trees[[tree_name]] = glm(Has_Apical_GU_Child ~ Burst_Date + 
                                                                                        Position_A + 
                                                                                        Position_Ancestor_A + 
                                                                                        Nature_Ancestor_F,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
    }
 
    ### selected GLM ###
    # For all trees
    selected_glm.has_apical_gu_child.all = step(complete_glm.has_apical_gu_child.all, trace = tracestep)
    if (verbose >= 3) summary(selected_glm.has_apical_gu_child.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.has_apical_gu_child.trees = list()
    for(tree_name in trees){
         selected_glm.has_apical_gu_child.trees[[tree_name]] = step(complete_glm.has_apical_gu_child.trees[[tree_name]], trace = tracestep)
    }


    ##############################################
    #### Has_Lateral_GU_Children
    ##############################################
    if (verbose >= 1) print("Estimate Has_Lateral_GU_Children") 
    ### complete GLM ###
    # For all trees
    complete_glm.has_lateral_gu_children.all = glm( Has_Lateral_GU_Children ~ Tree_Fruit_Load + 
                                                                              Burst_Date + 
                                                                              Position_A + 
                                                                              Position_Ancestor_A + 
                                                                              Nature_Ancestor_F,
                                                    family = binomial, data=data, subset = index_bursted.all)
    if (verbose >= 3) summary(complete_glm.has_lateral_gu_children.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.has_lateral_gu_children.trees = list()
    for(tree_name in trees){
        complete_glm.has_lateral_gu_children.trees[[tree_name]] = glm(Has_Lateral_GU_Children ~ Burst_Date + 
                                                                                                Position_A + 
                                                                                                Position_Ancestor_A + 
                                                                                                Nature_Ancestor_F,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
    }
 
    ### selected GLM ###
    # For all trees
    selected_glm.has_lateral_gu_children.all = step(complete_glm.has_lateral_gu_children.all, trace = tracestep)
    if (verbose >= 3) summary(selected_glm.has_lateral_gu_children.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.has_lateral_gu_children.trees = list()
    for(tree_name in trees){
         selected_glm.has_lateral_gu_children.trees[[tree_name]] = step(complete_glm.has_lateral_gu_children.trees[[tree_name]], trace = tracestep)
    }

        
    ##############################################
    #### Number of lateral GU
    ##############################################
    if (verbose >= 1) print("Estimate Number of lateral GU") 
    index_lateral.all = which(Vegetative_Burst == 1 & Has_Lateral_GU_Children == 1)

    #On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
    #On enlève donc 1 au nombre de latérales afin de commencer à 0.
    ####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
    Nb_lateral_gu = Nb_Lateral_GU_Children -1

    index_lateral.trees = list()
    for(tree_name in trees){
        if(tree_name=="loaded"){
            index_lateral.trees[[tree_name]] = which(Tree_Fruit_Load == 1 & Vegetative_Burst == 1 & Has_Lateral_GU_Children == 1)
        }
        else if(tree_name=="notloaded"){
            index_lateral.trees[[tree_name]] = which(Tree_Fruit_Load == 0 & Vegetative_Burst == 1 & Has_Lateral_GU_Children == 1)
        }
        else{
            index_lateral.trees[[tree_name]] = which(tree == tree_name & Vegetative_Burst == 1 & Has_Lateral_GU_Children == 1)
        }
    }


    ### complete GLM ###
    # For all trees
    complete_glm.nb_lateral_gu.all = glm( Nb_lateral_gu  ~ Tree_Fruit_Load + 
                                                           Burst_Date + 
                                                           Position_A + 
                                                           Position_Ancestor_A + 
                                                           Nature_Ancestor_F,
        family = poisson, data=data, subset = index_lateral.all)
    if (verbose >= 3) summary(complete_glm.nb_lateral_gu.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.nb_lateral_gu.trees = list()
    for(tree_name in trees){
        complete_glm.nb_lateral_gu.trees[[tree_name]] = glm(Nb_lateral_gu ~ 
            Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_F,
            family = poisson, data=data, subset=index_lateral.trees[[tree_name]])
    }


    ### selected GLM ###
    # For all trees
    selected_glm.nb_lateral_gu.all = step(complete_glm.nb_lateral_gu.all, trace = tracestep)
    if (verbose >= 3) summary(selected_glm.nb_lateral_gu.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.nb_lateral_gu.trees = list()
    for(tree_name in trees){
        selected_glm.nb_lateral_gu.trees[[tree_name]] = step(complete_glm.nb_lateral_gu.trees[[tree_name]], trace = tracestep)
    }




    ##############################################
    #### Burst date of children    
    ##############################################
    if (verbose >= 1) print("Estimate Burst date of children") 

        
    ### complete GLM ###
    # For all trees  
    complete_vglm.burst_date_children.all = vglm( Burst_Date_Children ~ Tree_Fruit_Load + 
                                                                        Burst_Date + 
                                                                        Position_A + 
                                                                        Position_Ancestor_A + 
                                                                        Nature_Ancestor_F,
                                                  family = cumulative(parallel=TRUE) ,data=data, subset = index_bursted.all)
    if (verbose >= 3) {
        summary(complete_vglm.burst_date_children.all) # Log-likelihood: 
        print(paste("AIC:",get_vglm_AIC(complete_vglm.burst_date_children.all)))
    }

    # For each tree, loaded trees and not loaded trees
    complete_vglm.burst_date_children.trees = list()
    #AIC.vglm.burst_date_children.tree = list()
    for(tree_name in trees){
        if(length(index_bursted.trees[[tree_name]])>MinNbGUForGLM){

            complete_vglm.burst_date_children.trees[[tree_name]] = vglm(Burst_Date_Children ~ Burst_Date + 
                                                                                             Position_A + 
                                                                                             Position_Ancestor_A + 
                                                                                             Nature_Ancestor_F,
                                                family = cumulative(parallel=TRUE), data=data, subset=index_bursted.trees[[tree_name]])
            #AIC.vglm.burst_date_children.tree[[tree_name]] = get_vglm_AIC(complete_vglm.burst_date_children.tree[[tree_name]])
        }
    }


    ### selected GLM ###
    # For all trees


    # la fonction step ne s applique pas a la classe des vglm
    # ===>> selectioner le model a la main, AIC = 2k - 2ln(L)

    #............ TODO



    ##############################################
    #### Delta Burst date of children    
    ##############################################
    if (verbose >= 1) print("Estimate Burst delta date of children with vglm") 

    ### complete GLM ###
    # For all trees  
    complete_vglm.burst_delta_date_children.all = vglm( Burst_Delta_Date_Children ~ Tree_Fruit_Load + 
                                                                        Burst_Date + 
                                                                        Position_A + 
                                                                        Position_Ancestor_A + 
                                                                        Nature_Ancestor_F,
                                                  family = cumulative(parallel=TRUE) ,data=data, subset = index_bursted.all)
    if (verbose >= 3) {
        summary(complete_vglm.burst_delta_date_children.all) # Log-likelihood: 
        print(paste("AIC:",get_vglm_AIC(complete_vglm.burst_delta_date_children.all)))
    }

    # For each tree, loaded trees and not loaded trees
    complete_vglm.burst_delta_date_children.trees = list()
    #AIC.vglm.burst_delta_date_children.tree = list()
    for(tree_name in trees){
        if(length(index_bursted.trees[[tree_name]])>MinNbGUForGLM){

            complete_vglm.burst_delta_date_children.trees[[tree_name]] = vglm(Burst_Delta_Date_Children ~ Burst_Date + 
                                                                                             Position_A + 
                                                                                             Position_Ancestor_A + 
                                                                                             Nature_Ancestor_F,
                                                family = cumulative(parallel=TRUE), data=data, subset=index_bursted.trees[[tree_name]])
            #AIC.vglm.burst_date_children.tree[[tree_name]] = get_vglm_AIC(complete_vglm.burst_date_children.tree[[tree_name]])
        }
    }


    ### selected GLM ###
    # For all trees


    # la fonction step ne s applique pas a la classe des vglm
    # ===>> selectioner le model a la main, AIC = 2k - 2ln(L)

    #............ TODO

    ##############################################
    #### Delta Burst date of children  with poisson  
    ##############################################
    if (verbose >= 1) print("Estimate Burst delta date of children with poisson") 




    ### complete GLM ###
    # For all trees
    complete_glm.burst_delta_date_children_poisson.all = glm( Burst_Delta_Date_ChildrenP  ~ Tree_Fruit_Load + 
                                                           Burst_Date + 
                                                           Position_A + 
                                                           Position_Ancestor_A + 
                                                           Nature_Ancestor_F,
        family = poisson, data=data, subset = index_bursted.all)
    if (verbose >= 3) summary(complete_glm.burst_delta_date_children_poisson.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.burst_delta_date_children_poisson.trees = list()
    for(tree_name in trees){
        complete_glm.burst_delta_date_children_poisson.trees[[tree_name]] = glm(Burst_Delta_Date_ChildrenP ~ 
            Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_F,
            family = poisson, data=data, subset=index_bursted.trees[[tree_name]])
    }


    ### selected GLM ###
    # For all trees
    selected_glm.burst_delta_date_children_poisson.all = step(complete_glm.burst_delta_date_children_poisson.all, trace = tracestep)
    if (verbose >= 3) summary(selected_glm.burst_delta_date_children_poisson.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.burst_delta_date_children_poisson.trees = list()
    for(tree_name in trees){
        selected_glm.burst_delta_date_children_poisson.trees[[tree_name]] = step(complete_glm.burst_delta_date_children_poisson.trees[[tree_name]], trace = tracestep)
    }


    ##############################################
    #### Flowering 
    ##############################################
    if (verbose >= 1) print("Estimate Flowering") 
    index_extremity.all = which(is_terminal == 1)

    index_extremity.trees = list()
    for(tree_name in trees){
        if(tree_name=="loaded"){
            index_extremity.trees[[tree_name]] = which(Tree_Fruit_Load == 1 & is_terminal == 1)
        }else if(tree_name=="notloaded"){
            index_extremity.trees[[tree_name]] = which(Tree_Fruit_Load == 0 & is_terminal == 1)
        }else{
            index_extremity.trees[[tree_name]] = which(tree == tree_name & is_terminal == 1)
        }
    }
        
    ### complete GLM ###
    # For all trees   
    complete_glm.flowering.all = glm( Flowering ~ Tree_Fruit_Load + 
                                                  Burst_Date + 
                                                  Position_A + 
                                                  Position_Ancestor_A + 
                                                  Nature_Ancestor_F,
                                family = binomial, data=data, subset = index_extremity.all)
    if (verbose >= 3) summary(complete_glm.flowering.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.flowering.trees = list()
    for(tree_name in trees){
        complete_glm.flowering.trees[[tree_name]] = glm(Flowering ~ Burst_Date + 
                                                                    Position_A + 
                                                                    Position_Ancestor_A + 
                                                                    Nature_Ancestor_F,
            family = binomial, data=data, subset=index_extremity.trees[[tree_name]])
    }
 


    ### selected GLM ###
    # For all trees
    selected_glm.flowering.all = step(complete_glm.flowering.all, trace = tracestep) 
    if (verbose >= 3) summary(selected_glm.flowering.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.flowering.trees = list()
    for(tree_name in trees){
        selected_glm.flowering.trees[[tree_name]] = step(complete_glm.flowering.trees[[tree_name]], trace = tracestep)
    }



    ##############################################
    #### Number of inflorescences 
    ##############################################
    if (verbose >= 1) print("Estimate Number of inflorescences") 
    index_flowering.all = which(Flowering == 1)
    Nb_inflo = Nb_Inflorescence -1

    index_flowering.trees = list()
    for(tree_name in trees){
        if(tree_name=="loaded"){
            index_flowering.trees[[tree_name]] = which(Tree_Fruit_Load == 1 & Flowering == 1)
        }else if(tree_name=="notloaded"){
            index_flowering.trees[[tree_name]] = which(Tree_Fruit_Load == 0 & Flowering == 1)
        }else{
            index_flowering.trees[[tree_name]] = which(tree == tree_name & Flowering == 1)
        }
    }


    ### complete GLM ###
    # For all trees
    complete_glm.nb_inflorescences.all = glm( Nb_inflo ~ Tree_Fruit_Load + 
                                                         Burst_Date + 
                                                         Position_A + 
                                                         Position_Ancestor_A + 
                                                         Nature_Ancestor_F,
        family = poisson, data=data, subset = index_flowering.all)
    if (verbose >= 3) summary(complete_glm.nb_inflorescences.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.nb_inflorescences.trees = list()
    for(tree_name in trees){
        complete_glm.nb_inflorescences.trees[[tree_name]] = glm( Nb_inflo ~ Burst_Date + 
                                                                            Position_A + 
                                                                            Position_Ancestor_A + 
                                                                            Nature_Ancestor_F,
            family = poisson, data=data, subset=index_flowering.trees[[tree_name]])
    }


    ### selected GLM ###
    # For all trees
    selected_glm.nb_inflorescences.all = step(complete_glm.nb_inflorescences.all, trace = tracestep) 
    if (verbose >= 3) summary(selected_glm.nb_inflorescences.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.nb_inflorescences.trees = list()
    for(tree_name in trees){
        selected_glm.nb_inflorescences.trees[[tree_name]] = step(complete_glm.nb_inflorescences.trees[[tree_name]], trace = tracestep)
    }


    ##############################################
    #### Date of inflorescences
    ##############################################
    if (verbose >= 1) print("Estimate Date of inflorescences") 

    has_flowering_week.all = which(Flowering_Week > 0)
    if(length(has_flowering_week.all) > MinNbGUForGLM){
        complete_vglm.flowering_week.all = vglm( Flowering_Week ~ Tree_Fruit_Load + 
                                                                  Burst_Date + 
                                                                  Position_A + 
                                                                  Position_Ancestor_A + 
                                                                  Nature_Ancestor_F,
            family = cumulative(parallel=TRUE) ,data=data, subset = index_flowering.all)
        if (verbose >= 3) {
            summary(complete_vglm.flowering_week.all) # Log-likelihood: 
            print(paste("AIC:",get_vglm_AIC(complete_vglm.flowering_week.all)))
        } 

        # For each tree, loaded trees and not loaded trees
        complete_vglm.flowering_week.trees = list()
        AIC.vglm.flowering_week.trees = list()
        for(tree_name in trees){
            if(length(index_flowering.trees[[tree_name]]) > MinNbGUForGLM){
                complete_vglm.flowering_week.trees[[tree_name]] = vglm(Flowering_Week ~ Burst_Date + 
                                                                                        Position_A + 
                                                                                        Position_Ancestor_A + 
                                                                                        Nature_Ancestor_F,
                            family = cumulative(parallel=TRUE), data=data, subset=index_flowering.trees[[tree_name]])
                AIC.vglm.flowering_week.trees[[tree_name]] = get_vglm_AIC(complete_vglm.flowering_week.trees[[tree_name]])
            }
        }
    }
    else {
        print(paste("Not enougth flowering week specified for year",year,":",length(has_flowering_week.all)))
        complete_vglm.flowering_week.all = NULL
        complete_vglm.flowering_week.trees = list()
    }
       
       
    ##############################################
    #### Fruiting 
    ##############################################
    if (verbose >= 1) print("Estimate Fruiting") 
    index_fruiting.all = which(Flowering == 1 & Tree_Fruit_Load == 1)

    index_fruiting.trees = list()
    for(tree_name in trees){
        if(tree_name=="loaded"){
            index_fruiting.trees[[tree_name]] = which(Tree_Fruit_Load == 1 & Flowering == 1)
        }else if(tree_name=="notloaded"){
            index_fruiting.trees[[tree_name]] = which(Tree_Fruit_Load == 0 & Flowering == 1)
        }else{
            index_fruiting.trees[[tree_name]] = which(tree == tree_name & Flowering == 1)
        }
    }
        
    ### complete GLM ###
    # For all trees   
    complete_glm.fruiting.all = glm( Fruiting ~ Burst_Date + 
                                                  Position_A + 
                                                  Position_Ancestor_A + 
                                                  Nature_Ancestor_F,
                                family = binomial, data=data, subset = index_fruiting.all)
    if (verbose >= 3) summary(complete_glm.fruiting.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.fruiting.trees = list()
    for(tree_name in trees){
        complete_glm.fruiting.trees[[tree_name]] = NULL
            #     glm(Fruiting ~ Burst_Date + 
            #                                                         Position_A + 
            #                                                         Position_Ancestor_A + 
            #                                                         Nature_Ancestor_F,
            # family = binomial, data=data, subset=index_flowering.trees[[tree_name]])
    }
 


    ### selected GLM ###
    # For all trees
    selected_glm.fruiting.all = step(complete_glm.fruiting.all, trace = tracestep) 
    if (verbose >= 3) summary(selected_glm.fruiting.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.fruiting.trees = list()
    for(tree_name in trees){
        selected_glm.fruiting.trees[[tree_name]] = NULL # step(complete_glm.fruiting.trees[[tree_name]], trace = tracestep)
    }


   ##############################################
    #### Number of fruits 
    ##############################################
    if (verbose >= 1) print("Estimate Number of fruits") 
    index_fruiting.all = which(Fruiting == 1)
    Nb_fruits = Nb_Fruits -1

    index_fruiting.trees = list()
    for(tree_name in trees){
        if(tree_name=="loaded"){
            index_fruiting.trees[[tree_name]] = which(Tree_Fruit_Load == 1 & Fruiting == 1)
        }else if(tree_name=="notloaded"){
            index_fruiting.trees[[tree_name]] = which(Tree_Fruit_Load == 0 & Fruiting == 1)
        }else{
            index_fruiting.trees[[tree_name]] = which(tree == tree_name & Fruiting == 1)
        }
    }


    ### complete GLM ###
    # For all trees
    complete_glm.nb_fruits.all = glm( Nb_fruits ~ Burst_Date + 
                                                  Position_A + 
                                                  Position_Ancestor_A + 
                                                  Nature_Ancestor_F,
        family = poisson, data=data, subset = index_fruiting.all)
    if (verbose >= 3) summary(complete_glm.nb_fruits.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.nb_fruits.trees = list()
    for(tree_name in trees){
        if(tree_name=="notloaded" | tree_name== "B14" | tree_name =="F6"){
            complete_glm.nb_fruits.trees[[tree_name]] = NULL
        }
        else {
        complete_glm.nb_fruits.trees[[tree_name]] = NULL
        # glm( Nb_fruits ~ Burst_Date + 
        #                                                                     Position_A + 
        #                                                                     Position_Ancestor_A + 
        #                                                                     Nature_Ancestor_F,
        #     family = poisson, data=data, subset=index_fruiting.trees[[tree_name]])
        }
    }


    ### selected GLM ###
    # For all trees
    selected_glm.nb_fruits.all = step(complete_glm.nb_fruits.all, trace = tracestep) 
    if (verbose >= 3) summary(selected_glm.nb_fruits.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.nb_fruits.trees = list()
    for(tree_name in trees){
        if (is.null(complete_glm.nb_fruits.trees[[tree_name]])){selected_glm.nb_fruits.trees[[tree_name]] = NULL}
        else {selected_glm.nb_fruits.trees[[tree_name]] = step(complete_glm.nb_fruits.trees[[tree_name]], trace = tracestep)}
    }

       

    detach(data)

    ##############################################
    #### Writing probabilities
    ##############################################


    path_complete_glm = paste(output_dir,"complete_glm/",sep="")
    path_complete_glm_all_trees = paste(path_complete_glm,"all_trees/",sep="")

    if(verbose >= 1) print(paste("Write complete glm proba tables from all trees of year",year))
    year = paste("within_",year,sep="")
    writing_glm_tables(complete_glm.vegetative_burst.all, 
                       complete_glm.has_apical_gu_child.all, 
                       complete_glm.has_lateral_gu_children.all, 
                       complete_glm.nb_lateral_gu.all, 
                       complete_vglm.burst_date_children.all, 
                       complete_vglm.burst_delta_date_children.all, 
                       complete_glm.burst_delta_date_children_poisson.all, 
                       complete_glm.flowering.all, 
                       complete_glm.nb_inflorescences.all, 
                       complete_vglm.flowering_week.all,
                       complete_glm.fruiting.all, 
                       complete_glm.nb_fruits.all, 
                       path_complete_glm_all_trees, year, verbose >= 2)

    for(tree_name in trees){
        if(verbose >= 1) print(paste("Write complete glm proba tables from tree",tree_name))
        path_complete_glm_tree = paste(path_complete_glm,tree_name,"/",sep="")
        writing_glm_tables(complete_glm.vegetative_burst.trees[[tree_name]], 
                           complete_glm.has_lateral_gu_children.trees[[tree_name]], 
                           complete_glm.has_apical_gu_child.trees[[tree_name]], 
                           complete_glm.nb_lateral_gu.trees[[tree_name]], 
                           complete_vglm.burst_date_children.trees[[tree_name]], 
                           complete_vglm.burst_delta_date_children.trees[[tree_name]], 
                           complete_glm.burst_delta_date_children_poisson.trees[[tree_name]], 
                           complete_glm.flowering.trees[[tree_name]], 
                           complete_glm.nb_inflorescences.trees[[tree_name]], 
                           complete_vglm.flowering_week.trees[[tree_name]], 
                           complete_glm.fruiting.trees[[tree_name]], 
                           complete_glm.nb_fruits.trees[[tree_name]], 
                           path_complete_glm_tree, year, verbose >= 2)
    }

    path_selected_glm = paste(output_dir,"selected_glm/",sep="")
    path_selected_glm_all_trees = paste(path_selected_glm,"all_trees/",sep="")

    if(verbose >= 1) print("Write selected glm proba tables from all trees")
    writing_glm_tables(selected_glm.vegetative_burst.all, 
                       selected_glm.has_apical_gu_child.all, 
                       selected_glm.has_lateral_gu_children.all, 
                       selected_glm.nb_lateral_gu.all, 
                       complete_vglm.burst_date_children.all, 
                       complete_vglm.burst_delta_date_children.all, 
                       selected_glm.burst_delta_date_children_poisson.all, 
                       selected_glm.flowering.all, 
                       selected_glm.nb_inflorescences.all, 
                       complete_vglm.flowering_week.all,
                       selected_glm.fruiting.all, 
                       selected_glm.nb_fruits.all, 
                       path_selected_glm_all_trees, year, verbose >= 2)

    for(tree_name in trees){
        if(verbose >= 1) print(paste("Write selected glm proba tables from tree",tree_name))
        path_selected_glm_tree = paste(path_selected_glm,tree_name,"/",sep="")
        writing_glm_tables(selected_glm.vegetative_burst.trees[[tree_name]], 
                           selected_glm.has_apical_gu_child.trees[[tree_name]], 
                           selected_glm.has_lateral_gu_children.trees[[tree_name]], 
                           selected_glm.nb_lateral_gu.trees[[tree_name]], 
                           complete_vglm.burst_date_children.trees[[tree_name]], 
                           complete_vglm.burst_delta_date_children.trees[[tree_name]], 
                           selected_glm.burst_delta_date_children_poisson.trees[[tree_name]], 
                           selected_glm.flowering.trees[[tree_name]], 
                           selected_glm.nb_inflorescences.trees[[tree_name]], 
                           complete_vglm.flowering_week.trees[[tree_name]], 
                           selected_glm.fruiting.trees[[tree_name]], 
                           selected_glm.nb_fruits.trees[[tree_name]], 
                           path_selected_glm_tree, year, verbose >= 2)
    }

}

determining_glm_tables_within_cycle_for_year = function(input_dir, year, verbose = 0) {
    table_gu_within_cycle = read.csv(paste(input_dir,"table_within_cycle_",year,".csv",sep=""),header = TRUE)
    determining_glm_tables_within_cycle(table_gu_within_cycle, year, verbose = verbose)
}



determining_glm_tables_between_cycle = function(data, year, with_burstdate = FALSE, verbose = FALSE) {

    # Removing of GU born in a month with less than 4 GUs born in this month
    # for( month in 1:12){
    #     nb_month = which(data$Burst_Date==month)
    #     if(0 < length(nb_month) & length(nb_month) <= 4){
    #         data = data[-nb_month,]
    #     }
    # }
    MinNbGUForGLM = 30

    # Assign covariables as factors
    if (with_burstdate == TRUE)
        data$Burst_Date=as.factor(data$Burst_Date)    
    data$Tree_Fruit_Load = as.factor(data$Tree_Fruit_Load)
    data$Position_A = as.factor(data$Position_A)
    data$Nature_F = as.factor(data$Nature_F)

    ## Assign delta date as ordered factor
    # Delta_Burst_date_child = as.factor(data$Delta_Burst_date_child)
    # Delta_burst_date_child = ordered(Delta_Burst_date_child)

    # Assign date as ordered factor
    level_order = c("106","107","108","109","110","111","112","101","102","103","104","105",
                    "206","207","208","209","210","211","212","201","202","203","204","205")

    data$Burst_Date_Children = ordered(data$Burst_Date_Children, levels = level_order)
    data$Burst_Date_Children = factor(data$Burst_Date_Children)


    attach(data)
    summary(data)

    # To make glm for each tree :
    trees = levels(tree)
    trees = c(trees, "loaded", "notloaded")


    tracestep = 0
    if (verbose >= 3) tracestep = 1


    ##############################################
    #### Vegetative Burst
    ##############################################
    if (verbose >= 1) print("Estimate Vegetative Burst") 
    index_trees = list()
    for(tree_name in trees){
        if(tree_name=="loaded"){
            index_trees[[tree_name]] = which(Tree_Fruit_Load == 1)
        }
        else if(tree_name=="notloaded"){
            index_trees[[tree_name]] = which(Tree_Fruit_Load == 0)
        }
        else{
            index_trees[[tree_name]] = which(tree == tree_name)
        }
    }



    ### complete GLM ###
    # For all trees
    if (with_burstdate == TRUE)
        complete_glm.vegetative_burst.all = glm( Vegetative_Burst ~ Tree_Fruit_Load + Burst_Date + Position_A  +  Nature_F,
                                                family = binomial, data=data)
    else 
        complete_glm.vegetative_burst.all = glm( Vegetative_Burst ~ Tree_Fruit_Load + Position_A  +  Nature_F,
                                                family = binomial, data=data)
    if (verbose >= 3) summary(complete_glm.vegetative_burst.all) # AIC : 1711
        
    # For each tree, loaded trees and not loaded trees
    complete_glm.vegetative_burst.trees = list()
    for(tree_name in trees){
        if (with_burstdate == TRUE)
            complete_glm.vegetative_burst.trees[[tree_name]] = glm(Vegetative_Burst ~ Burst_Date + Position_A + Nature_F,
                                                                   family = binomial, data=data, subset=index_trees[[tree_name]])
        else
            complete_glm.vegetative_burst.trees[[tree_name]] = glm(Vegetative_Burst ~ Position_A + Nature_F,
                                                                   family = binomial, data=data, subset=index_trees[[tree_name]])
    }


    ### selected GLM ###
    # For all trees
    selected_glm.vegetative_burst.all = step(complete_glm.vegetative_burst.all, trace = tracestep)   
    if (verbose >= 3) summary(selected_glm.vegetative_burst.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.vegetative_burst.trees = list()
    for(tree_name in trees){
        selected_glm.vegetative_burst.trees[[tree_name]] = step(complete_glm.vegetative_burst.trees[[tree_name]], trace = tracestep)
    }

    index_bursted.all = which(Vegetative_Burst == 1)

    index_bursted.trees = list()
    for(tree_name in trees){
        if(tree_name=="loaded"){
            index_bursted.trees[[tree_name]] = which(Tree_Fruit_Load == 1 & Vegetative_Burst == 1)
        }
        else if(tree_name=="notloaded"){
            index_bursted.trees[[tree_name]] = which(Tree_Fruit_Load == 0 & Vegetative_Burst == 1)
        }
        else{
            index_bursted.trees[[tree_name]] = which(tree == tree_name & Vegetative_Burst == 1)
        }
    }


    ##############################################
    #### Has_Apical_GU_Child
    ##############################################
    if (verbose >= 1) print("Estimate Has_Apical_GU_Child") 

    ### complete GLM ###
    # For all trees
    if (with_burstdate == TRUE)
        complete_glm.has_apical_gu_child.all = glm( Has_Apical_GU_Child ~ Tree_Fruit_Load + 
                                                                              Burst_Date + 
                                                                              Position_A + 
                                                                              Nature_F,
                                                        family = binomial, data=data, subset = index_bursted.all)
    else
        complete_glm.has_apical_gu_child.all = glm( Has_Apical_GU_Child ~ Tree_Fruit_Load + 
                                                                              Position_A + 
                                                                              Nature_F,
                                                        family = binomial, data=data, subset = index_bursted.all)
    
    if (verbose >= 3) summary(complete_glm.has_apical_gu_child.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.has_apical_gu_child.trees = list()
    for(tree_name in trees){
        if (with_burstdate == TRUE)
            complete_glm.has_apical_gu_child.trees[[tree_name]] = glm(Has_Apical_GU_Child ~ Burst_Date + 
                                                                                                Position_A + 
                                                                                                Nature_F,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
        else
            complete_glm.has_apical_gu_child.trees[[tree_name]] = glm(Has_Apical_GU_Child ~ Position_A + 
                                                                                                    Nature_F,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
    }
 
    ### selected GLM ###
    # For all trees
    selected_glm.has_apical_gu_child.all = step(complete_glm.has_apical_gu_child.all, trace = tracestep)
    if (verbose >= 3) summary(selected_glm.has_lateral_gu_children.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.has_apical_gu_child.trees = list()
    for(tree_name in trees){
         selected_glm.has_apical_gu_child.trees[[tree_name]] = step(complete_glm.has_apical_gu_child.trees[[tree_name]], trace = tracestep)
    }


        
    ##############################################
    #### Has_Lateral_GU_Children
    ##############################################
    if (verbose >= 1) print("Estimate Has_Lateral_GU_Children") 

    ### complete GLM ###
    # For all trees
    if (with_burstdate == TRUE)
        complete_glm.has_lateral_gu_children.all = glm( Has_Lateral_GU_Children ~ Tree_Fruit_Load + 
                                                                              Burst_Date + 
                                                                              Position_A + 
                                                                              Nature_F,
                                                        family = binomial, data=data, subset = index_bursted.all)
    else
        complete_glm.has_lateral_gu_children.all = glm( Has_Lateral_GU_Children ~ Tree_Fruit_Load + 
                                                                              Position_A + 
                                                                              Nature_F,
                                                        family = binomial, data=data, subset = index_bursted.all)
    
    if (verbose >= 3) summary(complete_glm.has_lateral_gu_children.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.has_lateral_gu_children.trees = list()
    for(tree_name in trees){
        if (with_burstdate == TRUE)
            complete_glm.has_lateral_gu_children.trees[[tree_name]] = glm(Has_Lateral_GU_Children ~ Burst_Date + 
                                                                                                Position_A + 
                                                                                                Nature_F,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
        else
            complete_glm.has_lateral_gu_children.trees[[tree_name]] = glm(Has_Lateral_GU_Children ~ Position_A + 
                                                                                                    Nature_F,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
    }
 
    ### selected GLM ###
    # For all trees
    selected_glm.has_lateral_gu_children.all = step(complete_glm.has_lateral_gu_children.all, trace = tracestep)
    if (verbose >= 3) summary(selected_glm.has_lateral_gu_children.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.has_lateral_gu_children.trees = list()
    for(tree_name in trees){
         selected_glm.has_lateral_gu_children.trees[[tree_name]] = step(complete_glm.has_lateral_gu_children.trees[[tree_name]], trace = tracestep)
    }


        
    ##############################################
    #### Number of lateral GU
    ##############################################
    if (verbose >= 1) print("Estimate Number of lateral GU") 
    index_lateral.all = which(Vegetative_Burst == 1 & Has_Lateral_GU_Children == 1)

    #On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
    #On enlève donc 1 au nombre de latérales afin de commencer à 0.
    ####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
    Nb_lateral_gu = Nb_Lateral_GU_Children -1

    index_lateral.trees = list()
    for(tree_name in trees){
        if(tree_name=="loaded"){
            index_lateral.trees[[tree_name]] = which(Tree_Fruit_Load == 1 & Vegetative_Burst == 1 & Has_Lateral_GU_Children == 1)
        }
        else if(tree_name=="notloaded"){
            index_lateral.trees[[tree_name]] = which(Tree_Fruit_Load == 0 & Vegetative_Burst == 1 & Has_Lateral_GU_Children == 1)
        }
        else{
            index_lateral.trees[[tree_name]] = which(tree == tree_name & Vegetative_Burst == 1 & Has_Lateral_GU_Children == 1)
        }
    }


    ### complete GLM ###
    # For all trees
    if (with_burstdate == TRUE)
        complete_glm.nb_lateral_gu.all = glm( Nb_lateral_gu  ~ Tree_Fruit_Load + 
                                                           Burst_Date + 
                                                           Position_A + 
                                                           Nature_F,
                     family = poisson, data=data, subset = index_lateral.all)
    else
        complete_glm.nb_lateral_gu.all = glm( Nb_lateral_gu  ~ Tree_Fruit_Load + 
                                                           Position_A + 
                                                           Nature_F,
                     family = poisson, data=data, subset = index_lateral.all)

    if (verbose >= 3) summary(complete_glm.nb_lateral_gu.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.nb_lateral_gu.trees = list()
    for(tree_name in trees){
        if (with_burstdate == TRUE)
            complete_glm.nb_lateral_gu.trees[[tree_name]] = glm(Nb_lateral_gu ~ Burst_Date + 
                                                                                Position_A +
                                                                                Nature_F,
                family = poisson, data=data, subset=index_lateral.trees[[tree_name]])
        else
            complete_glm.nb_lateral_gu.trees[[tree_name]] = glm(Nb_lateral_gu ~ Position_A +
                                                                                Nature_F,
                family = poisson, data=data, subset=index_lateral.trees[[tree_name]])
    }


    ### selected GLM ###
    # For all trees
    selected_glm.nb_lateral_gu.all = step(complete_glm.nb_lateral_gu.all, trace = tracestep)
    if (verbose >= 3) summary(selected_glm.nb_lateral_gu.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.nb_lateral_gu.trees = list()
    for(tree_name in trees){
        selected_glm.nb_lateral_gu.trees[[tree_name]] = step(complete_glm.nb_lateral_gu.trees[[tree_name]], trace = tracestep)
    }




    ##############################################
    #### Burst date of children    
    ##############################################
    if (verbose >= 1) print("Estimate Burst date of children") 

        
    ### complete GLM ###
    # For all trees  
    if (with_burstdate == TRUE)
        complete_vglm.burst_date_children.all = vglm( Burst_Date_Children ~ Tree_Fruit_Load + 
                                                                            Burst_Date + 
                                                                            Position_A + 
                                                                            Nature_F,
                                                      family = cumulative(parallel=TRUE) ,data=data, subset = index_bursted.all)
    else
        complete_vglm.burst_date_children.all = vglm( Burst_Date_Children ~ Tree_Fruit_Load + 
                                                                            Position_A + 
                                                                            Nature_F,
                                                      family = cumulative(parallel=TRUE) ,data=data, subset = index_bursted.all)

    if (verbose >= 3) {
        summary(complete_vglm.burst_date_children.all) # Log-likelihood: 
        print(paste("AIC:",get_vglm_AIC(complete_vglm.burst_date_children.all)))
    }

    # For each tree, loaded trees and not loaded trees
    complete_vglm.burst_date_children.tree = list()
    #AIC.vglm.burst_date_children.tree = list()
    for(tree_name in trees){
        if(length(index_bursted.trees[[tree_name]])>MinNbGUForGLM){

            if (with_burstdate == TRUE)
                complete_vglm.burst_date_children.tree[[tree_name]] = vglm(Burst_Date_Children ~ Burst_Date + 
                                                                                                 Position_A + 
                                                                                                 Nature_F,
                                                        family = cumulative(parallel=TRUE), data=data, subset=index_bursted.trees[[tree_name]])
            else
                complete_vglm.burst_date_children.tree[[tree_name]] = vglm(Burst_Date_Children ~ Position_A + 
                                                                                                 Nature_F,
                                                        family = cumulative(parallel=TRUE), data=data, subset=index_bursted.trees[[tree_name]])
            #AIC.vglm.burst_date_children.tree[[tree_name]] = get_vglm_AIC(complete_vglm.burst_date_children.tree[[tree_name]])
        }
    }


    ### selected GLM ###
    # For all trees


    # la fonction step ne s applique pas a la classe des vglm
    # ===>> selectioner le model a la main, AIC = 2k - 2ln(L)

    #............ TODO





    detach(data)

    ##############################################
    #### Writing probabilities
    ##############################################


    path_complete_glm = paste(output_dir,"complete_glm/",sep="")
    path_complete_glm_all_trees = paste(path_complete_glm,"all_trees/",sep="")

    year = paste("between_",year,sep="")

    if(verbose >= 1) print(paste("Write complete glm proba tables from all trees of year",year))
    writing_glm_tables(complete_glm.vegetative_burst.all, 
                       complete_glm.has_apical_gu_child.all, 
                       complete_glm.has_lateral_gu_children.all, 
                       complete_glm.nb_lateral_gu.all, 
                       complete_vglm.burst_date_children.all, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       path_complete_glm_all_trees, year, verbose >= 2)

    for(tree_name in trees){
        if(verbose >= 1) print(paste("Write complete glm proba tables from tree",tree_name))
        path_complete_glm_tree = paste(path_complete_glm,tree_name,"/",sep="")
        writing_glm_tables(complete_glm.vegetative_burst.trees[[tree_name]], 
                           complete_glm.has_apical_gu_child.trees[[tree_name]], 
                           complete_glm.has_lateral_gu_children.trees[[tree_name]], 
                           complete_glm.nb_lateral_gu.trees[[tree_name]], 
                           complete_vglm.burst_date_children.tree[[tree_name]], 
                           NULL, 
                           NULL, 
                           NULL, 
                           NULL, 
                           NULL, 
                           NULL, 
                           NULL, 
                           path_complete_glm_tree, year, verbose >= 2)
    }

    path_selected_glm = paste(output_dir,"selected_glm/",sep="")
    path_selected_glm_all_trees = paste(path_selected_glm,"all_trees/",sep="")

    if(verbose >= 1) print("Write selected glm proba tables from all trees")
    writing_glm_tables(selected_glm.vegetative_burst.all, 
                       selected_glm.has_apical_gu_child.all, 
                       selected_glm.has_lateral_gu_children.all, 
                       selected_glm.nb_lateral_gu.all, 
                       complete_vglm.burst_date_children.all, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       path_selected_glm_all_trees, year, verbose >= 2)

    for(tree_name in trees){
        if(verbose >= 1) print(paste("Write selected glm proba tables from tree",tree_name))
        path_selected_glm_tree = paste(path_selected_glm,tree_name,"/",sep="")
        writing_glm_tables(selected_glm.vegetative_burst.trees[[tree_name]], 
                           selected_glm.has_apical_gu_child.trees[[tree_name]], 
                           selected_glm.has_lateral_gu_children.trees[[tree_name]], 
                           selected_glm.nb_lateral_gu.trees[[tree_name]], 
                           complete_vglm.burst_date_children.tree[[tree_name]], 
                           NULL, 
                           NULL, 
                           NULL, 
                           NULL, 
                           NULL, 
                           NULL, 
                           NULL, 
                           path_selected_glm_tree, year, verbose >= 2)
    }

}

determining_glm_tables_between_cycle_for_year = function(input_dir, year, with_burstdate = FALSE, verbose = 0) {
    table_gu_within_cycle = read.csv(paste(input_dir,"table_between_cycle_",year,".csv",sep=""),header = TRUE)
    summary(table_gu_within_cycle)
    determining_glm_tables_between_cycle(table_gu_within_cycle, year,with_burstdate, verbose)
}

print("start")
verbose = 2
determining_glm_tables_within_cycle_for_year(input_dir, "04", verbose)
determining_glm_tables_within_cycle_for_year(input_dir, "05", verbose)

determining_glm_tables_between_cycle_for_year(input_dir, "03to0405", FALSE, verbose)
determining_glm_tables_between_cycle_for_year(input_dir, "04to05", TRUE, verbose)

