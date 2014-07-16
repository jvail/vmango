#setwd("~/Develop/vplants/branches/mangosim/src/vplants/mangosim/glm_estimation")
#
#
### Importation of data :

share_dir = '../../../../share/'
input_dir = paste(share_dir,'glm_estimate_input/cogshall/', sep="")
output_dir = paste(share_dir,'glm_output_proba/cogshall/', sep="")
if (file.exists(output_dir)){
    dir.create(output_dir,recursive=TRUE)
}



library(VGAM)

get_glm_AIC = function(myglm){
    k = myglm@rank
    logL = myglm@criterion$loglikelihood
    AIC = 2*k - 2*logL
    return(AIC)
}


determine_table_prob_from_glm = function(myglm){

    level_Tree_Fruit_Load = as.factor(0:1)
    level_Position_A = as.factor(0:1)
    level_Position_Ancestor_A = as.factor(0:1)
    level_Nature_Ancestor_V = as.factor(0:1)
    level_Nature_V = as.factor(0:1)
    level_all_Burst_Date = as.factor(1:12)

    #variables = NULL
    #level_Burst_Date = level_all_Burst_Date

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
        else {
            variables = NULL
            level_Burst_Date = level_all_Burst_Date
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
        else {
            variables = NULL
            level_Burst_Date = level_all_Burst_Date
        }
    }

    produit_cartesien = expand.grid(level_Tree_Fruit_Load,
                                    level_Burst_Date,
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
 
    data_probs = unique(produit_cartesien[variables])

    print("ok")
    if (is_vglm){
        print("predict vglm proba")
        if(!is.null(variables)){
            probs = predictvglm(myglm, newdata= data_probs,type="response")
            for(i in 1:length(colnames(probs)) ){
                data_probs[colnames(probs)[i] ] = probs[,i]
            }
        }
        else{
            probs = predictvglm(myglm,type="response")[1,]
            months_p = colnames(myglm@y)
            data_probs = data.frame(probs[1])
            row.names(data_probs) = NULL
            for(i in 2:length(months_p) ){
                data_probs = cbind(data_probs,probs[i])
            }
            colnames(data_probs)= months_p
        }
    }
    else {
        if(myglm$family[1]=="binomial" || myglm$family[1]=="poisson"){  
            print("predict glm proba")
            if (!is.null(variables)) {
                print("predict")
                probs = predict(myglm, newdata=data_probs, type="response")
                print("assign proba")
                data_probs["probas"]=probs
            }
            else{
                probs = predict(myglm,type="response")[1]
                data_probs = data.frame(probs)
            }
        }
    }
    print("ok")

    if("Burst_Date" %in% variables){
        other_level_Burst_Date = level_all_Burst_Date[!level_all_Burst_Date %in% level_Burst_Date]
        if (length(other_level_Burst_Date) > 0) {
            other_produit_cartesien = expand.grid(level_Tree_Fruit_Load, other_level_Burst_Date,level_Position_A,level_Position_Ancestor_A,level_Nature_Ancestor_V,level_Nature_V)
            names(other_produit_cartesien) = c("Tree_Fruit_Load","Burst_Date","Position_A","Position_Ancestor_A","Nature_Ancestor_V","Nature_V")
            other_data_probs = unique(other_produit_cartesien[variables])
            probs_null = rep(0,length(other_data_probs[,1]))
            if( class(myglm)[1]=="vglm"){
                for(i in 1:length(colnames(probs)) ){
                    other_data_probs[colnames(probs)[i] ] = 1./length(colnames(probs))
                }
            }else{
                other_data_probs$probas = probs_null
            }
            data_probs = rbind(data_probs,other_data_probs)
        }
    }
    print("ok")

    return(data_probs)
}

writing_glm_tables = function(vegetative_burst,has_lateral_gu, nb_lateral_gu, burst_date_children, flowering, nb_inflo, output_dir, year){
    if (file.exists(output_dir) == FALSE){
        dir.create(output_dir,recursive=TRUE)
    }
    table_prob_vegetative_burst = determine_table_prob_from_glm(vegetative_burst)
    write.csv(table_prob_vegetative_burst,file=paste(output_dir,"vegetative_burst_",year,".csv", sep=""), row.names = FALSE)

    table_prob_has_lateral_gu = determine_table_prob_from_glm(has_lateral_gu)
    write.csv(table_prob_has_lateral_gu,file=paste(output_dir,"has_lateral_gu_children_",year,".csv",sep=""), row.names = FALSE)

    table_prob_nb_lateral_gu = determine_table_prob_from_glm(nb_lateral_gu)
    write.csv(table_prob_nb_lateral_gu,file=paste(output_dir,"nb_lateral_gu_children_",year,".csv",sep=""), row.names = FALSE)

    if( !is.null(burst_date_children) ){
        table_prob_burst_date_children = determine_table_prob_from_glm(burst_date_children)
        write.csv(table_prob_burst_date_children,file=paste(output_dir,"burst_date_children_",year,".csv",sep=""), row.names = FALSE)
    }

    table_prob_flowering = determine_table_prob_from_glm(flowering)
    write.csv(table_prob_flowering,file=paste(output_dir,"flowering_",year,".csv",sep=""), row.names = FALSE)

    table_prob_nb_inflo = determine_table_prob_from_glm(nb_inflo)
    write.csv(table_prob_nb_inflo,file=paste(output_dir,"nb_inflorescences_",year,".csv",sep=""), row.names = FALSE)
}


determining_glm_tables_for_year = function(input_dir, year) {
    table_gu_within_cycle = read.csv(paste(input_dir,"table_within_cycle_",year,".csv",sep=""),header = TRUE)
    determining_glm_tables(table_gu_within_cycle, year)
}

determining_glm_tables = function(data, year) {


    # Removing of GU born in a month of less than 4 GUs born in this month
    for( month in 1:12){
        nb_month = which(data$Burst_Date==month)
        if(0 < length(nb_month) & length(nb_month) <= 4){
            data = data[-nb_month,]
        }
    }

    # Assign covariables as factors
    data$Burst_Date=as.factor(data$Burst_Date)
    data$Tree_Fruit_Load = as.factor(data$Tree_Fruit_Load)
    data$Position_Ancestor_A = as.factor(data$Position_Ancestor_A)
    data$Position_A = as.factor(data$Position_A)
    data$Nature_Ancestor_V = as.factor(data$Nature_Ancestor_V)

    ## Assign delta date as ordered factor
    # Delta_Burst_date_child = as.factor(data$Delta_Burst_date_child)
    # Delta_burst_date_child = ordered(Delta_Burst_date_child)

    # Assign date as ordered factor
    level_order = c("7","8","9","10","11","12","1","2","3","4","5","6")
    Burst_Date_Children = ordered(data$Burst_Date_Children, levels = level_order)
    Burst_Date_Children = factor(Burst_Date_Children)


    attach(data)
    summary(data)

    # To make glm for each tree :
    trees = levels(data$tree)
    trees = c(trees, "loaded", "notloaded")



    ##############################################
    #### Vegetative Burst
    ##############################################
    index_trees = list()
    for(name_tree in trees){
        if(name_tree=="loaded"){
            index_trees[[name_tree]] = which(data$Tree_Fruit_Load == 1)
        }
        else if(name_tree=="notloaded"){
            index_trees[[name_tree]] = which(data$Tree_Fruit_Load == 0)
        }
        else{
            index_trees[[name_tree]] = which(data$tree == name_tree)
        }
    }



    ### complete GLM ###
    # For all trees
    complete_glm.vegetative_burst.all = glm( Vegetative_Burst ~ Tree_Fruit_Load + 
                                                                Burst_Date + 
                                                                Position_A + 
                                                                Position_Ancestor_A + 
                                                                Nature_Ancestor_V,
        family = binomial, data=data)
    summary(complete_glm.vegetative_burst.all) # AIC : 1711
        
    # For each tree, loaded trees and not loaded trees
    complete_glm.vegetative_burst.trees = list()
    for(name_tree in trees){
        complete_glm.vegetative_burst.trees[[name_tree]] = glm(Vegetative_Burst ~ Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
            family = binomial, data=data, subset=index_trees[[name_tree]])
    }


    ### selected GLM ###
    # For all trees
    selected_glm.vegetative_burst.all = step(complete_glm.vegetative_burst.all)   
    summary(selected_glm.vegetative_burst.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.vegetative_burst.trees = list()
    for(name_tree in trees){
        selected_glm.vegetative_burst.trees[[name_tree]] = step(complete_glm.vegetative_burst.trees[[name_tree]])
    }


    ##############################################
    #### Has_Lateral_GU_Children
    ##############################################
    index_bursted.all = which(data$Vegetative_Burst == 1)

    index_bursted.trees = list()
    for(name_tree in trees){
        if(name_tree=="loaded"){
            index_bursted.trees[[name_tree]] = which(data$Tree_Fruit_Load == 1 & data$Vegetative_Burst == 1)
        }
        else if(name_tree=="notloaded"){
            index_bursted.trees[[name_tree]] = which(data$Tree_Fruit_Load == 0 & data$Vegetative_Burst == 1)
        }
        else{
            index_bursted.trees[[name_tree]] = which(data$tree == name_tree & data$Vegetative_Burst == 1)
        }
    }


    ### complete GLM ###
        # For all trees
    complete_glm.has_lateral_gu_children.all = glm( Has_Lateral_GU_Children ~ Tree_Fruit_Load + Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
        family = binomial, data=data, subset = index_bursted.all)
    summary(complete_glm.has_lateral_gu_children.all) # AIC : 

        # For each tree, loaded trees and not loaded trees
    complete_glm.has_lateral_gu_children.trees = list()
    for(name_tree in trees){
        complete_glm.has_lateral_gu_children.trees[[name_tree]] = glm(Has_Lateral_GU_Children ~ 
            Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
            family = binomial, data=data, subset=index_bursted.trees[[name_tree]])
    }
 
    ### selected GLM ###
        # For all trees
    selected_glm.has_lateral_gu_children.all = step(complete_glm.has_lateral_gu_children.all)
    summary(selected_glm.has_lateral_gu_children.all) # AIC : 

        # For each tree, loaded trees and not loaded trees
    selected_glm.has_lateral_gu_children.trees = list()
    for(name_tree in trees){
         selected_glm.has_lateral_gu_children.trees[[name_tree]] = step(complete_glm.has_lateral_gu_children.trees[[name_tree]])
    }


        
    ##############################################
    #### Number of lateral GU
    ##############################################
    index_lateral.all = which(data$Vegetative_Burst == 1 & data$Has_Lateral_GU_Children == 1)

    #On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
    #On enlève donc 1 au nombre de latérales afin de commencer à 0.
    ####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
    Nb_lateral_gu = data$Nb_Lateral_GU_Children -1

    index_lateral.trees = list()
    for(name_tree in trees){
        if(name_tree=="loaded"){
            index_lateral.trees[[name_tree]] = which(data$Tree_Fruit_Load == 1 & data$Vegetative_Burst == 1 & data$Has_Lateral_GU_Children == 1)
        }
        else if(name_tree=="notloaded"){
            index_lateral.trees[[name_tree]] = which(data$Tree_Fruit_Load == 0 & data$Vegetative_Burst == 1 & data$Has_Lateral_GU_Children == 1)
        }
        else{
            index_lateral.trees[[name_tree]] = which(data$tree == name_tree & data$Vegetative_Burst == 1 & data$Has_Lateral_GU_Children == 1)
        }
    }


    ### complete GLM ###
    # For all trees
    complete_glm.nb_lateral_gu.all = glm( Nb_lateral_gu  ~ Tree_Fruit_Load + Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
        family = poisson, data=data, subset = index_lateral.all)
    summary(complete_glm.nb_lateral_gu.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.nb_lateral_gu.trees = list()
    for(name_tree in trees){
        complete_glm.nb_lateral_gu.trees[[name_tree]] = glm(Nb_lateral_gu ~ 
            Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
            family = poisson, data=data, subset=index_lateral.trees[[name_tree]])
    }


    ### selected GLM ###
    # For all trees
    selected_glm.nb_lateral_gu.all = step(complete_glm.nb_lateral_gu.all)
    summary(selected_glm.nb_lateral_gu.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.nb_lateral_gu.trees = list()
    for(name_tree in trees){
        selected_glm.nb_lateral_gu.trees[[name_tree]] = step(complete_glm.nb_lateral_gu.trees[[name_tree]])
    }




    ##############################################
    #### Delta burst date of daughters    
    ##############################################
    # detach(data)
    # data = table_INSIDE_for_glm_04_loaded_cogshall
    # data$Burst_Date = as.factor(data$Burst_Date)
    # Delta_Burst_date_child = as.factor(data$Delta_Burst_date_child)
    # Delta_burst_date_child = ordered(Delta_Burst_date_child)
    # attach(data)


    # vglm.Delta_burst_date_child.04_complet = vglm( Delta_burst_date_child ~ Tree_Fruit_Load + Burst_Date + position_mother + position_ancestor + nature_ancestor,
        # family = cumulative(parallel=T) ,data=data, subset = index_bursted)   
    # summary(vglm.Delta_burst_date_child.04_complet)   # Log-likelihood: 



    ##############################################
    #### Burst date of children    
    ##############################################

        
    ### complete GLM ###
    # For all trees  
    complete_vglm.burst_date_children.all = vglm( Burst_Date_Children ~ Tree_Fruit_Load + Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
        family = cumulative(parallel=T) ,data=data, subset = index_bursted.all)
    summary(complete_vglm.burst_date_children.all) # Log-likelihood: 
    get_glm_AIC(complete_vglm.burst_date_children.all) # 

    # For each tree, loaded trees and not loaded trees
    complete_vglm.burst_date_children.tree = list()
    AIC.vglm.burst_date_children.tree = list()
    for(name_tree in trees){
        if(length(index_bursted.trees[[name_tree]])>50){

            complete_vglm.burst_date_children.tree[[name_tree]] = vglm(Burst_Date_Children ~ 
                Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
                family = cumulative(parallel=T), data=data, subset=index_bursted.trees[[name_tree]])
            AIC.vglm.burst_date_children.tree[[name_tree]] = get_glm_AIC(complete_vglm.burst_date_children.tree[[name_tree]])
        }
    }


    ### selected GLM ###
    # For all trees


    # la fonction step ne s applique pas a la classe des vglm
    # ===>> selectioner le model a la main, AIC = 2k - 2ln(L)

    #............ TODO




    ##############################################
    #### Flowering 
    ##############################################
    index_extremity.all = which(data$is_terminal == 1)

    index_extremity.trees = list()
    for(name_tree in trees){
        if(name_tree=="loaded"){
            index_extremity.trees[[name_tree]] = which(data$Tree_Fruit_Load == 1 & data$is_terminal == 1)
        }else if(name_tree=="notloaded"){
            index_extremity.trees[[name_tree]] = which(data$Tree_Fruit_Load == 0 & data$is_terminal == 1)
        }else{
            index_extremity.trees[[name_tree]] = which(data$tree == name_tree & data$is_terminal == 1)
        }
    }
        
    ### complete GLM ###
    # For all trees   
    complete_glm.flowering.all = glm( Flowering ~ Tree_Fruit_Load + Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
        family = binomial, data=data, subset = index_extremity.all)
    summary(complete_glm.flowering.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.flowering.trees = list()
    for(name_tree in trees){
        complete_glm.flowering.trees[[name_tree]] = glm(Flowering ~ 
            Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
            family = binomial, data=data, subset=index_extremity.trees[[name_tree]])
    }
 


    ### selected GLM ###
    # For all trees
    selected_glm.flowering.all = step(complete_glm.flowering.all) 
    summary(selected_glm.flowering.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.flowering.trees = list()
    for(name_tree in trees){
        selected_glm.flowering.trees[[name_tree]] = step(complete_glm.flowering.trees[[name_tree]])
    }



    ##############################################
    #### Number of inflorescences 
    ##############################################
    index_flowering.all = which(data$Flowering == 1)
    Nb_inflo = data$Nb_Inflorescence -1

    index_flowering.trees = list()
    for(name_tree in trees){
        if(name_tree=="loaded"){
            index_flowering.trees[[name_tree]] = which(data$Tree_Fruit_Load == 1 & data$Flowering == 1)
        }else if(name_tree=="notloaded"){
            index_flowering.trees[[name_tree]] = which(data$Tree_Fruit_Load == 0 & data$Flowering == 1)
        }else{
            index_flowering.trees[[name_tree]] = which(data$tree == name_tree & data$Flowering == 1)
        }
    }


    ### complete GLM ###
    # For all trees
    complete_glm.nb_inflorescences.all = glm( Nb_inflo ~ Tree_Fruit_Load + Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
        family = poisson, data=data, subset = index_flowering.all)
    summary(complete_glm.nb_inflorescences.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.nb_inflorescences.trees = list()
    for(name_tree in trees){
        complete_glm.nb_inflorescences.trees[[name_tree]] = glm( Nb_inflo ~
            Burst_Date + Position_A + Position_Ancestor_A + Nature_Ancestor_V,
            family = poisson, data=data, subset=index_flowering.trees[[name_tree]])
    }


    ### selected GLM ###
    # For all trees
    selected_glm.nb_inflorescences.all = step(complete_glm.nb_inflorescences.all) 
    summary(selected_glm.nb_inflorescences.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    selected_glm.nb_inflorescences.trees = list()
    for(name_tree in trees){
        selected_glm.nb_inflorescences.trees[[name_tree]] = step(complete_glm.nb_inflorescences.trees[[name_tree]])
    }


    ##############################################
    #### Date of inflorescences
    ##############################################
    #No enougth data

       
       
       
       

    detach(data)



    ################ Complet
    ####################################
    # loaded as factor
    #___________________________________


    path_complete_glm = paste(output_dir,"complete_glm/",sep="")
    path_complete_glm_all_trees = paste(path_complete_glm,"all_trees/",sep="")


    writing_glm_tables(complete_glm.vegetative_burst.all, 
                       complete_glm.has_lateral_gu_children.all, 
                       complete_glm.nb_lateral_gu.all, 
                       complete_vglm.burst_date_children.all, 
                       complete_glm.flowering.all, 
                       complete_glm.nb_inflorescences.all, 
                       path_complete_glm_all_trees, year)

    for(name_tree in trees){
        path_complete_glm_tree = paste(path_complete_glm,name_tree,"/",sep="")
        writing_glm_tables(complete_glm.vegetative_burst.trees[[name_tree]], 
                           complete_glm.has_lateral_gu_children.trees[[name_tree]], 
                           complete_glm.nb_lateral_gu.trees[[name_tree]], 
                           complete_vglm.burst_date_children.tree[[name_tree]], 
                           complete_glm.flowering.trees[[name_tree]], 
                           complete_glm.nb_inflorescences.trees[[name_tree]], 
                           path_complete_glm_tree, year)
    }

    path_selected_glm = paste(output_dir,"selected_glm/",sep="")
    path_selected_glm_all_trees = paste(path_selected_glm,"all_trees/",sep="")

    writing_glm_tables(selected_glm.vegetative_burst.all, 
                       selected_glm.has_lateral_gu_children.all, 
                       selected_glm.nb_lateral_gu.all, 
                       complete_vglm.burst_date_children.all, 
                       selected_glm.flowering.all, 
                       selected_glm.nb_inflorescences.all, 
                       path_selected_glm_all_trees, year)

    for(name_tree in trees){
        path_selected_glm_tree = paste(path_selected_glm,name_tree,"/",sep="")
        writing_glm_tables(selected_glm.vegetative_burst.trees[[name_tree]], 
                           selected_glm.has_lateral_gu_children.trees[[name_tree]], 
                           selected_glm.nb_lateral_gu.trees[[name_tree]], 
                           complete_vglm.burst_date_children.tree[[name_tree]], 
                           selected_glm.flowering.trees[[name_tree]], 
                           selected_glm.nb_inflorescences.trees[[name_tree]], 
                           path_selected_glm_tree, year)
    }

}

determining_glm_tables_for_year = function(input_dir, year) {
    table_gu_within_cycle = read.csv(paste(input_dir,"table_within_cycle_",year,".csv",sep=""),header = TRUE)
    determining_glm_tables(table_gu_within_cycle, year)
}

determining_glm_tables_for_year(input_dir, "04")
determining_glm_tables_for_year(input_dir, "05")


