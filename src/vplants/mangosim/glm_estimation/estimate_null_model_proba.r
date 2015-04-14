#setwd("~/Develop/vplants/branches/mangosim/src/vplants/mangosim/glm_estimation")
#
#
### Importation of data :

share_dir = '../../../../share/'
input_dir = paste(share_dir,'glm_estimate_input/cogshall/', sep="")
output_dir = paste(share_dir,'glm_output_proba/cogshall/null_glm/', sep="")
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

determine_table_prob_from_glm = function(myglm){
    if( class(myglm)[1]=="vglm" ){
        probability = predictvglm(myglm,type="response")[1,] # on predit sans valeur de x
        delta_months_p = colnames(myglm@y)
        data_probs = t(data.frame(probability))
        colnames(data_probs)= delta_months_p
    }
    else if(myglm$family[1]=="binomial" || myglm$family[1]=="poisson"){  
        probability = predict(myglm,type="response")[1]
        data_probs = data.frame(probability)
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


    ## Assign delta date as ordered factor
    Burst_Delta_Date_Children = as.factor(data$Burst_Delta_Date_Children)
    Burst_Delta_Date_Children = ordered(Burst_Delta_Date_Children)

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
    complete_glm.vegetative_burst.all = glm( Vegetative_Burst ~ 1,
                                             family = binomial, data=data)
    if (verbose >= 3) summary(complete_glm.vegetative_burst.all) # AIC : 1711
        
    # For each tree, loaded trees and not loaded trees
    complete_glm.vegetative_burst.trees = list()
    for(tree_name in trees){
        complete_glm.vegetative_burst.trees[[tree_name]] = glm(Vegetative_Burst ~ 1,
                                                               family = binomial, data=data, subset=index_trees[[tree_name]])
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
    complete_glm.has_apical_gu_child.all = glm( Has_Apical_GU_Child ~ 1,
                                                    family = binomial, data=data, subset = index_bursted.all)
    if (verbose >= 3) summary(complete_glm.has_apical_gu_child.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.has_apical_gu_child.trees = list()
    for(tree_name in trees){
        complete_glm.has_apical_gu_child.trees[[tree_name]] = glm(Has_Apical_GU_Child ~ 1,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
    }



    ##############################################
    #### Has_Lateral_GU_Children
    ##############################################
    if (verbose >= 1) print("Estimate Has_Lateral_GU_Children") 
    ### complete GLM ###
    # For all trees
    complete_glm.has_lateral_gu_children.all = glm( Has_Lateral_GU_Children ~ 1,
                                                    family = binomial, data=data, subset = index_bursted.all)
    if (verbose >= 3) summary(complete_glm.has_lateral_gu_children.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.has_lateral_gu_children.trees = list()
    for(tree_name in trees){
        complete_glm.has_lateral_gu_children.trees[[tree_name]] = glm(Has_Lateral_GU_Children ~ 1,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
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
    complete_glm.nb_lateral_gu.all = glm( Nb_lateral_gu  ~ 1,
        family = poisson, data=data, subset = index_lateral.all)
    if (verbose >= 3) summary(complete_glm.nb_lateral_gu.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.nb_lateral_gu.trees = list()
    for(tree_name in trees){
        complete_glm.nb_lateral_gu.trees[[tree_name]] = glm(Nb_lateral_gu ~ 1,
            family = poisson, data=data, subset=index_lateral.trees[[tree_name]])
    }



    ##############################################
    #### Burst date of children    
    ##############################################
    if (verbose >= 1) print("Estimate Burst date of children") 

        
    ### complete GLM ###
    # For all trees  
    complete_vglm.burst_date_children.all = vglm( Burst_Date_Children ~ 1,
                                                  family = cumulative(parallel=T) ,data=data, subset = index_bursted.all)
    if (verbose >= 3) {
        summary(complete_vglm.burst_date_children.all) # Log-likelihood: 
        print(paste("AIC:",get_vglm_AIC(complete_vglm.burst_date_children.all)))
    }

    # For each tree, loaded trees and not loaded trees
    complete_vglm.burst_date_children.trees = list()
    #AIC.vglm.burst_date_children.tree = list()
    for(tree_name in trees){
        if(length(index_bursted.trees[[tree_name]])>MinNbGUForGLM){

            complete_vglm.burst_date_children.trees[[tree_name]] = vglm(Burst_Date_Children ~ 1,
                                                family = cumulative(parallel=T), data=data, subset=index_bursted.trees[[tree_name]])
            #AIC.vglm.burst_date_children.tree[[tree_name]] = get_vglm_AIC(complete_vglm.burst_date_children.tree[[tree_name]])
        }
    }

    ##############################################
    #### Delta Burst date of children    
    ##############################################
    if (verbose >= 1) print("Estimate Burst delta date of children with vglm") 

        
    ### complete GLM ###
    # For all trees  
    complete_vglm.burst_delta_date_children.all = vglm( Burst_Delta_Date_Children ~ 1,
                                                  family = cumulative(parallel=T) ,data=data, subset = index_bursted.all)
    if (verbose >= 3) {
        summary(complete_vglm.burst_delta_date_children.all) # Log-likelihood: 
        print(paste("AIC:",get_vglm_AIC(complete_vglm.burst_delta_date_children.all)))
    }

    # For each tree, loaded trees and not loaded trees
    complete_vglm.burst_delta_date_children.trees = list()
    #AIC.vglm.burst_delta_date_children.tree = list()
    for(tree_name in trees){
        if(length(index_bursted.trees[[tree_name]])>MinNbGUForGLM){

            complete_vglm.burst_delta_date_children.trees[[tree_name]] = vglm(Burst_Delta_Date_Children ~ 1,
                                                family = cumulative(parallel=T), data=data, subset=index_bursted.trees[[tree_name]])
            #AIC.vglm.burst_date_children.tree[[tree_name]] = get_vglm_AIC(complete_vglm.burst_date_children.tree[[tree_name]])
        }
    }



    ##############################################
    #### Delta Burst date of children  with poisson  
    ##############################################
    if (verbose >= 1) print("Estimate Burst delta date of children with poisson") 


    ### complete GLM ###
    # For all trees
    complete_glm.burst_delta_date_children_poisson.all = glm( Burst_Delta_Date_ChildrenP  ~ 1,
        family = poisson, data=data, subset = index_bursted.all)
    if (verbose >= 3) summary(complete_glm.burst_delta_date_children_poisson.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.burst_delta_date_children_poisson.trees = list()
    for(tree_name in trees){
        complete_glm.burst_delta_date_children_poisson.trees[[tree_name]] = glm(Burst_Delta_Date_ChildrenP ~ 1,
            family = poisson, data=data, subset=index_bursted.trees[[tree_name]])
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
    complete_glm.flowering.all = glm( Flowering ~ 1,
                                family = binomial, data=data, subset = index_extremity.all)
    if (verbose >= 3) summary(complete_glm.flowering.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.flowering.trees = list()
    for(tree_name in trees){
        complete_glm.flowering.trees[[tree_name]] = glm(Flowering ~ 1,
            family = binomial, data=data, subset=index_extremity.trees[[tree_name]])
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
    complete_glm.nb_inflorescences.all = glm( Nb_inflo ~ 1,
        family = poisson, data=data, subset = index_flowering.all)
    if (verbose >= 3) summary(complete_glm.nb_inflorescences.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.nb_inflorescences.trees = list()
    for(tree_name in trees){
        complete_glm.nb_inflorescences.trees[[tree_name]] = glm( Nb_inflo ~ 1,
            family = poisson, data=data, subset=index_flowering.trees[[tree_name]])
    }



    ##############################################
    #### Date of inflorescences
    ##############################################
    if (verbose >= 1) print("Estimate Date of inflorescences") 

    has_flowering_week.all = which(Flowering_Week > 0)
    if(length(has_flowering_week.all) > MinNbGUForGLM){
        complete_vglm.flowering_week.all = vglm( Flowering_Week ~ 1,
            family = cumulative(parallel=T) ,data=data, subset = index_flowering.all)
        if (verbose >= 3) {
            summary(complete_vglm.flowering_week.all) # Log-likelihood: 
            print(paste("AIC:",get_vglm_AIC(complete_vglm.flowering_week.all)))
        } 

        # For each tree, loaded trees and not loaded trees
        complete_vglm.flowering_week.trees = list()
        AIC.vglm.flowering_week.trees = list()
        for(tree_name in trees){
            if(length(index_flowering.trees[[tree_name]]) > MinNbGUForGLM){
                complete_vglm.flowering_week.trees[[tree_name]] = vglm(Flowering_Week ~ 1,
                            family = cumulative(parallel=T), data=data, subset=index_flowering.trees[[tree_name]])
                AIC.vglm.flowering_week.trees[[tree_name]] = get_vglm_AIC(complete_vglm.flowering_week.trees[[tree_name]])
            }
        }
    }
    else {
        print(paste("Not enougth flowering week specified for year",year,":",length(has_flowering_week.all)))
        complete_vglm.flowering_week.all = NULL
        complete_vglm.flowering_week.trees = list()
    }
       
       
       

    detach(data)

    ##############################################
    #### Writing probabilities
    ##############################################


    path_complete_glm_all_trees = paste(output_dir,"all_trees/",sep="")

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
                       path_complete_glm_all_trees, year, verbose >= 2)

    for(tree_name in trees){
        if(verbose >= 1) print(paste("Write complete glm proba tables from tree",tree_name))
        path_complete_glm_tree = paste(output_dir,tree_name,"/",sep="")
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
                           path_complete_glm_tree, year, verbose >= 2)
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
        complete_glm.vegetative_burst.all = glm( Vegetative_Burst ~ 1,
                                                family = binomial, data=data)
    else 
        complete_glm.vegetative_burst.all = glm( Vegetative_Burst ~ 1,
                                                family = binomial, data=data)
    if (verbose >= 3) summary(complete_glm.vegetative_burst.all) # AIC : 1711
        
    # For each tree, loaded trees and not loaded trees
    complete_glm.vegetative_burst.trees = list()
    for(tree_name in trees){
        if (with_burstdate == TRUE)
            complete_glm.vegetative_burst.trees[[tree_name]] = glm(Vegetative_Burst ~ 1,
                                                                   family = binomial, data=data, subset=index_trees[[tree_name]])
        else
            complete_glm.vegetative_burst.trees[[tree_name]] = glm(Vegetative_Burst ~ 1,
                                                                   family = binomial, data=data, subset=index_trees[[tree_name]])
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
        complete_glm.has_apical_gu_child.all = glm( Has_Apical_GU_Child ~ 1,
                                                        family = binomial, data=data, subset = index_bursted.all)
    else
        complete_glm.has_apical_gu_child.all = glm( Has_Apical_GU_Child ~ 1,
                                                        family = binomial, data=data, subset = index_bursted.all)
    
    if (verbose >= 3) summary(complete_glm.has_apical_gu_child.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.has_apical_gu_child.trees = list()
    for(tree_name in trees){
        if (with_burstdate == TRUE)
            complete_glm.has_apical_gu_child.trees[[tree_name]] = glm(Has_Apical_GU_Child ~ 1,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
        else
            complete_glm.has_apical_gu_child.trees[[tree_name]] = glm(Has_Apical_GU_Child ~ 1,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
    }
 

        
    ##############################################
    #### Has_Lateral_GU_Children
    ##############################################
    if (verbose >= 1) print("Estimate Has_Lateral_GU_Children") 

    ### complete GLM ###
    # For all trees
    if (with_burstdate == TRUE)
        complete_glm.has_lateral_gu_children.all = glm( Has_Lateral_GU_Children ~ 1,
                                                        family = binomial, data=data, subset = index_bursted.all)
    else
        complete_glm.has_lateral_gu_children.all = glm( Has_Lateral_GU_Children ~ 1,
                                                        family = binomial, data=data, subset = index_bursted.all)
    
    if (verbose >= 3) summary(complete_glm.has_lateral_gu_children.all) # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.has_lateral_gu_children.trees = list()
    for(tree_name in trees){
        if (with_burstdate == TRUE)
            complete_glm.has_lateral_gu_children.trees[[tree_name]] = glm(Has_Lateral_GU_Children ~ 1,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
        else
            complete_glm.has_lateral_gu_children.trees[[tree_name]] = glm(Has_Lateral_GU_Children ~ 1,
                                                    family = binomial, data=data, subset=index_bursted.trees[[tree_name]])
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
        complete_glm.nb_lateral_gu.all = glm( Nb_lateral_gu  ~ 1,
                     family = poisson, data=data, subset = index_lateral.all)
    else
        complete_glm.nb_lateral_gu.all = glm( Nb_lateral_gu  ~ 1,
                     family = poisson, data=data, subset = index_lateral.all)

    if (verbose >= 3) summary(complete_glm.nb_lateral_gu.all)  # AIC : 

    # For each tree, loaded trees and not loaded trees
    complete_glm.nb_lateral_gu.trees = list()
    for(tree_name in trees){
        if (with_burstdate == TRUE)
            complete_glm.nb_lateral_gu.trees[[tree_name]] = glm(Nb_lateral_gu ~ 1,
                family = poisson, data=data, subset=index_lateral.trees[[tree_name]])
        else
            complete_glm.nb_lateral_gu.trees[[tree_name]] = glm(Nb_lateral_gu ~ 1,
                family = poisson, data=data, subset=index_lateral.trees[[tree_name]])
    }




    ##############################################
    #### Burst date of children    
    ##############################################
    if (verbose >= 1) print("Estimate Burst date of children") 

        
    ### complete GLM ###
    # For all trees  
    if (with_burstdate == TRUE)
        complete_vglm.burst_date_children.all = vglm( Burst_Date_Children ~ 1,
                                                      family = cumulative(parallel=T) ,data=data, subset = index_bursted.all)
    else
        complete_vglm.burst_date_children.all = vglm( Burst_Date_Children ~ 1,
                                                      family = cumulative(parallel=T) ,data=data, subset = index_bursted.all)

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
                complete_vglm.burst_date_children.tree[[tree_name]] = vglm(Burst_Date_Children ~ 1,
                                                        family = cumulative(parallel=T), data=data, subset=index_bursted.trees[[tree_name]])
            else
                complete_vglm.burst_date_children.tree[[tree_name]] = vglm(Burst_Date_Children ~ 1,
                                                        family = cumulative(parallel=T), data=data, subset=index_bursted.trees[[tree_name]])
            #AIC.vglm.burst_date_children.tree[[tree_name]] = get_vglm_AIC(complete_vglm.burst_date_children.tree[[tree_name]])
        }
    }




    detach(data)

    ##############################################
    #### Writing probabilities
    ##############################################

    path_complete_glm_all_trees = paste(output_dir,"all_trees/",sep="")

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
                       path_complete_glm_all_trees, year, verbose >= 2)

    for(tree_name in trees){
        if(verbose >= 1) print(paste("Write complete glm proba tables from tree",tree_name))
        path_complete_glm_tree = paste(output_dir,tree_name,"/",sep="")
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
                           path_complete_glm_tree, year, verbose >= 2)
    }


}

determining_glm_tables_between_cycle_for_year = function(input_dir, year, with_burstdate = FALSE, verbose = 0) {
    table_gu_within_cycle = read.csv(paste(input_dir,"table_between_cycle_",year,".csv",sep=""),header = TRUE)
    summary(table_gu_within_cycle)
    determining_glm_tables_between_cycle(table_gu_within_cycle, year,with_burstdate, verbose)
}

print("start")
verbose = 1
determining_glm_tables_within_cycle_for_year(input_dir, "04", verbose)
determining_glm_tables_within_cycle_for_year(input_dir, "05", verbose)

determining_glm_tables_between_cycle_for_year(input_dir, "03to0405", FALSE, verbose)
determining_glm_tables_between_cycle_for_year(input_dir, "04to05", TRUE, verbose)

