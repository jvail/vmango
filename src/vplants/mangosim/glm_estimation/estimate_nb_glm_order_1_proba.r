# setwd("~/Develop/vplants/branches/mangosim/src/vplants/mangosim/glm_estimation")
#
#
### Import of data :

localdir = getSrcDirectory(function(x) {x})
print(localdir)
setwd(localdir)

share_dir = '../../../../share/'
input_dir = paste(share_dir,'glm_estimate_input/cogshall/', sep="")
output_dir = paste(share_dir,'glm_output_proba2/cogshall/', sep="")
if (file.exists(output_dir)){
    dir.create(output_dir,recursive=TRUE)
}

source("util_glm.r")




vsummary_output = function(glm, data, outfile, subset = NULL, verbose = FALSE){

  out = file(outfile,open="wt")
  
  split = FALSE #(verbose >=3)
  sink(file = out, split = split)
  
  print("******** DATA *************")
  if(length(subset) > 0)
    ndata = data[subset,]
  else ndata = data
  print(summary(ndata))
  
  print("******** GLM  *************")
  print(paste("Formula :",glm.formula.text(glm),sep=''))
  print(summary(glm))

  
  if (is.vglm(glm)){
    print("******** Likelihood  *************")
    print(glm@criterion$loglikelihood)
    print(paste('Estimated',vglm.logLik(glm, ndata)))
  }

  sink()
  close(out)
}


generate_glm = function(variable, family, data, subset, year, verbose, factors = c("Burst_Date","Position_A","Position_Ancestor_A","Nature_Ancestor_F"))
{
    tracestep = 0
    # if (verbose >= 3) tracestep = 0

    path_complete_glm = paste(output_dir,"complete_glm/",sep="")
    path_selected_glm = paste(output_dir,"selected_glm/",sep="")

    if (file.exists(path_complete_glm) == FALSE) dir.create(path_complete_glm,recursive=TRUE)
    if (file.exists(path_selected_glm) == FALSE) dir.create(path_selected_glm,recursive=TRUE)

    fname = tolower(variable)

    formula = as.formula(paste(variable," ~ ", paste(factors,collapse=" + "),sep=""))
    
    #print("complete glm")
    complete_glm.all = glm( formula , family = family, data = data, subset = subset)

    #print("complete glm summary")
    vsummary_output(complete_glm.all, data, paste(path_complete_glm,fname,"_",year,"_summary.txt",sep=""), subset = subset, verbose)

    #print("complete glm proba")
    proba.complete_glm.all = glm.proba_and_counts(complete_glm.all, data, subset)

    #print("complete glm proba writting")
    write.csv(proba.complete_glm.all,file=paste(path_complete_glm, fname, "_", year,".csv", sep=""), row.names = FALSE)

    ### selected GLM ###
    #print("selected glm")
    selected_glm.all = step(complete_glm.all, trace = tracestep)   

    #print("selected glm summary")
    vsummary_output(selected_glm.all, data, paste(path_selected_glm,fname,"_",year,"_summary.txt",sep=""), subset, verbose)

    #print("selected glm proba")
    proba.selected_glm.all = glm.proba_and_counts(selected_glm.all, data, subset)

    #print("selected glm proba writting")
    write.csv(proba.selected_glm.all,file=paste(path_selected_glm, fname, "_", year,".csv", sep=""), row.names = FALSE)

}



generate_vglm = function(variable, data, subset, year, verbose, factors = c("Burst_Date","Position_A","Position_Ancestor_A","Nature_Ancestor_F"))
{
    tracestep = 0
    # if (verbose >= 3) tracestep = 0

    path_complete_glm = paste(output_dir,"complete_glm/",sep="")
    path_selected_glm = paste(output_dir,"selected_glm/",sep="")

    if (file.exists(path_complete_glm) == FALSE) dir.create(path_complete_glm,recursive=TRUE)
    if (file.exists(path_selected_glm) == FALSE) dir.create(path_selected_glm,recursive=TRUE)

    fname = tolower(variable)

    formula = as.formula(paste(variable," ~ ", paste(factors,collapse=" + "),sep=""))
    
    
    ndata = data[subset,]
    
    print("complete glm")
    complete_glm.all = vglm( formula , family = cumulative(parallel=TRUE), data = ndata)

    print("complete glm proba")    
    res = vglm.proba_and_counts (complete_glm.all, ndata)
    proba.complete_glm.all = res[[1]]
    nbelement.complete_glm.all = res[[2]]

    print("complete glm proba writting")
    write.csv(proba.complete_glm.all,file=paste(path_complete_glm, fname, "_", year,".csv", sep=""), row.names = FALSE)
    write.csv(nbelement.complete_glm.all,file=paste(path_complete_glm, fname, "_", year,"_nbelements.csv", sep=""), row.names = FALSE)
    #write.ftable(nbelement.complete_glm.all,file=paste(path_complete_glm, fname, "_", year,"_nbelements.csv", sep=""))

    print("complete glm summary")
    vsummary_output(complete_glm.all, ndata, paste(path_complete_glm,fname,"_",year,"_summary.txt",sep=""), verbose=verbose)
    
    ### selected GLM ###
    
    # la fonction step ne s applique pas a la classe des vglm
    # ===>> selectioner le model a la main, AIC = 2k - 2ln(L)

    print("selected glm")
    #selected_glm.all = complete_glm.all
    selected_glm.all = vglm.step(complete_glm.all, data = ndata)

    print("selected glm proba")
    res = vglm.proba_and_counts (selected_glm.all, ndata)
    proba.selected_glm.all = res[[1]]
    nbelement.selected_glm.all = res[[2]]
    
    print("selected glm proba writting")
    write.csv(proba.selected_glm.all,file=paste(path_selected_glm, fname, "_", year,".csv", sep=""), row.names = FALSE)
    write.csv(nbelement.selected_glm.all,file=paste(path_selected_glm, fname, "_", year,"_nbelements.csv", sep=""), row.names = FALSE)

    print("selected glm summary")
    vsummary_output(selected_glm.all, ndata,  paste(path_selected_glm,fname,"_",year,"_summary.txt",sep=""), verbose=verbose)
    
}

determining_glm_tables_within_cycle = function(data, year, verbose = 0) {


    year = paste("within_",year,sep="")

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

    
    #attach(data)
    summary(data)


    if (verbose >= 1) print(paste("Cycle ",year)) 

    ##############################################
    #### Vegetative Burst
    ##############################################
    if (verbose >= 1) print("Estimate Vegetative Burst") 
    index_loaded.all = which(data$Tree_Fruit_Load == 1)

    generate_glm("Vegetative_Burst", family = binomial, data=data, subset= index_loaded.all, year= year, verbose = verbose)


    ##############################################
    #### Has_Apical_GU_Child
    ##############################################
    if (verbose >= 1) print("Estimate Has_Apical_GU_Child") 
    index_bursted.all = which(data$Tree_Fruit_Load == 1 & data$Vegetative_Burst == 1)

    generate_glm("Has_Apical_GU_Child", family = binomial, data=data, subset= index_bursted.all, year= year, verbose = verbose)


    ##############################################
    #### Has_Lateral_GU_Children
    ##############################################
    if (verbose >= 1) print("Estimate Has_Lateral_GU_Children") 


    generate_glm("Has_Lateral_GU_Children", family = binomial, data=data, subset= index_bursted.all, year= year, verbose = verbose)

        
    ##############################################
    #### Number of lateral GU
    ##############################################
    if (verbose >= 1) print("Estimate Number of lateral GU") 
    index_lateral.all = which(data$Tree_Fruit_Load == 1 & data$Vegetative_Burst == 1 & data$Has_Lateral_GU_Children == 1)

    #On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
    #On enlève donc 1 au nombre de latérales afin de commencer à 0.
    ####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
    data$Nb_Lateral_GU_Children = data$Nb_Lateral_GU_Children -1

    generate_glm("Nb_Lateral_GU_Children", family = poisson, data=data, subset= index_lateral.all, year= year, verbose = verbose)


    ##############################################
    #### Burst date of children    
    ##############################################
    if (verbose >= 1) print("Estimate Burst date of children") 


    generate_vglm("Burst_Date_Children",  data=data, subset= index_bursted.all, year= year, verbose = verbose)
        


    ##############################################
    #### Delta Burst date of children    
    ##############################################
    if (verbose >= 1) print("Estimate Burst delta date of children with vglm") 

    generate_vglm("Burst_Delta_Date_Children",  data=data, subset= index_bursted.all, year= year, verbose = verbose)


    ##############################################
    #### Delta Burst date of children  with poisson  
    ##############################################
    if (verbose >= 1) print("Estimate Burst delta date of children with poisson") 

    data$Burst_Delta_Date_Children_Poisson = data$Burst_Delta_Date_Children - 1

    generate_glm("Burst_Delta_Date_Children_Poisson", family = poisson, data=data, subset= index_bursted.all, year= year, verbose = verbose)


    ##############################################
    #### Flowering 
    ##############################################
    if (verbose >= 1) print("Estimate Flowering") 
    index_extremity.all = which(data$Tree_Fruit_Load == 1 & data$is_terminal == 1)

    generate_glm("Flowering", family = binomial, data=data, subset= index_extremity.all, year= year, verbose = verbose)


    ##############################################
    #### Number of inflorescences 
    ##############################################
    if (verbose >= 1) print("Estimate Number of inflorescences") 
    index_flowering.all = which(data$Tree_Fruit_Load == 1 & data$Flowering == 1)
    data$Nb_Inflorescences = data$Nb_Inflorescence -1

    generate_glm("Nb_Inflorescences", family = poisson, data=data, subset= index_flowering.all, year= year, verbose = verbose)


    ##############################################
    #### Date of inflorescences
    ##############################################
    if (verbose >= 1) print("Estimate Date of inflorescences") 

    has_flowering_week.all = which(data$Flowering_Week > 0)
    if(length(has_flowering_week.all) > MinNbGUForGLM){
        generate_vglm("Flowering_Week",  data=data, subset= index_flowering.all, year= year, verbose = verbose)

    }
    else {
        print(paste("Not enougth flowering week specified for year",year,":",length(has_flowering_week.all)))
    }
       
       
    ##############################################
    #### Fruiting 
    ##############################################
    if (verbose >= 1) print("Estimate Fruiting") 
    generate_glm("Fruiting", family = binomial, data=data, subset= index_flowering.all, year= year, verbose = verbose)


   ##############################################
    #### Number of fruits 
    ##############################################
    if (verbose >= 1) print("Estimate Number of fruits") 
    index_fruiting.all = which(data$Tree_Fruit_Load == 1 & data$Fruiting == 1)
    data$Nb_Fruits = data$Nb_Fruits -1

    generate_glm("Nb_Fruits", family = poisson, data=data, subset= index_fruiting.all, year= year, verbose = verbose)



    #detach(data)


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
verbose = 3
determining_glm_tables_within_cycle_for_year(input_dir, "04", verbose)
#determining_glm_tables_within_cycle_for_year(input_dir, "05", verbose)

#determining_glm_tables_between_cycle_for_year(input_dir, "03to0405", FALSE, verbose)
#determining_glm_tables_between_cycle_for_year(input_dir, "04to05", TRUE, verbose)




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

