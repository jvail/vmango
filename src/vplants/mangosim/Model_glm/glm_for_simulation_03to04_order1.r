setwd("D:/openalea/mangosim/src/vplants/mangosim/Model_glm")
#
#
### Importation of data :

share_dir = '../../../../share/'

table_TRANSITION_for_glm_03to04_cogshall = read.csv(paste(share_dir,"model_glm/table_TRANSITION_03to04_cogshall.csv",sep=""),header = TRUE)

##################################################################################################
data.03_04 = table_TRANSITION_for_glm_03to04_cogshall

# Assign covariables as factors
data.03_04$is_loaded = as.factor(data.03_04$is_loaded)
data.03_04$position_mother_L = as.factor(data.03_04$position_mother_L)
data.03_04$nature_mother_V = as.factor(data.03_04$nature_mother_V)
# Assign date as ordered factor
level_order = c("7","8","9","10","11","12","1","2","3","4","5","6")
data.03_04$Burst_date_child = ordered(data.03_04$Burst_date_child, levels = level_order)
data.03_04$Burst_date_child = factor(data.03_04$Burst_date_child)

attach(data.03_04)
summary(data.03_04)

# To make glm for each tree :
trees = levels(data.03_04$tree)
trees = c(trees, "loaded", "notloaded")



##############################################
#### Burst
##############################################
index_trees.03_04 = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees.03_04[[name_tree]] = which(data.03_04$is_loaded == 1)
    }else if(name_tree=="notloaded"){
        index_trees.03_04[[name_tree]] = which(data.03_04$is_loaded == 0)
    }else{
        index_trees.03_04[[name_tree]] = which(data.03_04$tree == name_tree)
    }
}

#__________________________________________
## GLM null : 
#__________________________________________
    # For all trees
glm.burst.03_04_null = glm( Burst ~ 1,
    family = binomial, data=data.03_04)
summary(glm.burst.03_04_null)
# AIC: 613.85
    

    # For each tree, loaded trees and not loaded trees
list_glm.burst.03_04_null_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.burst.03_04_null_tree[[name_tree]] = glm(Burst ~ 1,
        family = binomial, data=data.03_04, subset=index_trees.03_04[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 


#__________________________________________
## GLM complet : 
#__________________________________________
    # For all trees
glm.burst.03_04_complet = glm( Burst ~ is_loaded + position_mother_L + nature_mother_V,
    family = binomial, data=data.03_04)
summary(glm.burst.03_04_complet) # AIC : 
    
    # For each tree, loaded trees and not loaded trees
list_glm.burst.03_04_complet_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.burst.03_04_complet_tree[[name_tree]] = glm(Burst ~  position_mother_L + nature_mother_V,
        family = binomial, data=data.03_04, subset=index_trees.03_04[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 


#__________________________________________
## GLM selected : 
#__________________________________________
    # Fore all trees
step.glm.burst.03_04 = step(glm.burst.03_04_complet)   # Burst ~ 
summary(step.glm.burst.03_04) # AIC : 

    # For each tree, loaded trees and not loaded trees
list_step.glm.burst.03_04_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_step.glm.burst.03_04_tree[[name_tree]] = step(list_glm.burst.03_04_complet_tree[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 



##############################################
#### Lateral GU daughter
##############################################
index_bursted.03_04 = which(data.03_04$Burst == 1)

index_trees_bursted.03_04 = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees_bursted.03_04[[name_tree]] = which(data.03_04$is_loaded == 1 & data.03_04$Burst == 1)
    }else if(name_tree=="notloaded"){
        index_trees_bursted.03_04[[name_tree]] = which(data.03_04$is_loaded == 0 & data.03_04$Burst == 1)
    }else{
        index_trees_bursted.03_04[[name_tree]] = which(data.03_04$tree == name_tree & data.03_04$Burst == 1)
    }
}

#__________________________________________
## GLM null :
#__________________________________________ 
    # For all trees
glm.Lateral_GU_daughter.03_04_null = glm( Lateral_GU_daughter ~ 1,
    family = binomial, data=data.03_04, subset=index_bursted.03_04)
summary(glm.Lateral_GU_daughter.03_04_null) # AIC: 1180.7

    # For each tree, loaded trees and not loaded trees
list_glm.Lateral_GU_daughter.03_04_null_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.Lateral_GU_daughter.03_04_null_tree[[name_tree]] = glm(Lateral_GU_daughter ~ 1,
        family = binomial, data=data.03_04, subset=index_trees_bursted.03_04[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC:

#__________________________________________
## GLM complet : 
#__________________________________________
    # For all trees
glm.Lateral_GU_daughter.03_04_complet = glm( Lateral_GU_daughter ~ is_loaded + position_mother_L + nature_mother_V,
    family = binomial, data=data.03_04, subset = index_bursted.03_04)
summary(glm.Lateral_GU_daughter.03_04_complet) # AIC : 

    # For each tree, loaded trees and not loaded trees
list_glm.Lateral_GU_daughter.03_04_complet_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.Lateral_GU_daughter.03_04_complet_tree[[name_tree]] = glm(Lateral_GU_daughter ~ 
        position_mother_L + nature_mother_V,
        family = binomial, data=data.03_04, subset=index_trees_bursted.03_04[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC:

#__________________________________________
## GLM selected : 
#__________________________________________
    # Fore all trees
step.glm.Lateral_GU_daughter.03_04 = step(glm.Lateral_GU_daughter.03_04_complet) # Lateral_GU_daughter ~ 
summary(step.glm.Lateral_GU_daughter.03_04) # AIC : 

    # For each tree, loaded trees and not loaded trees
list_step.glm.Lateral_GU_daughter.03_04_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_step.glm.Lateral_GU_daughter.03_04_tree[[name_tree]] = step(list_glm.Lateral_GU_daughter.03_04_complet_tree[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC:

##############################################
#### Number of lateral GU
##############################################
index_lateral.03_04 = which(data.03_04$Burst == 1 & data.03_04$Lateral_GU_daughter == 1)
#On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
#On enlève donc 1 au nombre de latérales afin de commencer à 0.
####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
No_lateral_gu = data.03_04$No_Lateral_GU -1

index_trees_bursted_lateral.03_04 = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees_bursted_lateral.03_04[[name_tree]] = which(data.03_04$is_loaded == 1 & data.03_04$Burst == 1 & data.03_04$Lateral_GU_daughter == 1)
    }else if(name_tree=="notloaded"){
        index_trees_bursted_lateral.03_04[[name_tree]] = which(data.03_04$is_loaded == 0 & data.03_04$Burst == 1 & data.03_04$Lateral_GU_daughter == 1)
    }else{
        index_trees_bursted_lateral.03_04[[name_tree]] = which(data.03_04$tree == name_tree & data.03_04$Burst == 1 & data.03_04$Lateral_GU_daughter == 1)
    }
}

#__________________________________________
## GLM null : 
#__________________________________________
    # For all trees
glm.no_lateral_GU.03_04_null = glm( No_lateral_gu ~ 1,
    family = poisson, data=data.03_04, subset = index_lateral.03_04)
summary(glm.no_lateral_GU.03_04_null) # AIC:
    
    # For each tree, loaded trees and not loaded trees
list_glm.no_lateral_GU.03_04_null_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.no_lateral_GU.03_04_null_tree[[name_tree]] = glm( No_lateral_gu ~ 1,
        family = poisson, data=data.03_04, subset=index_trees_bursted_lateral.03_04[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC:


#__________________________________________
## GLM complet : 
#__________________________________________
    # For all trees
glm.no_lateral_GU.03_04_complet = glm( No_lateral_gu  ~ is_loaded + position_mother_L + nature_mother_V,
    family = poisson, data=data.03_04, subset = index_lateral.03_04)
summary(glm.no_lateral_GU.03_04_complet)  # AIC : 

    # For each tree, loaded trees and not loaded trees
list_glm.no_lateral_GU.03_04_complet_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.no_lateral_GU.03_04_complet_tree[[name_tree]] = glm(No_lateral_gu ~ 
        position_mother_L + nature_mother_V,
        family = poisson, data=data.03_04, subset=index_trees_bursted_lateral.03_04[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC:


#__________________________________________
## GLM selected : 
#__________________________________________
    # Fore all trees
step.glm.no_lateral_GU.03_04 = step(glm.no_lateral_GU.03_04_complet) # No_lateral_gu  ~ 
summary(step.glm.no_lateral_GU.03_04)  # AIC : 

    # For each tree, loaded trees and not loaded trees
list_step.glm.no_lateral_GU.03_04_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_step.glm.no_lateral_GU.03_04_tree[[name_tree]] = step(list_glm.no_lateral_GU.03_04_complet_tree[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC:


##############################################
#### Burst date of daughters    
##############################################
library(VGAM)

# k : nb de parametre du model, L : la vraisemblance, AIC = 2k - 2ln(L)
get_AIC = function(myvglm){
    k = myvglm@rank
    logL = myvglm@criterion$loglikelihood
    AIC = 2*k - 2*logL
return(AIC)
}

#__________________________________________
## GLM null : 
#__________________________________________
    # For all trees
vglm.Burst_date_child.03_04_null = vglm( Burst_date_child ~ 1,
    family = cumulative(parallel=T) ,data=data.03_04, subset = index_bursted.03_04)
summary(vglm.Burst_date_child.03_04_null)  # Log-likelihood: 
get_AIC(vglm.Burst_date_child.03_04_null)   # AIC : 

    # For each tree, loaded trees and not loaded trees
list_vglm.Burst_date_child.03_04_null_tree = list()
list_AIC_vglm.Burst_date_child.03_04_null_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_vglm.Burst_date_child.03_04_null_tree[[name_tree]] = vglm( Burst_date_child ~ 1,
        family = cumulative(parallel=T), data=data.03_04, subset=index_trees_bursted.03_04[[name_tree]])
    list_AIC_vglm.Burst_date_child.03_04_null_tree[[name_tree]] = get_AIC(list_vglm.Burst_date_child.03_04_null_tree[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC:


#__________________________________________
## GLM complet : 
#__________________________________________
    # For all trees  
vglm.Burst_date_child.03_04_complet = vglm( Burst_date_child ~ is_loaded + position_mother_L + nature_mother_V,
    family = cumulative(parallel=T) ,data=data.03_04, subset = index_bursted.03_04)
summary(vglm.Burst_date_child.03_04_complet) # Log-likelihood: -892.3279
get_AIC(vglm.Burst_date_child.03_04_complet) # 1818.7

    # For each tree, loaded trees and not loaded trees
list_vglm.Burst_date_child.03_04_complet_tree = list()
list_AIC_vglm.Burst_date_child.03_04_complet_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_vglm.Burst_date_child.03_04_complet_tree[[name_tree]] = vglm(Burst_date_child ~ 
        position_mother_L + nature_mother_V,
        family = cumulative(parallel=T), data=data.03_04, subset=index_trees_bursted.03_04[[name_tree]])
        list_AIC_vglm.Burst_date_child.03_04_complet_tree[[name_tree]] = get_AIC(list_vglm.Burst_date_child.03_04_complet_tree[[name_tree]])
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 


#__________________________________________
## GLM selected : 
#__________________________________________
    # Fore all trees


# la fonction step ne s applique pas a la classe des vglm
# ===>> selectioner le model a la main, AIC = 2k - 2ln(L)

#............ TODO


detach(data.03_04)

level_is_loaded = as.factor(0:1)
level_position_mother_L = as.factor(0:1)
level_position_ancestor_L = as.factor(0:1)
level_nature_ancestor_V = as.factor(0:1)
level_nature_mother_V = as.factor(0:1)
level_all_burst_date_mother = c("03-05","03-06","03-07","03-08","03-09","03-10","03-11","03-12","04-01","04-02","04-03","04-04","04-05","04-06","04-07")
get_table_prob_variable_glm = function(myglm){
    if( class(myglm)[1]=="vglm" ){
        if(length(myglm@xlevels)>0){
            level_burst_date_mother = myglm@xlevels$burst_date_mother
            if(is.null(level_burst_date_mother)){
                level_burst_date_mother = level_all_burst_date_mother
            }
            variables = slot(myglm@terms[[1]],"term.labels")
        }else{
            variables = NULL
            level_burst_date_mother = level_all_burst_date_mother
        }
    }else{
        if(!is.null(myglm$xlevels)){
            level_burst_date_mother = myglm$xlevels$burst_date_mother
            if(is.null(level_burst_date_mother)){
                level_burst_date_mother = level_all_burst_date_mother
            }
            variables = colnames(myglm$model)[2:length(colnames(myglm$model))]
        }else{
            variables = NULL
            level_burst_date_mother = level_all_burst_date_mother
        }
    }
    produit_cartesien = expand.grid(level_is_loaded,level_burst_date_mother,level_position_mother_L,level_position_ancestor_L,level_nature_ancestor_V,level_nature_mother_V)
    names(produit_cartesien) = c("is_loaded","burst_date_mother","position_mother_L","position_ancestor_L","nature_ancestor_V","nature_mother_V")
    data_probs = unique(produit_cartesien[variables])
    if( class(myglm)[1]=="vglm" ){
        if(!is.null(variables)){
            probs = predictvglm(myglm,newdata=data_probs,type="response")
            for(i in 1:length(colnames(probs)) ){
                data_probs[colnames(probs)[i] ] = probs[,i]
            }
        }else{
            probs = predictvglm(myglm,type="response")[1,]
            months_p = colnames(myglm@y)
            data_probs = data.frame(probs[1])
            row.names(data_probs) = NULL
            for(i in 2:length(months_p) ){
                data_probs = cbind(data_probs,probs[i])
            }
            colnames(data_probs)= months_p
        }
    }else if(myglm$family[1]=="binomial" || myglm$family[1]=="poisson"){  
        if(!is.null(variables)){
            probs = predict(myglm,newdata=data_probs,type="response")
            data_probs["probas"]=probs
        }else{
            probs = predict(myglm,type="response")[1]
            data_probs = data.frame(probs)
        }
    }
    if("burst_date_mother" %in% variables){
        other_level_burst_date_mother = level_all_burst_date_mother[!level_all_burst_date_mother %in% level_burst_date_mother]
        if(length(other_level_burst_date_mother)>0){
            other_produit_cartesien = expand.grid(level_is_loaded, other_level_burst_date_mother,level_position_mother_L,level_position_ancestor_L,level_nature_ancestor_V,level_nature_mother_V)
            names(other_produit_cartesien) = c("is_loaded","burst_date_mother","position_mother_L","position_ancestor_L","nature_ancestor_V","nature_mother_V")
            other_data_probs = unique(other_produit_cartesien[variables])
            probs_null = rep(0,length(other_data_probs[,1]))
            if( class(myglm)[1]=="vglm"){
                for(i in 1:length(colnames(probs)) ){
                    other_data_probs[colnames(probs)[i] ] = probs_null
                }
            }else{
                other_data_probs$probas = probs_null
            }
            data_probs = rbind(data_probs,other_data_probs)
        }
    }
return(data_probs)
}

################ Null
####################################
# loaded as factor
#___________________________________
table_prob_glm.burst.03_04_null = get_table_prob_variable_glm(glm.burst.03_04_null)
write.csv(table_prob_glm.burst.03_04_null,file=paste(share_dir,"model_glm/glm_null/loaded_as_factor/table_prob_glm_burst_03_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.03_04_null = get_table_prob_variable_glm(glm.Lateral_GU_daughter.03_04_null)
write.csv(table_prob_glm.Lateral_GU_daughter.03_04_null,file=paste(share_dir,"model_glm/glm_null/loaded_as_factor/table_prob_glm_Lateral_GU_daughter_03_04.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.03_04_null = get_table_prob_variable_glm(glm.no_lateral_GU.03_04_null)
write.csv(table_prob_glm.no_lateral_GU.03_04_null,file=paste(share_dir,"model_glm/glm_null/loaded_as_factor/table_prob_glm_no_lateral_GU_03_04.csv",sep=""), row.names = FALSE)
table_prob_vglm.Burst_date_child.03_04_null = get_table_prob_variable_glm(vglm.Burst_date_child.03_04_null)
write.csv(table_prob_vglm.Burst_date_child.03_04_null,file=paste(share_dir,"model_glm/glm_null/loaded_as_factor/table_prob_vglm_Burst_date_child_03_04.csv",sep=""), row.names = FALSE)


# by_tree
#___________________________________
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    path_file = paste(share_dir,"model_glm/glm_null/by_tree/",sep="")
    path_file_tree = paste(path_file,name_tree,sep="")
    
    table_prob_glm.burst.03_04_null_tree = get_table_prob_variable_glm(list_glm.burst.03_04_null_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_burst_03_04.csv",sep="")
    write.csv(table_prob_glm.burst.03_04_null_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Lateral_GU_daughter.03_04_null_tree = get_table_prob_variable_glm(list_glm.Lateral_GU_daughter.03_04_null_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_Lateral_GU_daughter_03_04.csv",sep="")
    write.csv(table_prob_glm.Lateral_GU_daughter.03_04_null_tree,file=path_final, row.names = FALSE)

    table_prob_glm.no_lateral_GU.03_04_null_tree = get_table_prob_variable_glm(list_glm.no_lateral_GU.03_04_null_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_no_lateral_GU_03_04.csv",sep="")
    write.csv(table_prob_glm.no_lateral_GU.03_04_null_tree,file=path_final, row.names = FALSE)

    table_prob_vglm.Burst_date_child.03_04_null_tree = get_table_prob_variable_glm(list_vglm.Burst_date_child.03_04_null_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_vglm_Burst_date_child_03_04.csv",sep="")
    write.csv(table_prob_vglm.Burst_date_child.03_04_null_tree,file=path_final, row.names = FALSE)
}


################ Complet
####################################
# loaded as factor
#___________________________________
table_prob_glm.burst.03_04_complet = get_table_prob_variable_glm(glm.burst.03_04_complet)
write.csv(table_prob_glm.burst.03_04_complet,file=paste(share_dir,"model_glm/glm_complet/loaded_as_factor/table_prob_glm_burst_03_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.03_04_complet = get_table_prob_variable_glm(glm.Lateral_GU_daughter.03_04_complet)
write.csv(table_prob_glm.Lateral_GU_daughter.03_04_complet,file=paste(share_dir,"model_glm/glm_complet/loaded_as_factor/table_prob_glm_Lateral_GU_daughter_03_04.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.03_04_complet = get_table_prob_variable_glm(glm.no_lateral_GU.03_04_complet)
write.csv(table_prob_glm.no_lateral_GU.03_04_complet,file=paste(share_dir,"model_glm/glm_complet/loaded_as_factor/table_prob_glm_no_lateral_GU_03_04.csv",sep=""), row.names = FALSE)
table_prob_vglm.Burst_date_child.03_04_complet = get_table_prob_variable_glm(vglm.Burst_date_child.03_04_complet)
write.csv(table_prob_vglm.Burst_date_child.03_04_complet,file=paste(share_dir,"model_glm/glm_complet/loaded_as_factor/table_prob_vglm_Burst_date_child_03_04.csv",sep=""), row.names = FALSE)


# by_tree
#___________________________________
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    path_file = paste(share_dir,"model_glm/glm_complet/by_tree/",sep="")
    path_file_tree = paste(path_file,name_tree,sep="")
    
    table_prob_glm.burst.03_04_complet_tree = get_table_prob_variable_glm(list_glm.burst.03_04_complet_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_burst_03_04.csv",sep="")
    write.csv(table_prob_glm.burst.03_04_complet_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Lateral_GU_daughter.03_04_complet_tree = get_table_prob_variable_glm(list_glm.Lateral_GU_daughter.03_04_complet_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_Lateral_GU_daughter_03_04.csv",sep="")
    write.csv(table_prob_glm.Lateral_GU_daughter.03_04_complet_tree,file=path_final, row.names = FALSE)

    table_prob_glm.no_lateral_GU.03_04_complet_tree = get_table_prob_variable_glm(list_glm.no_lateral_GU.03_04_complet_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_no_lateral_GU_03_04.csv",sep="")
    write.csv(table_prob_glm.no_lateral_GU.03_04_complet_tree,file=path_final, row.names = FALSE)

    table_prob_vglm.Burst_date_child.03_04_complet_tree = get_table_prob_variable_glm(list_vglm.Burst_date_child.03_04_complet_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_vglm_Burst_date_child_03_04.csv",sep="")
    write.csv(table_prob_vglm.Burst_date_child.03_04_complet_tree,file=path_final, row.names = FALSE)
}

################ Selected
####################################
# loaded as factor
#___________________________________
table_prob_glm.burst.03_04_selected = get_table_prob_variable_glm(step.glm.burst.03_04)
write.csv(table_prob_glm.burst.03_04_selected,file=paste(share_dir,"model_glm/glm_selected/loaded_as_factor/table_prob_glm_burst_03_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.03_04_selected = get_table_prob_variable_glm(step.glm.Lateral_GU_daughter.03_04)
write.csv(table_prob_glm.Lateral_GU_daughter.03_04_selected,file=paste(share_dir,"model_glm/glm_selected/loaded_as_factor/table_prob_glm_Lateral_GU_daughter_03_04.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.03_04_selected = get_table_prob_variable_glm(step.glm.no_lateral_GU.03_04)
write.csv(table_prob_glm.no_lateral_GU.03_04_selected,file=paste(share_dir,"model_glm/glm_selected/loaded_as_factor/table_prob_glm_no_lateral_GU_03_04.csv",sep=""), row.names = FALSE)
#table_prob_vglm.Burst_date_child.03_04_selected = get_table_prob_variable_glm(step.vglm.Burst_date_child.03_04)
#write.csv(table_prob_vglm.Burst_date_child.03_04_selected,file=paste(share_dir,"model_glm/glm_selected/loaded_as_factor/table_prob_vglm_Burst_date_child_03_04.csv",sep=""), row.names = FALSE)


# by_tree
#___________________________________
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    path_file = paste(share_dir,"model_glm/glm_selected/by_tree/",sep="")
    path_file_tree = paste(path_file,name_tree,sep="")
    
    table_prob_glm.burst.03_04_selected_tree = get_table_prob_variable_glm(list_step.glm.burst.03_04_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_burst_03_04.csv",sep="")
    write.csv(table_prob_glm.burst.03_04_selected_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Lateral_GU_daughter.03_04_selected_tree = get_table_prob_variable_glm(list_step.glm.Lateral_GU_daughter.03_04_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_Lateral_GU_daughter_03_04.csv",sep="")
    write.csv(table_prob_glm.Lateral_GU_daughter.03_04_selected_tree,file=path_final, row.names = FALSE)

    table_prob_glm.no_lateral_GU.03_04_selected_tree = get_table_prob_variable_glm(list_step.glm.no_lateral_GU.03_04_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_no_lateral_GU_03_04.csv",sep="")
    write.csv(table_prob_glm.no_lateral_GU.03_04_selected_tree,file=path_final, row.names = FALSE)

    #table_prob_vglm.Burst_date_child.03_04_selected_tree = get_table_prob_variable_glm(list_step.vglm.Burst_date_child.03_04_tree[[name_tree]])
    #path_final = paste(path_file_tree,"/table_prob_vglm_Burst_date_child_03_04.csv",sep="")
    #write.csv(table_prob_vglm.Burst_date_child.03_04_selected_tree,file=path_final, row.names = FALSE)
}

