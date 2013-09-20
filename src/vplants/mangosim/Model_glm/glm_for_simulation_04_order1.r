setwd("D:/openalea/mangosim/src/vplants/mangosim/Model_glm")
#
#
### Importation of data :

share_dir = '../../../../share/'
table_INSIDE_for_glm_04_loaded_cogshall = read.csv(paste(share_dir,"model_glm/table_INSIDE_04_cogshall.csv",sep=""),header = TRUE)

#################################################
#################################################
data.04 = table_INSIDE_for_glm_04_loaded_cogshall
# Removing of GU born in a month of less than 4 GUs born in this month
months = levels(data.04$burst_date_mother)
for( month in 1:length(months)){
    nb_month = which(data.04$burst_date_mother==months[month])
    if(0 < length(nb_month) & length(nb_month) <= 4){
        data.04 = data.04[-nb_month,]
    }
}
# Assign covariables as factors
data.04$burst_date_mother=as.factor(as.character(data.04$burst_date_mother))
data.04$is_loaded = as.factor(data.04$is_loaded)
data.04$position_ancestor_L = as.factor(data.04$position_ancestor_L)
data.04$position_mother_L = as.factor(data.04$position_mother_L)
data.04$nature_ancestor_V = as.factor(data.04$nature_ancestor_V)
## Assign delta date as ordered factor
# Delta_Burst_date_child = as.factor(data.04$Delta_Burst_date_child)
# Delta_burst_date_child = ordered(Delta_Burst_date_child)

# Assign date as ordered factor
Date_Burst_daughter = as.factor(data.04$Date_burst_daughter)
Date_Burst_daughter = ordered(Date_Burst_daughter)

attach(data.04)
summary(data.04)

# To make glm for each tree :
trees = levels(data.04$tree)
trees = c(trees, "loaded", "notloaded")



##############################################
#### Burst
##############################################
index_trees.04 = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees.04[[name_tree]] = which(data.04$is_loaded == 1)
    }else if(name_tree=="notloaded"){
        index_trees.04[[name_tree]] = which(data.04$is_loaded == 0)
    }else{
        index_trees.04[[name_tree]] = which(data.04$tree == name_tree)
    }
}

#__________________________________________
## GLM null : 
#__________________________________________
    # For all trees
glm.burst.04_null = glm( Burst ~ 1,
    family = binomial, data=data.04)
summary(glm.burst.04_null)  # AIC: 
    

    # For each tree, loaded trees and not loaded trees
list_glm.burst.04_null_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.burst.04_null_tree[[name_tree]] = glm(Burst ~ 1,
        family = binomial, data=data.04, subset=index_trees.04[[name_tree]])
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
glm.burst.04_complet = glm( Burst ~ is_loaded + burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
    family = binomial, data=data.04)
summary(glm.burst.04_complet) # AIC : 
    
    # For each tree, loaded trees and not loaded trees
list_glm.burst.04_complet_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.burst.04_complet_tree[[name_tree]] = glm(Burst ~ burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
        family = binomial, data=data.04, subset=index_trees.04[[name_tree]])
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
step.glm.burst.04 = step(glm.burst.04_complet)   # Burst ~ 
summary(step.glm.burst.04) # AIC : 

    # For each tree, loaded trees and not loaded trees
list_step.glm.burst.04_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_step.glm.burst.04_tree[[name_tree]] = step(list_glm.burst.04_complet_tree[[name_tree]])
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notlaoded: AIC: 


##############################################
#### Lateral GU daughter
##############################################
index_bursted.04 = which(data.04$Burst == 1)

index_trees_bursted.04 = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees_bursted.04[[name_tree]] = which(data.04$is_loaded == 1 & data.04$Burst == 1)
    }else if(name_tree=="notloaded"){
        index_trees_bursted.04[[name_tree]] = which(data.04$is_loaded == 0 & data.04$Burst == 1)
    }else{
        index_trees_bursted.04[[name_tree]] = which(data.04$tree == name_tree & data.04$Burst == 1)
    }
}

#__________________________________________
## GLM null :
#__________________________________________ 
    # For all trees
glm.Lateral_GU_daughter.04_null = glm( Lateral_GU_daughter ~ 1,
    family = binomial, data=data.04)
summary(glm.Lateral_GU_daughter.04_null) # AIC: 

    # For each tree, loaded trees and not loaded trees
list_glm.Lateral_GU_daughter.04_null_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.Lateral_GU_daughter.04_null_tree[[name_tree]] = glm(Lateral_GU_daughter ~ 1,
        family = binomial, data=data.04, subset=index_trees_bursted.04[[name_tree]])
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
glm.Lateral_GU_daughter.04_complet = glm( Lateral_GU_daughter ~ is_loaded + burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
    family = binomial, data=data.04, subset = index_bursted.04)
summary(glm.Lateral_GU_daughter.04_complet) # AIC : 

    # For each tree, loaded trees and not loaded trees
list_glm.Lateral_GU_daughter.04_complet_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.Lateral_GU_daughter.04_complet_tree[[name_tree]] = glm(Lateral_GU_daughter ~ 
        burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
        family = binomial, data=data.04, subset=index_trees_bursted.04[[name_tree]])
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
step.glm.Lateral_GU_daughter.04 = step(glm.Lateral_GU_daughter.04_complet) # Lateral_GU_daughter ~ is_loaded + burst_date_mother + position_mother_L
summary(step.glm.Lateral_GU_daughter.04) # AIC : 

    # For each tree, loaded trees and not loaded trees
list_step.glm.Lateral_GU_daughter.04_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_step.glm.Lateral_GU_daughter.04_tree[[name_tree]] = step(list_glm.Lateral_GU_daughter.04_complet_tree[[name_tree]])
}
# B10 :     
# B12 :     
# B14 :     
# F2 :      
# F6 :      
# loaded :  AIC: 
#notlaoded: AIC: 


    
##############################################
#### Number of lateral GU
##############################################
index_lateral.04 = which(data.04$Burst == 1 & data.04$Lateral_GU_daughter == 1)
#On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
#On enlève donc 1 au nombre de latérales afin de commencer à 0.
####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
No_lateral_gu = data.04$No_Lateral_GU -1

index_trees_bursted_lateral.04 = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees_bursted_lateral.04[[name_tree]] = which(data.04$is_loaded == 1 & data.04$Burst == 1 & data.04$Lateral_GU_daughter == 1)
    }else if(name_tree=="notloaded"){
        index_trees_bursted_lateral.04[[name_tree]] = which(data.04$is_loaded == 0 & data.04$Burst == 1 & data.04$Lateral_GU_daughter == 1)
    }else{
        index_trees_bursted_lateral.04[[name_tree]] = which(data.04$tree == name_tree & data.04$Burst == 1 & data.04$Lateral_GU_daughter == 1)
    }
}

#__________________________________________
## GLM null : 
#__________________________________________
    # For all trees
glm.no_lateral_GU.04_null = glm( No_lateral_gu ~ 1,
    family = poisson, data=data.04, subset = index_lateral.04)
summary(glm.no_lateral_GU.04_null) # AIC: 
    
    # For each tree, loaded trees and not loaded trees
list_glm.no_lateral_GU.04_null_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.no_lateral_GU.04_null_tree[[name_tree]] = glm( No_lateral_gu ~ 1,
        family = poisson, data=data.04, subset=index_trees_bursted_lateral.04[[name_tree]])
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
glm.no_lateral_GU.04_complet = glm( No_lateral_gu  ~ is_loaded + burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
    family = poisson, data=data.04, subset = index_lateral.04)
summary(glm.no_lateral_GU.04_complet)  # AIC : 

    # For each tree, loaded trees and not loaded trees
list_glm.no_lateral_GU.04_complet_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.no_lateral_GU.04_complet_tree[[name_tree]] = glm(No_lateral_gu ~ 
        burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
        family = poisson, data=data.04, subset=index_trees_bursted_lateral.04[[name_tree]])
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
step.glm.no_lateral_GU.04 = step(glm.no_lateral_GU.04_complet) # No_lateral_gu  ~ is_loaded + position_mother_L + position_ancestor_L + nature_ancestor_V
summary(step.glm.no_lateral_GU.04)  # AIC : 

    # For each tree, loaded trees and not loaded trees
list_step.glm.no_lateral_GU.04_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_step.glm.no_lateral_GU.04_tree[[name_tree]] = step(list_glm.no_lateral_GU.04_complet_tree[[name_tree]])
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 207.5  
# loaded :  AIC: 388.3  
#notlaoded: AIC: 549    




##############################################
#### Delta burst date of daughters    
##############################################
# detach(data.04)
# data.04 = table_INSIDE_for_glm_04_loaded_cogshall
# data.04$burst_date_mother = as.factor(data.04$burst_date_mother)
# Delta_Burst_date_child = as.factor(data.04$Delta_Burst_date_child)
# Delta_burst_date_child = ordered(Delta_Burst_date_child)
# attach(data.04)

library(VGAM)

vglm.Delta_burst_date_child.04_complet = vglm( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = cumulative(parallel=T) ,data=data.04, subset = index_bursted.04)   
summary(vglm.Delta_burst_date_child.04_complet)   # Log-likelihood: 



##############################################
#### Burst date of daughters    
##############################################
# detach(data.04)
# data.04 = table_INSIDE_for_glm_04_loaded_cogshall
# data.04$burst_date_mother = as.factor(data.04$burst_date_mother)
# Date_Burst_daughter = as.factor(data.04$Date_burst_daughter)
# Date_Burst_daughter = ordered(Date_Burst_daughter)
# attach(data.04)

# k : nb de parametre du model, L : la vraisemblance, AIC = 2k - 2ln(L)
get_AIC = function(myglm){
    k = myglm@rank
    logL = myglm@criterion$loglikelihood
    AIC = 2*k - 2*logL
return(AIC)
}

#__________________________________________
## GLM null : 
#__________________________________________
    # For all trees
vglm.Date_burst_daughter.04_null = vglm( Date_Burst_daughter ~ 1,
    family = cumulative(parallel=T) ,data=data.04, subset = index_bursted.04)
summary(vglm.Date_burst_daughter.04_null)  # Log-likelihood: 
get_AIC(vglm.Date_burst_daughter.04_null)   # AIC : 

    # For each tree, loaded trees and not loaded trees
list_vglm.Date_burst_daughter.04_null_tree = list()
list_AIC_vglm.Date_burst_daughter.04_null_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_vglm.Date_burst_daughter.04_null_tree[[name_tree]] = vglm( Date_Burst_daughter ~ 1,
        family = cumulative(parallel=T), data=data.04, subset=index_trees_bursted.04[[name_tree]])
    list_AIC_vglm.Date_burst_daughter.04_null_tree[[name_tree]] = get_AIC(list_vglm.Date_burst_daughter.04_null_tree[[name_tree]])
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
vglm.Date_burst_daughter.04_complet = vglm( Date_Burst_daughter ~ is_loaded + burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
    family = cumulative(parallel=T) ,data=data.04, subset = index_bursted.04)
summary(vglm.Date_burst_daughter.04_complet) # Log-likelihood: 
get_AIC(vglm.Date_burst_daughter.04_complet) # 

    # For each tree, loaded trees and not loaded trees
list_vglm.Date_burst_daughter.04_complet_tree = list()
list_AIC_vglm.Date_burst_daughter.04_complet_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(length(index_trees_bursted.04[[name_tree]])>50){
        # list_ind_poor_in_month = c()
        # for( month in 1:length(months)){
            # nb_month = which(data.04$burst_date_mother==months[month] & data.04$tree==name_tree)
            # if(0 < length(nb_month) & length(nb_month) <= 4){
                # list_ind_poor_in_month = c(list_ind_poor_in_month,nb_month)
            # }
        # }
        # index_trees_bursted.04[[name_tree]] = which()
        list_vglm.Date_burst_daughter.04_complet_tree[[name_tree]] = vglm(Date_Burst_daughter ~ 
            burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
            family = cumulative(parallel=T), data=data.04, subset=index_trees_bursted.04[[name_tree]])
        list_AIC_vglm.Date_burst_daughter.04_complet_tree[[name_tree]] = get_AIC(list_vglm.Date_burst_daughter.04_complet_tree[[name_tree]])
    }
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      no enougth data (only 23)
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




##############################################
#### Flowering 
##############################################
index_extremity.04 = which(data.04$is_in_extremity == 1)

index_trees_extremity.04 = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees_extremity.04[[name_tree]] = which(data.04$is_loaded == 1 & data.04$is_in_extremity == 1)
    }else if(name_tree=="notloaded"){
        index_trees_extremity.04[[name_tree]] = which(data.04$is_loaded == 0 & data.04$is_in_extremity == 1)
    }else{
        index_trees_extremity.04[[name_tree]] = which(data.04$tree == name_tree & data.04$is_in_extremity == 1)
    }
}

#__________________________________________
## GLM null :
#__________________________________________ 
    # For all trees
glm.Flowering.04_null = glm( Flowering ~ 1,
    family = binomial, data=data.04, subset = index_extremity.04)
summary(glm.Flowering.04_null) # AIC: 

    # For each tree, loaded trees and not loaded trees
list_glm.Flowering.04_null_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.Flowering.04_null_tree[[name_tree]] = glm( Flowering ~ 1,
        family = binomial, data=data.04, subset=index_trees_extremity.04[[name_tree]])
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
glm.Flowering.04_complet = glm( Flowering ~ is_loaded + burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
    family = binomial, data=data.04, subset = index_extremity.04)
summary(glm.Flowering.04_complet) # AIC : 

    # For each tree, loaded trees and not loaded trees
list_glm.Flowering.04_complet_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.Flowering.04_complet_tree[[name_tree]] = glm(Flowering ~ 
        burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
        family = binomial, data=data.04, subset=index_trees_extremity.04[[name_tree]])
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
step.glm.Flowering.04 = step(glm.Flowering.04_complet) # Flowering ~ is_loaded + burst_date_mother + position_mother_L + nature_ancestor_V
summary(step.glm.Flowering.04)  # AIC : 

    # For each tree, loaded trees and not loaded trees
list_step.glm.Flowering.04_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_step.glm.Flowering.04_tree[[name_tree]] = step(list_glm.Flowering.04_complet_tree[[name_tree]])
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 


##############################################
#### Number of inflorescences 
##############################################
index_flowering.04 = which(data.04$Flowering == 1)
No_inflo = data.04$No_inflorescences -1

index_trees_flowering.04 = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees_flowering.04[[name_tree]] = which(data.04$is_loaded == 1 & data.04$Flowering == 1)
    }else if(name_tree=="notloaded"){
        index_trees_flowering.04[[name_tree]] = which(data.04$is_loaded == 0 & data.04$Flowering == 1)
    }else{
        index_trees_flowering.04[[name_tree]] = which(data.04$tree == name_tree & data.04$Flowering == 1)
    }
}

#__________________________________________
## GLM null : 
#__________________________________________
    # For all trees
glm.No_inflorescences.04_null = glm( No_inflo ~ 1,
    family = poisson, data=data.04, subset = index_flowering.04)
summary(glm.No_inflorescences.04_null) # AIC: 
    
    # For each tree, loaded trees and not loaded trees
list_glm.No_inflorescences.04_null_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.No_inflorescences.04_null_tree[[paste("glm.No_inflorescences.04_null_",name_tree, sep="")]] = glm( No_inflo ~ 1,
        family = poisson, data=data.04, subset=index_trees_flowering.04[[name_tree]])
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
glm.No_inflorescences.04_complet = glm( No_inflo ~ is_loaded + burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
    family = poisson, data=data.04, subset = index_flowering.04)
summary(glm.No_inflorescences.04_complet)  # AIC : 

    # For each tree, loaded trees and not loaded trees
list_glm.No_inflorescences.04_complet_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.No_inflorescences.04_complet_tree[[name_tree]] = glm( No_inflo ~
        burst_date_mother + position_mother_L + position_ancestor_L + nature_ancestor_V,
        family = poisson, data=data.04, subset=index_trees_flowering.04[[name_tree]])
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
step.glm.No_inflorescences.04 = step(glm.No_inflorescences.04_complet) # No_inflo ~ is_loaded + position_mother_L + nature_ancestor_V
summary(step.glm.No_inflorescences.04)  # AIC : 

    # For each tree, loaded trees and not loaded trees
list_step.glm.No_inflorescences.04_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_step.glm.No_inflorescences.04_tree[[name_tree]] = step(list_glm.No_inflorescences.04_complet_tree[[name_tree]])
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 

##############################################
#### Date of inflorescences
##############################################
#No enougth data

   
   
   
   

detach(data.04)


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
table_prob_glm.burst.04_null = get_table_prob_variable_glm(glm.burst.04_null)
write.csv(table_prob_glm.burst.04_null,file=paste(share_dir,"model_glm/glm_null/loaded_as_factor/table_prob_glm_burst_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.04_null = get_table_prob_variable_glm(glm.Lateral_GU_daughter.04_null)
write.csv(table_prob_glm.Lateral_GU_daughter.04_null,file=paste(share_dir,"model_glm/glm_null/loaded_as_factor/table_prob_glm_Lateral_GU_daughter_04.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.04_null = get_table_prob_variable_glm(glm.no_lateral_GU.04_null)
write.csv(table_prob_glm.no_lateral_GU.04_null,file=paste(share_dir,"model_glm/glm_null/loaded_as_factor/table_prob_glm_no_lateral_GU_04.csv",sep=""), row.names = FALSE)
table_prob_vglm.Date_burst_daughter.04_null = get_table_prob_variable_glm(vglm.Date_burst_daughter.04_null)
write.csv(table_prob_vglm.Date_burst_daughter.04_null,file=paste(share_dir,"model_glm/glm_null/loaded_as_factor/table_prob_vglm_Date_burst_daughter_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Flowering.04_null = get_table_prob_variable_glm(glm.Flowering.04_null)
write.csv(table_prob_glm.Flowering.04_null,file=paste(share_dir,"model_glm/glm_null/loaded_as_factor/table_prob_glm_Flowering_04.csv",sep=""), row.names = FALSE)
table_prob_glm.No_inflorescences.04_null = get_table_prob_variable_glm(glm.No_inflorescences.04_null)
write.csv(table_prob_glm.No_inflorescences.04_null,file=paste(share_dir,"model_glm/glm_null/loaded_as_factor/table_prob_glm_No_inflorescences_04.csv",sep=""), row.names = FALSE)


# by_tree
#___________________________________
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    path_file = paste(share_dir,"model_glm/glm_null/by_tree/",sep="")
    path_file_tree = paste(path_file,name_tree,sep="")
    
    table_prob_glm.burst.04_null_tree = get_table_prob_variable_glm(list_glm.burst.04_null_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_burst_04.csv",sep="")
    write.csv(table_prob_glm.burst.04_null_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Lateral_GU_daughter.04_null_tree = get_table_prob_variable_glm(list_glm.Lateral_GU_daughter.04_null_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_Lateral_GU_daughter_04.csv",sep="")
    write.csv(table_prob_glm.Lateral_GU_daughter.04_null_tree,file=path_final, row.names = FALSE)

    table_prob_glm.no_lateral_GU.04_null_tree = get_table_prob_variable_glm(list_glm.no_lateral_GU.04_null_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_no_lateral_GU_04.csv",sep="")
    write.csv(table_prob_glm.no_lateral_GU.04_null_tree,file=path_final, row.names = FALSE)

    table_prob_vglm.Date_burst_daughter.04_null_tree = get_table_prob_variable_glm(list_vglm.Date_burst_daughter.04_null_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_vglm_Date_burst_daughter_04.csv",sep="")
    write.csv(table_prob_vglm.Date_burst_daughter.04_null_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Flowering.04_null = get_table_prob_variable_glm(glm.Flowering.04_null)
    path_final = paste(path_file_tree,"/table_prob_glm_Flowering_04.csv",sep="")
    write.csv(table_prob_glm.Flowering.04_null,file=path_final, row.names = FALSE)
    
    table_prob_glm.No_inflorescences.04_null = get_table_prob_variable_glm(glm.No_inflorescences.04_null)
    path_final = paste(path_file_tree,"/table_prob_glm_No_inflorescences_04.csv",sep="")
    write.csv(table_prob_glm.No_inflorescences.04_null,file=path_final, row.names = FALSE)
}


################ Complet
####################################
# loaded as factor
#___________________________________
table_prob_glm.burst.04_complet = get_table_prob_variable_glm(glm.burst.04_complet)
write.csv(table_prob_glm.burst.04_complet,file=paste(share_dir,"model_glm/glm_complet/loaded_as_factor/table_prob_glm_burst_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.04_complet = get_table_prob_variable_glm(glm.Lateral_GU_daughter.04_complet)
write.csv(table_prob_glm.Lateral_GU_daughter.04_complet,file=paste(share_dir,"model_glm/glm_complet/loaded_as_factor/table_prob_glm_Lateral_GU_daughter_04.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.04_complet = get_table_prob_variable_glm(glm.no_lateral_GU.04_complet)
write.csv(table_prob_glm.no_lateral_GU.04_complet,file=paste(share_dir,"model_glm/glm_complet/loaded_as_factor/table_prob_glm_no_lateral_GU_04.csv",sep=""), row.names = FALSE)
table_prob_vglm.Date_burst_daughter.04_complet = get_table_prob_variable_glm(vglm.Date_burst_daughter.04_complet)
write.csv(table_prob_vglm.Date_burst_daughter.04_complet,file=paste(share_dir,"model_glm/glm_complet/loaded_as_factor/table_prob_vglm_Date_burst_daughter_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Flowering.04_complet = get_table_prob_variable_glm(glm.Flowering.04_complet)
write.csv(table_prob_glm.Flowering.04_complet,file=paste(share_dir,"model_glm/glm_complet/loaded_as_factor/table_prob_glm_Flowering_04.csv",sep=""), row.names = FALSE)
table_prob_glm.No_inflorescences.04_complet = get_table_prob_variable_glm(glm.No_inflorescences.04_complet)
write.csv(table_prob_glm.No_inflorescences.04_complet,file=paste(share_dir,"model_glm/glm_complet/loaded_as_factor/table_prob_glm_No_inflorescences_04.csv",sep=""), row.names = FALSE)


# by_tree
#___________________________________
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    path_file = paste(share_dir,"model_glm/glm_complet/by_tree/",sep="")
    path_file_tree = paste(path_file,name_tree,sep="")
    
    table_prob_glm.burst.04_complet_tree = get_table_prob_variable_glm(list_glm.burst.04_complet_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_burst_04.csv",sep="")
    write.csv(table_prob_glm.burst.04_complet_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Lateral_GU_daughter.04_complet_tree = get_table_prob_variable_glm(list_glm.Lateral_GU_daughter.04_complet_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_Lateral_GU_daughter_04.csv",sep="")
    write.csv(table_prob_glm.Lateral_GU_daughter.04_complet_tree,file=path_final, row.names = FALSE)

    table_prob_glm.no_lateral_GU.04_complet_tree = get_table_prob_variable_glm(list_glm.no_lateral_GU.04_complet_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_no_lateral_GU_04.csv",sep="")
    write.csv(table_prob_glm.no_lateral_GU.04_complet_tree,file=path_final, row.names = FALSE)

    if( !is.null(list_vglm.Date_burst_daughter.04_complet_tree[[name_tree]]) ){
        table_prob_vglm.Date_burst_daughter.04_complet_tree = get_table_prob_variable_glm(list_vglm.Date_burst_daughter.04_complet_tree[[name_tree]])
        path_final = paste(path_file_tree,"/table_prob_vglm_Date_burst_daughter_04.csv",sep="")
        write.csv(table_prob_vglm.Date_burst_daughter.04_complet_tree,file=path_final, row.names = FALSE)
    }
    
    table_prob_glm.Flowering.04_complet = get_table_prob_variable_glm(glm.Flowering.04_complet)
    path_final = paste(path_file_tree,"/table_prob_glm_Flowering_04.csv",sep="")
    write.csv(table_prob_glm.Flowering.04_complet,file=path_final, row.names = FALSE)
    
    table_prob_glm.No_inflorescences.04_complet = get_table_prob_variable_glm(glm.No_inflorescences.04_complet)
    path_final = paste(path_file_tree,"/table_prob_glm_No_inflorescences_04.csv",sep="")
    write.csv(table_prob_glm.No_inflorescences.04_complet,file=path_final, row.names = FALSE)
}

################ Selected
####################################
# loaded as factor
#___________________________________
table_prob_glm.burst.04_selected = get_table_prob_variable_glm(step.glm.burst.04)
write.csv(table_prob_glm.burst.04_selected,file=paste(share_dir,"model_glm/glm_selected/loaded_as_factor/table_prob_glm_burst_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.04_selected = get_table_prob_variable_glm(step.glm.Lateral_GU_daughter.04)
write.csv(table_prob_glm.Lateral_GU_daughter.04_selected,file=paste(share_dir,"model_glm/glm_selected/loaded_as_factor/table_prob_glm_Lateral_GU_daughter_04.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.04_selected = get_table_prob_variable_glm(step.glm.no_lateral_GU.04)
write.csv(table_prob_glm.no_lateral_GU.04_selected,file=paste(share_dir,"model_glm/glm_selected/loaded_as_factor/table_prob_glm_no_lateral_GU_04.csv",sep=""), row.names = FALSE)
#table_prob_vglm.Date_burst_daughter.04_selected = get_table_prob_variable_glm(step.vglm.Date_burst_daughter.04)
#write.csv(table_prob_vglm.Date_burst_daughter.04_selected,file=paste(share_dir,"model_glm/glm_selected/loaded_as_factor/table_prob_vglm_Date_burst_daughter_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Flowering.04_selected = get_table_prob_variable_glm(step.glm.Flowering.04)
write.csv(table_prob_glm.Flowering.04_selected,file=paste(share_dir,"model_glm/glm_selected/loaded_as_factor/table_prob_glm_Flowering_04.csv",sep=""), row.names = FALSE)
table_prob_glm.No_inflorescences.04_selected = get_table_prob_variable_glm(step.glm.No_inflorescences.04)
write.csv(table_prob_glm.No_inflorescences.04_selected,file=paste(share_dir,"model_glm/glm_selected/loaded_as_factor/table_prob_glm_No_inflorescences_04.csv",sep=""), row.names = FALSE)


# by_tree
#___________________________________
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    path_file = paste(share_dir,"model_glm/glm_selected/by_tree/",sep="")
    path_file_tree = paste(path_file,name_tree,sep="")
    
    table_prob_glm.burst.04_selected_tree = get_table_prob_variable_glm(list_step.glm.burst.04_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_burst_04.csv",sep="")
    write.csv(table_prob_glm.burst.04_selected_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Lateral_GU_daughter.04_selected_tree = get_table_prob_variable_glm(list_step.glm.Lateral_GU_daughter.04_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_Lateral_GU_daughter_04.csv",sep="")
    write.csv(table_prob_glm.Lateral_GU_daughter.04_selected_tree,file=path_final, row.names = FALSE)

    table_prob_glm.no_lateral_GU.04_selected_tree = get_table_prob_variable_glm(list_step.glm.no_lateral_GU.04_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_no_lateral_GU_04.csv",sep="")
    write.csv(table_prob_glm.no_lateral_GU.04_selected_tree,file=path_final, row.names = FALSE)

    # if( !is.null(list_step.vglm.Date_burst_daughter.04_tree[[name_tree]]) ){
        # table_prob_vglm.Date_burst_daughter.04_selected_tree = get_table_prob_variable_glm(list_step.vglm.Date_burst_daughter.04_tree[[name_tree]])
        # path_final = paste(path_file_tree,"/table_prob_vglm_Date_burst_daughter_04.csv",sep="")
        # write.csv(table_prob_vglm.Date_burst_daughter.04_selected_tree,file=path_final, row.names = FALSE)
    # }
    
    table_prob_glm.Flowering.04_selected = get_table_prob_variable_glm(step.glm.Flowering.04)
    path_final = paste(path_file_tree,"/table_prob_glm_Flowering_04.csv",sep="")
    write.csv(table_prob_glm.Flowering.04_selected,file=path_final, row.names = FALSE)
    
    table_prob_glm.No_inflorescences.04_selected = get_table_prob_variable_glm(step.glm.No_inflorescences.04)
    path_final = paste(path_file_tree,"/table_prob_glm_No_inflorescences_04.csv",sep="")
    write.csv(table_prob_glm.No_inflorescences.04_selected,file=path_final, row.names = FALSE)
}

