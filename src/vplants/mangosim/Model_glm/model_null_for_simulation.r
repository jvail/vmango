setwd("D:/openalea/mangosim/src/vplants/mangosim/Model_glm")
#
#
### Importation of data :

share_dir = '../../../../share/'
table_for_nul_model_cogshall = read.csv(paste(share_dir,"model_glm/table_for_nul_model_cogshall.csv",sep=""),header = TRUE)

#################################################
#################################################
Data = table_for_nul_model_cogshall


# Assign delta date as ordered factor
Delta_burst_date_child = as.factor(Data$Delta_Burst_date_child)
Delta_burst_date_child = ordered(Delta_burst_date_child)

# Assign date as ordered factor
Date_Burst_daughter = as.factor(Data$Date_burst_daughter)
Date_Burst_daughter = ordered(Date_Burst_daughter)

# Assign date of flowering as ordered factor
Flowering_date = as.factor(Data$Flowering_Date)
Flowering_date = ordered(Flowering_date)

attach(Data)
summary(Data)

# To make model for each tree :
trees = levels(Data$tree)
trees = c(trees, "loaded", "notloaded")
# indices for cycle
index_04 = which(Data$cycle==4)
index_05 = which(Data$cycle==5)

##############################################
#### Burst
##############################################
index_trees = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees[[name_tree]] = which(Data$is_loaded == 1)
    }else if(name_tree=="notloaded"){
        index_trees[[name_tree]] = which(Data$is_loaded == 0)
    }else{
        index_trees[[name_tree]] = which(Data$tree == name_tree)
    }
}

#__________________________________________
## For cycle 4 :
#__________________________________________
    # For all trees
glm.burst.04_nul = glm( Burst ~ 1,
    family = binomial, data=Data, subset=index_04 )
summary(glm.burst.04_nul)  # AIC: 
    

    # For each tree, loaded trees and not loaded trees
list_glm.burst.04_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.burst.04_nul_tree[[name_tree]] = glm(Burst ~ 1,
        family = binomial, data=Data, subset=intersect(index_trees[[name_tree]], index_04) )
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC:  
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 


#__________________________________________
## For cycle 5 :
#__________________________________________
    # For all trees
glm.burst.05_nul = glm( Burst ~ 1,
    family = binomial, data=Data)
summary(glm.burst.05_nul)  # AIC:
    

    # For each tree, loaded trees and not loaded trees
list_glm.burst.05_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.burst.05_nul_tree[[name_tree]] = glm(Burst ~ 1,
        family = binomial, data=Data, subset=intersect(index_trees[[name_tree]],index_05) )
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
index_bursted = which(Data$Burst == 1)

index_trees_bursted = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees_bursted[[name_tree]] = which(Data$is_loaded == 1 & Data$Burst == 1)
    }else if(name_tree=="notloaded"){
        index_trees_bursted[[name_tree]] = which(Data$is_loaded == 0 & Data$Burst == 1)
    }else{
        index_trees_bursted[[name_tree]] = which(Data$tree == name_tree & Data$Burst == 1)
    }
}

#__________________________________________
## For cycle 4 :
#__________________________________________ 
    # For all trees
glm.Lateral_GU_daughter.04_nul = glm( Lateral_GU_daughter ~ 1,
    family = binomial, data=Data, subset=intersect(index_bursted,index_04) )
summary(glm.Lateral_GU_daughter.04_nul) # AIC: 

    # For each tree, loaded trees and not loaded trees
list_glm.Lateral_GU_daughter.04_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.Lateral_GU_daughter.04_nul_tree[[name_tree]] = glm(Lateral_GU_daughter ~ 1,
        family = binomial, data=Data, subset=intersect( index_trees_bursted[[name_tree]], index_04) )
}
# B10 :     AIC:  
# B12 :     AIC: 
# B14 :     AIC:  
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC:  
#notloaded: AIC:   


#__________________________________________
## For cycle 5 :
#__________________________________________ 
    # For all trees
glm.Lateral_GU_daughter.05_nul = glm( Lateral_GU_daughter ~ 1,
    family = binomial, data=Data, subset=intersect(index_bursted,index_05) )
summary(glm.Lateral_GU_daughter.05_nul) # AIC: 

    # For each tree, loaded trees and not loaded trees
list_glm.Lateral_GU_daughter.05_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.Lateral_GU_daughter.05_nul_tree[[name_tree]] = glm(Lateral_GU_daughter ~ 1,
        family = binomial, data=Data, subset = intersect(index_trees_bursted[[name_tree]], index_05) )
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
index_lateral = which(Data$Burst == 1 & Data$Lateral_GU_daughter == 1)
#On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
#On enlève donc 1 au nombre de latérales afin de commencer à 0.
####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
No_lateral_gu = Data$No_Lateral_GU -1

index_trees_bursted_lateral = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees_bursted_lateral[[name_tree]] = which(Data$is_loaded == 1 & Data$Burst == 1 & Data$Lateral_GU_daughter == 1)
    }else if(name_tree=="notloaded"){
        index_trees_bursted_lateral[[name_tree]] = which(Data$is_loaded == 0 & Data$Burst == 1 & Data$Lateral_GU_daughter == 1)
    }else{
        index_trees_bursted_lateral[[name_tree]] = which(Data$tree == name_tree & Data$Burst == 1 & Data$Lateral_GU_daughter == 1)
    }
}

#__________________________________________
## For cycle 4 : 
#__________________________________________
    # For all trees
glm.no_lateral_GU.04_nul = glm( No_lateral_gu ~ 1,
    family = poisson, data=Data, subset = intersect(index_lateral,index_04) )
summary(glm.no_lateral_GU.04_nul) # AIC: 
    
    # For each tree, loaded trees and not loaded trees
list_glm.no_lateral_GU.04_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.no_lateral_GU.04_nul_tree[[name_tree]] = glm( No_lateral_gu ~ 1,
        family = poisson, data=Data, subset= intersect(index_trees_bursted_lateral[[name_tree]],index_04) )
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC:  
# loaded :  AIC:  
#notloaded: AIC:  


#__________________________________________
## For cycle 5 : 
#__________________________________________
    # For all trees
glm.no_lateral_GU.05_nul = glm( No_lateral_gu ~ 1,
    family = poisson, data=Data, subset = intersect(index_lateral,index_05) )
summary(glm.no_lateral_GU.05_nul) # AIC: 
    
    # For each tree, loaded trees and not loaded trees
list_glm.no_lateral_GU.05_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.no_lateral_GU.05_nul_tree[[name_tree]] = glm( No_lateral_gu ~ 1,
        family = poisson, data=Data, subset= intersect(index_trees_bursted_lateral[[name_tree]],index_05) )
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC:  
# loaded :  AIC:  
#notloaded: AIC:  

##############################################
#### Delta burst date of daughters    
##############################################
library(VGAM)
# k : nb de parametre du model, L : la vraisemblance, AIC = 2k - 2ln(L)
get_AIC = function(myglm){
    k = myglm@rank
    logL = myglm@criterion$loglikelihood
    AIC = 2*k - 2*logL
return(AIC)
}

#__________________________________________
## For cycle 4 : 
#__________________________________________
    # For all trees
vglm.Delta_burst_date_child.04_nul = vglm( Delta_burst_date_child ~ 1,
    family = cumulative(parallel=T) ,data=Data, subset = intersect(index_bursted,index_04) )   
summary(vglm.Delta_burst_date_child.04_nul)   # Log-likelihood: 
get_AIC(vglm.Delta_burst_date_child.04_nul)

    # For each tree, loaded trees and not loaded trees
list_vglm.Delta_burst_date_child.04_nul_tree = list()
list_AIC_vglm.Delta_burst_date_child.04_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_vglm.Delta_burst_date_child.04_nul_tree[[name_tree]] = vglm( Delta_burst_date_child ~ 1,
        family = cumulative(parallel=T), data=Data, subset=intersect(index_trees_bursted[[name_tree]],index_04) )
    list_AIC_vglm.Delta_burst_date_child.04_nul_tree[[name_tree]] = get_AIC(list_vglm.Delta_burst_date_child.04_nul_tree[[name_tree]])
}
list_AIC_vglm.Delta_burst_date_child.04_nul_tree
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 

#__________________________________________
## For cycle 5 : 
#__________________________________________
    # For all trees
vglm.Delta_burst_date_child.05_nul = vglm( Delta_burst_date_child ~ 1,
    family = cumulative(parallel=T) ,data=Data, subset = intersect(index_bursted,index_05) )   
summary(vglm.Delta_burst_date_child.05_nul)   # Log-likelihood: 
get_AIC(vglm.Delta_burst_date_child.05_nul)

    # For each tree, loaded trees and not loaded trees
list_vglm.Delta_burst_date_child.05_nul_tree = list()
list_AIC_vglm.Delta_burst_date_child.05_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_vglm.Delta_burst_date_child.05_nul_tree[[name_tree]] = vglm( Delta_burst_date_child ~ 1,
        family = cumulative(parallel=T), data=Data, subset=intersect(index_trees_bursted[[name_tree]],index_05) )
    list_AIC_vglm.Delta_burst_date_child.05_nul_tree[[name_tree]] = get_AIC(list_vglm.Delta_burst_date_child.05_nul_tree[[name_tree]])
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 


##############################################
#### Flowering 
##############################################
index_extremity = which(Data$is_in_extremity == 1)

index_trees_extremity = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees_extremity[[name_tree]] = which(Data$is_loaded == 1 & Data$is_in_extremity == 1)
    }else if(name_tree=="notloaded"){
        index_trees_extremity[[name_tree]] = which(Data$is_loaded == 0 & Data$is_in_extremity == 1)
    }else{
        index_trees_extremity[[name_tree]] = which(Data$tree == name_tree & Data$is_in_extremity == 1)
    }
}

#__________________________________________
## For cycle 4 :
#__________________________________________ 
    # For all trees
glm.Flowering.04_nul = glm( Flowering ~ 1,
    family = binomial, data=Data, subset = intersect(index_extremity,index_04) )
summary(glm.Flowering.04_nul) # AIC: 

    # For each tree, loaded trees and not loaded trees
list_glm.Flowering.04_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.Flowering.04_nul_tree[[name_tree]] = glm( Flowering ~ 1,
        family = binomial, data=Data, subset=intersect(index_trees_extremity[[name_tree]],index_04) )
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 


#__________________________________________
## For cycle 5 :
#__________________________________________ 
    # For all trees
glm.Flowering.05_nul = glm( Flowering ~ 1,
    family = binomial, data=Data, subset = intersect(index_extremity,index_05) )
summary(glm.Flowering.05_nul) # AIC: 

    # For each tree, loaded trees and not loaded trees
list_glm.Flowering.05_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.Flowering.05_nul_tree[[name_tree]] = glm( Flowering ~ 1,
        family = binomial, data=Data, subset=intersect(index_trees_extremity[[name_tree]],index_05) )
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
index_flowering = which(Data$Flowering == 1)
No_inflo = Data$No_inflorescences -1

index_trees_flowering = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    if(name_tree=="loaded"){
        index_trees_flowering[[name_tree]] = which(Data$is_loaded == 1 & Data$Flowering == 1)
    }else if(name_tree=="notloaded"){
        index_trees_flowering[[name_tree]] = which(Data$is_loaded == 0 & Data$Flowering == 1)
    }else{
        index_trees_flowering[[name_tree]] = which(Data$tree == name_tree & Data$Flowering == 1)
    }
}

#__________________________________________
## For cycle 4 : 
#__________________________________________
    # For all trees
glm.No_inflorescences.04_nul = glm( No_inflo ~ 1,
    family = poisson, data=Data, subset = intersect(index_flowering,index_04) )
summary(glm.No_inflorescences.04_nul) # AIC: 
    
    # For each tree, loaded trees and not loaded trees
list_glm.No_inflorescences.04_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.No_inflorescences.04_nul_tree[[name_tree]] = glm( No_inflo ~ 1,
        family = poisson, data=Data, subset=intersect(index_trees_flowering[[name_tree]],index_04) )
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC:  
# loaded :  AIC:  
#notloaded: AIC:  


#__________________________________________
## For cycle 5 : 
#__________________________________________
    # For all trees
glm.No_inflorescences.05_nul = glm( No_inflo ~ 1,
    family = poisson, data=Data, subset = intersect(index_flowering,index_05) )
summary(glm.No_inflorescences.05_nul) # AIC: 
    
    # For each tree, loaded trees and not loaded trees
list_glm.No_inflorescences.05_nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_glm.No_inflorescences.05_nul_tree[[name_tree]] = glm( No_inflo ~ 1,
        family = poisson, data=Data, subset=intersect(index_trees_flowering[[name_tree]],index_05) )
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

#__________________________________________
## For both cycle : 
#__________________________________________
    # For all trees
vglm.Flowering_Date.nul = vglm( Flowering_date ~ 1,
    family = cumulative(parallel=T) ,data=Data, subset = index_flowering)
summary(vglm.Flowering_Date.nul)  # Log-likelihood: 
get_AIC(vglm.Flowering_Date.nul)   # AIC : 

    # For each tree, loaded trees and not loaded trees
list_vglm.Flowering_Date.nul_tree = list()
list_AIC_vglm.Flowering_Date.nul_tree = list()
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    list_vglm.Flowering_Date.nul_tree[[name_tree]] = vglm( Flowering_date ~ 1,
        family = cumulative(parallel=T), data=Data, subset=index_trees_flowering[[name_tree]])
    list_AIC_vglm.Flowering_Date.nul_tree[[name_tree]] = get_AIC(list_vglm.Flowering_Date.nul_tree[[name_tree]])
}
# B10 :     AIC: 
# B12 :     AIC: 
# B14 :     AIC: 
# F2 :      AIC: 
# F6 :      AIC: 
# loaded :  AIC: 
#notloaded: AIC: 


detach(Data)

get_table_prob_variable_glm = function(myglm){
    if( class(myglm)[1]=="vglm" ){
        probs = predictvglm(myglm,type="response")[1,]
        delta_months_p = colnames(myglm@y)
        data_probs = data.frame(probs[1])
        row.names(data_probs) = NULL
        for(i in 2:length(delta_months_p) ){
            data_probs = cbind(data_probs,probs[i])
        }
        colnames(data_probs)= delta_months_p
    }else if(myglm$family[1]=="binomial" || myglm$family[1]=="poisson"){  
        probs = predict(myglm,type="response")[1]
        data_probs = data.frame(probs)
    }
return(data_probs)
}

# For all trees
#___________________________________
table_prob_glm.burst.04_nul = get_table_prob_variable_glm(glm.burst.04_nul)
write.csv(table_prob_glm.burst.04_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_glm_burst_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.04_nul = get_table_prob_variable_glm(glm.Lateral_GU_daughter.04_nul)
write.csv(table_prob_glm.Lateral_GU_daughter.04_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_glm_Lateral_GU_daughter_04.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.04_nul = get_table_prob_variable_glm(glm.no_lateral_GU.04_nul)
write.csv(table_prob_glm.no_lateral_GU.04_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_glm_no_lateral_GU_04.csv",sep=""), row.names = FALSE)
table_prob_vglm.Delta_burst_date_child.04_nul = get_table_prob_variable_glm(vglm.Delta_burst_date_child.04_nul)
write.csv(table_prob_vglm.Delta_burst_date_child.04_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_vglm_Delta_burst_date_child_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Flowering.04_nul = get_table_prob_variable_glm(glm.Flowering.04_nul)
write.csv(table_prob_glm.Flowering.04_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_glm_Flowering_04.csv",sep=""), row.names = FALSE)
table_prob_glm.No_inflorescences.04_nul = get_table_prob_variable_glm(glm.No_inflorescences.04_nul)
write.csv(table_prob_glm.No_inflorescences.04_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_glm_No_inflorescences_04.csv",sep=""), row.names = FALSE)
table_prob_glm.burst.05_nul = get_table_prob_variable_glm(glm.burst.05_nul)
write.csv(table_prob_glm.burst.05_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_glm_burst_05.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.05_nul = get_table_prob_variable_glm(glm.Lateral_GU_daughter.05_nul)
write.csv(table_prob_glm.Lateral_GU_daughter.05_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_glm_Lateral_GU_daughter_05.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.05_nul = get_table_prob_variable_glm(glm.no_lateral_GU.05_nul)
write.csv(table_prob_glm.no_lateral_GU.05_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_glm_no_lateral_GU_05.csv",sep=""), row.names = FALSE)
table_prob_vglm.Delta_burst_date_child.05_nul = get_table_prob_variable_glm(vglm.Delta_burst_date_child.05_nul)
write.csv(table_prob_vglm.Delta_burst_date_child.05_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_vglm_Delta_burst_date_child_05.csv",sep=""), row.names = FALSE)
table_prob_glm.Flowering.05_nul = get_table_prob_variable_glm(glm.Flowering.05_nul)
write.csv(table_prob_glm.Flowering.05_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_glm_Flowering_05.csv",sep=""), row.names = FALSE)
table_prob_glm.No_inflorescences.05_nul = get_table_prob_variable_glm(glm.No_inflorescences.05_nul)
write.csv(table_prob_glm.No_inflorescences.05_nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_glm_No_inflorescences_05.csv",sep=""), row.names = FALSE)
table_prob_vglm.Flowering_Date.nul = get_table_prob_variable_glm(vglm.Flowering_Date.nul)
write.csv(table_prob_vglm.Flowering_Date.nul,file=paste(share_dir,"model_glm/model_nul/by_all_trees/table_prob_vglm_Flowering_Date.csv",sep=""), row.names = FALSE)



# by_tree
#___________________________________
for(tree in 1:length(trees)){
    name_tree = trees[tree]
    path_file = paste(share_dir,"model_glm/model_nul/by_tree/",sep="")
    path_file_tree = paste(path_file,name_tree,sep="")
    
    table_prob_glm.burst.04_nul_tree = get_table_prob_variable_glm(list_glm.burst.04_nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_burst_04.csv",sep="")
    write.csv(table_prob_glm.burst.04_nul_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Lateral_GU_daughter.04_nul_tree = get_table_prob_variable_glm(list_glm.Lateral_GU_daughter.04_nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_Lateral_GU_daughter_04.csv",sep="")
    write.csv(table_prob_glm.Lateral_GU_daughter.04_nul_tree,file=path_final, row.names = FALSE)

    table_prob_glm.no_lateral_GU.04_nul_tree = get_table_prob_variable_glm(list_glm.no_lateral_GU.04_nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_no_lateral_GU_04.csv",sep="")
    write.csv(table_prob_glm.no_lateral_GU.04_nul_tree,file=path_final, row.names = FALSE)

    table_prob_vglm.Delta_burst_date_child.04_nul_tree = get_table_prob_variable_glm(list_vglm.Delta_burst_date_child.04_nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_vglm_Delta_burst_date_child_04.csv",sep="")
    write.csv(table_prob_vglm.Delta_burst_date_child.04_nul_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Flowering.04_nul = get_table_prob_variable_glm(glm.Flowering.04_nul)
    path_final = paste(path_file_tree,"/table_prob_glm_Flowering_04.csv",sep="")
    write.csv(table_prob_glm.Flowering.04_nul,file=path_final, row.names = FALSE)
    
    table_prob_glm.No_inflorescences.04_nul = get_table_prob_variable_glm(glm.No_inflorescences.04_nul)
    path_final = paste(path_file_tree,"/table_prob_glm_No_inflorescences_04.csv",sep="")
    write.csv(table_prob_glm.No_inflorescences.04_nul,file=path_final, row.names = FALSE)
    
    table_prob_glm.burst.05_nul_tree = get_table_prob_variable_glm(list_glm.burst.05_nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_burst_05.csv",sep="")
    write.csv(table_prob_glm.burst.05_nul_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Lateral_GU_daughter.05_nul_tree = get_table_prob_variable_glm(list_glm.Lateral_GU_daughter.05_nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_Lateral_GU_daughter_05.csv",sep="")
    write.csv(table_prob_glm.Lateral_GU_daughter.05_nul_tree,file=path_final, row.names = FALSE)

    table_prob_glm.no_lateral_GU.05_nul_tree = get_table_prob_variable_glm(list_glm.no_lateral_GU.05_nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_no_lateral_GU_05.csv",sep="")
    write.csv(table_prob_glm.no_lateral_GU.05_nul_tree,file=path_final, row.names = FALSE)

    table_prob_vglm.Delta_burst_date_child.05_nul_tree = get_table_prob_variable_glm(list_vglm.Delta_burst_date_child.05_nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_vglm_Delta_burst_date_child_05.csv",sep="")
    write.csv(table_prob_vglm.Delta_burst_date_child.05_nul_tree,file=path_final, row.names = FALSE)
    
    table_prob_glm.Flowering.05_nul = get_table_prob_variable_glm(list_glm.Flowering.05_nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_Flowering_05.csv",sep="")
    write.csv(table_prob_glm.Flowering.05_nul,file=path_final, row.names = FALSE)
    
    table_prob_glm.No_inflorescences.05_nul = get_table_prob_variable_glm(list_glm.No_inflorescences.05_nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_glm_No_inflorescences_05.csv",sep="")
    write.csv(table_prob_glm.No_inflorescences.05_nul,file=path_final, row.names = FALSE)

    table_prob_vglm.Flowering_Date.nul = get_table_prob_variable_glm(list_vglm.Flowering_Date.nul_tree[[name_tree]])
    path_final = paste(path_file_tree,"/table_prob_vglm_Flowering_Date.csv",sep="")
    write.csv(table_prob_vglm.Flowering_Date.nul,file=path_final, row.names = FALSE)
}
