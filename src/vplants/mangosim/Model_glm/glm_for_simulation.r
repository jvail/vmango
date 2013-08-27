setwd("D:/openalea/mangosim/src/vplants/mangosim/Model_glm")
#
#
### Importation of data :

share_dir = '../../../../share/'
table_INSIDE_for_glm_04_loaded_cogshall = read.csv(paste(share_dir,"model_glm/table_INSIDE_04_cogshall.csv",sep=""),header = TRUE)
table_INSIDE_for_glm_05_loaded_cogshall = read.csv(paste(share_dir,"model_glm/table_INSIDE_05_cogshall.csv",sep=""),header = TRUE)
table_TRANSITION_for_glm_03to04_cogshall = read.csv(paste(share_dir,"model_glm/table_TRANSITION_03to04_cogshall.csv",sep=""),header = TRUE)
table_TRANSITION_for_glm_04to05_cogshall = read.csv(paste(share_dir,"model_glm/table_TRANSITION_04to05_cogshall.csv",sep=""),header = TRUE)



#################################################
data.04 = table_INSIDE_for_glm_04_loaded_cogshall
attach(data.04)
summary(data.04)


data.04$burst_date_mother=as.factor(data.04$burst_date_mother)



#### Burst
#-----------
glm.burst.04_complet = glm( Burst ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = binomial, data=data.04)
summary(glm.burst.04_complet)
# AIC : 1715

step.glm.burst.04 = step(glm.burst.04_complet)
#glm.burst.04_select = glm( Burst ~ is_loaded + burst_date_mother + position_mother,
#    family = binomial, data=data.04)
summary(step.glm.burst.04)
# AIC : 1711.5


#### Lateral GU daughter
#------------------------
index_bursted.04 = which(data.04$Burst == "yes")
glm.Lateral_GU_daughter.04_complet = glm( Lateral_GU_daughter ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = binomial, data=data.04, subset = index_bursted.04)
summary(glm.Lateral_GU_daughter.04_complet)
# AIC : 994

step.glm.Lateral_GU_daughter.04 = step(glm.Lateral_GU_daughter.04_complet)
#glm.Lateral_GU_daughter.04_select = glm( Lateral_GU_daughter ~ is_loaded + position_mother,
#    family = binomial, data=data.04, subset = index_bursted.04)
summary(step.glm.Lateral_GU_daughter.04)
# AIC : 989
 

#### Number of lateral GU
#--------------------------
index_lateral.04 = which(data.04$Burst == "yes" & data.04$Lateral_GU_daughter == "yes")
#On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
#On enlève donc 1 au nombre de latérales afin de commencer à 0.
####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
No_lateral_gu = data.04$No_Lateral_GU -1
glm.no_lateral_GU.04_complet = glm( No_lateral_gu  ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = poisson, data=data.04, subset = index_lateral.04)
summary(glm.no_lateral_GU.04_complet)
# AIC : 944.5

step.glm.no_lateral_GU.04 = step(glm.no_lateral_GU.04_complet)
#glm.no_lateral_GU.04_select = glm( No_lateral_gu  ~ is_loaded + position_mother + position_ancestor + nature_ancestor,
#    family = poisson, data=data.04, subset = index_lateral.04)
summary(step.glm.no_lateral_GU.04)
# AIC : 942.7

#### Delta burst date of daughters    
#----------------------------------
library(MASS)
glm.Delta_Burst_date_child.04_complet = glm.nb( Delta_Burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    data=data.04, subset = index_bursted.04)
summary(glm.Delta_Burst_date_child.04_complet)
# AIC : 2668.5

step.glm.Delta_Burst_date_child.04 = step(glm.Delta_Burst_date_child.04_complet)
#glm.Delta_Burst_date_child.04_select = glm.nb( Delta_Burst_date_child ~ is_loaded + burst_date_mother,
#    data=data.04, subset = index_bursted.04)
summary(step.glm.Delta_Burst_date_child.04)
# AIC : 2662.7

### ==> trop peu de variable explicative, on essaye avec une loi multinomial
require(nnet)
Delta_burst_date_child = as.factor(data.04$Delta_Burst_date_child)
multinom.Delta_Burst_date_child.04_complet = multinom( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    data=data.04, subset = index_bursted.04)
summary(multinom.Delta_Burst_date_child.04_complet)
# AIC : 1509.56

step.multinom.Delta_Burst_date_child.04 = step(multinom.Delta_Burst_date_child.04_complet)
#multinom.Delta_Burst_date_child.04_select = multinom( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + nature_ancestor,
#    data=data.04, subset = index_bursted.04)
summary(step.multinom.Delta_Burst_date_child.04)
# AIC : 1508.67

### et si on ordonne la variable
#require(MASS)
detach(data.04)
data.04 = table_INSIDE_for_glm_04_loaded_cogshall
#data.04$burst_date_mother[data.04$burst_date_mother==5]="5-6-7"
#data.04$burst_date_mother[data.04$burst_date_mother==6]="5-6-7"
#data.04$burst_date_mother[data.04$burst_date_mother==7]="5-6-7"
#data.04$burst_date_mother[data.04$burst_date_mother==9]="9-10"
#data.04$burst_date_mother[data.04$burst_date_mother==10]="9-10"
data.04$burst_date_mother=as.factor(data.04$burst_date_mother)
attach(data.04)
Delta_burst_date_child[Delta_burst_date_child=='<NA>'] = NA 
Delta_burst_date_child = as.ordered(Delta_burst_date_child)


#polr.Delta_Burst_date_child.04_complet = polr( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
#    data=data.04, subset = index_bursted.04)
# il y a trop de facteurs a cause de burst_date_mother
# ==> utilisons vglm de la librairie VGAM
library(VGAM)

vglm.Delta_Burst_date_child.04_complet = vglm( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = cumulative(parallel=T) ,data=data.04, subset = index_bursted.04)
    
summary(vglm.Delta_Burst_date_child.04_complet)
# Log-likelihood: -935.8131 

# la fonction step ne s'applique pas à la classe des vglm
# ===>> selectioner le model a la main   TODO




#### Flowering 
#--------------
index_extremity.04 = which(data.04$is_in_extremity == 'yes')
glm.Flowering.04_complet = glm( Flowering ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = binomial, data=data.04, subset = index_extremity.04)
summary(glm.Flowering.04_complet)
# AIC : 2494.9

step.glm.Flowering.04 = step(glm.Flowering.04_complet)
# Flowering ~ is_loaded + burst_date_mother + position_mother + nature_ancestor
summary(step.glm.Flowering.04)
# AIC : 1476.1

#### Number of inflorescences 
#------------------------------
index_flowering.04 = which(data.04$Flowering == "yes")

No_inflo = data.04$No_inflorescences -1
glm.No_inflorescences.04_complet = glm( No_inflo ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = poisson, data=data.04, subset = index_flowering.04)
summary(glm.No_inflorescences.04_complet)
# AIC : 2495

step.glm.No_inflorescences.04 = step(glm.No_inflorescences.04_complet)
#glm.No_inflorescences.04_select = glm( No_inflo ~ is_loaded + burst_date_mother + position_mother + nature_ancestor,
#    family = poisson, data=data.04, subset = index_flowering.04)
summary(step.glm.No_inflorescences.04)
# AIC : 2490

#### Date of inflorescences
#---------------------------
    # No enougth data (only 56 dates) for study.


detach(data.04)
##################################################################################################
data.03_04 = table_TRANSITION_for_glm_03to04_cogshall
attach(data.03_04)
summary(data.03_04)
 

#### Burst
#----------
glm.burst.03_04_complet = glm( Burst ~ is_loaded + position_mother + nature_mother,
    family = binomial, data=data.03_04)
summary(glm.burst.03_04_complet)
# AIC : 557

step(glm.burst.03_04_complet)
# We can not perform the AIC, all variables influence the burst


#### Lateral GU daughter
#------------------------
index_bursted.03_04 = which(data.03_04$Burst == "yes")
glm.Lateral_GU_daughter.03_04_complet = glm( Lateral_GU_daughter ~ is_loaded + position_mother + nature_mother,
    family = binomial, data=data.03_04, subset = index_bursted.03_04)
summary(glm.Lateral_GU_daughter.03_04_complet)
# AIC : 283

step.glm.Lateral_GU_daughter.03_04 = step(glm.Lateral_GU_daughter.03_04_complet)
# Lateral_GU_daughter ~ position_mother + nature_mother
summary(step.glm.Lateral_GU_daughter.03_04)
# AIC : 282


#### Number of lateral GU
#-------------------------
index_lateral.03_04 = which(data.03_04$Burst == "yes" & data.03_04$Lateral_GU_daughter == "yes")
#On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
#On enlève donc 1 au nombre de latérales afin de commencer à 0.
####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
No_lateral_gu = data.03_04$No_Lateral_GU -1
glm.no_lateral_GU.03_04_complet = glm( No_lateral_gu  ~ is_loaded + position_mother + nature_mother,
    family = poisson, data=data.03_04, subset = index_lateral.03_04)
summary(glm.no_lateral_GU.03_04_complet)
# AIC : 688.55

step.glm.no_lateral_GU.03_04 = step(glm.no_lateral_GU.03_04_complet)
# No_lateral_gu  ~ is_loaded + position_mother
summary(step.glm.no_lateral_GU.03_04)
# AIC : 688.54

#### Date of children
#---------------------
# For this case, we do not have dates of mother ==> can not have the difference.
#require(MASS)
level_order = c("7","8","9","10","11","12","1","2","3","4","5","6")
Burst_date_child = ordered(Burst_date_child, levels = level_order)
Burst_date_child = factor(Burst_date_child)
vglm.Burst_date_child.03_04_complet = vglm( Burst_date_child ~ is_loaded + position_mother + nature_mother,
    family = cumulative(parallel = TRUE), data=data.03_04, subset = index_bursted.03_04)
summary(vglm.Burst_date_child.03_04_complet)
# Log-likelihood: -383.8829 



detach(data.03_04)

level_is_loaded = c("yes","no")
level_position_mother = c("A","L")
level_position_ancestor = c("A","L")
level_nature_ancestor = c("F","V")
level_nature_mother = c("F","V")
level_all_burst_date_mother = as.factor(1:12)
get_table_prob_variable_glm = function(myglm){
    if( class(myglm)[1]=="vglm" ){
        level_burst_date_mother = myglm@xlevels$burst_date_mother
        if(is.null(level_burst_date_mother)){
            level_burst_date_mother = level_all_burst_date_mother
        }
        variables = slot(myglm@terms[[1]],"term.labels")
    }else{
        level_burst_date_mother = myglm$xlevels$burst_date_mother
        if(is.null(level_burst_date_mother)){
            level_burst_date_mother = level_all_burst_date_mother
        }
        variables = colnames(myglm$model)[2:length(colnames(myglm$model))]
    }
    produit_cartesien = expand.grid(level_is_loaded,level_burst_date_mother,level_position_mother,level_position_ancestor,level_nature_ancestor,level_nature_mother)
    names(produit_cartesien) = c("is_loaded","burst_date_mother","position_mother","position_ancestor","nature_ancestor","nature_mother")
    data_probs = unique(produit_cartesien[variables])
    if( class(myglm)[1]=="vglm" ){
        probs = predictvglm(myglm,newdata=data_probs,type="response")
        for(i in 1:length(colnames(probs)) ){
            data_probs[colnames(probs)[i] ] = probs[,i]
        }
    }else if(myglm$family[1]=="binomial" || myglm$family[1]=="poisson"){        
        probs = predict(myglm,newdata=data_probs,type="response")
        data_probs["probas"]=probs
    }
    other_level_burst_date_mother = level_all_burst_date_mother[!level_all_burst_date_mother %in% level_burst_date_mother]
    if(length(other_level_burst_date_mother)>0){
        other_produit_cartesien = expand.grid(level_is_loaded, other_level_burst_date_mother,level_position_mother,level_position_ancestor,level_nature_ancestor,level_nature_mother)
        names(other_produit_cartesien) = c("is_loaded","burst_date_mother","position_mother","position_ancestor","nature_ancestor","nature_mother")
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
return(data_probs)
}
table_prob_glm.Burst.04_complet = get_table_prob_variable_glm(glm.burst.04_complet)
write.csv(table_prob_glm.Burst.04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_Burst_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.04_complet = get_table_prob_variable_glm(glm.Lateral_GU_daughter.04_complet)
write.csv(table_prob_glm.Lateral_GU_daughter.04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_Lateral_GU_daughter_04.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.04_complet = get_table_prob_variable_glm(glm.no_lateral_GU.04_complet)
write.csv(table_prob_glm.no_lateral_GU.04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_no_lateral_GU_04.csv",sep=""), row.names = FALSE)
table_prob_vglm.Delta_Burst_date_child.04_complet = get_table_prob_variable_glm(vglm.Delta_Burst_date_child.04_complet)
write.csv(table_prob_vglm.Delta_Burst_date_child.04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_vglm_Delta_Burst_date_child_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Flowering.04_complet = get_table_prob_variable_glm(glm.Flowering.04_complet)
write.csv(table_prob_glm.Flowering.04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_Flowering_04.csv",sep=""), row.names = FALSE)
table_prob_glm.No_inflorescences.04_complet = get_table_prob_variable_glm(glm.No_inflorescences.04_complet)
write.csv(table_prob_glm.No_inflorescences.04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_No_inflorescences_04.csv",sep=""), row.names = FALSE)
table_prob_glm.burst.03_04_complet = get_table_prob_variable_glm(glm.burst.03_04_complet)
write.csv(table_prob_glm.burst.03_04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_burst_03_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.03_04_complet = get_table_prob_variable_glm(glm.Lateral_GU_daughter.03_04_complet)
write.csv(table_prob_glm.Lateral_GU_daughter.03_04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_Lateral_GU_daughter_03_04.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.03_04_complet = get_table_prob_variable_glm(glm.no_lateral_GU.03_04_complet)
write.csv(table_prob_glm.no_lateral_GU.03_04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_no_lateral_GU_03_04.csv",sep=""), row.names = FALSE)
table_prob_vglm.Burst_date_child.03_04_complet = get_table_prob_variable_glm(vglm.Burst_date_child.03_04_complet)
write.csv(table_prob_vglm.Burst_date_child.03_04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_vglm_Burst_date_child_03_04.csv",sep=""), row.names = FALSE)






############################################################################################
data.05 = table_INSIDE_for_glm_05_loaded_cogshall
data.05$burst_date_mother = as.factor(data.05$burst_date_mother)
attach(data.05)
summary(data.05)

#### Burst
#-----------
glm.burst.05_complet = glm( Burst ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = binomial, data=data.05)
summary(glm.burst.05_complet)
# AIC : 1143.2

step.glm.burst.05 = step(glm.burst.05_complet)
#Burst ~ is_loaded + burst_date_mother + position_mother + position_ancestor 
summary(step.glm.burst.05)
# AIC : 1141.6


#### Lateral GU daughter
#------------------------
index_bursted.05 = which(data.05$Burst == "yes")
glm.Lateral_GU_daughter.05_complet = glm( Lateral_GU_daughter ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = binomial, data=data.05, subset = index_bursted.05)
summary(glm.Lateral_GU_daughter.05_complet)
# AIC : 582.99

step.glm.Lateral_GU_daughter.05 = step(glm.Lateral_GU_daughter.05_complet)
# Lateral_GU_daughter ~ is_loaded + burst_date_mother + position_mother + nature_ancestor
summary(step.glm.Lateral_GU_daughter.05)
# AIC : 581.35


#### Number of lateral GU
#--------------------------
index_lateral.05 = which(data.05$Burst == "yes" & data.05$Lateral_GU_daughter == "yes")
#On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
#On enlève donc 1 au nombre de latérales afin de commencer à 0.
####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
No_lateral_gu = data.05$No_Lateral_GU -1
glm.no_lateral_GU.05_complet = glm( No_lateral_gu  ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = poisson, data=data.05, subset = index_lateral.05)
summary(glm.no_lateral_GU.05_complet)
# AIC : 670

step.glm.no_lateral_GU.05 = step(glm.no_lateral_GU.05_complet)
#No_lateral_gu  ~ is_loaded + burst_date_mother + position_mother + position_ancestor 
summary(step.glm.no_lateral_GU.05)
# AIC : 669

#### Delta burst date of daughters    
#----------------------------------
#require(MASS)
detach(data.05)
data.05 = table_INSIDE_for_glm_05_loaded_cogshall
data.05$burst_date_mother = as.factor(data.05$burst_date_mother)
Delta_burst_date_child = as.factor(data.05$Delta_Burst_date_child)
Delta_burst_date_child = ordered(Delta_burst_date_child)
attach(data.05)


vglm.Delta_Burst_date_child.05_complet = vglm( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = cumulative(parallel = TRUE ), data=data.05, subset = index_bursted.05)
summary(vglm.Delta_Burst_date_child.05_complet)
# Log-likelihood: -640.7234


#### Flowering 
#--------------
index_extremity.05 = which(data.05$is_in_extremity == 'yes')
glm.Flowering.05_complet = glm( Flowering ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = binomial, data=data.05, subset = index_extremity.05)
summary(glm.Flowering.05_complet)
# AIC : 4154.6

step.glm.Flowering.05 = step(glm.Flowering.05_complet)
# Flowering ~ is_loaded + burst_date_mother + position_mother
summary(step.glm.Flowering.05)
# AIC : 1451.2

#### Number of inflorescences 
#------------------------------
index_flowering.05 = which(data.05$Flowering == "yes")

No_inflo = data.05$No_inflorescences -1
glm.No_inflorescences.05_complet = glm( No_inflo ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = poisson, data=data.05, subset = index_flowering.05)
summary(glm.No_inflorescences.05_complet)
# AIC : 6228

step.glm.No_inflorescences.05 = step(glm.No_inflorescences.05_complet)
# No_inflo ~ is_loaded + position_mother
summary(step.glm.No_inflorescences.05)
# AIC : 6226

#### Date of inflorescences
#---------------------------
detach(data.05)
data.05 = table_INSIDE_for_glm_05_loaded_cogshall
data.05$burst_date_mother=as.factor(data.05$burst_date_mother)
Flowering_date = as.factor(data.05$Flowering_Date)
Flowering_date = ordered(Flowering_date)
attach(data.05)


vglm.Flowering_Date.05_complet = vglm( Flowering_date ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = cumulative(parallel = TRUE), data=data.05, subset = index_flowering.05)
summary(vglm.Flowering_Date.05_complet)
# Log-likelihood: -5326.792



detach(data.05)



##################################################################################################
data.04_05 = table_TRANSITION_for_glm_04to05_cogshall
attach(data.04_05)
summary(data.04_05)
 

#### Burst
#----------
glm.burst.04_05_complet = glm( Burst ~ is_loaded + burst_date_mother + position_mother + nature_mother,
    family = binomial, data=data.04_05)
summary(glm.burst.04_05_complet)
# AIC : 1729

step.glm.burst.04_05 = step(glm.burst.04_05_complet)
# Burst ~ is_loaded + burst_date_mother + position_mother
summary(step.glm.burst.04_05)
#AIC : 1727


#### Lateral GU daughter
#------------------------
index_bursted.04_05 = which(data.04_05$Burst == "yes")
glm.Lateral_GU_daughter.04_05_complet = glm( Lateral_GU_daughter ~ is_loaded + burst_date_mother + position_mother + nature_mother,
    family = binomial, data=data.04_05, subset = index_bursted.04_05)
summary(glm.Lateral_GU_daughter.04_05_complet)
# AIC : 296

step.glm.Lateral_GU_daughter.04_05 = step(glm.Lateral_GU_daughter.04_05_complet)
# We can not perform the AIC, all variables influence lateral position


#### Number of lateral GU
#-------------------------
index_lateral.04_05 = which(data.04_05$Burst == "yes" & data.04_05$Lateral_GU_daughter == "yes")
#On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
#On enlève donc 1 au nombre de latérales afin de commencer à 0.
####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
No_lateral_gu = data.04_05$No_Lateral_GU -1
glm.no_lateral_GU.04_05_complet = glm( No_lateral_gu  ~ is_loaded + burst_date_mother + position_mother + nature_mother,
    family = poisson, data=data.04_05, subset = index_lateral.04_05)
summary(glm.no_lateral_GU.04_05_complet)
# AIC : 3165

step.glm.no_lateral_GU.04_05 = step(glm.no_lateral_GU.04_05_complet)
# We can not perform the AIC, all variables influence the number of lateral GU

#### Delta date of children
#---------------------

detach(data.04_05)
data.04_05 = table_TRANSITION_for_glm_04to05_cogshall
data.04_05$burst_date_mother = as.factor(as.numeric(as.character(data.04_05$burst_date_mother)))
Delta_burst_date = as.factor(data.04_05$Delta_Burst_date)
Delta_burst_date = ordered(Delta_burst_date)
attach(data.04_05)

vglm.Delta_burst_date.04_05_complet = vglm( Delta_burst_date ~ is_loaded + burst_date_mother + position_mother + nature_mother,
    family = cumulative(parallel = TRUE), data=data.04_05, subset = index_bursted.04_05)
summary(vglm.Delta_burst_date.04_05_complet)
# Log-likelihood: -1583.573



detach(data.04_05)

table_prob_glm.Burst.05_complet = get_table_prob_variable_glm(glm.burst.05_complet)
write.csv(table_prob_glm.Burst.05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_Burst_05.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.05_complet = get_table_prob_variable_glm(glm.Lateral_GU_daughter.05_complet)
write.csv(table_prob_glm.Lateral_GU_daughter.05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_Lateral_GU_daughter_05.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.05_complet = get_table_prob_variable_glm(glm.no_lateral_GU.05_complet)
write.csv(table_prob_glm.no_lateral_GU.05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_no_lateral_GU_05.csv",sep=""), row.names = FALSE)
table_prob_vglm.Delta_Burst_date_child.05_complet = get_table_prob_variable_glm(vglm.Delta_Burst_date_child.05_complet)
write.csv(table_prob_vglm.Delta_Burst_date_child.05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_vglm_Delta_Burst_date_child_05.csv",sep=""), row.names = FALSE)
table_prob_glm.Flowering.05_complet = get_table_prob_variable_glm(glm.Flowering.05_complet)
write.csv(table_prob_glm.Flowering.05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_Flowering_05.csv",sep=""), row.names = FALSE)
table_prob_glm.No_inflorescences.05_complet = get_table_prob_variable_glm(glm.No_inflorescences.05_complet)
write.csv(table_prob_glm.No_inflorescences.05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_No_inflorescences_05.csv",sep=""), row.names = FALSE)
table_prob_vglm.Flowering_Date.05_complet = get_table_prob_variable_glm(vglm.Flowering_Date.05_complet)
write.csv(table_prob_vglm.Flowering_Date.05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_vglm_Date_inflo_05.csv",sep=""), row.names = FALSE)
table_prob_glm.burst.04_05_complet = get_table_prob_variable_glm(glm.burst.04_05_complet)
write.csv(table_prob_glm.burst.04_05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_burst_04_05.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.04_05_complet = get_table_prob_variable_glm(glm.Lateral_GU_daughter.04_05_complet)
write.csv(table_prob_glm.Lateral_GU_daughter.04_05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_Lateral_GU_daughter_04_05.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.04_05_complet = get_table_prob_variable_glm(glm.no_lateral_GU.04_05_complet)
write.csv(table_prob_glm.no_lateral_GU.04_05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_no_lateral_GU_04_05.csv",sep=""), row.names = FALSE)
table_prob_vglm.Delta_burst_date.04_05_complet = get_table_prob_variable_glm(vglm.Delta_burst_date.04_05_complet)
write.csv(table_prob_vglm.Delta_burst_date.04_05_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_vglm_Delta_burst_date_04_05.csv",sep=""), row.names = FALSE)



