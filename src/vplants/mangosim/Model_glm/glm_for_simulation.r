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
 

predict_variable_inside =  function(variable,model="complet",cycle=4, is_Loaded=NA, burst_date_Mother=NA, position_Mother=NA, position_Ancestor=NA, nature_Ancestor=NA){
    is_loaded = c(is_Loaded)
    burst_date_mother = c(burst_date_Mother)
    position_mother = c(position_Mother)
    position_ancestor = c(position_Ancestor)
    nature_ancestor = c(nature_Ancestor)
    response_var = data.frame(is_loaded,burst_date_mother,position_mother,position_ancestor,nature_ancestor)
    if(model == "complet"){
        if(variable == "burst"){ myglm = glm.burst.04_complet
        }else if(variable == "Lateral_GU_daughter"){ myglm = glm.Lateral_GU_daughter.04_complet
        }else if(variable == "Flowering"){ myglm = glm.Flowering.04_complet
        }else{}
    }else{
        if(variable == "burst"){myglm = step.glm.burst.04
        }else if(variable == "Lateral_GU_daughter"){myglm = step.glm.Lateral_GU_daughter.04
        }else if (variable == "Flowering"){myglm = step.glm.Flowering.04
        }else {}
        keeps = colnames(myglm$model)[2:length(colnames(myglm$model))]
        response_var = response_var[,keeps,drop=FALSE]
    }
    proba = predict(myglm,newdata=response_var, type = "response")
    if(myglm$family[1]=="binomial"){
        proba = predict(myglm,newdata=response_var, type = "response")
        value = rbinom(n=1,size=1,prob=proba)
    }else if(myglm$family[1]=="poisson"){
        proba = predict(myglm,newdata=response_var, type = "response")
        value = rpois(n=1,lambda=proba)
    }else{
        proba = predict(myglm,newdata=response_var, type = "probs")
        cumsum_proba = cumsum(proba)
        U = runif(n=1)
        for(ip in 1:(length(proba)-1) ){
            if( cumsum_proba[ip]<= U & U < cumsum_proba[ip+1]){ value = as.integer(names(cumsum_proba[ip+1]) ) }
        }
    }
return(value)
}


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
require(MASS)
detach(data.04)
data.04 = table_INSIDE_for_glm_04_loaded_cogshall
#data.04$burst_date_mother[data.04$burst_date_mother==5]="5-6-7"
#data.04$burst_date_mother[data.04$burst_date_mother==6]="5-6-7"
#data.04$burst_date_mother[data.04$burst_date_mother==7]="5-6-7"
#data.04$burst_date_mother[data.04$burst_date_mother==9]="9-10"
#data.04$burst_date_mother[data.04$burst_date_mother==10]="9-10"
#data.04$burst_date_mother=as.factor(data.04$burst_date_mother)
attach(data.04)

polr.Delta_Burst_date_child.04_complet = polr( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    data=data.04, subset = index_bursted.04)
    
summary(polr.Delta_Burst_date_child.04_complet)
# AIC : 2145

step.polr.Delta_Burst_date_child.04 = step(polr.Delta_Burst_date_child.04_complet)
# Delta_burst_date_child ~ is_loaded + burst_date_mother 
summary(step.polr.Delta_Burst_date_child.04)
# AIC : 2140

data.04$burst_date_mother=as.factor(data.04$burst_date_mother)

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
##############################################
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
#glm.Lateral_GU_daughter.03_04_select = glm( Lateral_GU_daughter ~ position_mother + nature_mother,
#    family = binomial, data=data.03_04, subset = index_bursted.03_04)
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
#glm.no_lateral_GU.03_04_select = glm( No_lateral_gu  ~ is_loaded + position_mother,
#    family = poisson, data=data.03_04, subset = index_lateral.03_04)
summary(step.glm.no_lateral_GU.03_04)
# AIC : 688.54

#### Date of children
#---------------------
# For this case, we do not have dates of mother ==> can not have the difference.
require(MASS)
data.03_04$Burst_date_child = as.factor(data.03_04$Burst_date_child)
polr.Burst_date_child.03_04_complet = polr( Burst_date_child ~ is_loaded + position_mother + nature_mother,
    data=data.03_04, subset = index_bursted.03_04)
summary(polr.Burst_date_child.03_04_complet)
# AIC : 788

step(polr.Burst_date_child.03_04_complet)
# We can not perform the AIC, all variables influence the date



level_is_loaded = c("yes","no")
level_position_mother = c("A","L")
level_position_ancestor = c("A","L")
level_nature_ancestor = c("F","V")
level_nature_mother = c("F","V")
get_table_prob_variable_glm = function(myglm){
    if( is.null(myglm$xlevels$burst_date_mother) ){
        level_burst_date_mother = 1:12
    }else{
        level_burst_date_mother = myglm$xlevels$burst_date_mother
    }
    produit_cartesien = expand.grid(level_is_loaded,level_burst_date_mother,level_position_mother,level_position_ancestor,level_nature_ancestor,level_nature_mother)
    names(produit_cartesien) = c("is_loaded","burst_date_mother","position_mother","position_ancestor","nature_ancestor","nature_mother")
    variables = colnames(myglm$model)[2:length(colnames(myglm$model))]
    data_probs = unique(produit_cartesien[variables])
    if( is.null(myglm$family[1]) ){
        probs = predict(myglm,newdata=data_probs,type="probs")
        for(i in 1:length(colnames(probs)) ){
            data_probs[colnames(probs)[i] ] = probs[,i]
        }
    }else if(myglm$family[1]=="binomial" || myglm$family[1]=="poisson"){
        probs = predict(myglm,newdata=data_probs,type="response")
        data_probs["probas"]=probs
    }
return(data_probs)
}
table_prob_glm.Burst.04_complet = get_table_prob_variable_glm(glm.burst.04_complet)
write.csv(table_prob_glm.Burst.04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_Burst_04.csv",sep=""), row.names = FALSE)
table_prob_glm.Lateral_GU_daughter.04_complet = get_table_prob_variable_glm(glm.Lateral_GU_daughter.04_complet)
write.csv(table_prob_glm.Lateral_GU_daughter.04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_Lateral_GU_daughter_04.csv",sep=""), row.names = FALSE)
table_prob_glm.no_lateral_GU.04_complet = get_table_prob_variable_glm(glm.no_lateral_GU.04_complet)
write.csv(table_prob_glm.no_lateral_GU.04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_no_lateral_GU_04.csv",sep=""), row.names = FALSE)
table_prob_polr.Delta_Burst_date_child.04_complet = get_table_prob_variable_glm(polr.Delta_Burst_date_child.04_complet)
write.csv(table_prob_polr.Delta_Burst_date_child.04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_polr_Delta_Burst_date_child_04.csv",sep=""), row.names = FALSE)
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
table_prob_glm.no_lateral_GU.03_04_complet = get_table_prob_variable_glm(glm.no_lateral_GU.03_04_complet)
write.csv(table_prob_glm.no_lateral_GU.03_04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_glm_no_lateral_GU_03_04.csv",sep=""), row.names = FALSE)
table_prob_polr.Burst_date_child.03_04_complet = get_table_prob_variable_glm(polr.Burst_date_child.03_04_complet)
write.csv(table_prob_polr.Burst_date_child.03_04_complet,file=paste(share_dir,"model_glm/table_prob_glm_complet/table_prob_polr_Burst_date_child_03_04.csv",sep=""), row.names = FALSE)





detach(data.03_04)
############################################################################################
data.05 = table_INSIDE_for_glm_05_loaded_cogshall
attach(data.05)
summary(data.05)
data.05$burst_date_mother = as.factor(data.05$burst_date_mother)

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
# Lateral_GU_daughter ~ is_loaded + burst_date_mother + position_mother + position_ancestor
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
#No_lateral_gu  ~ is_loaded + position_mother + position_ancestor + nature_ancestor,
summary(step.glm.no_lateral_GU.05)
# AIC : 669

#### Date of inflorescences
#---------------------------
require(MASS)
detach(data.05)
data.05 = table_INSIDE_for_glm_05_loaded_cogshall
attach(data.05)

polr.Delta_Burst_date_child.05_complet = polr( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    data=data.05, subset = index_bursted.05)
    
summary(polr.Delta_Burst_date_child.05_complet)
# AIC : 

step.polr.Delta_Burst_date_child.05 = step(polr.Delta_Burst_date_child.05_complet)
# Delta_burst_date_child ~ is_loaded + burst_date_mother 
summary(step.polr.Delta_Burst_date_child.05)
# AIC : 

data.05$burst_date_mother=as.factor(data.05$burst_date_mother)



detach(data.05)



