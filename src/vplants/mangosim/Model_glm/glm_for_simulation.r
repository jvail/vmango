setwd("D:/openalea/vplants/mangosim/src/vplants/mangosim/Model_glm")
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

#data.04$burst_date_mother = factor(data.04$burst_date_mother ) 

# Burst
glm.burst.04_complet = glm( Burst ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = binomial, data=data.04)
summary(glm.burst.04_complet)

step.glm.burst.04 = step(glm.burst.04_complet)

glm.burst.04_select = glm( Burst ~ burst_date_mother + position_mother + nature_ancestor,
    family = binomial, data=data.04)
summary(glm.burst.04_select)

# Lateral GU daughter
index_bursted.04 = which(data.04$Burst == "yes")
glm.Lateral_GU_daughter.04_complet = glm( Lateral_GU_daughter ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = binomial, data=data.04, subset = index_bursted.04)
summary(glm.Lateral_GU_daughter.04_complet)

step.glm.Lateral_GU_daughter.04 = step(glm.Lateral_GU_daughter.04_complet)

glm.Lateral_GU_daughter.04_select = glm( Lateral_GU_daughter ~ is_loaded + burst_date_mother + position_mother,
    family = binomial, data=data.04, subset = index_bursted.04)
 summary(glm.Lateral_GU_daughter.04_select)

# Number of lateral GU
index_lateral.04 = which(data.04$Burst == "yes" & data.04$Lateral_GU_daughter == "yes")
#On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
#On enlève donc 1 au nombre de latérales afin de commencer à 0.
####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
No_lateral_gu = data.04$No_Lateral_GU -1
glm.no_lateral_GU.04_complet = glm( No_lateral_gu  ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = poisson, data=data.04, subset = index_lateral.04)
summary(glm.no_lateral_GU.04_complet)

step.glm.no_lateral_GU.04 = step(glm.no_lateral_GU.04_complet)

glm.no_lateral_GU.04_select = glm( No_lateral_gu  ~ is_loaded + burst_date_mother + position_mother + nature_ancestor,
    family = poisson, data=data.04, subset = index_lateral.04)
summary(glm.no_lateral_GU.04_select)

# Delta burst date of daughters    
library(MASS)
glm.Delta_Burst_date_child.04_complet = glm.nb( Delta_Burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    data=data.04, subset = index_bursted.04)
summary(glm.Delta_Burst_date_child.04_complet)
# AIC : 2706.7
step(glm.Delta_Burst_date_child.04_complet)

glm.Delta_Burst_date_child.04_select = glm.nb( Delta_Burst_date_child ~ is_loaded + burst_date_mother,
    data=data.04, subset = index_bursted.04)
summary(glm.Delta_Burst_date_child.04_select)
# AIC : 2700.8

### ==> trop peu de variable explicative, on essaye avec une loi multinomial
require(nnet)
Delta_burst_date_child = as.factor(data.04$Delta_Burst_date_child)
multinom.Delta_Burst_date_child.04_complet = multinom( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    data=data.04, subset = index_bursted.04)
summary(multinom.Delta_Burst_date_child.04_complet)
# AIC : 1911
step(multinom.Delta_Burst_date_child.04_complet)

multinom.Delta_Burst_date_child.04_select = multinom( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + nature_ancestor,
    data=data.04, subset = index_bursted.04)
summary(multinom.Delta_Burst_date_child.04_select)
# AIC : 1909

### et si on ordonne la variable
require(MASS)
polr.Delta_Burst_date_child.04_complet = polr( Delta_burst_date_child ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    data=data.04, subset = index_bursted.04)
summary(polr.Delta_Burst_date_child.04_complet)
# AIC : 2145
step(polr.Delta_Burst_date_child.04_complet)

polr.Delta_Burst_date_child.04_select = polr( Delta_burst_date_child ~ is_loaded + burst_date_mother ,
    data=data.04, subset = index_bursted.04)
summary(polr.Delta_Burst_date_child.04_select)
# AIC : 2140


# Flowering 
index_extremity.04 = which(data.04$is_in_extremity == 'yes')
glm.Flowering.04_complet = glm( Flowering ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = binomial, data=data.04, subset = index_extremity.04)
summary(glm.Flowering.04_complet)
# AIC : 1584

step(glm.Flowering.04_complet)
# Toutes les variables sont influentes, l'AIC ne peut pas être amélioré


# Number of inflorescences 
index_flowering.04 = which(data.04$Flowering == "yes")

No_inflo = data.04$No_inflorescences -1
glm.No_inflorescences.04_complet = glm( No_inflo ~ is_loaded + burst_date_mother + position_mother + position_ancestor + nature_ancestor,
    family = poisson, data=data.04, subset = index_flowering.04)
summary(glm.No_inflorescences.04_complet)
# AIC : 

step(glm.No_inflorescences.04_complet)














    
#################################################
data.05 = table_INSIDE_for_glm_05_loaded_cogshall
attach(data.05)
summary(data.05)







