

group_factor_values = function(data, variable, groups) {  
  # group values of a factor
  # Args:
  #   data : The dataframe to modify
  #   variable : the factor name
  #   groups : a list of vectors of values indicating the groups
  # Returns:
  #   The new dataframe
  
  if (is.factor(data[[variable]])) {
    clevel = levels(data[[variable]])
    for (i in 1:length(clevel)) {
      val = clevel[i]
      for(group in groups) {
        if (val %in% group){
          clevel[i] =  paste(group, collapse="-")
        }
      }
    }
    levels(data[[variable]]) = clevel
  }
  else {
    for (group in groups) {
      groupname = paste(group, collapse="-")
      data[[variable]] = replace(data[[variable]], data[[variable]] %in% group, groupname)
    }
    
    data[[variable]] = as.factor(data[[variable]])
  }
  return (data)
}



filter_monthes = function(data, subset, minminalcount = 6) {
  # Remove individuals which occurs in month with not enought individuals.
  # Args:
  #   data : the dataframe
  #   subset : a selection of individual
  #   minimalcount : the minimal number of individu per month
  # Returns:
  #   An updated subset with removed individuals.
    nsubset = subset
    nbelemmonth = table(data[nsubset,"Burst_Month"])
    for (i in 1:length(nbelemmonth)){
      m = names(nbelemmonth)[i]
      count = nbelemmonth[i]
      if(0 < count & count < minminalcount){
        lsubset = data[nsubset,"Burst_Month"] != m
        nsubset = nsubset[lsubset]
        print(paste("Filter value", m, "for factor", "Burst_Month" , ":", count, '->',length(nsubset) ))
      }
    }
    return (nsubset)
}

share_dir = '../../../../share/'
input_dir = paste(share_dir,'glm_estimate_input/cogshall/', sep="")

data = read.csv(paste(input_dir,"table_within_cycle_05.csv",sep=""),header = TRUE)


# Make factors
knowfactors = c("Nature_Ancestor_F", "Position_A", "Position_Ancestor_A", "Vegetative_Burst" )
factors = intersect(names(data), knowfactors)
for (name in factors) { data[[name]] = factor(data[[name]]) }

# Make ordinal factors
burst_month_level = c(6:12,1:5)
data$Burst_Month = factor(data$Burst_Month, ordered = TRUE, levels = burst_month_level )
data$Burst_Date_GU_Children = factor(data$Burst_Date_GU_Children, ordered = TRUE, levels = burst_month_level )

# Remove Mixed Inflo and month with not enought individual
subset = which(data$Mixed_Inflo == 0)
subset = filter_monthes(data, subset)

tracestep = -1

################# Vegetative_Burst #####################
# Regroup month for Vegetative_Burst
ndata = group_factor_values(data, "Burst_Month", list(6:12,1:2))

complete_glm.burst = glm( Vegetative_Burst ~ Burst_Month + Position_A + Position_Ancestor_A + Nature_Ancestor_F , family = binomial, data = ndata, subset = subset)
#print(summary(complete_glm.burst))

#print('Step')
selected_glm.burst = step(complete_glm.burst, trace = tracestep)
#print(summary(selected_glm.burst))
#stop("Vegetative_Burst")

################# Nb_Lateral_GU_Children #####################
# Regroup month for Nb_Lateral_GU_Children
data$Nb_Lateral_GU_Children = data$Nb_Lateral_GU_Children -1

ndata = group_factor_values(data, "Burst_Month",list( 8:12, c(1,2)))
print(ndata[,c('Nb_Lateral_GU_Children','Has_Lateral_GU_Children')])

subset = which(ndata$Mixed_Inflo == 0 & ndata$Has_Lateral_GU_Children == 1)
subset = filter_monthes(ndata, subset)

ndata = ndata[subset,]
#print(table(ndata$Nb_Lateral_GU_Children))

complete_glm.nblat = glm( Nb_Lateral_GU_Children ~ Burst_Month + Position_A + Position_Ancestor_A + Nature_Ancestor_F , family = poisson, data = ndata)
#print(summary(complete_glm.nblat))

#print('Step')
selected_glm.nblat = step(complete_glm.nblat, trace = tracestep)
#print(summary(selected_glm.nblat))
#stop("Nb_Lateral_GU_Children")


################# Burst_Date_GU_Children #####################
source("util_glm.r")

subset = which(data$Mixed_Inflo == 0 & data$Vegetative_Burst == 1)
subset = filter_monthes(data, subset)

ndata = group_factor_values(data,'Burst_Date_GU_Children', list(c(10,11,12)))
ndata = group_factor_values(ndata, "Burst_Month", list(c(8,9,10), c(12, 1, 2) ))
ndata = data[subset,]

complete_glm.all = vglm( Burst_Date_GU_Children ~ Burst_Month + Position_A + Position_Ancestor_A + Nature_Ancestor_F , family = cumulative(parallel=TRUE), data = ndata)
print(summary(complete_glm.all))

print('Step')
selected_glm.all = vglm.step(complete_glm.all, ndata)
print(summary(selected_glm.all))

