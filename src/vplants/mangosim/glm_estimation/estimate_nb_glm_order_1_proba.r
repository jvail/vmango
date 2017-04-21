### Import of data :
localdir = getSrcDirectory(function(x) {x})
print(localdir)
if (length(localdir) != 0){
    setwd(localdir)
}

EXCLUDE_FACTORS = NULL
EFACTOR_POST_REMOVAL = TRUE

# input and output directory
share_dir = '../../../../share/'
input_dir = paste(share_dir,'glm_estimate_input/cogshall/', sep="")
output_basedir = paste(share_dir,'glm_output_proba/cogshall/', sep="")

generate_outputdir = function(exclude_factors = EXCLUDE_FACTORS){
  if (is.null(exclude_factors) == TRUE){
    output_dir <<- paste(output_basedir,'allfactors','/',sep='');
  } else {
    output_dir <<- paste(output_basedir,'without_',paste(tolower(exclude_factors),collapse='_and_'),'/',sep='');
  }
  print(output_dir)
  
  if (file.exists(output_dir) == FALSE){
    dir.create(output_dir,recursive=TRUE)
  }
}

generate_outputdir(EXCLUDE_FACTORS)

source("util_glm.r")

NOFILTER = FALSE

############################################################# Summary functions ###########################################################################################################################

mdebug = FALSE
use_fruiting_state = TRUE



vsummary_output = function(glm, data, outfile, subset = NULL, verbose = FALSE, fromglm = NULL){
  if(length(subset) > 0)  ndata = data[subset,]
  else ndata = data
  
  if (mdebug == FALSE) {
    out = file(outfile,open="wt")
    split = FALSE #(verbose >=3)
    sink(file = out, split = split)
  
    print("******** DATA *************")
    print(summary(ndata))
  }
  
  print("******** GLM  *************")
  print(paste("Formula :",glm.formula.text(glm),sep=''))
  if(!is.null(fromglm)) print(paste("Initial :", glm.formula.text(fromglm),sep=''))
    
  if (mdebug == FALSE) {
    print("***** Summary *****")
    print(summary(glm))
    if (!is.vglm(glm)) {
      print("***** Anova *****")
      print(anova(glm))
    }
  }
    
  
  if (is.vglm(glm)){
    print("******** Likelihood  *************")
    print(glm@criterion$loglikelihood)
    print(paste('Estimated',vglm.logLik(glm, ndata)))
  }
    
  if (mdebug == FALSE) {
    sink()
    close(out)
  }
}


general_summary_output = function( data, outfile){
  if (mdebug == FALSE) {
    out = file(outfile,open="wt")
    split = FALSE #(verbose >=3)
    sink(file = out, split = split)
    
    print("******** R *************")
    print(version$version.string)
    
    print("******** DATA *************")
    print(summary(data))

    sink()
    close(out)
  }
}


############################################################### Factors and level manipulation utilities #########################################################################################################################

set_factor = function(data, name) {
  if (name %in% colnames(data))
    data[[name]] = as.factor(data[[name]])
  return (data)
}


burst_month_level = c(6:12,1:5)
children_burst_month_level = c(burst_month_level,burst_month_level+100,burst_month_level+200)


ordered_factors = list(Burst_Month = burst_month_level, 
                       Burst_Date_GU_Children = children_burst_month_level, 
                       Burst_Date_MI_Children = children_burst_month_level, 
                       Flowering_Week = 1:20)

set_data_factors = function(data,  factors) {
  # Assign covariables as factors
  
  for (name in factors) { 
    if (name %in% colnames(data)) {
        if( (name %in% names(ordered_factors))) {
          data[[name]] = factor(data[[name]], ordered = TRUE, levels = intersect(ordered_factors[[name]], unique(data[[name]])) )
        }
        else {
          data[[name]] = factor(data[[name]])
        }
    }
  }
  return(data)
}

knowfactors = c("Burst_Date_GU_Children", "Burst_Month",  "Cycle", "Flowering", "Fruiting", "Has_Apical_GU_Child", "Has_Lateral_GU_Children",  "Nature_Ancestor_F", "Nature_F", "Position_A", "Position_Ancestor_A", "Reiteration", "Tree_Fruit_Load", "Vegetative_Burst", "is_terminal", "Burst_Date_MI_Children", "Has_Apical_MI_Child",  "Has_Lateral_MI_Children", "MixedInflo_Burst",  "Nb_Lateral_MI_Children")

prepare_data_factors = function(data) {
  factors = intersect(names(data), knowfactors)
  data = set_data_factors(data, factors)
  
  if (!use_fruiting_state && 'Nature_Ancestor_F' %in% names(data)) {
    levels(data$Nature_Ancestor_F) = c(0,1,1) 
  }

  if (!use_fruiting_state && 'Nature_F' %in% names(data)) {
    levels(data$Nature_F) = c(0,1,1) 
  }
  #data$Nature_SF = data$Nature_F
  #levels(data$Nature_SF) = c(0,1,1) 

  return (data)
}

filter_factors = function(name, factors, excludes, includes) {
  if (is.null(excludes) && is.null(includes)) { return (factors) }
  
  if (name %in% names(excludes)) { lfactors = setdiff(factors,excludes[[name]]) }
  else { lfactors = factors }
  
  if (name %in% names(includes)) { lfactors = c(includes[[name]],lfactors, recursive=TRUE) }
  
  
  return (lfactors)
}

date_factors = list('Burst_Month', 'Burst_Date_GU_Children', 'Burst_Date_MI_Children', 'Flowering_Week')

select_date_factor = function(factors) {
  for (f in factors){
    if (f %in% date_factors) return (f)
  }
  return (NA)
}

grouping_monthes = function(propname, data, monthgroups, factors) {
  if (is.null(monthgroups)) return (data)
  if (!(propname %in% names(monthgroups))) return (data)

  dfactor = select_date_factor(factors)
  return (group_factor_values(data, dfactor, monthgroups[[propname]]))
}

group_factor_values = function(data, variable, groups) {  
  # group values of a factor
  # Args:
  #   data : The dataframe to modify
  #   variable : the factor name
  #   groups : a list of vectors of values indicating the groups
  # Returns:
  #   The modified dataframe
  
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
    print(paste("Group", variable ,":", paste(levels(data[[variable]]), collapse = " , ")))
  }
  else {
    for (group in groups) {
      groupname = paste(group, collapse="-")
      print(paste("Group", variable ,":", groupname))
      data[[variable]] = replace(data[[variable]], data[[variable]] %in% group, groupname)
    }
    
    data[[variable]] = as.factor(data[[variable]])
  }
  return (data)
}


filter_monthes = function(factors, data, subset, minminalcount = 6) {
  # Remove individuals which occurs in month with not enought individuals.
  # Args:
  #   factors: a set of factors containing the date factors.
  #   data : the dataframe
  #   subset : a selection of individual
  #   minimalcount : the minimal number of individu per month
  # Returns:
  #   An updated subset with removed individuals.
  dfactor = select_date_factor(factors)
  if (!is.na(dfactor)) {
     nsubset = subset
     nbelemmonth = table(data[nsubset,dfactor])
     for (i in 1:length(nbelemmonth)){
       m = names(nbelemmonth)[i]
       count = nbelemmonth[i]
       if(0 < count & count < minminalcount){
         lsubset = data[nsubset,dfactor] != m
         nsubset = nsubset[lsubset]
         print(paste("Filter value", m, "for factor", dfactor , ":", count, '->',length(nsubset) ))
       }
       #else print(paste("Find month",m,":", nbelemmonth, '->',length(nsubset) ))
     }
     return (nsubset)
  } 
  else return (subset)
}

global_factor_filtering = function(factors){
  if (is.null(EXCLUDE_FACTORS) == FALSE){
    if (EXCLUDE_FACTORS == "all") { factors = NULL }
    else if (!is.null(factors)){
      factors = setdiff(factors, EXCLUDE_FACTORS)
      if (length(factors) == 0) 
        factors = NULL
    }
  }
  return (factors)  
}
################################################################## GLM and VGLM generation ######################################################################################################################
gen_formula.text = function(variable, factors) {
  if (is.null(factors)) { mformula = paste(variable," ~ 1", sep="") }
  else { mformula = paste(variable," ~ ", paste(factors,collapse=" + "), sep="") }
  return (mformula)
}

check_factor_variability = function(factors, data, subset) {
  factortoremove = c()
  for (factor in factors){
    if (length(unique(data[subset,factor])) <= 1){
      print(paste(variable,"has no variability for factor ",factor,": ", paste(val,collapse = ',')))
      factortoremove = c(factortoremove, factor)
    }
  }
  if(length(factortoremove) > 0){
    factors = setdiff(factors, factortoremove)
  }
  return (factors)
}

generate_glm = function(variable, family, data, subset, year, verbose = 0, factors = c("Burst_Month","Position_A","Position_Ancestor_A","Nature_Ancestor_F"), tag = NULL)
{
    tracestep = 0

    if (!NOFILTER)
      subset = filter_monthes(factors, data, subset)

    factors = check_factor_variability(factors, data, subset)
    gfactors = factors
    factors = global_factor_filtering(gfactors)
    
    path_complete_glm    = paste(output_dir,"complete_glm/",sep="")
    path_selected_glm    = paste(output_dir,"selected_glm/",sep="")
    path_interaction_glm = paste(output_dir,"interaction_glm/",sep="")

    if (file.exists(path_complete_glm) == FALSE)    dir.create(path_complete_glm,    recursive=TRUE)
    
    fname = tolower(variable)
    if (!is.null(tag)) fname = paste(tag ,'_', fname,sep="")
    basefname = paste(fname, "_", year, sep="")
    


    if (length(unique(data[subset,variable])) <= 1) {
      val = unique(data[subset,variable])
      print(paste(variable,"has no variability : ", paste(val,collapse = ',')))
      print(data[subset,variable])
      
      proba.complete_glm.all = data.frame(probability=val,number=length(data[subset,variable]))
      write.csv(proba.complete_glm.all,file=paste(path_complete_glm, basefname,".csv", sep=""), row.names = FALSE)
      if (!is.null(EXCLUDE_FACTORS) && (EXCLUDE_FACTORS == "all")) { return (NA) } 
      write.csv(proba.complete_glm.all,file=paste(path_selected_glm, basefname,".csv", sep=""), row.names = FALSE)
      write.csv(proba.complete_glm.all,file=paste(path_interaction_glm, basefname,".csv", sep=""), row.names = FALSE)
      
      return (NA)
    }

    mformula = as.formula(gen_formula.text(variable,factors))
    #if (is.null(factors)) { formula = as.formula(paste(variable," ~ 1", sep="")) }
    #else { formula = as.formula(paste(variable," ~ ", paste(factors,collapse=" + "), sep="")) }
    #print("complete glm")
    complete_glm = glm( mformula , family = family, data = data, subset = subset)
    

    #print("complete glm summary")
    vsummary_output(complete_glm, data, paste(path_complete_glm,basefname,"_summary.txt",sep=""), subset = subset, verbose)
    
    proba.complete_glm = glm.proba_and_counts(complete_glm, data, subset)
    

    #print("complete glm proba writting")
    write.csv(proba.complete_glm,file=paste(path_complete_glm, basefname,".csv", sep=""), row.names = FALSE)

    if (!is.null(EXCLUDE_FACTORS) && (EXCLUDE_FACTORS == "all")) { return (NA) } 
    
    ### selected GLM ###
    if (file.exists(path_selected_glm) == FALSE)    dir.create(path_selected_glm,    recursive=TRUE)
    
    #print("selected glm")
    if (is.null(factors)) { selected_glm =  complete_glm }
    else if (!is.null(EXCLUDE_FACTORS) && EFACTOR_POST_REMOVAL) {
      formula = as.formula(gen_formula.text(variable, gfactors))
      complete_glm = glm( formula , family = family, data = data, subset = subset)
      selected_glm = step(complete_glm, trace = tracestep)
      lfactors = glm.factors(selected_glm)
      lfactors = global_factor_filtering(lfactors)
      formula = as.formula(gen_formula.text(variable, lfactors))
      selected_glm = glm( formula , family = family, data = data, subset = subset)
    } else { selected_glm = step(complete_glm, trace = tracestep) }

    #print("selected glm summary")
    vsummary_output(selected_glm, data, paste(path_selected_glm,basefname,"_summary.txt",sep=""), subset, verbose, fromglm = complete_glm)

    #print("selected glm proba")
    proba.selected_glm = glm.proba_and_counts(selected_glm, data, subset)

    #print("selected glm proba writting")
    write.csv(proba.selected_glm,file=paste(path_selected_glm, basefname,".csv", sep=""), row.names = FALSE)
    
    ### interaction GLM ###
    if (file.exists(path_interaction_glm) == FALSE) dir.create(path_interaction_glm, recursive=TRUE)
    
    interaction_glm = glm.test.interactions(selected_glm, data = data, subset = subset, trace = tracestep)

    vsummary_output(interaction_glm, data, paste(path_interaction_glm,basefname,"_summary.txt",sep=""), subset, verbose, fromglm = complete_glm)

    proba.interaction_glm = glm.proba_and_counts(interaction_glm, data, subset)
    
    write.csv(proba.interaction_glm,file=paste(path_interaction_glm, basefname,".csv", sep=""), row.names = FALSE)
}

#
#' Generate the multinomial glm for a process
#
generate_vglm = function(variable, data, subset, year, verbose, factors = c("Burst_Month","Position_A","Position_Ancestor_A","Nature_Ancestor_F"), tag = NULL)
{
    tracestep = 0
    # if (verbose >= 3) tracestep = 0
    
    if (!NOFILTER)
       subset = filter_monthes(factors, data, subset)
    
    gfactors = factors
    factors = global_factor_filtering(gfactors)
    #data = set_data_factors(data, factors)
        
    path_complete_glm    = paste(output_dir,"complete_glm/",sep="")
    path_selected_glm    = paste(output_dir,"selected_glm/",sep="")
    path_interaction_glm = paste(output_dir,"interaction_glm/",sep="")

    if (file.exists(path_complete_glm) == FALSE)    dir.create(path_complete_glm,    recursive=TRUE)
    
    fname = tolower(variable)
    if (!is.null(tag)) fname = paste(tag ,'_', fname,sep="")
    basefname = paste(fname, "_", year,sep="")
    
    ndata = data[subset,]
    
    if (length(unique(ndata[,variable])) <= 1) {
      val = unique(ndata[,variable])
      print(paste(variable,"has no variability : ", val))
      print(ndata[,variable])
      
      proba.complete_glm = data.frame(c(1))
      names(proba.complete_glm) = val
      nbelement.complete_glm = data.frame(length(data[subset,variable]))
      names(nbelement.complete_glm) = val
      write.csv(proba.complete_glm,file=paste(path_complete_glm, basefname,".csv", sep=""), row.names = FALSE)
      write.csv(nbelement.complete_glm,file=paste(path_complete_glm, basefname,"_nbelements.csv", sep=""), row.names = FALSE)
      if (!is.null(EXCLUDE_FACTORS) && (EXCLUDE_FACTORS == "all")) { return (NA) } 
      write.csv(proba.complete_glm,file=paste(path_selected_glm, basefname,".csv", sep=""), row.names = FALSE)
      write.csv(nbelement.complete_glm,file=paste(path_selected_glm, basefname,"_nbelements.csv", sep=""), row.names = FALSE)
      write.csv(proba.complete_glm,file=paste(path_interaction_glm, basefname,".csv", sep=""), row.names = FALSE)
      write.csv(nbelement.complete_glm,file=paste(path_interaction_glm, basefname,"_nbelements.csv", sep=""), row.names = FALSE)
      return (NA)
    }    

    formula = as.formula(gen_formula.text(variable,factors))
    
    if (verbose >= 2) print("complete glm")
    complete_glm = vglm( formula , family = cumulative(parallel=TRUE), data = ndata)

    if (verbose >= 2) print("complete glm proba")
    res = vglm.proba_and_counts (complete_glm, ndata)
    proba.complete_glm = res[[1]]
    nbelement.complete_glm = res[[2]]

    if (verbose >= 2) print("complete glm proba writting")
    write.csv(proba.complete_glm,file=paste(path_complete_glm, basefname,".csv", sep=""), row.names = FALSE)
    write.csv(nbelement.complete_glm,file=paste(path_complete_glm, basefname,"_nbelements.csv", sep=""), row.names = FALSE)

    if (verbose >= 2) print("complete glm summary")
    vsummary_output(complete_glm, ndata, paste(path_complete_glm,basefname,"_summary.txt",sep=""), verbose=verbose)

    if (!is.null(EXCLUDE_FACTORS) && (EXCLUDE_FACTORS == "all")) { return (NA) } 
    
    ### selected GLM ###
    if (file.exists(path_selected_glm) == FALSE)    dir.create(path_selected_glm,    recursive=TRUE)
    
    if (verbose >= 2) print("selected glm")
    if (is.null(factors)) { selected_glm =  complete_glm }
    else if (!is.null(EXCLUDE_FACTORS) && EFACTOR_POST_REMOVAL) {
      formula = as.formula(gen_formula.text(variable, gfactors))
      complete_glm = vglm( formula , family = cumulative(parallel=TRUE), data = ndata)
      selected_glm = vglm.step(complete_glm, data = ndata)
      lfactors = glm.factors(selected_glm)
      lfactors = global_factor_filtering(lfactors)
      formula = as.formula(gen_formula.text(variable, lfactors))
      selected_glm = vglm( formula , family = cumulative(parallel=TRUE), data = ndata)
    }
    else { selected_glm = vglm.step(complete_glm, data = ndata) }

    if (verbose >= 2) print("selected glm proba")
    res = vglm.proba_and_counts (selected_glm, ndata)
    proba.selected_glm = res[[1]]
    nbelement.selected_glm = res[[2]]
    
    if (verbose >= 2) print("selected glm proba writting")
    write.csv(proba.selected_glm,file=paste(path_selected_glm, basefname,".csv", sep=""), row.names = FALSE)
    write.csv(nbelement.selected_glm,file=paste(path_selected_glm, basefname,"_nbelements.csv", sep=""), row.names = FALSE)

    if (verbose >= 2) print("selected glm summary")
    vsummary_output(selected_glm, ndata,  paste(path_selected_glm,basefname,"_summary.txt",sep=""), verbose=verbose, fromglm = complete_glm)

    ### interaction GLM ###
    if (file.exists(path_interaction_glm) == FALSE) dir.create(path_interaction_glm, recursive=TRUE)
    
    if (verbose >= 2) print("interaction glm")
    interaction_glm = glm.test.interactions(selected_glm, data = ndata, trace = tracestep)

    if (verbose >= 2) print("interaction glm proba")
    res = vglm.proba_and_counts (interaction_glm, ndata)
    proba.interaction_glm = res[[1]]
    nbelement.interaction_glm = res[[2]]
    
    if (verbose >= 2) print("interaction glm proba writting")
    write.csv(proba.interaction_glm,file=paste(path_interaction_glm, basefname,".csv", sep=""), row.names = FALSE)
    write.csv(nbelement.interaction_glm,file=paste(path_interaction_glm, basefname,"_nbelements.csv", sep=""), row.names = FALSE)
    
    if (verbose >= 2) print("interaction glm summary")
    vsummary_output(interaction_glm, ndata,  paste(path_interaction_glm,basefname,"_summary.txt",sep=""), verbose=verbose, fromglm = complete_glm)
    
}

################################################################### Determine Developement #####################################################################################################################

#
#' Determine the different probabilities for the vegetative development processes 
#
determine_vegetative_development = function(data, subset_selection, year, yeartag, tag, verbose, 
                                            factors, children_date_test = TRUE, 
                                            exclude = NULL, include = NULL, monthgroups = NULL,
                                            mixedinflo = FALSE, datemultimode = FALSE) {
  if (mixedinflo == FALSE) {
    Burst = "Vegetative_Burst"
    Has_Apical_Child = "Has_Apical_GU_Child"
    Has_Lateral_Children = "Has_Lateral_GU_Children"
    Nb_Lateral_Children = "Nb_Lateral_GU_Children"
    Nb_Children = "Nb_GU_Children"
    Burst_Date_Children = "Burst_Date_GU_Children"
    Burst_Delta_Date_Children = "Burst_Delta_Date_GU_Children"
  }
  else {
    Burst = "MixedInflo_Burst"
    Has_Apical_Child = "Has_Apical_MI_Child"
    Has_Lateral_Children = "Has_Lateral_MI_Children"
    Nb_Lateral_Children = "Nb_Lateral_MI_Children"
    Nb_Children = "Nb_MI_Children"
    Burst_Date_Children = "Burst_Date_MI_Children"
    Burst_Delta_Date_Children = "Burst_Delta_Date_MI_Children"
  }
  
  if (NOFILTER) {
    exclude = NULL
    include = NULL
    monthgroups = NULL
  }
  
  ############################################## Vegetative Burst ##############################################
  if (verbose >= 1) print("########### Estimate Burst") 
  index_loaded = which(subset_selection)

  lfactors = filter_factors(Burst, factors, exclude, include)
  ldata = grouping_monthes(Burst, data, monthgroups, lfactors)
  generate_glm(Burst, family = binomial, data=ldata, subset= index_loaded, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  
  ############################################## Number of GU ############################################## 
  bursted_selection = subset_selection & data[,Burst] == 1 
  
  if (verbose >= 1) print("########### Estimate Number Children ") 
  
  nbchild_selection = bursted_selection 
  index_nbchild = which(nbchild_selection)
  
  if (length(index_nbchild) > 10){
    #On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
    #On enlève donc 1 au nombre de latérales afin de commencer à 0.
    ####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
    data[[Nb_Children]] = data[,Nb_Children] -1
    
    #data$Nb_Lateral_GU_Children = data$Nb_Lateral_GU_Children -1
    
    lfactors = filter_factors(Nb_Children,factors, exclude, include)
    ldata = grouping_monthes(Nb_Children, data, monthgroups, lfactors)
    generate_glm(Nb_Children, family = poisson, data=ldata, subset= index_nbchild, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  }
  
  ############################################## Has_Apical_GU_Child ############################################## 
  if (verbose >= 1) print("########### Estimate Has_Apical_Child")
  
  index_bursted = which(bursted_selection)
  
  lfactors = filter_factors(Has_Apical_Child,factors, exclude, include)
  ldata = grouping_monthes(Has_Apical_Child, data, monthgroups, lfactors)
  generate_glm(Has_Apical_Child, family = binomial, data=ldata, subset= index_bursted, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  
  
  ############################################## Has_Lateral_GU_Children ############################################## 
  if (verbose >= 1) print("########### Estimate Has_Lateral_Children") 
  
  has_lateral_selection = bursted_selection & data[,Has_Apical_Child] == 1 
  #print(ldata[has_lateral_selection,Has_Lateral_Children])
  lfactors = filter_factors(Has_Lateral_Children,factors, exclude, include)
  ldata = grouping_monthes(Has_Lateral_Children, data, monthgroups, lfactors)
  generate_glm(Has_Lateral_Children, family = binomial, data=ldata, subset= has_lateral_selection, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  
  
  ############################################## Number of lateral GU ############################################## 
  
  if (verbose >= 1) print("########### Estimate Number of Lateral Children ") 
  
  lateral_selection = bursted_selection & data[,Has_Lateral_Children] == 1
  index_lateral = which(lateral_selection)
  
  if (length(index_lateral) > 10){
    #On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
    #On enlève donc 1 au nombre de latérales afin de commencer à 0.
    ####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
    data[[Nb_Lateral_Children]] = data[,Nb_Lateral_Children] -1
    #data$Nb_Lateral_GU_Children = data$Nb_Lateral_GU_Children -1
    
    lfactors = filter_factors(Nb_Lateral_Children,factors, exclude, include)
    ldata = grouping_monthes(Nb_Lateral_Children, data, monthgroups, lfactors)
    generate_glm(Nb_Lateral_Children, family = poisson, data=ldata, subset= index_lateral, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  }
  

  ############################################## Burst date of children  ############################################## 
  if (verbose >= 1) print("########### Estimate Burst Date of Children") 
  
  lfactors = filter_factors(Burst_Date_Children,factors, exclude, include)
  ldata = grouping_monthes(Burst_Date_Children, data, monthgroups, lfactors)
  monthes = unique(ldata[[Burst_Date_Children]])
  monthes = monthes[!is.na(monthes)]
  if (length(monthes) >= 1){
    bdate_subset = index_bursted
    bddate_subset = index_bursted
    if (datemultimode) {
      bdate_selection = bursted_selection & data[,'Nature_Ancestor_F'] == 1
      bdate_subset = which(bdate_selection)
      print(lfactors)
      if ('Nature_Ancestor_F' %in% lfactors) {
        lfactors = lfactors[-match('Nature_Ancestor_F',lfactors)]
      }
      print(lfactors)
    }
    generate_vglm(Burst_Date_Children,  data=ldata, subset= bdate_subset, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  
    if (children_date_test) {
      
      ############################################## Delta Burst date of children ############################################## 
      if (verbose >= 1) print("########### Estimate Burst Delta Date of Children with vglm") 
      
      lfactors = filter_factors(Burst_Delta_Date_Children,factors, exclude, include)
      ldata = grouping_monthes(Burst_Delta_Date_Children, data, monthgroups, lfactors)
      ldata[[Burst_Delta_Date_Children]] = factor(ldata[[Burst_Delta_Date_Children]], ordered=TRUE)
      if (datemultimode) {
        bddate_selection = bursted_selection & data[,'Nature_Ancestor_F'] == 0
        bddate_subset = which(bddate_selection)
        if ('Nature_Ancestor_F' %in% lfactors) lfactors = lfactors[-match('Nature_Ancestor_F',lfactors)]
      }
      generate_vglm(Burst_Delta_Date_Children,  data=ldata, subset= bddate_subset, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
      
      
      ############################################## Delta Burst date of children  with poisson   ############################################## 
      if (verbose >= 1) print("########### Estimate Burst Delta Date of Children with poisson") 
      
      Burst_Delta_Date_Children_Poisson = paste(Burst_Delta_Date_Children,"_Poisson",sep="")
      data[[Burst_Delta_Date_Children_Poisson]] = data[,Burst_Delta_Date_Children] - 1
      
      lfactors = filter_factors(Burst_Delta_Date_Children_Poisson,factors, exclude, include)
      ldata = grouping_monthes(Burst_Delta_Date_Children_Poisson, data, monthgroups, lfactors)
      if (datemultimode) {
        if ('Nature_Ancestor_F' %in% lfactors) lfactors = lfactors[-match('Nature_Ancestor_F',lfactors)]
      }
      
      generate_glm(Burst_Delta_Date_Children_Poisson, family = poisson, data=ldata, subset= bddate_subset, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    }
  }
  else {
    print(paste("Not enought different burst date :", monthes, sep='' ))
  }
}


##########################################################Reproductive development ##############################################################################################################################

#
#' Determine the different probabilities for the reproductive development processes 
#
determine_reproductive_development = function(data, subset_selection, year, yeartag, tag, verbose, factors, exclude = NULL, include = NULL, monthgroups = NULL) {
    MinNbGUForGLM = 30
  
    if (NOFILTER) {
      exclude = NULL
      include = NULL
      monthgroups = NULL
    }
    
    ############################################## Flowering  ############################################## 
    if (verbose >= 1) print("########### Estimate Flowering") 
    
    terminal_selection = subset_selection & data$is_terminal == 1
    index_extremity.all = which(terminal_selection)
    
    lfactors = filter_factors("Flowering",factors, exclude, include)
    ldata = grouping_monthes("Flowering", data, monthgroups, lfactors)
    generate_glm("Flowering", family = binomial, data=ldata, subset= index_extremity.all, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    
    
    ############################################## Number of inflorescences  ############################################## 
    if (verbose >= 1) print("########### Estimate Number of inflorescences") 
    
    flowering_selection = subset_selection  & data$Flowering == 1
    flowering_selection[is.na(flowering_selection)] = FALSE
    index_flowering = which(flowering_selection)
    data$Nb_Inflorescences = data$Nb_Inflorescences -1
    
    lfactors = filter_factors("Nb_Inflorescences",factors, exclude, include)
    ldata = grouping_monthes("Nb_Inflorescences", data, monthgroups, lfactors)
    generate_glm("Nb_Inflorescences", family = poisson, data=ldata, subset= index_flowering, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    
    
    ############################################## Date of inflorescences ############################################## 
      if (verbose >= 1) print("########### Estimate Date of inflorescences") 
      
      has_flowering_week_selection = flowering_selection & data$Flowering_Week > 0
      index_has_flowering_week.all = which(has_flowering_week_selection)
      if(length(index_has_flowering_week.all) > MinNbGUForGLM){
        
        lfactors = filter_factors("Flowering_Week",factors, exclude, include)
        ldata = grouping_monthes("Flowering_Week", data, monthgroups, lfactors)
        ldata$Flowering_Week = factor(ldata$Flowering_Week, ordered = TRUE)
        generate_vglm("Flowering_Week",  data=ldata, subset= index_has_flowering_week.all, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
        
        if (verbose >= 1) print("########### Estimate Date of inflorescences with Poisson") 
        data$Flowering_Week_Poisson = data$Flowering_Week - 1
        lfactors = filter_factors("Flowering_Week_Poisson",factors, exclude, include)
        ldata = grouping_monthes("Flowering_Week_Poisson", data, monthgroups, lfactors)
        generate_glm("Flowering_Week_Poisson", family = poisson, data=ldata, subset= index_has_flowering_week.all, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
        
      }
      else {
        print(paste("Not enougth flowering week specified for year",year,":",length(index_has_flowering_week.all)))
      }

    
    ############################################## Fruiting  ############################################## 
    if (verbose >= 1) print("########### Estimate Fruiting") 
    lfactors = filter_factors("Fruiting",factors, exclude, include)
    if ('Flowering_Week' %in% lfactors) {
        data$Flowering_Week = factor(data$Flowering_Week, ordered = TRUE)
    }
    ldata = grouping_monthes("Fruiting", data, monthgroups, lfactors)
    generate_glm("Fruiting", family = binomial, data=ldata, subset= index_flowering, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    
    
    ############################################## Number of fruits  ############################################## 
    if (verbose >= 1) print("########### Estimate Number of fruits") 
    
    fruiting_selection = subset_selection & data$Fruiting == 1
    index_fruiting = which(fruiting_selection)
    data$Nb_Fruits = data$Nb_Fruits -1
    
    lfactors = filter_factors("Nb_Fruits",factors, exclude, include)
    if ('Nb_Inflorescences' %in% lfactors) {
      data$Nb_Inflorescences = factor(data$Nb_Inflorescences+1, ordered = TRUE)
    }
    
    ldata = grouping_monthes("Nb_Fruits", data, monthgroups, lfactors)
    if(length(index_fruiting) > MinNbGUForGLM){
      generate_glm("Nb_Fruits", family = poisson, data=ldata, subset= index_fruiting, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    }
    else {
      print(paste("Not enougth fruits specified for year",year,":",length(index_fruiting)))
    }

    ############################################## Weigth of fruits  ############################################## 
    if (verbose >= 1) print("########### Estimate Weight of fruits") 
    
    lfactors = filter_factors("Fruit_Weight",factors, exclude, include)
    ldata = grouping_monthes("Fruit_Weight", data, monthgroups, lfactors)
    if(length(index_fruiting) > MinNbGUForGLM){
      generate_glm("Fruit_Weight", family = gaussian, data=ldata, subset= index_fruiting, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    }
    else {
      print(paste("Not enougth fruits specified for year",year,":",length(index_fruiting)))
    }
    
    ############################################## Harvest Date of fruits  ############################################## 
    if (verbose >= 1) print("###########  Estimate Harvest Date of fruits") 
    
    has_harvest_week_selection = fruiting_selection & data$Harvest_Week > 0
    index_has_harvest_week = which(has_harvest_week_selection)
    if(length(index_has_harvest_week) > MinNbGUForGLM){
      

      lfactors = filter_factors("Harvest_Week",factors, exclude, include)
      ldata = grouping_monthes("Harvest_Week", data, monthgroups, lfactors)
      ldata$Harvest_Week = factor(ldata$Harvest_Week, ordered = TRUE)
      generate_vglm("Harvest_Week",  data=ldata, subset= index_has_harvest_week, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
      
      if (verbose >= 1) print("########### Estimate Harvest Date of fruits with Poisson") 
      data$Harvest_Week_Poisson = data$Harvest_Week - 1
      lfactors = filter_factors("Harvest_Week_Poisson",factors, exclude, include)
      ldata = grouping_monthes("Harvest_Week_Poisson", data, monthgroups, lfactors)
      generate_glm("Harvest_Week_Poisson", family = poisson, data=ldata, subset= index_has_harvest_week, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
      
    }
    else {
      print(paste("Not enougth harvest week specified for year",year,":",length(index_has_harvest_week)))
    }
    
    if (verbose >= 1) print("Done")
}


##################################################################### Within Cycle ###################################################################################################################


determining_glm_tables_within_cycle = function(data, year, verbose = 0, selection = NULL, tag = NULL, factors = c( "Burst_Month", "Position_A","Position_Ancestor_A","Nature_Ancestor_F")) {
  
  yeartag = paste("within_",year,sep="")
  
  # data = prepare_data_factors(data)
  levels(data$Nature_Ancestor_F) = c(0,1,1) 
  
  # summary(data)
  if (is.null(selection)) { subset_selection = data$Tree_Fruit_Load == 1 }
  else {                    subset_selection = selection & data$Tree_Fruit_Load == 1 }
  
  if (verbose >= 1) print(paste("*** Cycle : ",year,"***")) 
  if (verbose >= 1) print(paste("*** Factors : ",paste(factors,collapse=" , "),"***"))
  
  exclude = list("Has_Apical_GU_Child" = c("Position_Ancestor_A"),
                 "Has_Lateral_GU_Children" = c("Position_Ancestor_A", "Nature_Ancestor_F"),
                 "Nb_Inflorescences" = c("Position_Ancestor_A"),
                 "Fruiting" = c('Nature_Ancestor_F'),
                 "Nb_Fruits" = c("Position_Ancestor_A")
                 )
  
  include = list(   )
  
  monthgroups = list() #Vegetative_Burst = list(1:2,3:5,6:11))

  datemultimode = FALSE
  
  if (year == '04') {
    #monthgroups$Has_Lateral_GU_Children = list( c(10,11,12,1))
    exclude$Vegetative_Burst =  c("Nature_Ancestor_F")
    exclude$Has_Apical_GU_Child = append(exclude[["Has_Apical_GU_Child"]],  c("Nature_Ancestor_F", "Burst_Month"))
    monthgroups$Burst_Date_GU_Children = list(c(9,10) )
    monthgroups$Burst_Delta_Date_GU_Children = list(c(9,10) )
    exclude$Burst_Delta_Date_GU_Children =  c("Position_A")
    monthgroups$Burst_Delta_Date_GU_Children_Poisson = list(c(9,10) )
    
    #exclude$Fruiting = append(exclude[["Fruiting"]],  "Burst_Month")
    #exclude$Nb_Fruits = append(exclude[["Nb_Fruits"]],  "Nature_Ancestor_F")
    #exclude$Flowering =  c("Nature_Ancestor_F")
    #include$Flowering =  c("Nature_Ancestor_SF")
    monthgroups$Vegetative_Burst = list(1:2,3:5,9:11)
    
    monthgroups$Flowering = list(c(12,1,2,3,4))
    monthgroups$Nb_Inflorescences = list(c(1,2,3,4,5) )
    monthgroups$Fruiting = list(c(1,2), c(3,4,5) )
    monthgroups$Fruit_Weigth = list(c(1,2), c(3,4,5) )
    
    include$Nb_Fruits = c("Nb_Inflorescences")
    
  }

  if (year == '05') {
    monthgroups = list(Vegetative_Burst = list(6:12,1:2))
    #exclude$Vegetative_Burst =  c("Position_A")
    exclude$Has_Apical_GU_Child = append(exclude[["Has_Apical_GU_Child"]],  c("Burst_Month"))
    
    #monthgroups$Has_Lateral_GU_Children = list( c(10,11,12,1), c(2,3))
    monthgroups$Has_Lateral_GU_Children = list( c(8,9), c(10,11,12,1))
    monthgroups$Nb_Lateral_GU_Children = list( 8:12, c(1,2))
    
    monthgroups$Burst_Date_GU_Children = list(c(8,9,10), c(12, 1, 2) )
    monthgroups$Burst_Delta_Date_GU_Children = list( c(8,9,10), c(12, 1, 2) )
    #exclude$Burst_Date_Children = c("Position_A")
    monthgroups$Burst_Delta_Date_GU_Children_Poisson = list( c(10, 11) )

    exclude$Burst_Date_GU_Children = c("Position_Ancestor_A")
    #if(!NOFILTER)
    #  data = group_factor_values(data,'Burst_Date_GU_Children', list(c(10,11,12)))  
    exclude$Burst_Delta_Date_GU_Children = c("Position_Ancestor_A")
    #data = group_factor_values(data,'Burst_Delta_Date_GU_Children', list(c(1,2),c(5,6)))  
    
    
    monthgroups$Flowering = list( c(8,9), c(11,12,1),c(2,3))
    
    exclude$Flowering_Week_Poisson = c("Position_Ancestor_A")
    monthgroups$Flowering_Week = list( c(2,3))

    #exclude$Nb_Inflorescences = c("Burst_Month")
    #include$Nb_Inflorescences = c("Flowering_Week")
    # ne sort pas Flowering_Week
    monthgroups$Nb_Inflorescences = list( c(8,9,10,11))


    #exclude$Fruiting = append(exclude[["Fruiting"]],  "Position_A")
    #monthgroups$Fruiting = list(c(1,2) )
    #exclude$Fruiting = append(exclude[["Fruiting"]],  c("Burst_Month"))
    include$Fruiting =  c("Flowering_Week")
    monthgroups$Fruiting = list( 1:6, 7:10, 11:12) # A corriger !!!!!

    #exclude$Fruit_Weight =  c("Nature_Ancestor_F")
    #include$Nb_Fruits = c("Flowering_Week")
    include$Nb_Fruits = c("Nb_Inflorescences")
    include$Fruit_Weight = c("Flowering_Week")
    include$Harvest_Week = c("Flowering_Week","Nb_Inflorescences")
    exclude$Harvest_Week = c("Burst_Month")
    include$Harvest_Week_Poisson = c("Flowering_Week","Nb_Inflorescences")
    exclude$Harvest_Week_Poisson = c("Burst_Month")
    # ne marche pas : pas assez d'éléments ?
    datemultimode = FALSE # TRUE
  }
  
  if (year == '0405') {
    exclude$Vegetative_Burst = append(exclude[["Vegetative_Burst"]],  c("Position_Ancestor_A"))
    monthgroups$Has_Apical_GU_Child = list( c(1,2)) #, c(11,12))
    #monthgroups$Has_Lateral_GU_Children = list(c(8,9), c(10,11,12), c(1,2))
    monthgroups$Nb_Lateral_GU_Children = list(c(8,9))
    monthgroups$Burst_Date_GU_Children = list( c(1, 2) )
    monthgroups$Burst_Delta_Date_GU_Children = list( c( 10, 11, 12) )
    monthgroups$Burst_Delta_Date_GU_Children_Poisson = list( c( 10, 11, 12), c(1,2) )
    
    monthgroups$Flowering = list( c(8,9,10,11), c(12,1),c(2,3,4))
    monthgroups$Nb_Inflorescences = list( c(8,9,10,11), c(1,2))
    monthgroups$Flowering_Week = list( c(8,9,10,11), c(2,3))
    exclude$Flowering_Week = c("Position_Ancestor_A")
    monthgroups$Fruiting = list( c(8,9,10,11), c(12,1,2), c(3,4))
  }  
  
  #exclude = NULL
  #monthgroups = NULL
  #include = NULL
  
  print("determine_vegetative_development")
  determine_vegetative_development(data=data, subset_selection= subset_selection,
                                  year=year, yeartag= yeartag, tag = tag, verbose = verbose,
                                  factors = factors, exclude = exclude, include = include,
                                  monthgroups=monthgroups, datemultimode = datemultimode)
  
  print("determine_reproductive_development")
  determine_reproductive_development(data=data, subset_selection= subset_selection, year=year, yeartag= yeartag, tag = tag, verbose = verbose, factors = factors, exclude = exclude, include = include, monthgroups=monthgroups)    
  
}

determining_glm_tables_mixed_inflo_within_cycle = function(data, year, verbose = 0, selection = NULL, tag = NULL, factors = c( "Position_Ancestor_A","Cycle")) {
  # Position_A : 59 10 
  # Position_Ancestor_A : 36 28
  # Nature_Ancestor_F : 43 17 ???
  yeartag = paste("within_",year,sep="")
  
  # data = prepare_data_factors(data)
  
  data$Flowering_Week = factor(data$Flowering_Week)
  
  # summary(data)
  if (is.null(selection)) { subset_selection = data$Tree_Fruit_Load == 1 }
  else {                    subset_selection = selection & data$Tree_Fruit_Load == 1 }
  
  if (verbose >= 1) print(paste("*** Cycle : ",year," Mixed Inflorescences ***")) 
  if (verbose >= 1) print(paste("*** Factors : ",paste(factors,collapse=" , "),"***"))
  
  exclude = NULL
  # Testing 
  include = list(#"Has_Lateral_GU_Children" = c("Has_Apical_GU_Child")
                 #,"Nb_Lateral_GU_Children" = c("Has_Apical_GU_Child")
                 ) 
  monthgroups = list()
  
  
  
  determine_vegetative_development(data=data, subset_selection= subset_selection, year=year, yeartag= yeartag, 
                                   tag = tag, verbose = verbose, factors = factors, 
                                   exclude = exclude, include = include, monthgroups= monthgroups)

}

###################################################################### Between Cycles ##################################################################################################################

determining_glm_tables_between_cycle = function(data, year, verbose = FALSE, selection = NULL, tag = NULL) {

    yeartag = paste("between_",year,sep="")
    
    factors = c("Position_A","Nature_F")  # "Tree_Fruit_Load"
    if ("Burst_Month" %in% colnames(data)) factors = c("Burst_Month", factors)
    
    tracestep = 0
    if (verbose >= 3) tracestep = 1

    if (is.null(selection)) { subset_selection = data$Tree_Fruit_Load == 1 }
    else {                    subset_selection = selection & data$Tree_Fruit_Load == 1 }

    if (verbose >= 1) print(paste("*** Between Cycles",year,"***")) 
    if (verbose >= 1) print(paste("*** Factors : ",paste(factors,collapse=" , "),"***"))

    exclude = list()
    
    include = list(#"Has_Lateral_GU_Children" = c("Has_Apical_GU_Child"),
                   "Nb_Lateral_GU_Children" = c("Has_Apical_GU_Child")) 
    
    monthgroups = list()
    
    if (year == '03to0405') {
      if (!NOFILTER) 
        data = group_factor_values(data,'Burst_Date_GU_Children', list(c(111,112),c(101,102,103,104),c(209:212,201:202)))
    
    }
    if (year == '04to05') {
      monthgroups$Vegetative_Burst = list( c(9,10), c(1,2,3,4,5))
      exclude$Has_Apical_GU_Child = c('Position_A')
      monthgroups$Has_Apical_GU_Child = list( c(10,11,12), c(1,2), c(3,4,5))
      monthgroups$Has_Lateral_GU_Children = list( c(3,4,5))
      monthgroups$Nb_Lateral_GU_Children = list(c(10,11,12), c(3,4,5))
      monthgroups$Burst_Date_GU_Children = list(c(10,11,12), c(3,4,5))

      #if (!NOFILTER) 
      #  data = group_factor_values(data,'Burst_Date_GU_Children', list(c(110,111,112)))
    }
    
    determine_vegetative_development(data=data, subset_selection= subset_selection, year=year, yeartag= yeartag, 
                                     tag = tag, verbose = verbose, factors = factors, 
                                     exclude = exclude, include = include, monthgroups= monthgroups)

}

determining_glm_tables_mixed_inflo_between_cycle = function(data, year, verbose = FALSE, selection = NULL, tag = NULL) {
  
  yeartag = paste("between_",year,sep="")

  factors = c("Position_A")
  if ("Burst_Month" %in% colnames(data)) factors = c("Burst_Month", factors)
  #else factors = NULL
  
  exclude = list()
  
  include = list()
                 #"Has_Lateral_GU_Children" = c("Has_Apical_GU_Child"),
                 #"Nb_Lateral_GU_Children" = c("Has_Apical_GU_Child")) 
  
  monthgroups = list()
  
  if (year == '04to05') {
      monthgroups$MixedInflo_Burst = list(9:12,1:3,4:5)
  }
  

  tracestep = 0
  if (verbose >= 3) tracestep = 1
  
  if (is.null(selection)) { subset_selection = data$Tree_Fruit_Load == 1 }
  else {                    subset_selection = selection & data$Tree_Fruit_Load == 1 }
  
  if (verbose >= 1) print(paste("*** Between Cycles",year,"Mixed Inflorescences ***")) 
  if (verbose >= 1) print(paste("*** Factors : ",paste(factors,collapse=" , "),"***"))
  
  determine_vegetative_development(data=data, subset_selection = subset_selection, year=year, yeartag= yeartag, 
                                   tag = tag, verbose = verbose, factors = factors, 
                                   exclude = exclude, include = include, monthgroups= monthgroups,
                                   mixedinflo = TRUE)
  
}


####################################################################### Import of data and launch of scripts #################################################################################################################

determining_glm_tables_within_cycle_for_year = function(input_dir, year = NULL, verbose = 0) {
  data = read.csv(paste(input_dir,"table_within_cycle_",year,".csv",sep=""), header = TRUE)
  data = prepare_data_factors(data)
  
  general_summary_output(data,paste(output_dir,"info_table_within_",year,".txt",sep=""))
  determining_glm_tables_within_cycle(data, year, verbose = verbose, selection=data$Mixed_Inflo == 0, tag='gu')
}

determining_glm_tables_mixedinflo_within_cycle_for_year = function(input_dir, year = NULL, verbose = 0) {
  data = read.csv(paste(input_dir,"table_within_cycle_",year,".csv",sep=""), header = TRUE)
  data = prepare_data_factors(data)
  
  general_summary_output(data,paste(output_dir,"info_table_within_",year,".txt",sep=""))
  determining_glm_tables_mixed_inflo_within_cycle(data, year, verbose = verbose, selection=data$Mixed_Inflo == 1, tag='mi')
}



determining_glm_tables_between_cycle_for_year = function(input_dir, year, verbose = 0) {
    data = read.csv(paste(input_dir,"table_between_cycle_",year,".csv",sep=""), header = TRUE)
    data = prepare_data_factors(data)
    general_summary_output(data,paste(output_dir,"info_table_between_",year,".txt",sep=""))
    if (length(which(is.na(data$Burst_Month))) == length(data$Burst_Month)) data$Burst_Month = NULL
    determining_glm_tables_between_cycle(data, year, verbose, tag='gu')
  
    determining_glm_tables_mixed_inflo_between_cycle(data, year, verbose, tag='gu')
}

####################### MAIN #######################################@

#print("start")

test = function() {
  data = read.csv(paste(input_dir,"table_within_cycle_04.csv",sep=""),header = TRUE)
  #data = read.csv(paste(input_dir,"table_between_cycle_03to0405.csv",sep=""),header = TRUE)
  
  #data = prepare_data_factors(data, month_cycle_order)
  data$Burst_Month = as.factor(data$Burst_Month)
  data$Burst_Month = ordered(data$Burst_Month, levels = month_cycle_order)
  
  subset_selection = data$Tree_Fruit_Load == 1
  subset= which(subset_selection)
  
  factors = c("Burst_Month", "Position_A" , "Position_Ancestor_A" , "Nature_Ancestor_F")
  
  #generate_glm("Vegetative_Burst", family = binomial, data=data, subset= subset, year= "04", verbose = 1, factors = factors)

  ndata = data[subset,]
  
  formula = paste(variable," ~ ", paste(factors,collapse=" + "),sep="")
  formula = as.formula(formula)
  myglm = glm( formula , family = binomial, data = ndata)
  summary(myglm)
  myglm = glm( ndata$Vegetative_Burst ~ ndata$Burst_Month , family = binomial, data = ndata)
  summary(myglm)
  res = vglm.proba_and_counts (myglm, ndata)
  
  
  #generate_vglm("Burst_Delta_Date_Children",  data=data, subset= index_bursted.all, year= "04", verbose = verbose, factors = totfactors)
  
  bursted_selection = subset_selection & data$Vegetative_Burst == 1
  index_bursted.all = which(bursted_selection)
  subset= index_bursted.all
  ndata = data[subset,]
  
  formula2 = as.formula("Burst_Delta_Date_GU_Children ~ Burst_Month + Position_A + Burst_Month:Position_A")
  myglm2 = vglm( formula2 , family = cumulative(parallel=TRUE), data = ndata)
  res2 = vglm.proba_and_counts (myglm2, ndata)
}

main = function() {
  verbose = 1
  determining_glm_tables_within_cycle_for_year(input_dir, "04",   verbose)
  #determining_glm_tables_mixedinflo_within_cycle_for_year(input_dir, "0405", verbose)
  #determining_glm_tables_within_cycle_for_year(input_dir, "05",   verbose)
  
  #determining_glm_tables_between_cycle_for_year(input_dir, "03to0405", verbose)
  #determining_glm_tables_between_cycle_for_year(input_dir, "04to05",   verbose)
}

main()

gen_constraint_glm = function() {
  initialvalue = EXCLUDE_FACTORS
  EXCLUDE_FACTORS <<- c('Nature_F','Nature_Ancestor_F')
  generate_outputdir(EXCLUDE_FACTORS)
  main()
  for (ef in c('Burst_Month','Position_A','Position_Ancestor_A','all')) {
    EXCLUDE_FACTORS <<- ef
    generate_outputdir(ef)
    main()
  }
  EXCLUDE_FACTORS <<- initialvalue
}

gen_constraint_glm()
