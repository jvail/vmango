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
if (file.exists(output_dir) == FALSE){
    dir.create(output_dir,recursive=TRUE)
}

source("util_glm.r")


########################################################################################################################################################################################

mdebug = FALSE

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
    print(summary(glm))
    # print(glht(glm))
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


########################################################################################################################################################################################

set_factor = function(data, name) {
  if (name %in% colnames(data))
    data[[name]] = as.factor(data[[name]])
  return (data)
}


children_burst_date_level = c("6","7","8","9","10","11","12","1","2","3","4","5",
                              "106","107","108","109","110","111","112","101","102","103","104","105",
                              "206","207","208","209","210","211","212","201","202","203","204","205")


set_data_factors = function(data,  factors) {
  # Assign covariables as factors
  

  for (name in factors) { #c('Burst_Semester', 'Position_A', 'Position_Ancestor_A', 'Nature_F', 'Nature_Ancestor_F', 'Mixed_Inflo', 'Tree_Fruit_Load', 'Cycle')){
    if (name %in% colnames(data) && name != "Burst_Date_GU_Children") data[[name]] = as.factor(data[[name]])
  }
  
  if ("Burst_Date_GU_Children" %in% factors) {
    data$Burst_Date_GU_Children = ordered(data$Burst_Date_GU_Children, levels = children_burst_date_level)
    data$Burst_Date_GU_Children = factor(data$Burst_Date_GU_Children)
  }
  
  return(data)
  
}

filter_factors = function(name, factors, excludes, includes) {
  if (is.null(excludes) && is.null(includes)) { return (factors) }
  
  if (name %in% names(excludes)) { lfactors = setdiff(factors,excludes[[name]]) }
  else { lfactors = factors }
  
  if (name %in% names(includes)) { lfactors = c(includes[[name]],lfactors, recursive=TRUE) }
  
  return (lfactors)
}

grouping_monthes = function(propname, data, monthgroups) {
  if (is.null(monthgroups)) return (data)
  if (!(propname %in% names(monthgroups))) return (data)

  for (group in monthgroups[[propname]]) {
    print(paste("Group month :", paste(group, collapse=' , ')))
    data$Burst_Month = replace(data$Burst_Month, data$Burst_Month %in% group, paste(group, collapse="-"))
  }
  
  data$Burst_Month = as.factor(data$Burst_Month)

  return (data)
}


check_month_data = function(factors, data, subset) {
  #return (subset)
  if ("Burst_Month" %in% factors){
     nsubset = subset
     for (m in 1:12){
       nbelemmonth = length(which(data[nsubset,]$Burst_Month == m))
       if(0 < nbelemmonth & nbelemmonth <= 5){
         lsubset = data[nsubset,]$Burst_Month != m
         nsubset = nsubset[lsubset]
         print(paste("Remove month",m,":", nbelemmonth, '->',length(nsubset) ))
       }
       #else print(paste("Find month",m,":", nbelemmonth, '->',length(nsubset) ))
     }
     return (nsubset)
  } 
  else return (subset)
}

########################################################################################################################################################################################

generate_glm = function(variable, family, data, subset, year, verbose = 0, factors = c("Burst_Month","Position_A","Position_Ancestor_A","Nature_Ancestor_F"), tag = NULL, exclude = NULL)
{
    tracestep = 0
    # if (verbose >= 3) tracestep = 0
    
    subset = check_month_data(factors, data, subset)
    
    #if ("Burst_Month" %in% colnames(data)) {
    #if ("Burst_Month" %in% factors) {
    #  data$Burst_Month = ordered(data$Burst_Month, levels = month_cycle_order)
    #  data$Burst_Month = as.factor(data$Burst_Month)
    #}
    
    data = set_data_factors(data, factors)
    
    path_complete_glm = paste(output_dir,"complete_glm/",sep="")
    path_selected_glm = paste(output_dir,"selected_glm/",sep="")

    if (file.exists(path_complete_glm) == FALSE) dir.create(path_complete_glm, recursive=TRUE)
    if (file.exists(path_selected_glm) == FALSE) dir.create(path_selected_glm, recursive=TRUE)

    fname = tolower(variable)
    if (!is.null(tag)) fname = paste(fname, '_', tag,sep="")
    basefname = paste(fname, "_", year, sep="")
    
    if (is.null(factors)) { formula = as.formula(paste(variable," ~ 1", sep="")) }
    else { formula = as.formula(paste(variable," ~ ", paste(factors,collapse=" + "), sep="")) }
    
    
    #print("complete glm")
    complete_glm.all = glm( formula , family = family, data = data, subset = subset)

    #print("complete glm summary")
    vsummary_output(complete_glm.all, data, paste(path_complete_glm,basefname,"_summary.txt",sep=""), subset = subset, verbose)

    #print("complete glm proba")
    proba.complete_glm.all = glm.proba_and_counts(complete_glm.all, data, subset)

    #print("complete glm proba writting")
    write.csv(proba.complete_glm.all,file=paste(path_complete_glm, basefname,".csv", sep=""), row.names = FALSE)

    ### selected GLM ###
    #print("selected glm")
    selected_glm.all = step(complete_glm.all, trace = tracestep)   

    #print("selected glm summary")
    vsummary_output(selected_glm.all, data, paste(path_selected_glm,basefname,"_summary.txt",sep=""), subset, verbose, fromglm = complete_glm.all)

    #print("selected glm proba")
    proba.selected_glm.all = glm.proba_and_counts(selected_glm.all, data, subset)

    #print("selected glm proba writting")
    write.csv(proba.selected_glm.all,file=paste(path_selected_glm, basefname,".csv", sep=""), row.names = FALSE)

}

########################################################################################################################################################################################

#
#' Generate the multinomial glm for a process
#
generate_vglm = function(variable, data, subset, year, verbose, factors = c("Burst_Month","Position_A","Position_Ancestor_A","Nature_Ancestor_F"), tag = NULL, exclude = NULL)
{
    tracestep = 0
    # if (verbose >= 3) tracestep = 0

    subset = check_month_data(factors, data, subset)
    
    data = set_data_factors(data, factors)
    
        
    path_complete_glm = paste(output_dir,"complete_glm/",sep="")
    path_selected_glm = paste(output_dir,"selected_glm/",sep="")

    if (file.exists(path_complete_glm) == FALSE) dir.create(path_complete_glm,recursive=TRUE)
    if (file.exists(path_selected_glm) == FALSE) dir.create(path_selected_glm,recursive=TRUE)

    fname = tolower(variable)
    if (!is.null(tag)) fname = paste(fname, '_', tag,sep="")
    basefname = paste(fname, "_", year,sep="")
    
    if (is.null(factors)) { formula = as.formula(paste(variable," ~ 1", sep="")) }
    else { formula = as.formula(paste(variable," ~ ", paste(factors,collapse=" + "), sep="")) }

    ndata = data[subset,]
    
    if (verbose >= 2) print("complete glm")
    complete_glm.all = vglm( formula , family = cumulative(parallel=TRUE), data = ndata)

    if (verbose >= 2) print("complete glm proba")
    res = vglm.proba_and_counts (complete_glm.all, ndata)
    proba.complete_glm.all = res[[1]]
    nbelement.complete_glm.all = res[[2]]

    if (verbose >= 2) print("complete glm proba writting")
    write.csv(proba.complete_glm.all,file=paste(path_complete_glm, basefname,".csv", sep=""), row.names = FALSE)
    write.csv(nbelement.complete_glm.all,file=paste(path_complete_glm, basefname,"_nbelements.csv", sep=""), row.names = FALSE)

    if (verbose >= 2) print("complete glm summary")
    vsummary_output(complete_glm.all, ndata, paste(path_complete_glm,basefname,"_summary.txt",sep=""), verbose=verbose)
    
    ### selected GLM ###

    if (verbose >= 2) print("selected glm")
    #selected_glm.all = complete_glm.all
    selected_glm.all = vglm.step(complete_glm.all, data = ndata)

    if (verbose >= 2) print("selected glm proba")
    res = vglm.proba_and_counts (selected_glm.all, ndata)
    proba.selected_glm.all = res[[1]]
    nbelement.selected_glm.all = res[[2]]
    
    if (verbose >= 2) print("selected glm proba writting")
    write.csv(proba.selected_glm.all,file=paste(path_selected_glm, basefname,".csv", sep=""), row.names = FALSE)
    write.csv(nbelement.selected_glm.all,file=paste(path_selected_glm, basefname,"_nbelements.csv", sep=""), row.names = FALSE)

    if (verbose >= 2) print("selected glm summary")
    vsummary_output(selected_glm.all, ndata,  paste(path_selected_glm,basefname,"_summary.txt",sep=""), verbose=verbose, fromglm = complete_glm.all)
    
}
########################################################################################################################################################################################

#
#' Determine the different probabilities for the vegetative development processes 
#
determine_vegetative_development = function(data, subset_selection, year, yeartag, tag, verbose, 
                                            factors, children_date_test = TRUE, 
                                            exclude = NULL, include = NULL, monthgroups = NULL,
                                            mixedinflo = FALSE) {
  if (mixedinflo == FALSE) {
    Burst = "Vegetative_Burst"
    Has_Apical_Child = "Has_Apical_GU_Child"
    Has_Lateral_Children = "Has_Lateral_GU_Children"
    Nb_Lateral_Children = "Nb_Lateral_GU_Children"
    Burst_Date_Children = "Burst_Date_GU_Children"
    Burst_Delta_Date_Children = "Burst_Delta_Date_GU_Children"
  }
  else {
    Burst = "MixedInflo_Burst"
    Has_Apical_Child = "Has_Apical_MI_Child"
    Has_Lateral_Children = "Has_Lateral_MI_Children"
    Nb_Lateral_Children = "Nb_Lateral_MI_Children"
    Burst_Date_Children = "Burst_Date_MI_Children"
    Burst_Delta_Date_Children = "Burst_Delta_Date_MI_Children"
  }
  
  ##############################################
  #### Vegetative Burst
  ##############################################
  if (verbose >= 1) print("########### Estimate Burst") 
  index_loaded = which(subset_selection)

    

  lfactors = filter_factors(Burst, factors, exclude, include)
  ldata = grouping_monthes(Burst, data, monthgroups)
  generate_glm(Burst, family = binomial, data=ldata, subset= index_loaded, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  
  ##############################################
  #### Has_Apical_GU_Child
  ##############################################
  if (verbose >= 1) print("########### Estimate Has_Apical_Child")
  
  bursted_selection = subset_selection & data[,Burst] == 1
  index_bursted = which(bursted_selection)
  
  lfactors = filter_factors(Has_Apical_Child,factors, exclude, include)
  ldata = grouping_monthes(Has_Apical_Child, data, monthgroups)
  generate_glm(Has_Apical_Child, family = binomial, data=ldata, subset= index_bursted, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  
  
  ##############################################
  #### Has_Lateral_GU_Children
  ##############################################
  if (verbose >= 1) print("########### Estimate Has_Lateral_Children") 
  
  lfactors = filter_factors(Has_Lateral_Children,factors, exclude, include)
  ldata = grouping_monthes(Has_Lateral_Children, data, monthgroups)
  generate_glm(Has_Lateral_Children, family = binomial, data=ldata, subset= index_bursted, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  
  
  ##############################################
  #### Number of lateral GU
  ##############################################
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
    ldata = grouping_monthes(Nb_Lateral_Children, data, monthgroups)
    generate_glm(Nb_Lateral_Children, family = poisson, data=ldata, subset= index_lateral, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  }
  
  ##############################################
  #### Burst date of children    
  ##############################################
  if (verbose >= 1) print("########### Estimate Burst Date of Children") 
  
  lfactors = filter_factors(Burst_Date_Children,factors, exclude, include)
  ldata = grouping_monthes(Burst_Date_Children, data, monthgroups)
  monthes = unique(ldata[[Burst_Date_Children]])
  monthes = monthes[!is.na(monthes)]
  print(monthes)
  if (length(monthes) >= 2){
    generate_vglm(Burst_Date_Children,  data=ldata, subset= index_bursted, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
  
    if (children_date_test) {
      
      ##############################################
      #### Delta Burst date of children    
      ##############################################
      if (verbose >= 1) print("########### Estimate Burst Delta Date of Children with vglm") 
      
      lfactors = filter_factors(Burst_Delta_Date_Children,factors, exclude, include)
      ldata = grouping_monthes(Burst_Delta_Date_Children, data, monthgroups)
      generate_vglm(Burst_Delta_Date_Children,  data=ldata, subset= index_bursted, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
      
      
      ##############################################
      #### Delta Burst date of children  with poisson  
      ##############################################
      if (verbose >= 1) print("########### Estimate Burst Delta Date of Children with poisson") 
      
      Burst_Delta_Date_Children_Poisson = paste(Burst_Delta_Date_Children,"_Poisson",sep="")
      data[[Burst_Delta_Date_Children_Poisson]] = data[,Burst_Delta_Date_Children] - 1
      
      lfactors = filter_factors(Burst_Delta_Date_Children_Poisson,factors, exclude, include)
      ldata = grouping_monthes(Burst_Delta_Date_Children_Poisson, data, monthgroups)
      generate_glm(Burst_Delta_Date_Children_Poisson, family = poisson, data=ldata, subset= index_bursted, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    }
  }
  else {
    print(paste("Not enought different burst date :", monthes, sep='' ))
  }
}


########################################################################################################################################################################################

#
#' Determine the different probabilities for the reproductive development processes 
#
determine_reproductive_development = function(data, subset_selection, year, yeartag, tag, verbose, factors, exclude = NULL, include = NULL, monthgroups = NULL) {
    MinNbGUForGLM = 30
  
    ##############################################
    #### Flowering 
    ##############################################
    if (verbose >= 1) print("########### Estimate Flowering") 
    
    terminal_selection = subset_selection & data$is_terminal == 1
    index_extremity.all = which(terminal_selection)
    
    lfactors = filter_factors("Flowering",factors, exclude, include)
    ldata = grouping_monthes("Flowering", data, monthgroups)
    generate_glm("Flowering", family = binomial, data=ldata, subset= index_extremity.all, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    
    
    ##############################################
    #### Number of inflorescences 
    ##############################################
    if (verbose >= 1) print("########### Estimate Number of inflorescences") 
    
    flowering_selection = subset_selection & data$Flowering == 1
    index_flowering.all = which(flowering_selection)
    data$Nb_Inflorescences = data$Nb_Inflorescence -1
    
    lfactors = filter_factors("Nb_Inflorescences",factors, exclude, include)
    ldata = grouping_monthes("Nb_Inflorescences", data, monthgroups)
    generate_glm("Nb_Inflorescences", family = poisson, data=ldata, subset= index_flowering.all, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    
    
    ##############################################
    #### Date of inflorescences
    ##############################################
      if (verbose >= 1) print("########### Estimate Date of inflorescences") 
      
      has_flowering_week_selection = flowering_selection & data$Flowering_Week > 0
      index_has_flowering_week.all = which(has_flowering_week_selection)
      
      lfactors = filter_factors("Flowering_Week",factors, exclude, include)
      ldata = grouping_monthes("Flowering_Week", data, monthgroups)
      if(length(index_has_flowering_week.all) > MinNbGUForGLM){
        generate_vglm("Flowering_Week",  data=ldata, subset= index_has_flowering_week.all, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
      }
      else {
        print(paste("Not enougth flowering week specified for year",year,":",length(index_has_flowering_week.all)))
      }

    
    ##############################################
    #### Fruiting 
    ##############################################
    if (verbose >= 1) print("########### Estimate Fruiting") 
    lfactors = filter_factors("Fruiting",factors, exclude, include)
    ldata = grouping_monthes("Fruiting", data, monthgroups)
    generate_glm("Fruiting", family = binomial, data=ldata, subset= index_flowering.all, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    
    
    ##############################################
    #### Number of fruits 
    ##############################################
    if (verbose >= 1) print("########### Estimate Number of fruits") 
    
    fruiting_selection = subset_selection & data$Fruiting == 1
    index_fruiting.all = which(fruiting_selection)
    data$Nb_Fruits = data$Nb_Fruits -1
    
    lfactors = filter_factors("Nb_Fruits",factors, exclude, include)
    ldata = grouping_monthes("Nb_Fruits", data, monthgroups)
    if(length(index_fruiting.all) > MinNbGUForGLM){
      generate_glm("Nb_Fruits", family = poisson, data=ldata, subset= index_fruiting.all, year= yeartag, verbose = verbose, factors = lfactors, tag = tag)
    }
    else {
      print(paste("Not enougth fruits specified for year",year,":",length(index_fruiting.all)))
    }
    if (verbose >= 1) print("Done")
}


########################################################################################################################################################################################




determining_glm_tables_within_cycle = function(data, year, verbose = 0, selection = NULL, tag = NULL, factors = c( "Burst_Month", "Position_A","Position_Ancestor_A","Nature_Ancestor_F")) {
  
  yeartag = paste("within_",year,sep="")
  
  # data = prepare_data_factors(data)
  
  # summary(data)
  if (is.null(selection)) { subset_selection = data$Tree_Fruit_Load == 1 }
  else {                    subset_selection = selection & data$Tree_Fruit_Load == 1 }
  
  if (verbose >= 1) print(paste("*** Cycle : ",year,"***")) 
  if (verbose >= 1) print(paste("*** Factors : ",paste(factors,collapse=" , "),"***"))
  
  exclude = list("Has_Apical_GU_Child" = c("Position_Ancestor_A"),
                 "Has_Lateral_GU_Children" = c("Position_Ancestor_A", "Nature_Ancestor_F"),
                 "Nb_Inflorescences" = c("Position_Ancestor_A"),
                 "Fruiting" = c('Nature_Ancestor_F'),
                 "Nb_Fruits" = c("Position_Ancestor_A"))
  include = NULL #list("Vegetative_Burst" = c("Burst_Semester"))
  
  monthgroups = list(Vegetative_Burst = list(1:2,3:5,6:11))
  
  if (year == '04') {
    #monthgroups$Has_Lateral_GU_Children = list( c(10,11,12,1))
    exclude$Vegetative_Burst =  c("Nature_Ancestor_F")
    exclude$Has_Apical_GU_Child = append(exclude[["Has_Apical_GU_Child"]],  c("Nature_Ancestor_F", "Burst_Month"))
    monthgroups$Burst_Date_GU_Children = list(c(9,10) )
    monthgroups$Burst_Delta_Date_GU_Children = list(c(9,10) )
    exclude$Burst_Delta_Date_GU_Children =  c("Position_A")
    monthgroups$Burst_Delta_Date_GU_Children_Poisson = list(c(9,10) )
    
    exclude$Fruiting = append(exclude[["Fruiting"]],  "Burst_Month")
    exclude$Nb_Fruits = append(exclude[["Nb_Fruits"]],  "Nature_Ancestor_F")
    monthgroups$Flowering = list(c(12,1),c(2,3,4))
    monthgroups$Nb_Inflorescences = list(c(1,2), c(3,4,5) )
    monthgroups$Fruiting = list(c(1,2), c(3,4,5) )
  }

  if (year == '05') {
    monthgroups = list(Vegetative_Burst = list(6:12))
    #exclude$Vegetative_Burst =  c("Position_A")
    exclude$Has_Apical_GU_Child = append(exclude[["Has_Apical_GU_Child"]],  c("Burst_Month"))
    monthgroups$Has_Lateral_GU_Children = list( c(10,11,12,1))
    monthgroups$Nb_Lateral_GU_Children = list( c(8,9), c(10,11,12), c(1,2))
    monthgroups$Burst_Date_GU_Children = list(c(8,9,10), c(12, 1, 2) )
    monthgroups$Burst_Delta_Date_GU_Children = list( c( 1, 2) )
    #exclude$Burst_Date_Children = c("Position_A")
    monthgroups$Burst_Delta_Date_GU_Children_Poisson = list( c(10, 11), c( 1, 2) )

    monthgroups$Flowering = list( c(8,9), c(11,12,1),c(2,3))
    monthgroups$Nb_Inflorescences = list( c(8,9,10,11))
    monthgroups$Flowering_Week = list( c(8,9,10,11), c(2,3))
    exclude$Flowering_Week = c("Position_Ancestor_A")
    exclude$Fruiting = append(exclude[["Fruiting"]],  "Position_A")
  }
  
  if (year == '0405') {
    exclude$Vegetative_Burst = append(exclude[["Vegetative_Burst"]],  c("Position_Ancestor_A"))
    monthgroups$Has_Apical_GU_Child = list( c(1,2)) #, c(11,12))
    monthgroups$Has_Lateral_GU_Children = list(c(8,9), c(10,11,12), c(1,2))
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
                                   monthgroups=monthgroups)
  
  print("determine_reproductive_development")
  determine_reproductive_development(data=data, subset_selection= subset_selection, year=year, yeartag= yeartag, tag = tag, verbose = verbose, factors = factors, exclude = exclude, include = include, monthgroups=monthgroups)    
  
}

determining_glm_tables_mixed_inflo_within_cycle = function(data, year, verbose = 0, selection = NULL, tag = NULL, factors = c( "Position_Ancestor_A")) {
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
  include = list("Nb_Lateral_GU_Children" = c("Has_Apical_GU_Child")) 
  monthgroups = list()
  
  
  
  determine_vegetative_development(data=data, subset_selection= subset_selection, year=year, yeartag= yeartag, 
                                   tag = tag, verbose = verbose, factors = factors, 
                                   exclude = exclude, include = include, monthgroups= monthgroups)

}

########################################################################################################################################################################################

determining_glm_tables_between_cycle = function(data, year, verbose = FALSE, selection = NULL, tag = NULL) {

    yeartag = paste("between_",year,sep="")
    
    factors = c("Position_A","Nature_F")  # "Tree_Fruit_Load"
    if ("Burst_Month" %in% colnames(data)) factors = c("Burst_Month", factors)
    
    summary(data)

    tracestep = 0
    if (verbose >= 3) tracestep = 1

    if (is.null(selection)) { subset_selection = data$Tree_Fruit_Load == 1 }
    else {                    subset_selection = selection & data$Tree_Fruit_Load == 1 }

    if (verbose >= 1) print(paste("*** Between Cycles",year,"***")) 
    if (verbose >= 1) print(paste("*** Factors : ",paste(factors,collapse=" , "),"***"))
    
    determine_vegetative_development(data=data, subset_selection= subset_selection, year=year, yeartag= yeartag, 
                                     tag = tag, verbose = verbose, factors = factors)

}

determining_glm_tables_mixed_inflo_between_cycle = function(data, year, verbose = FALSE, selection = NULL, tag = NULL) {
  
  yeartag = paste("between_",year,sep="")

  factors = c("Position_A")
  if ("Burst_Month" %in% colnames(data)) factors = c("Burst_Month", factors)
  #else factors = NULL
  
  
  summary(data)
  
  tracestep = 0
  if (verbose >= 3) tracestep = 1
  
  if (is.null(selection)) { subset_selection = data$Tree_Fruit_Load == 1 }
  else {                    subset_selection = selection & data$Tree_Fruit_Load == 1 }
  
  if (verbose >= 1) print(paste("*** Between Cycles",year,"Mixed Inflorescences ***")) 
  if (verbose >= 1) print(paste("*** Factors : ",paste(factors,collapse=" , "),"***"))
  
  determine_vegetative_development(data=data, subset_selection= subset_selection, year=year, yeartag= yeartag, 
                                   tag = tag, verbose = verbose, factors = factors, mixedinflo = TRUE)
  
}


########################################################################################################################################################################################

determining_glm_tables_within_cycle_for_year = function(input_dir, year = NULL, verbose = 0) {
  data = read.csv(paste(input_dir,"table_within_cycle_",year,".csv",sep=""), header = TRUE)
  general_summary_output(data,paste(output_dir,"info_table_within_",year,".txt",sep=""))
  determining_glm_tables_within_cycle(data, year, verbose = verbose, selection=data$Mixed_Inflo == FALSE)
  determining_glm_tables_mixed_inflo_within_cycle(data, year, verbose = verbose, selection=data$Mixed_Inflo == TRUE, tag='mixedinflo')
}



determining_glm_tables_between_cycle_for_year = function(input_dir, year, verbose = 0) {
    data = read.csv(paste(input_dir,"table_between_cycle_",year,".csv",sep=""), header = TRUE)
    general_summary_output(data,paste(output_dir,"info_table_between_",year,".txt",sep=""))
    if (length(which(is.na(data$Burst_Month))) == length(data$Burst_Month)) data$Burst_Month = NULL
    determining_glm_tables_between_cycle(data, year, verbose)
    determining_glm_tables_mixed_inflo_between_cycle(data, year, verbose)
}


#print("start")
verbose = 1
determining_glm_tables_within_cycle_for_year(input_dir, "04",   verbose)
determining_glm_tables_within_cycle_for_year(input_dir, "05",   verbose)
determining_glm_tables_within_cycle_for_year(input_dir, "0405", verbose)

determining_glm_tables_between_cycle_for_year(input_dir, "03to0405", verbose)
determining_glm_tables_between_cycle_for_year(input_dir, "04to05",   verbose)


test = function() {
  data = read.csv(paste(input_dir,"table_within_cycle_04.csv",sep=""),header = TRUE)

  #data = prepare_data_factors(data, month_cycle_order)
  data$Burst_Month = as.factor(data$Burst_Month)
  data$Burst_Month = ordered(data$Burst_Month, levels = month_cycle_order)
  
  subset_selection = data$Tree_Fruit_Load == 1
  subset= which(subset_selection)
  
  factors = c("Burst_Month") # , "Position_A" , "Position_Ancestor_A" , "Nature_Ancestor_F")
  
  #generate_glm("Vegetative_Burst", family = binomial, data=data, subset= subset, year= "04", verbose = 1, factors = factors)

  ndata = data[subset,]
  
  formula = as.formula("Vegetative_Burst ~ Burst_Month")
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
  
  formula = as.formula("Burst_Delta_Date_Children ~ Burst_Month")
  myglm2 = vglm( formula , family = cumulative(parallel=TRUE), data = ndata)
  res = vglm.proba_and_counts (myglm, ndata)

  formula2 = as.formula("Burst_Delta_Date_Children ~ Burst_Month")
  myglm2 = vglm( formula2 , family = cumulative(parallel=TRUE), data = ndata)
  res2 = vglm.proba_and_counts (myglm2, ndata)
}
