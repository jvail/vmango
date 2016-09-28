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


vsummary_output = function(glm, data, outfile, subset = NULL, verbose = FALSE){

  if(length(subset) > 0)  ndata = data[subset,]
  else ndata = data
  
  mdebug = FALSE
  if (mdebug == FALSE) {
    out = file(outfile,open="wt")
    split = FALSE #(verbose >=3)
    sink(file = out, split = split)
  
    print("******** DATA *************")
    print(summary(ndata))
  }
  
    print("******** GLM  *************")
    print(paste("Formula :",glm.formula.text(glm),sep=''))
    
  if (mdebug == FALSE) {
    print(summary(glm))
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

########################################################################################################################################################################################

generate_glm = function(variable, family, data, subset, year, verbose = 0, factors = c("Burst_Month","Position_A","Position_Ancestor_A","Nature_Ancestor_F"), tag = NULL)
{
    tracestep = 0
    # if (verbose >= 3) tracestep = 0

    path_complete_glm = paste(output_dir,"complete_glm/",sep="")
    path_selected_glm = paste(output_dir,"selected_glm/",sep="")

    if (file.exists(path_complete_glm) == FALSE) dir.create(path_complete_glm, recursive=TRUE)
    if (file.exists(path_selected_glm) == FALSE) dir.create(path_selected_glm, recursive=TRUE)

    fname = tolower(variable)
    if (!is.null(tag)) fname = paste(fname, '_', tag)
    basefname = paste(fname, "_", year, sep="")
    
    formula = as.formula(paste(variable," ~ ", paste(factors,collapse=" + "), sep=""))
    
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
    vsummary_output(selected_glm.all, data, paste(path_selected_glm,basefname,"_summary.txt",sep=""), subset, verbose)

    #print("selected glm proba")
    proba.selected_glm.all = glm.proba_and_counts(selected_glm.all, data, subset)

    #print("selected glm proba writting")
    write.csv(proba.selected_glm.all,file=paste(path_selected_glm, basefname,".csv", sep=""), row.names = FALSE)

}

########################################################################################################################################################################################

#
#' Generate the multinomial glm for a process
#
generate_vglm = function(variable, data, subset, year, verbose, factors = c("Burst_Month","Position_A","Position_Ancestor_A","Nature_Ancestor_F"), tag = NULL)
{
    tracestep = 0
    # if (verbose >= 3) tracestep = 0

    path_complete_glm = paste(output_dir,"complete_glm/",sep="")
    path_selected_glm = paste(output_dir,"selected_glm/",sep="")

    if (file.exists(path_complete_glm) == FALSE) dir.create(path_complete_glm,recursive=TRUE)
    if (file.exists(path_selected_glm) == FALSE) dir.create(path_selected_glm,recursive=TRUE)

    fname = tolower(variable)
    if (!is.null(tag)) fname = paste(fname, '_', tag)
    basefname = paste(fname, "_", year)
    
    formula = as.formula(paste(variable," ~ ", paste(factors,collapse=" + "),sep=""))
    
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
    vsummary_output(selected_glm.all, ndata,  paste(path_selected_glm,basefname,"_summary.txt",sep=""), verbose=verbose)
    
}

########################################################################################################################################################################################

set_factor = function(data, name) {
  if (name %in% colnames(data))
    data[[name]] = as.factor(data[[name]])
  return (data)
}

prepare_data_factors = function(data,  children_burst_date_level = month_cycle_order) {
  # Assign covariables as factors
  
  if ("Burst_Month" %in% colnames(data)) {
    data$Burst_Month = ordered(data$Burst_Month, levels = month_cycle_order)
    data$Burst_Month = as.factor(data$Burst_Month)
  }
  
  for (name in c('Burst_Semester', 'Position_A', 'Position_Ancestor_A', 'Nature_F', 'Nature_Ancestor_F', 'Mixed_Inflo', 'Tree_Fruit_Load', 'Cycle')){
    if (name %in% colnames(data)) data[[name]] = as.factor(data[[name]])
  }

  if ("Burst_Date_Children" %in% colnames(data)) {
    data$Burst_Date_Children = ordered(data$Burst_Date_Children, levels = children_burst_date_level)
    data$Burst_Date_Children = factor(data$Burst_Date_Children)
  }
  
  return(data)
  
}


########################################################################################################################################################################################

#
#' Determine the different probabilities for the vegetative development processes 
#
determine_vegetative_development = function(data, subset_selection, year, yeartag, tag, verbose, factors, children_date_test = FALSE) {
    ##############################################
    #### Vegetative Burst
    ##############################################
    if (verbose >= 1) print("########### Estimate Vegetative Burst") 
    index_loaded.all = which(subset_selection)
    
    generate_glm("Vegetative_Burst", family = binomial, data=data, subset= index_loaded.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
    
    
    ##############################################
    #### Has_Apical_GU_Child
    ##############################################
    if (verbose >= 1) print("########### Estimate Has_Apical_GU_Child")
    
    bursted_selection = subset_selection & data$Vegetative_Burst == 1
    index_bursted.all = which(bursted_selection)
    
    generate_glm("Has_Apical_GU_Child", family = binomial, data=data, subset= index_bursted.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
    
    
    ##############################################
    #### Has_Lateral_GU_Children
    ##############################################
    if (verbose >= 1) print("########### Estimate Has_Lateral_GU_Children") 
    
    generate_glm("Has_Lateral_GU_Children", family = binomial, data=data, subset= index_bursted.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
    
    
    ##############################################
    #### Number of lateral GU
    ##############################################
    if (verbose >= 1) print("########### Estimate Number of lateral GU") 
    
    lateral_selection = bursted_selection & data$Has_Lateral_GU_Children == 1
    index_lateral.all = which(lateral_selection)
    
    #On choisi une loi de poisson. Néanmoins, pour Poisson la distribution doit commencer à 0 et pas à 1.
    #On enlève donc 1 au nombre de latérales afin de commencer à 0.
    ####Attention!!!Il ne faudra pas oublier de rajouter 1 ensuite lors de la simulation!!!
    data$Nb_Lateral_GU_Children = data$Nb_Lateral_GU_Children -1
    
    generate_glm("Nb_Lateral_GU_Children", family = poisson, data=data, subset= index_lateral.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
    
    
    ##############################################
    #### Burst date of children    
    ##############################################
    if (verbose >= 1) print("########### Estimate Burst date of children") 
    
    generate_vglm("Burst_Date_Children",  data=data, subset= index_bursted.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
    
    if (children_date_test) {
    
      ##############################################
      #### Delta Burst date of children    
      ##############################################
      #if (verbose >= 1) print("############################################## Estimate Burst delta date of children with vglm") 
      
      generate_vglm("Burst_Delta_Date_Children",  data=data, subset= index_bursted.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
      
      
      ##############################################
      #### Delta Burst date of children  with poisson  
      ##############################################
      if (verbose >= 1) print("########### Estimate Burst delta date of children with poisson") 
      
      data$Burst_Delta_Date_Children_Poisson = data$Burst_Delta_Date_Children - 1
      
      generate_glm("Burst_Delta_Date_Children_Poisson", family = poisson, data=data, subset= index_bursted.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
    }
}

########################################################################################################################################################################################

#
#' Determine the different probabilities for the reproductive development processes 
#
determine_reproductive_development = function(data, subset_selection, year, yeartag, tag, verbose, factors) {
    MinNbGUForGLM = 30
  
    ##############################################
    #### Flowering 
    ##############################################
    if (verbose >= 1) print("########### Estimate Flowering") 
    
    terminal_selection = subset_selection & data$is_terminal == 1
    index_extremity.all = which(terminal_selection)
    
    generate_glm("Flowering", family = binomial, data=data, subset= index_extremity.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
    
    
    ##############################################
    #### Number of inflorescences 
    ##############################################
    if (verbose >= 1) print("########### Estimate Number of inflorescences") 
    
    flowering_selection = subset_selection & data$Flowering == 1
    index_flowering.all = which(flowering_selection)
    data$Nb_Inflorescences = data$Nb_Inflorescence -1
    
    generate_glm("Nb_Inflorescences", family = poisson, data=data, subset= index_flowering.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
    
    
    ##############################################
    #### Date of inflorescences
    ##############################################
    if (year != '0405') {
      if (verbose >= 1) print("########### Estimate Date of inflorescences") 
      
      has_flowering_week_selection = flowering_selection & data$Flowering_Week > 0
      index_has_flowering_week.all = which(has_flowering_week_selection)
      
      if(length(index_has_flowering_week.all) > MinNbGUForGLM){
        generate_vglm("Flowering_Week",  data=data, subset= index_has_flowering_week.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
      }
      else {
        print(paste("Not enougth flowering week specified for year",year,":",length(index_has_flowering_week.all)))
      }
    }
    
    
    ##############################################
    #### Fruiting 
    ##############################################
    if (verbose >= 1) print("########### Estimate Fruiting") 
    generate_glm("Fruiting", family = binomial, data=data, subset= index_flowering.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
    
    
    ##############################################
    #### Number of fruits 
    ##############################################
    if (verbose >= 1) print("########### Estimate Number of fruits") 
    
    fruiting_selection = subset_selection & data$Fruiting == 1
    index_fruiting.all = which(fruiting_selection)
    data$Nb_Fruits = data$Nb_Fruits -1
    
    if(length(index_fruiting.all) > MinNbGUForGLM){
      generate_glm("Nb_Fruits", family = poisson, data=data, subset= index_fruiting.all, year= yeartag, verbose = verbose, factors = factors, tag = tag)
    }
    else {
      print(paste("Not enougth fruits specified for year",year,":",length(index_fruiting.all)))
    }
    if (verbose >= 1) print("Done")
}


########################################################################################################################################################################################


month_cycle_order = c("6","7","8","9","10","11","12","1","2","3","4","5")

determining_glm_tables_within_cycle = function(data, year, verbose = 0, selection = NULL, tag = NULL, factors = c("Burst_Month", "Position_A","Position_Ancestor_A","Nature_Ancestor_F")) {

    yeartag = paste("within_",year,sep="")

    data = prepare_data_factors(data)
    
    data$Flowering_Week = factor(data$Flowering_Week)
    
    # summary(data)
    if (is.null(selection)) { subset_selection = data$Tree_Fruit_Load == 1 }
    else {                    subset_selection = selection & data$Tree_Fruit_Load == 1 }
    
    totfactors = factors

    if (verbose >= 1) print(paste("*** Cycle : ",year,"***")) 
    if (verbose >= 1) print(paste("*** Factors : ",paste(factors,collapse=" , "),"***"))
    
    determine_vegetative_development(data=data, subset_selection= subset_selection, year=year, yeartag= yeartag, tag = tag, verbose = verbose, factors = totfactors)
    determine_reproductive_development(data=data, subset_selection= subset_selection, year=year, yeartag= yeartag, tag = tag, verbose = verbose, factors = totfactors)    
    
}

########################################################################################################################################################################################

determining_glm_tables_between_cycle = function(data, year, verbose = FALSE, selection = NULL, tag = NULL) {

    yeartag = paste("between_",year,sep="")
    
    # Assign date as ordered factor
    level_order = c("106","107","108","109","110","111","112","101","102","103","104","105",
                    "206","207","208","209","210","211","212","201","202","203","204","205")

    data = prepare_data_factors(data, level_order)
    

    factors = c("Position_A","Nature_F")  # "Tree_Fruit_Load"
    if ("Burst_Month" %in% colnames(data)) factors = c("Burst_Month", factors)
    
    summary(data)

    tracestep = 0
    if (verbose >= 3) tracestep = 1

    if (is.null(selection)) { subset_selection = data$Tree_Fruit_Load == 1 }
    else {                    subset_selection = selection & data$Tree_Fruit_Load == 1 }

    if (verbose >= 1) print(paste("*** Between Cycles",year,"***")) 
    if (verbose >= 1) print(paste("*** Factors : ",paste(factors,collapse=" , "),"***"))
    
    determine_vegetative_development(data=data, subset_selection= subset_selection, year=year, yeartag= yeartag, tag = tag, verbose = verbose, factors = factors)

}


########################################################################################################################################################################################

determining_glm_tables_within_cycle_for_year = function(input_dir, year = NULL, verbose = 0) {
  data = read.csv(paste(input_dir,"table_within_cycle_",year,".csv",sep=""), header = TRUE)
  determining_glm_tables_within_cycle(data, year, verbose = verbose, selection=data$Mixed_Inflo == FALSE)
  #determining_glm_tables_within_cycle(data, year, verbose = verbose, selection=data$Mixed_Inflo == TRUE, tag='mixedinflo', factors = c("Position_Ancestor_A"))
}



determining_glm_tables_between_cycle_for_year = function(input_dir, year, verbose = 0) {
    data = read.csv(paste(input_dir,"table_between_cycle_",year,".csv",sep=""), header = TRUE)
    if (length(which(is.na(data$Burst_Month))) == length(data$Burst_Month)) data$Burst_Month = NULL
    determining_glm_tables_between_cycle(data, year, verbose, selection=data$Mixed_Inflo == FALSE)
    #determining_glm_tables_between_cycle(data, year, verbose, selection=data$Mixed_Inflo == TRUE, tag='mixedinflo')
}

#print("start")
verbose = 1
determining_glm_tables_within_cycle_for_year(input_dir, "04",   verbose)
determining_glm_tables_within_cycle_for_year(input_dir, "05",   verbose)
#determining_glm_tables_within_cycle_for_year(input_dir, "0405", verbose)

determining_glm_tables_between_cycle_for_year(input_dir, "03to0405", verbose)
determining_glm_tables_between_cycle_for_year(input_dir, "04to05",   verbose)


test = function() {
  data = read.csv(paste(input_dir,"table_within_cycle_04.csv",sep=""),header = TRUE)

  data = prepare_data_factors(data, month_cycle_order)

  subset_selection = data$Tree_Fruit_Load == 1
  subset= which(subset_selection)
  
  factors = c("Burst_Month" , "Position_A" , "Position_Ancestor_A" , "Nature_Ancestor_F")
  
  #generate_glm("Vegetative_Burst", family = binomial, data=data, subset= subset, year= "04", verbose = 1, factors = factors)

  ndata = data[subset,]
  
  formula = as.formula("Vegetative_Burst ~ Burst_Month + Position_A + Position_Ancestor_A + Nature_Ancestor_F")
  myglm = glm( formula , family = binomial, data = ndata)
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
