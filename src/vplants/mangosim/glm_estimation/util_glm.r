
library(VGAM)
library(multcomp)

#' Check whether the glm is a vglm (from VGAM) or not
is.vglm = function(glm){
  return (class(glm)[1]=="vglm")
}

#' Retrieve the factors used in the GLM
glm.factors = function(glm){
  factors = NULL
  
  if (is.vglm(glm))
  {
    if(length(glm@xlevels)>0){  factors = (rownames(slot(glm@terms[[1]],"factors")))[-1]  }
    # else no factors
  }
  else
  {
    if(!is.null(glm$xlevels)){  factors = colnames(glm$model)[-1]  }
    # else no factors
  }
  return (factors)
}

#' Retrieve the variable to explain from the GLM
glm.variable = function(myglm){
  variable = NULL
  
  if (is.vglm(myglm))
  {
    if(length(myglm@xlevels)>0){ variable = rownames(slot(myglm@terms[[1]],"factors"))[1]  }
    else {                     variable = attr(attr(myglm@terms[[1]],"dataClasses"),"names")  }
  }
  else
  {
    if(!is.null(myglm$xlevels)){ variable = colnames(myglm$model)[[1]] }
    # else {    missing this   }
  }
  return (variable)
}

#' Retrieve the set of variable involved from the GLM, i.e. the 
glm.all.variables = function(myglm){
  variables = NULL
  
  if (is.vglm(myglm))
  {
    if(length(myglm@xlevels)>0){  variables = rownames(slot(myglm@terms[[1]],"factors"))    }
    else {                        variables = attr(attr(myglm@terms[[1]],"dataClasses"),"names")     }
  }
  else
  {
    if(!is.null(myglm$xlevels)){   variables = colnames(myglm$model)     }
  }
  return (variables)
}



glm.formula.text = function(glm) {
  factors = glm.factors(glm)
  if (length(factors) == 0)
    formula = paste(glm.variable(glm)," ~ 1",sep="")
  else 
    formula = paste(glm.variable(glm)," ~ ", paste(factors, collapse=" + "),sep="")
  return (formula)
}

glm.formula = function(glm) {
  return (as.formula(glm.formula.text(glm)))
}

# Fonction qui donne les effectifs par combinaison de modalités de facteurs dans les données initiales
glm.counts = function(myglm, data, subset = NULL){
  if (length(subset) > 0) { data = data[subset,] }
  
  if (is.vglm(myglm)){
    variables = glm.all.variables(myglm) 
    if (length(variables) > 1) {
      factors = variables[2:length(variables)]
    }
    else { factors = NULL }

    if (length(factors) > 0){
      subdata = data[, variables]  # fichier des combinaisons de modalités des facteurs signifs pour les observations 
      
      nb = ftable(subdata, row.vars = factors, col.vars=variables[1] )
      colval = attr(nb, "col.vars")
      
      nb = data.frame( expand.grid(rev(attr(nb, "row.vars"))),  unclass(nb))
      
      nb[, 1:(length(factors))] = nb[,factors]
      
      mnames = c(factors, colval[[1]])
      colnames(nb) = mnames
    }
    else {
      nb = ftable(data[variables[1]])
      colval = attr(nb, "col.vars")
      nb = data.frame(unclass(nb))
      colnames(nb) = colval[[1]]
    }
  }
  else {
    factors  = glm.factors(myglm)
    if (length(factors) > 0) {
      subdata = data[, factors]
      nb = as.data.frame(ftable(subdata))
    }
    else { nb = data.frame(Freq=c(nrow(data))) }
  }
  return (nb)
}


glm.table_probability = function(myglm){
  


  variables = NULL # in case of selected glm, contains only influent variables

  if (is.vglm(myglm))
  {
    if(length(myglm@xlevels)>0){
      variables = glm.factors(myglm)
      data_probs = expand.grid(myglm@xlevels)
    }
    
    if(!is.null(variables)){
      probability = predictvglm(myglm, newdata= data_probs,type="response")
      for(i in 1:length(colnames(probability)) ){
          data_probs[colnames(probability)[i] ] = probability[,i]
      }
    }
    else{
      # cas du glm null
      probability = predictvglm(myglm,type="response")[1,] # on predit sans valeur de x
      y_values = colnames(myglm@y) # on recupere le nom des mois (valeur possible de la variable y du glm)
      # on veut transformer probs en dataframe. Mais plutot en colonne qu'en ligne (du coup on fait une transposé)
      data_probs = t(data.frame(probability))
      colnames(data_probs)= y_values
    }
    
  }
  else
  {
    if(!is.null(myglm$xlevels)){
      variables = glm.factors(myglm)
      data_probs = expand.grid(myglm$xlevels)
    }
    
    if(myglm$family[1]=="binomial" || myglm$family[1]=="poisson"){  
    
      if (!is.null(variables)) {
        probability = predict(myglm, newdata=data_probs, type="response")
        data_probs["probability"]=probability
      }
      else{
        # cas du glm null
        probability = predict(myglm,type="response")[1]
        data_probs = data.frame(probability)
      }
    }
  }
  
  return(data_probs)
}

# Fonction qui change les valeurs des modalités des facteurs dans un tableau d'effectifs en caractères 
# et construit une variable code (combcode) qui est la combinaison des modalités des facteurs pour chaque ligne
unique.index = function(mydata, nbparam = -1){
  if (nbparam == -1) { nbparam = (ncol(mydata)-1) }
  #mydata[,1:nbparam] <- lapply(mydata[,1:nbparam], as.character) # transformation des valeurs en caractères (sinon il prend les niveaux de facteurs et non les valeurs)
  
  if(nbparam == 1)  codecomb = mydata[,1]
  else if(nbparam == 2)  codecomb = paste(mydata[,1], mydata[,2], sep="-")
  else if(nbparam == 3)  codecomb = paste(mydata[,1], mydata[,2], mydata[,3], sep="-")
  else if(nbparam == 4)  codecomb = paste(mydata[,1], mydata[,2], mydata[,3], mydata[,4], sep="-")
  else if(nbparam == 5)  codecomb = paste(mydata[,1], mydata[,2], mydata[,3], mydata[,4], mydata[,5], sep="-")
  else if(nbparam == 6)  codecomb = paste(mydata[,1], mydata[,2], mydata[,3], mydata[,4], mydata[,5], mydata[,6], sep="-")
  return (codecomb)
}

glm.proba_and_counts = function(myglm, data, subset){
  # probas estimées par combinaison de modalités des facteurs retenus
  probtable = glm.table_probability (myglm)
  
  # effectifs des données dans chaque modalité croisée des facteurs retenus
  nbelements = glm.counts(myglm, data=data, subset=subset)

  if (ncol(probtable) > 1) {
    probtablecodes = unique.index(probtable)
    nbelementscodes = unique.index(nbelements)
    
    # rajout des effectifs dans la table des probas estimées par le glm
    ind = match(probtablecodes, nbelementscodes, nomatch=NA)
    probtable$number = nbelements$Freq[ind]
  }
  else { 
    probtable$number = nbelements$Freq 
  }
  
  return (probtable)
}


vglm.proba_and_counts = function(myglm, data, subset = NULL){
  factors = glm.factors(myglm)

  probtable = glm.table_probability(myglm)
  
  # effectifs des données dans chaque modalité croisée des facteurs retenus
  nbelements = glm.counts(myglm, data=data, subset = subset)
  
  # rajout des effectifs dans la table des probas estimées par le glm
  nbfactors = length(factors)
  if (nbfactors > 0) {
    probtablecodes = unique.index(probtable, nbfactors)
    nbelementscodes = unique.index(nbelements, nbfactors)
    ind = match(probtablecodes, nbelementscodes, nomatch=NA)
    nbelements = nbelements[ind,colnames(probtable)] 
  }

  return (list(probtable,nbelements))
}


vglm.logLik = function(myglm, data, subset = NULL) {
  res = vglm.proba_and_counts(myglm, data, subset)
  proba = res[[1]]
  nbelement = res[[2]]

  firstcol = length(glm.factors(myglm))+1
  
  result = 0
  for (i in 1:nrow(proba)) {
    for (j in firstcol:ncol(proba)){
      iproba = proba[[i,j]]
      inbelem = nbelement[[i,j]]
      if (iproba > 0 & inbelem > 0){
        result = result + (log(iproba) * inbelem)
      }
    }
  }
  return (result)
}

glm.AIC = function(myglm, data, subset = NULL){
  if (is.vglm(myglm)) {
    k = myglm@rank
    logL = myglm@criterion$loglikelihood
    if (is.nan(logL)) {
      logL = vglm.logLik(myglm, data, subset)
    }
    aic = 2*k - 2*logL    
  }
  else {
    aic = AIC(myglm)
  }
  return(aic)
}

library(combinat)

vglm.step = function(glm, data, subset = NULL) {
  factors = glm.factors(glm)
  variable = glm.variable(glm)
  
  if(length(factors) == 0) { return (glm)}

  bestvglm = glm
  bestaic = glm.AIC(bestvglm, data, subset)
  
  mformula = paste(variable," ~ 1", sep="")

  candidatvglm = vglm(as.formula(mformula), family = glm@family, data = data, subset = subset)
  candidataic = glm.AIC(candidatvglm, data, subset)
  
  if (candidataic < bestaic) {
     bestvglm = candidatvglm
     bestaic = candidataic
  }

  if (length(factors) > 1) { 
    for(nbfactors in 1:(length(factors)-1)){
        factorcombini = combn(factors,nbfactors)
        for (lindex in 1:ncol(factorcombini)){
            lfactors = factorcombini[,lindex]
            ftext = paste(variable," ~ ", paste(lfactors,collapse=" + "),sep="")
            formula = as.formula(ftext)
            candidatvglm = vglm(formula = formula, family = glm@family, data = data, subset = subset)
            candidataic = glm.AIC(candidatvglm, data, subset)
            if (candidataic < bestaic) {
                bestvglm = candidatvglm
                bestaic = candidataic
            }
        }
    }
  }
  
  return (bestvglm)
}