library(shiny)
library(DT)
library(visNetwork)
library(dplyr)
library(jsonlite)


LowVariationRegu = function(min.variation, data.omics, ExpGroups, associations, AlltargetFs, omic.type, clinic.type) {
  
  if (all(is.na(min.variation))){
    
    for (ov in names(associations)){
      if(!is.null(associations[[ov]])){
        myreg=associations[[ov]][associations[[ov]][,1] %in% AlltargetFs,2] # removing regulators not associated to our targetFs
        data.omics[[ov]]=data.omics[[ov]][intersect(myreg, rownames(data.omics[[ov]])),] ## Reduced data.omics
        rm("myreg")
      }
    }
    #### Low variation cutoff is computed automatically
    data.omicsMean = vector("list", length=length(data.omics))
    if(!is.null(clinic.type)){j = 2}else{j=1}
    for(i in j:length(data.omics)){
      data.omicsMean[[i]]=t(apply(data.omics[[i]], 1, tapply, ExpGroups, mean))
    }
    names(data.omicsMean)=names(data.omics)
    
    percVar = c(10, 0.1)
    names(percVar) = 0:1
    percVar = percVar[as.character(omic.type)]
    names(percVar)=names(data.omicsMean)
    
    # Applying Low Variation filter
    LowVar=LowVariatFilter(data=data.omicsMean, method="sd", percVar=percVar, omic.type = omic.type, clinic.type = clinic.type)
    
    # data.omicsMean reduced: without NA and LV
    data.omicsMean=LowVar$data
    
    ## data.omics reduced: only mytargetFs and without NA and LV
    for (ov in names(data.omics)[j:length(data.omics)]){
      data.omics[[ov]] = data.omics[[ov]][rownames(data.omicsMean[[ov]]),]
      # Remove regulators from associations that have been removed due to LowVariation
      associations[[ov]] = associations[[ov]][associations[[ov]][,2] %in% rownames(data.omics[[ov]]),,drop = FALSE]
      
    }
    
  }
  else {
    
    #### Low variation cutoff is set by the user
    
    # removing regulators not associated to our targetFs only when there is associations matrix
    for (ov in names(associations)){
      if(!is.null(associations[[ov]])){
        myreg=associations[[ov]][associations[[ov]][,1] %in% AlltargetFs,2]
        data.omics[[ov]]=data.omics[[ov]][intersect(myreg, rownames(data.omics[[ov]])),]
        rm("myreg")
      }
    }
    
    # Creating vector for min.variation
    if (length(min.variation) == 1) {  ## Including min.variation = 0. I need a vector with omics names
      min.variation=rep(min.variation,length(data.omics))
      names(min.variation)=names(data.omics)
    } 
    
    # computing mean per condition in data.omics
    data.omicsMean=vector("list", length = length(data.omics))
    if(!is.null(clinic.type)){j = 2}else{j=1}
    for(i in j:length(data.omics)){
      data.omicsMean[[i]] = t(apply(data.omics[[i]], 1, tapply, ExpGroups, mean))
    }
    names(data.omicsMean) = names(data.omics)
    
    # Applying Low Variation filter
    LowVar=LowVariatFilter(data = data.omicsMean, method = "user", percVar = min.variation, omic.type = omic.type, clinic.type = clinic.type)
    
    data.omicsMean=LowVar$data  ## data.omicsMean reduced
    
    ## data.omics reduced: only mytargetFs and without NA and LV
    for (ov in names(data.omics)[j:length(data.omics)]){
      data.omics[[ov]] = data.omics[[ov]][rownames(data.omicsMean[[ov]]),]
      # Remove regulators from associations that have been removed due to LowVariation
      associations[[ov]] = associations[[ov]][associations[[ov]][,2] %in% rownames(data.omics[[ov]]),,drop = FALSE]
      
    }
    
  }
  
  rm("data.omicsMean"); gc()
  
  # Regulators removed due to low variation filter
  myregLV=LowVar$LV.reg
  rm("LowVar"); gc()
  
  cat("Number of regulators with low variation:\n")
  print(sapply(myregLV, length))
  cat("\n")
  
  return(list("myregLV" = myregLV, "data.omics" = data.omics, "associations" = associations))
}

LowVariatFilter=function(data, method, percVar, omic.type, clinic.type){
  
  SummaryRes = LV.reg = vector("list", length=length(data))
  names(SummaryRes) = names(LV.reg) = names(data)
  
  if(!is.null(clinic.type)){i = 2}else{i=1}
  
  for (ov in names(data)[i:length(data)]) {
    if(is.na(percVar[ov])){
      method.low="sd"
      if(omic.type[ov]==0){
        percVar[ov]=10
      }else{
        percVar[ov]==0.1
      }
    }else{
      method.low =method
    }
    if (omic.type[ov] == 0) {  # numerical regulators
      
      if (method.low=="sd") {
        met=apply(data[[ov]], 1, sd, na.rm=TRUE)  ## Compute standard deviation between conditions
        maxMet=max(met)*(percVar[ov]/100)  ## Compute minimum variation allowed
        myreg=met[met>maxMet]  # Regulators to be kept
        LV.reg[[ov]]=names(met[met<=maxMet]) ## Keep names of removed regulators
        data[[ov]]=data[[ov]][names(myreg), ,drop=FALSE]
      }
      
      if(method.low=="user") {
        if (min(dim(data[[ov]])) > 0) {
          met = apply(data[[ov]], 1, function(x) max(x, na.rm=TRUE)-min(x, na.rm=TRUE) )
          maxMet = percVar[ov] ## We don't consider the max, just the percentage defined by the user
          myreg=met[met>maxMet]
          LV.reg[[ov]]=names(met[met<=maxMet]) ## Keep names of removed regulators
          data[[ov]]=data[[ov]][names(myreg), , drop=FALSE]
        }
      }
    }
    
    if (omic.type[ov] == 1) {  # binary categorical regulators
      
      if(method.low=='sd'){
        met = apply(data[[ov]], 1, function (x) { max(x, na.rm = TRUE)-min(x, na.rm = TRUE) })  ## Compute maximun variation between groups
        maxMet=max(met)/10 ## Compute the minimum variation allowed
        myreg = met[met > maxMet]  # Regulators to be kept
        LV.reg[[ov]] = names(met[met <= maxMet]) ## Keep names of removed regulators
        data[[ov]] = data[[ov]][names(myreg), ,drop=FALSE]
      }
      
      if(method.low == 'user'){ 
        met = apply(data[[ov]], 1, function (x) { max(x, na.rm = TRUE)-min(x, na.rm = TRUE) })  ## Compute maximun variation between groups
        myreg = met[met > percVar[ov]]  # Regulators to be kept
        LV.reg[[ov]] = names(met[met <= percVar[ov]]) ## Keep names of removed regulators
        data[[ov]] = data[[ov]][names(myreg), ,drop=FALSE]
      }
    }
  }
  
  results=vector("list", length=2)
  results[[1]]=data
  results[[2]]=LV.reg
  names(results)=c("data", "LV.reg")
  
  return(results)
}

GetPairs1targetFAllReg = function (targetF, output) {
  
  if(output$arguments$method=='MLR'|| output$arguments$method=='ISGL'){
    
    reguSignif = output$ResultsPerTargetF[[targetF]]$relevantRegulators
    
    if (is.null(reguSignif)) {  # NO significant regulators
      return (NULL)
      
    } else {  # Significant regulators
      
      reguSignif = output$ResultsPerTargetF[[targetF]]$allRegulators[reguSignif,]
      reguSignif = reguSignif[,c("targetF", "regulator", "omic", "area", "filter")]
      return (reguSignif)
    }
    
  }
  if(output$arguments$method=='PLS1' || output$arguments$method=='PLS2'){
    
    reguSignif = output$ResultsPerTargetF[[targetF]]$significantRegulators
    
    if (is.null(reguSignif)) {  # NO significant regulators
      return (NULL)
      
    } else {  # Significant regulators
      
      reguSignif = output$ResultsPerTargetF[[targetF]]$allRegulators[reguSignif,]
      reguSignif = reguSignif[,c("targetF", "regulator", "omic", "area", "filter")]
      return (reguSignif)
    }
    
  }
  
}


# For all targetFs
GetPairstargetFRegulator = function (targetFs = NULL, output) {
  
  if (is.null(targetFs)) targetFs = rownames(output$GlobalSummary$ReguPerTargetF)
  
  myresults = do.call("rbind", lapply(targetFs, GetPairs1targetFAllReg, output))
  
  #   colnames(myresults) = c("targetF", "regulator", "omic", "area")
  return(myresults)
}

#Make the coefficients comparable

ComparableBetas = function(myresults, output){
  
  for(k in unique(myresults[,'targetF'])){
    sd_targetF = sd(output$arguments$targetData[k,])
    myresults[myresults[,"targetF"] == k, grep('Group',colnames(myresults)) ] = myresults[myresults[,"targetF"] == k, grep('Group',colnames(myresults)) ]/sd_targetF
  }
  return(myresults)
}

RegulationPerCondition = function(output, filterR2 = 0){
  
  #Filter to only those targetF of interest
  filtered_targetF = rownames(output$GlobalSummary$GoodnessOfFit)[which(output$GlobalSummary$GoodnessOfFit[,1]>filterR2)]
  output$ResultsPerTargetF = output$ResultsPerTargetF[names(output$ResultsPerTargetF) %in% filtered_targetF]
  
  # output: results of the getMLR/getPLS function.
  method = output$arguments$method
  #Add a progressbar
  num_iterations <- length(rownames(output$GlobalSummary$ReguPerTargetF))
  
  if(method =='MLR'|| method=='ISGL'){
    design = output$arguments$finaldesign
    Group = output$arguments$groups
    
    # Creo a partir de la funcion que ya estaba hecha (linea 1657) la tabla y le anyado los huecos en blanco y cambio el nombre a "representative".
    targetFs = rownames(output$GlobalSummary$ReguPerTargetF)
    myresults = do.call("rbind", lapply(targetFs, GetPairs1targetFAllReg, output))
    colnames(myresults) = c(colnames(myresults)[1:4], "representative")
    myresults[myresults[, "representative"] == "Model", "representative"] = ""
    if (is.null(design)){
      
      # Anyado la columna de coeficientes.
      coeffs = matrix(1, nrow(myresults), 1)
      colnames(coeffs) = "coefficients"
      rownames(coeffs) = rownames(myresults)
      myresults = cbind(myresults, coeffs)
      myresults[grep("_N", myresults[, "representative"]), "coefficients"] = -1  # Para cambiar el signo si pertenece al grupo de correlacionados negativamente
      
      for(k in unique(myresults[,"targetF"])){
        shiny::incProgress(amount = 1 / num_iterations)
        # Posicion y reguladores que son representantes.
        counts = grep("_R", myresults[myresults[,"targetF"] == k, "representative"]) # positions of representatives of mc
        representatives = myresults[myresults[,"targetF"] == k, "regulator"][counts]      # Devuelve el nombre real de los reguladores representantes
        omic.representative = myresults[myresults[,"targetF"] == k, c("regulator", "representative")][counts,]   # Columna Regulator y Representative
        
        # Necesito el if, si no da error. En caso de entrar, elimino las coletillas para que sea mas sencillo buscar y asignar el representante
        if(length(representatives) != 0){
          norow.nulls = which(myresults[myresults[,"targetF"] == k, "representative"] != "")
          myresults[myresults[,"targetF"] == k, "representative"][norow.nulls] = sub("_P", "", myresults[myresults[,"targetF"] == k, "representative"][norow.nulls])
          myresults[myresults[,"targetF"] == k, "representative"][norow.nulls] = sub("_N", "", myresults[myresults[,"targetF"] == k, "representative"][norow.nulls])
          myresults[myresults[,"targetF"] == k, "representative"][norow.nulls] = sub("_R", "", myresults[myresults[,"targetF"] == k, "representative"][norow.nulls])
          
          for(i in 1:length(representatives)){
            # Aquellos que se llamen igual "omica_mc(numero)", se les asignara el representante
            reg.rep = myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == representatives[i], "representative"]
            myresults[myresults[,"targetF"] == k & myresults[,"representative"] == reg.rep, "representative"] = representatives[i]
          }
          
          # Reguladores significativos del MLR. Pongo gsub() porque haciendo pruebas he visto que hay reguladores que se nombran `nombre regulador`.
          # Las comitas haran que no pueda encontrar el regulador
          # en la tabla. Sin embargo, creo sign.glm para manterner las comitas y poder acceder a la tabla de coeficientes
          significatives = gsub("`", "", names(output$ResultsPerTargetF[[k]]$coefficients[2:nrow(output$ResultsPerTargetF[[k]]$coefficients), 1]))
          sign.glm = names(output$ResultsPerTargetF[[k]]$coefficients[2:nrow(output$ResultsPerTargetF[[k]]$coefficients), 1])
          
          for(i in 1:length(significatives)){
            if(any(significatives[i] == omic.representative[,2])){
              # index.regul: para saber que regulador es el representante y asi todos los que tengan su nombre en la columna "representative" tendran su coeficiente del modelo MLR.
              index.regul = rownames(omic.representative)[which(omic.representative[,2] == significatives[i])]
              PN = myresults[myresults[,"targetF"] == k & myresults[,"representative"] == index.regul, "coefficients"]                        # Sera 1 o -1, segun tenga "_P" o "_N"
              myresults[myresults[,"targetF"] == k & myresults[,"representative"] == index.regul, "coefficients"] = PN*output$ResultsPerTargetF[[k]]$coefficients[sign.glm[i], 1]                                                    # Tendra signo de la tabla si es "_P" y signo opuesto si es "_N".
            } else {
              # En caso de no pertenecer a un grupo de reguladores correlacionados, cogera su coeficiente de la tabla y lo asignara a la posicion correspondiente
              myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == significatives[i], "coefficients"] = output$ResultsPerTargetF[[k]]$coefficients[sign.glm[i], 1]
            }
          }
          
        } else {
          # Si no presenta grupo de reguladores correlacionados, simplemente sacara los coeficientes de la tabla "coefficients"
          myresults[myresults[,"targetF"] == k, "coefficients"] = output$ResultsPerTargetF[[k]]$coefficients[2:nrow(output$ResultsPerTargetF[[k]]$coefficients), 1]
        }
      }
      
      
    } else {
      
      # Anyado las columnas de las condiciones experimentales. Pongo "Group" porque al hacer model.matrix() siempre coloca "Group" y lo que se almacena en el objeto Group
      index = unique(Group)
      names.groups = paste("Group", index, sep = "_")
      conditions = matrix(0, nrow(myresults), length(names.groups))
      colnames(conditions) = names.groups
      rownames(conditions) = rownames(myresults)
      myresults = cbind(myresults, conditions)
      
      for(k in unique(myresults[,"targetF"])){
        shiny::incProgress(amount = 1 / num_iterations)
        significant.regulators = output$ResultsPerTargetF[[k]]$relevantRegulators                    # Reguladores significativos.
        if(method =='MLR'){
          model.variables = gsub("`", "", rownames(output$ResultsPerTargetF[[k]]$coefficients))[-1]       # Reguladores e interacciones en el modelo.
          kc = 2
        } else{
          model.variables = gsub("`", "", rownames(output$ResultsPerTargetF[[k]]$coefficients))       # Reguladores e interacciones en el modelo.
          kc = 1
        }
        
        # Cojo las interacciones y creo objetos que contengan los reguladores que aparecen con interaccion, solas o ambas.
        interactions.model = gsub("`", "", rownames(output$ResultsPerTargetF[[k]]$coefficients)[grep(":", rownames(output$ResultsPerTargetF[[k]]$coefficients))])
        
        inter.variables = unlist(strsplit(interactions.model, ":", fixed = TRUE))
        if(is.null(inter.variables)){
          inter.variables = NULL                                                                            # No hay interacciones.
        } else {
          inter.variables = inter.variables[seq(2, length(inter.variables), by = 2)]                        # Reguladores que presentan interseccion con algun grupo.
        }
        
        variables.only = setdiff(setdiff(model.variables, interactions.model), inter.variables)             # Reguladores solos en el modelo, sin interacciones.
        if(length(grep("Group", variables.only)) != 0){                                                     # No puedo hacer la interseccion con las variables significativas porque me cargo tambien omic_mc: hay que eliminar Group si esta.
          variables.only = variables.only[-grep("Group", variables.only)]
        }
        
        variables.inter.only = intersect(inter.variables, model.variables)                                  # Reguladores con interaccion y solas.
        variables.inter = setdiff(inter.variables, model.variables)                                         # Reguladores con solo interaccion (no aparecen solas en el modelo).
        
        for(j in kc:nrow(output$ResultsPerTargetF[[k]]$coefficients)){
          regul = unlist(strsplit(gsub("`", "", rownames(output$ResultsPerTargetF[[k]]$coefficients)[j]), ":"))
          
          # Evaluo en que conjunto se encuentra el regulador correspondiente y segun eso asigno el coeficiente o sumo el nuevo coeficiente a lo que ya habia en esa posicion.
          if(any(regul %in% variables.only)){
            if(any(regul %in% significant.regulators)){
              myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regul, c(names.groups)] = output$ResultsPerTargetF[[k]]$coefficients[j,]
            } else {
              myresults[myresults[,"targetF"] == k & myresults[,"representative"] == regul, c(names.groups)] = output$ResultsPerTargetF[[k]]$coefficients[j,]
            }
          }
          
          if(any(regul %in% variables.inter)){
            if(any(regul %in% significant.regulators)){
              myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regul[2], regul[1]] = myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regul[2], regul[1]] + output$ResultsPerTargetF[[k]]$coefficients[j,]
            } else {
              myresults[myresults[,"targetF"] == k & myresults[,"representative"] == regul[2], regul[1]] = myresults[myresults[,"targetF"] == k & myresults[,"representative"] == regul[2], regul[1]] + output$ResultsPerTargetF[[k]]$coefficients[j,]
            }
          }
          
          if(any(regul %in% variables.inter.only)){
            if(any(regul %in% significant.regulators)){
              if(length(regul) == 1){
                myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regul, c(names.groups)] = myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regul, c(names.groups)] + output$ResultsPerTargetF[[k]]$coefficients[j,]
              } else {
                myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regul[2], regul[1]] = myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regul[2], regul[1]] + output$ResultsPerTargetF[[k]]$coefficients[j,]
              }
            } else {
              if(length(regul) == 1){
                myresults[myresults[,"targetF"] == k & myresults[,"representative"] == regul, c(names.groups)] = myresults[myresults[,"targetF"] == k & myresults[,"representative"] == regul, c(names.groups)] + output$ResultsPerTargetF[[k]]$coefficients[j,]
              } else {
                myresults[myresults[,"targetF"] == k & myresults[,"representative"] == regul[2], regul[1]] = myresults[myresults[,"targetF"] == k & myresults[,"representative"] == regul[2], regul[1]] + output$ResultsPerTargetF[[k]]$coefficients[j,]
              }
            }
          }
        }
        
        # Veo si hay representantes, en caso de haberlos asignara la misma fila del representante a los reguladores que acaben en "_P" y el opuesto a los que acaban en "_N".
        countsR = grep("_R", myresults[myresults[,"targetF"] == k, "representative"])
        
        if(length(countsR) != 0){
          countsR = myresults[myresults[,"targetF"] == k, 5:ncol(myresults)][countsR,]
          
          # Para los correlacionados positivamente: mete la misma fila de coeficientes del representante.
          countsP = countsR
          countsP[,"representative"] = sub("_R", "", countsP[,"representative"])
          countsP[,"representative"] = paste(countsP[,"representative"], "_P", sep = "")
          
          for(l in 1:nrow(countsP)){
            myresults[myresults[,"targetF"] == k & myresults[,"representative"] == countsP[l,"representative"], 6:ncol(myresults)] = countsP[l,2:ncol(countsP)]
          }
          
          # Para los correlacionados negativamente: mete la fila opuesta de coeficientes del representante.
          countsN = countsR
          countsN[,"representative"] = sub("_R", "", countsN[,"representative"])
          countsN[,"representative"] = paste(countsN[,"representative"], "_N", sep = "")
          
          for(l in 1:nrow(countsN)){
            myresults[myresults[,"targetF"] == k & myresults[,"representative"] == countsN[l,"representative"], 6:ncol(myresults)] = -countsN[l,2:ncol(countsN)]
          }
          
          counts = grep("_R", myresults[myresults[,"targetF"] == k, "representative"])
          representatives = myresults[myresults[,"targetF"] == k, "regulator"][counts]                                    # Devuelve el nombre real de los reguladores representantes
          omic.representative = myresults[myresults[,"targetF"] == k, c("regulator", "representative")][counts,]          # Columna Regulator y Representative
          
          # Necesito el if, sino da error. En caso de entrar, elimino las coletillas para que sea mas sencillo buscar y asignar el representante.
          if(length(representatives) != 0){
            norow.nulls = which(myresults[myresults[,"targetF"] == k, "representative"] != "")
            myresults[myresults[,"targetF"] == k, "representative"][norow.nulls] = sub("_P", "", myresults[myresults[,"targetF"] == k, "representative"][norow.nulls])
            myresults[myresults[,"targetF"] == k, "representative"][norow.nulls] = sub("_N", "", myresults[myresults[,"targetF"] == k, "representative"][norow.nulls])
            myresults[myresults[,"targetF"] == k, "representative"][norow.nulls] = sub("_R", "", myresults[myresults[,"targetF"] == k, "representative"][norow.nulls])
            
            for(i in 1:length(representatives)){
              # Aquellos que se llamen igual "omica_mc(numero)", se les asignara el representante.
              reg.rep = myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == representatives[i], "representative"]
              myresults[myresults[,"targetF"] == k & myresults[,"representative"] == reg.rep, "representative"] = representatives[i]
            }
          }
        }
      }
    }
    myresults = ComparableBetas(myresults, output)
    myresults[,6:ncol(myresults)] = signif(myresults[,6:ncol(myresults)], digits = 4) # Para que no salgan los numeros en diferentes notaciones
    
  }
  if(method=='PLS1' || method=='PLS2'){
    design = output$arguments$finaldesign
    Group = output$arguments$groups
    
    # Creo a partir de la funcion que ya estaba hecha (linea 1657) la tabla y le anyado los huecos en blanco y cambio el nombre a "representative".
    targetFs = rownames(output$GlobalSummary$ReguPerTargetF)
    myresults = do.call("rbind", lapply(targetFs, GetPairs1targetFAllReg, output))
    colnames(myresults) = c(colnames(myresults)[1:4], "representative")
    myresults[myresults[, "representative"] == "Model", "representative"] = ""
    
    if (is.null(design)){
      
      # Anyado la columna de coeficientes.
      coeffs = matrix(1, nrow(myresults), 1)
      colnames(coeffs) = "coefficients"
      rownames(coeffs) = rownames(myresults)
      myresults = cbind(myresults, coeffs)
      myresults[grep("_N", myresults[, "representative"]), "coefficients"] = -1  # Para cambiar el signo si pertenece al grupo de correlacionados negativamente
      
      for(k in unique(myresults[,"targetF"])){
        shiny::incProgress(amount = 1 / num_iterations)
        # Posicion y reguladores que son representantes.
        counts = grep("_R", myresults[myresults[,"targetF"] == k, "representative"]) # positions of representatives of mc
        representatives = myresults[myresults[,"targetF"] == k, "regulator"][counts]      # Devuelve el nombre real de los reguladores representantes
        omic.representative = myresults[myresults[,"targetF"] == k, c("regulator", "representative")][counts,]   # Columna Regulator y Representative
        
        # Necesito el if, si no da error. En caso de entrar, elimino las coletillas para que sea mas sencillo buscar y asignar el representante
        if(length(representatives) != 0){
          norow.nulls = which(myresults[myresults[,"targetF"] == k, "representative"] != "")
          myresults[myresults[,"targetF"] == k, "representative"][norow.nulls] = sub("_P", "", myresults[myresults[,"targetF"] == k, "representative"][norow.nulls])
          myresults[myresults[,"targetF"] == k, "representative"][norow.nulls] = sub("_N", "", myresults[myresults[,"targetF"] == k, "representative"][norow.nulls])
          myresults[myresults[,"targetF"] == k, "representative"][norow.nulls] = sub("_R", "", myresults[myresults[,"targetF"] == k, "representative"][norow.nulls])
          
          for(i in 1:length(representatives)){
            # Aquellos que se llamen igual "omica_mc(numero)", se les asignara el representante
            reg.rep = myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == representatives[i], "representative"]
            myresults[myresults[,"targetF"] == k & myresults[,"representative"] == reg.rep, "representative"] = representatives[i]
          }
          
          # Reguladores significativos del MLR. Pongo gsub() porque haciendo pruebas he visto que hay reguladores que se nombran `nombre regulador`.
          # Las comitas haran que no pueda encontrar el regulador
          # en la tabla. Sin embargo, creo sign.glm para manterner las comitas y poder acceder a la tabla de coeficientes
          significatives = gsub("`", "", names(output$ResultsPerTargetF[[k]]$coefficients[1:nrow(output$ResultsPerTargetF[[k]]$coefficients), 1]))
          sign.glm = names(output$ResultsPerTargetF[[k]]$coefficients[1:nrow(output$ResultsPerTargetF[[k]]$coefficients), 1])
          
          for(i in 1:length(significatives)){
            if(any(significatives[i] == omic.representative[,2])){
              # index.regul: para saber que regulador es el representante y asi todos los que tengan su nombre en la columna "representative" tendran su coeficiente del modelo MLR.
              index.regul = rownames(omic.representative)[which(omic.representative[,2] == significatives[i])]
              PN = myresults[myresults[,"targetF"] == k & myresults[,"representative"] == index.regul, "coefficients"]                        # Sera 1 o -1, segun tenga "_P" o "_N"
              myresults[myresults[,"targetF"] == k & myresults[,"representative"] == index.regul, "coefficients"] = PN*output$ResultsPerTargetF[[k]]$coefficients[sign.glm[i], 1]                                                    # Tendra signo de la tabla si es "_P" y signo opuesto si es "_N".
            } else {
              # En caso de no pertenecer a un grupo de reguladores correlacionados, cogera su coeficiente de la tabla y lo asignara a la posicion correspondiente
              myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == significatives[i], "coefficients"] = output$ResultsPerTargetF[[k]]$coefficients[sign.glm[i], 1]
            }
          }
          
        } else {
          # Si no presenta grupo de reguladores correlacionados, simplemente sacara los coeficientes de la tabla "coefficients"
          myresults[myresults[,"targetF"] == k, "coefficients"] = output$ResultsPerTargetF[[k]]$coefficients[1:nrow(output$ResultsPerTargetF[[k]]$coefficients), 1]
        }
      }
      
      
    } else {
      
      # Añado las columnas de las condiciones experimentales. Pongo "Group" porque al hacer model.matrix() siempre coloca "Group" y lo que se almacena en el objeto Group
      index = unique(Group)
      names.groups = paste("Group", index, sep = "_")
      conditions = matrix(0, nrow(myresults), length(names.groups))
      colnames(conditions) = names.groups
      rownames(conditions) = rownames(myresults)
      myresults = cbind(myresults, conditions)
      
      for(k in unique(myresults[,"targetF"])){
        
        shiny::incProgress(amount = 1 / num_iterations)
        
        significant.regulators = output$ResultsPerTargetF[[k]]$significantRegulators                    # Reguladores significativos.
        model.variables = gsub("`", "", rownames(output$ResultsPerTargetF[[k]]$coefficients))           # Reguladores e interacciones en el modelo.
        
        # Cojo las interacciones y creo objetos que contengan los reguladores que aparecen con interaccion, solas o ambas.
        interactions.model = gsub("`", "", rownames(output$ResultsPerTargetF[[k]]$coefficients)[grep(":", rownames(output$ResultsPerTargetF[[k]]$coefficients))])
        
        inter.variables = unlist(strsplit(interactions.model, ":", fixed = TRUE))
        if(is.null(inter.variables)){
          inter.variables = NULL                                                                            # No hay interacciones.
        } else {
          inter.variables = inter.variables[seq(2, length(inter.variables), by = 2)]                        # Reguladores que presentan interseccion con algun grupo.
        }
        
        variables.only = setdiff(setdiff(model.variables, interactions.model), inter.variables)             # Reguladores solos en el modelo, sin interacciones.
        if(length(grep("Group_", variables.only)) != 0){                                                     # No puedo hacer la interseccion con las variables significativas porque me cargo tambien omic_mc: hay que eliminar Group si esta.
          variables.only = variables.only[-grep("Group_", variables.only)]
        }
        
        variables.inter.only = intersect(inter.variables, model.variables)                                  # Reguladores con interaccion y solas.
        variables.inter = setdiff(inter.variables, model.variables)                                         # Reguladores con solo interaccion (no aparecen solas en el modelo).
        
        for(j in 1:nrow(output$ResultsPerTargetF[[k]]$coefficients)){
          regul = unlist(strsplit(gsub("`", "", rownames(output$ResultsPerTargetF[[k]]$coefficients)[j]), ":"))
          
          groups = regul[grepl(paste(paste0('Group_',names(table(Group))),collapse='|'), regul)]
          regula = setdiff(regul,groups)
          # Evaluo en que conjunto se encuentra el regulador correspondiente y segun eso asigno el coeficiente o sumo el nuevo coeficiente a lo que ya habia en esa posicion.
          if(any(regul %in% variables.only)){
            if(any(regul %in% significant.regulators)){
              myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regul, c(names.groups)] = output$ResultsPerTargetF[[k]]$coefficients[j,1]
            } 
          }
          #TO DO: regul[1] se supone que tendría que ser algo sobre los grupos y no un regulador
          if(any(regul %in% variables.inter)){
            if(any(regul %in% significant.regulators)){
              myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regula, groups] = myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regula, groups] + output$ResultsPerTargetF[[k]]$coefficients[j,1]
            } 
          }
          
          if(any(regul %in% variables.inter.only)){
            if(any(regul %in% significant.regulators)){
              if(length(regul) == 1){
                myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regul, c(names.groups)] = myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regul, c(names.groups)] + output$ResultsPerTargetF[[k]]$coefficients[j,1]
              } else {
                myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regula, groups] = myresults[myresults[,"targetF"] == k & myresults[,"regulator"] == regula, groups] + output$ResultsPerTargetF[[k]]$coefficients[j,1]
              }
            } 
          }
        }
      }
    }
    myresults = ComparableBetas(myresults, output)
    myresults[,6:ncol(myresults)] = signif(myresults[,6:ncol(myresults)], digits = 4) # Para que no salgan los numeros en diferentes notaciones
    myresults = myresults[,-5,drop=FALSE]
    
  }
  rownames(myresults)=NULL
  return(myresults)
}



biostat_colors <- c("#d6b0e9",
                    "#3bdfa2",
                    "#FDDB73",
                    "#d11d56",
                    "#657FCC",
                    "#126D6D",
                    "#FFB000",
                    "#6CD0D0",
                    "#F7FB7B")



networkMORE_cyj <- function(outputRegpcond, group1 = NULL, group2 = NULL, pc = 0, pathway = NULL, annotation = NULL) {
  
  create_graph <- function(df, pc) {
    df = df[df[,4] != 0, ]
    qc = quantile(abs(df[,4]), pc)[[1]]
    df = df[which(abs(df[,4]) >= qc), , drop = FALSE]
    
    # Procesar reguladores
    regulators <- unique(df[, c('regulator', 'omic')])
    #colors <- rainbow(length(unique(regulators$group)))
    colors <- biostat_colors
    regulators$color <- colors[match(regulators$omic, unique(regulators$omic))]
    
    # Crear nodos de reguladores y targets
    reg_nodes <- data.frame(
      id = regulators$regulator,
      omic = regulators$omic,
      color = regulators$color,
      stringsAsFactors = FALSE
    )
    
    targets <- data.frame(
      id = unique(df[, 'targetF']),
      omic = 'targetF',
      color = "#7332a7",
      stringsAsFactors = FALSE
    )
    
    #reg_nodes <- reg_nodes[!duplicated(reg_nodes$id), ]
    
    nodes <- rbind(targets, reg_nodes)
    
    num_unique = length(unique(nodes$omic))
    
    node_types = c('big text','ellipse','circle', 'square', 'hexagon', 'triangle-down','star', 'dot', 'icon dot','database','big square')[1:num_unique]
    nodes$shape <- node_types[match(nodes$omic, unique(nodes$omic))]
    
    if(any(grepl("tf", nodes$omic, ignore.case = TRUE))){
      i=grep('tf', nodes$omic, ignore.case = TRUE)
      nodes$shape[i]<- "triangle"
    }
    if(any(grepl("mirna", nodes$omic, ignore.case = TRUE))){
      i=grep('mirna', nodes$omic, ignore.case = TRUE)
      nodes$shape[i]<-'diamond'
    }
    nodes$shape[nodes$omic == "targetF"] <- "box"
    
    edges = data.frame(
      source = df[, 'regulator'],
      target = df[, 'targetF'],
      coef = round(df[, 4], 5),
      sign = ifelse(df[, 4] > 0, 'p', 
                    ifelse(df[, 4] < 0, 'n', 'z')),  # z para cero
      line = "continuous",  # línea continua por defecto
      stringsAsFactors = FALSE
    )
    
    return(list(nodes = nodes, edges = edges))
  }
  
  DifLineType <- function(df) {
    # --- INICIALIZAR COLUMNAS PARA EVITAR EL ERROR ---
    # Asegúrate de que df tenga al menos 8 columnas.
    # Si df tiene menos de 8 columnas, las agregamos e inicializamos con NA.
    # Esto evita el error "las nuevas columnas dejarían agujeros"
    if (ncol(df) < 7) {
      df$V7 <- NA # Crea la columna 7
    }
    if (ncol(df) < 8) {
      df$V8 <- NA # Crea la columna 8
    }
    # O, de forma más robusta, puedes inicializarlas con nombres significativos si no quieres depender de los índices numéricos
    # if (!"line" %in% colnames(df)) {
    #   df$line <- NA
    # }
    # if (!"sign" %in% colnames(df)) {
    #   df$sign <- NA
    # }
    # Luego usarías df$line y df$sign en lugar de df[,7] y df[,8]
    
    # --- Continúa con la lógica de tu función ---
    
    # Calcular el signo del efecto principal (basado en la diferencia, o en el coef del grupo2 si group1=0)
    # df[, 8] va a ser 'p' (positivo) o 'n' (negativo) para el efecto dominante/diferencial
    # Asegúrate de que df[,6] (la diferencia) también esté correctamente calculada y exista.
    # El error 'agujeros' se da si intentas asignar a df[,8] y la columna 7 no existe o viceversa.
    df[, 8] <- ifelse(sign(df[, 6]) == 1, 'p', 'n')
    df[df[, 4] == 0 & df[, 5] != 0, 8] <- ifelse(sign(df[df[, 4] == 0 & df[, 5] != 0, 5]) == 1, 'p', 'n')
    df[df[, 5] == 0 & df[, 4] != 0, 8] <- ifelse(sign(df[df[, 5] == 0 & df[, 4] != 0, 4]) == 1, 'p', 'n')
    
    # ---- Definir el tipo de línea (df[, 7]) ----
    
    # Inicializar df[,7] con un valor por defecto antes de las asignaciones condicionales
    # Esto es crucial para que todas las filas tengan un valor y no haya "agujeros" si alguna condición no se cumple
    df[, 7] <- 'default' # O 'continuous', o el valor que consideres base
    
    # CASO 1: Los grupos presentan el tipo de efecto opuesto
    condition_opuesto <- sign(df[, 4]) != sign(df[, 5]) & df[, 4] != 0 & df[, 5] != 0
    df[condition_opuesto, 7] <- 'dots' # Línea de puntos
    
    # CASO 2: Los dos grupos presentan el mismo tipo de efecto (ambos positivos o ambos negativos)
    condition_mismo_efecto <- sign(df[, 4]) == sign(df[, 5]) & df[, 4] != 0 & df[, 5] != 0
    
    # 2a: Mismo efecto, grupo de referencia (df[,4]) es mayor en magnitud
    condition_mismo_efecto_ref_mayor <- condition_mismo_efecto & abs(df[, 4]) > abs(df[, 5])
    df[condition_mismo_efecto_ref_mayor, 7] <- 'dashed' # Línea discontinua
    
    # 2b: Mismo efecto, grupo que se compara (df[,5]) es mayor en magnitud o igual
    condition_mismo_efecto_comp_mayor_o_igual <- condition_mismo_efecto & abs(df[, 5]) >= abs(df[, 4])
    df[condition_mismo_efecto_comp_mayor_o_igual, 7] <- 'continuous' # Línea continua
    
    # CASO 3: Un grupo no presenta regulación (coeficiente = 0) y el otro sí
    # 3a: Grupo de referencia (df[,4]) no regula (coef = 0), pero el grupo comparado (df[,5]) sí
    condition_ref_cero_comp_no_cero <- df[, 4] == 0 & df[, 5] != 0
    df[condition_ref_cero_comp_no_cero, 7] <- 'vertical_lines' # Línea de líneas verticales
    
    # 3b: Grupo comparado (df[,5]) no regula (coef = 0), pero el grupo de referencia (df[,4]) sí
    condition_comp_cero_ref_no_cero <- df[, 5] == 0 & df[, 4] != 0
    df[condition_comp_cero_ref_no_cero, 7] <- 'double_parallel_lines' # Línea de dobles líneas paralelas
    
    # El 'default' al principio ya cubre los casos no especificados, como cuando ambos coeficientes son 0.
    # df[is.na(df[, 7]), 7] <- 'default' # Ya no sería necesario si inicializamos bien.
    
    return(df)
  }
  
  if (!is.null(pathway)) {
    if (is.null(annotation)) stop('No annotation matrix was provided')
    targetFs <- annotation[which(annotation[, 3] == pathway), 1]
    if (length(targetFs) == 0) stop('No target feature was considered in that pathway')
    outputRegpcond <- outputRegpcond[outputRegpcond$targetF %in% targetFs, , drop = FALSE]
  }
  
  if (is.null(group1) && is.null(group2)) {
    
    return(stop("Warning: please select group1"))
    
  } else if (is.null(group2)) {
    ngroup <- grep(group1, colnames(outputRegpcond))
    df <- outputRegpcond[, c(1, 2, 3, ngroup)]
    return(create_graph(df, pc))
  } else {
    gr1 <- grep(group1, colnames(outputRegpcond))
    gr2 <- grep(group2, colnames(outputRegpcond))
    if (length(gr1) != 1 || length(gr2) != 1 || gr1 == gr2) stop("ERROR: group1 and group2 should be different names of groups to compare")
    
    df <- outputRegpcond[, c(1, 2, 3, gr1, gr2)]
    df[, 6] <- df[, 5] - df[, 4]
    df <- df[df[, 6] != 0, ]
    qc <- quantile(abs(df[, 6]), pc)[[1]]
    df <- df[abs(df[, 6]) >= qc, , drop = FALSE]
    df <- DifLineType(df)
    
    # Procesar reguladores
    regulators <- unique(df[, c('regulator', 'omic')])
    regulators$color <- biostat_colors[match(regulators$omic, unique(regulators$omic))]
    #colors <- setNames(biostat_colors[seq_along(unique(regulators$omic))], unique(regulators$omic))
    #regulators$color <- colors[regulators$omic]
    
    
    reg_nodes <- data.frame(
      id = regulators$regulator,
      omic = regulators$omic,
      color = regulators$color,
      stringsAsFactors = FALSE
    )
    
    targets <- data.frame(
      id = unique(df[, 'targetF']),
      omic = 'targetF',
      color = "#7332a7",
      stringsAsFactors = FALSE
    )
    
    nodes <- rbind(targets, reg_nodes)
    
    num_unique = length(unique(nodes$omic))
    
    node_types = c('big text','ellipse','circle', 'square', 'hexagon', 'triangle-down','star', 'dot', 'icon dot','database','big square')[1:num_unique]
    nodes$shape <- node_types[match(nodes$omic, unique(nodes$omic))]
    
    if(any(grepl("tf", nodes$omic, ignore.case = TRUE))){
      i=grep('tf', nodes$omic, ignore.case = TRUE)
      nodes$shape[i]<- "triangle"
    }
    if(any(grepl("mirna", nodes$omic, ignore.case = TRUE))){
      i=grep('mirna', nodes$omic, ignore.case = TRUE)
      nodes$shape[i]<-'diamond'
    }
    nodes$shape[nodes$omic == "targetF"] <- "box"
    
    edges <- data.frame(
      source = df[, 'regulator'],
      target = df[, 'targetF'],
      sign = df[, 8],
      line = df[, 7],
      stringsAsFactors = FALSE
    )
    
    return(list(nodes = nodes, edges = edges))
  }
}


style_edges_by_line_type <- function(edges) {
  if (nrow(edges) == 0 || is.null(edges$line) || is.null(edges$sign)) return(edges)
  
  edges$line <- as.character(edges$line)
  edges$sign <- as.character(edges$sign)
  
  edges <- edges %>%
    mutate(
      # Color del eje (basado en el signo)
      color = dplyr:: case_when(
        sign == "p" ~ "blue",
        sign == "n" ~ "red",
        TRUE ~ "gray"
      ),
      
      # Tipo de línea (dashes) basado en las nuevas reglas
      dashes = dplyr::case_when(
        line == "dots" ~ "[1, 5]",      # Patrón para puntos (guion corto, espacio largo)
        line == "dashed" ~ "[8, 8]",    # Patrón de guiones por defecto
        line == "vertical_lines" ~ "[1, 5, 10, 5, 1, 15]", # Patrón para líneas verticales (guion muy corto, espacio largo)
        line == "double_parallel_lines" ~ "[1, 3, 10, 15]", # Patrón para dobles líneas (simulación)
        TRUE ~ "[]"                     # Para línea continua
      ),
      
      # smooth.type para dashes (útil para puntos y a veces para otras estilizaciones)
      smooth.type = dplyr::case_when(
        line == "dots" ~ "dashes",       # Usa un tipo de "suavizado" que renderice puntos
        TRUE ~ "continuous"              # Por defecto, lineal (sin suavizado extra o patrones de guiones)
      ),
      smooth.roundness = 0.5, # Puede ajustar esto si usa smooth.type para curvas
      
      width = dplyr::case_when(
        line == "double_parallel_lines" ~ 2, # Quizás un poco más ancha para simular el doble
        TRUE ~ 2 # Ancho por defecto
      ),
      arrows = ""
    ) %>%
    # Asegurarse de que smooth.enabled sea TRUE si dashes es TRUE para algunos casos
    mutate(
      # ¡CAMBIO AQUÍ: Usamos | (OR vectorial) en lugar de || !
      smooth.enabled = (dashes != "[]" | smooth.type != "continuous") # Si dashes NO es vacío O smooth.type NO es continuo
    )
  
  return(edges)
}

generate_legend <- function(nodes) {
  
  # Crear una leyenda basada en combinaciones únicas de color y omic
  legend_items <- nodes %>%
    group_by(omic, color, shape) %>%
    slice(1) %>%  # tomar solo uno por grupo
    ungroup() %>%
    mutate(
      label = omic,
      size = 20 # tamaño del nodo en la leyenda
    ) %>%
    select(label, color, shape, size)
  
  return(legend_items)
}
