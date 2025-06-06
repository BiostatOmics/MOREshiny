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


biostat_colors <- c("#3bdfa2",
                    "#d11d56",
                    "#FDDB73",
                    "#7332a7",
                    "#d6b0e9",
                    "#657FCC",
                    "#126D6D",
                    "#6CD0D0",
                    "#F7FB7B",
                    "#FFB000")



networkMORE_cyj <- function(outputRegpcond, group1 = NULL, group2 = NULL, pc = 0, pathway = NULL, annotation = NULL) {
  
  create_graph <- function(df, pc) {
    df = df[df[,4] != 0, ]
    qc = quantile(abs(df[,4]), pc)[[1]]
    df = df[which(abs(df[,4]) >= qc), , drop = FALSE]
    
    # Procesar reguladores
    regulators <- unique(df[, c('regulator', 'omic')])
    regulators$group <- sub("^(.*?)-.*$", "\\1", regulators$regulator)
    colors <- rainbow(length(unique(regulators$group)))
    regulators$color <- colors[match(regulators$group, unique(regulators$group))]
    
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
      color = "lightgray",
      stringsAsFactors = FALSE
    )
    
    nodes <- rbind(targets, reg_nodes)
    
    edges = data.frame(
      source = df[, 'regulator'],
      target = df[, 'targetF'],
      coef = round(df[, 4], 5),
      sign = ifelse(df[, 4] > 0, 'p', 'n'),
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
    gr_cols <- grep("^Group", colnames(outputRegpcond), value = TRUE)
    df <- outputRegpcond[, c("regulator", "targetF", "omic", gr_cols)]
    df$mean_coef <- rowMeans(df[, 4:ncol(df)], na.rm = TRUE)
    df <- df[, c(1, 2, 3, ncol(df))]
    return(create_graph(df, pc))
  } else if (is.null(group2)) {
    ngroup <- grep(group1, colnames(outputRegpcond))
    df <- outputRegpcond[, c(1, 2, 3, ngroup)]
    return(create_graph(df, pc))
  } else {
    gr1 <- grep(group1, colnames(outputRegpcond))
    gr2 <- grep(group2, colnames(outputRegpcond))
    if (length(gr1) != 1 || length(gr2) != 1 || gr1 == gr2)
      stop("ERROR: group1 and group2 should be different names of groups to compare")
    
    df <- outputRegpcond[, c(1, 2, 3, gr1, gr2)]
    df[, 6] <- df[, 5] - df[, 4]
    df <- df[df[, 6] != 0, ]
    qc <- quantile(abs(df[, 6]), pc)[[1]]
    df <- df[abs(df[, 6]) >= qc, , drop = FALSE]
    df <- DifLineType(df)
    
    # Procesar reguladores
    regulators <- unique(df[, c('regulator', 'omic')])
    regulators$group <- sub("^(.*?)-.*$", "\\1", regulators$regulator)
    
    
    group_levels <- unique(regulators$group)
    colors <- setNames(biostat_colors[seq_along(group_levels)], group_levels)
    regulators$color <- colors[regulators$group]
    
    reg_nodes <- data.frame(
      id = regulators$regulator,
      omic = regulators$omic,
      color = regulators$color,
      stringsAsFactors = FALSE
    )
    
    targets <- data.frame(
      id = unique(df[, 'targetF']),
      omic = 'targetF',
      color = "lightgray",
      stringsAsFactors = FALSE
    )
    
    nodes <- rbind(targets, reg_nodes)
    
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
      color = dplyr::case_when(
        sign == "p" ~ "blue", # Coeficiente positivo
        sign == "n" ~ "red",  # Coeficiente negativo
        TRUE ~ "gray"         # Por defecto
      ),
      
      # Tipo de línea (dashes) basado en las nuevas reglas
      dashes = dplyr::case_when(
        line == "dots" ~ "[1, 5]",      # Patrón para puntos (guion corto, espacio largo)
        line == "dashed" ~ "[5, 5]",    # Patrón de guiones por defecto
        line == "vertical_lines" ~ "[1, 5]", # Patrón para líneas verticales (guion muy corto, espacio largo)
        line == "double_parallel_lines" ~ "[3, 3, 3, 3]", # Patrón para dobles líneas (simulación)
        TRUE ~ "[]"                     # Para línea continua
      ),
      
      # smooth.type para dashes (útil para puntos y a veces para otras estilizaciones)
      smooth.type = dplyr::case_when(
        line == "dots" ~ "dashes",       # Usa un tipo de "suavizado" que renderice puntos
        TRUE ~ "continuous"              # Por defecto, lineal (sin suavizado extra o patrones de guiones)
      ),
      smooth.roundness = 0.5, # Puede ajustar esto si usa smooth.type para curvas
      
      width = dplyr::case_when(
        line == "double_parallel_lines" ~ 4, # Quizás un poco más ancha para simular el doble
        TRUE ~ 2 # Ancho por defecto
      ),
      arrows = "to"
    ) %>%
    # Asegurarse de que smooth.enabled sea TRUE si dashes es TRUE para algunos casos
    mutate(
      # ¡CAMBIO AQUÍ: Usamos | (OR vectorial) en lugar de || !
      smooth.enabled = (dashes != "[]" | smooth.type != "continuous") # Si dashes NO es vacío O smooth.type NO es continuo
    )
  
  return(edges)
}

generate_legend <- function(nodes) {
  # Solo reguladores, ignoramos los nodos que son targets
  unique_nodes <- nodes[nodes$omic != "targetF", ]
  
  # Extraer ómica desde el nombre del regulador (antes del "-")
  unique_nodes$group <- sub("^(.*?)-.*$", "\\1", unique_nodes$id)
  
  # Etiqueta: "ómica"
  unique_nodes$label <- unique_nodes$group
  
  legend_items <- unique_nodes %>%
    distinct(color, label) %>%
    mutate(shape = "dot", size = 10)
  
  return(legend_items)
}