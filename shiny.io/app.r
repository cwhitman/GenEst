require(shiny)
require(DT)

#' @title Update the reactive value list when SE data are read in
#'
#' @description Update the rv list when the SE data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_data_SE <- function(rv, input){
  rv$data_SE <- read.csv(input$file_SE$datapath, stringsAsFactors = FALSE)
  rv$colNames_SE <- colnames(rv$data_SE)
  rv$colNames_all <- updateColNames_all(rv)
  rv$sizeclassCol <- updateSizeclassCol(input$sizeclassCol, rv$colNames_all)
  rv$colNames_SE_sel <- c(rv$sizeclassCol)
  rv$colNames_SE_nosel <- removeSelCols(rv$colNames_SE, rv$colNames_SE_sel)
  return(rv)
}

#' @title Update the reactive value list when CP data are read in
#'
#' @description Update the rv list when the CP data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_data_CP <- function(rv, input){
  rv$data_CP <- read.csv(input$file_CP$datapath, stringsAsFactors = FALSE)
  rv$colNames_CP <- colnames(rv$data_CP)
  rv$colNames_all <- updateColNames_all(rv)
  rv$sizeclassCol <- updateSizeclassCol(input$sizeclassCol, rv$colNames_all)
  rv$colNames_CP_sel <- c(rv$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  return(rv)
}

#' @title Update the reactive value list when SS data are read in
#'
#' @description Update the rv list when the SS data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_data_SS <- function(rv, input){
  rv$data_SS <- read.csv(input$file_SS$datapath, stringsAsFactors = FALSE)
  rv$colNames_SS <- colnames(rv$data_SS)
  return(rv)
}

#' @title Update the reactive value list when DWP data are read in
#'
#' @description Update the rv list when the DWP data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_data_DWP <- function(rv, input){
  rv$data_DWP <- read.csv(input$file_DWP$datapath, stringsAsFactors = FALSE)
  rv$colNames_DWP <- colnames(rv$data_DWP)
  return(rv)
}

#' @title Update the reactive value list when CO data are read in
#'
#' @description Update the rv list when the CO data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_data_CO <- function(rv, input){
  rv$data_CO <- read.csv(input$file_CO$datapath, stringsAsFactors = FALSE)
  rv$colNames_CO <- colnames(rv$data_CO)
  rv$colNames_COdates <- dateCols(rv$data_CO)
  rv$colNames_all <- updateColNames_all(rv)
  rv$sizeclassCol <- updateSizeclassCol(input$sizeclassCol, rv$colNames_all)
  return(rv)
}

#' @title Run the SE Models
#'
#' @description Use the inputs to run the SE models requested by the UI
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_run_SE <- function(rv, input){
  rv$obsCols_SE <- input$obsCols_SE
  rv$preds_SE <- input$preds_SE
  rv$predictors_SE <- prepPredictors(rv$preds_SE)
  rv$formula_p <- formula(paste0("p~", rv$predictors_SE))
  rv$formula_k <- formula(paste0("k~", rv$predictors_SE)) 
  rv$kFixedChoice <- input$kFixedChoice
  rv$kFixed <- setkFix(input$kFixedChoice, input$kFixed)
  rv$CL <- input$CL
  rv$sizeclassCol <- input$sizeclassCol
  rv$mods_SE <- suppressWarnings(
                  pkmSetSize(formula_p = rv$formula_p,
                    formula_k = rv$formula_k, data = rv$data_SE, 
                    obsCol = rv$obsCols_SE, sizeclassCol = rv$sizeclassCol,
                    kFixed = rv$kFixed, kInit = 0.7, CL = rv$CL, 
                    quiet = TRUE
                  ) 
                ) 
  rv$mods_SE_og <- rv$mods_SE
  rv$mods_SE <- pkmSetSizeFailRemove(rv$mods_SE)
  if (!all(unlist(pkmSetSizeFail(rv$mods_SE))) &&
      !any(unlist(lapply(rv$mods_SE_og, pkmSetAllFail)))){
    rv$sizeclasses <- updateSizeclasses(rv$data_SE, rv$sizeclassCol)
    rv$sizeclasses_SE <- rv$sizeclasses
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$outsizeclassSE)
    rv$sizeclass_SE <- rv$sizeclass
    rv$AICcTab_SE <- pkmSetAICcTab(rv$mods_SE[[rv$sizeclass_SE]], TRUE)
    rv$modOrder_SE <- as.numeric(row.names(rv$AICcTab_SE))
    rv$modNames_SE <- names(rv$mods_SE[[rv$sizeclass_SE]])[rv$modOrder_SE]
    rv$modNames_SEp <- modNameSplit(rv$modNames_SE, 1)
    rv$modNames_SEk <- modNameSplit(rv$modNames_SE, 2)
    rv$modSet_SE <- rv$mods_SE[[rv$sizeclass_SE]]
    rv$best_SE <- (names(rv$modSet_SE)[rv$modOrder_SE])[1]
    rv$modTab_SE <- rv$mods_SE[[rv$sizeclass_SE]][[rv$best_SE]]$cellwiseTable
    rv$modTabPretty_SE <- prettyModTabSE(rv$modTab_SE, rv$CL)
    rv$modTabDL_SE <- dlModTabSE(rv$modTab_SE, rv$CL)
    rv$figH_SE <- setFigH(rv$modSet_SE)
    rv$figW_SE <- setFigW(rv$modSet_SE)
  }
  return(rv)
}

#' @title Update the SE reactive values when the size class is chosen
#'
#' @description Update the SE reactive values when the size class is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_outsc_SE <- function(rv, input){
  if (length(rv$mods_SE) > 0){
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$outsizeclassSE)
    rv$sizeclass_SE <- rv$sizeclass
    rv$AICcTab_SE <- pkmSetAICcTab(rv$mods_SE[[rv$sizeclass_SE]], TRUE)
    rv$modOrder_SE <- as.numeric(row.names(rv$AICcTab_SE))
    rv$modNames_SE <- names(rv$mods_SE[[rv$sizeclass_SE]])[rv$modOrder_SE]
    rv$modNames_SEp <- modNameSplit(rv$modNames_SE, 1)
    rv$modNames_SEk <- modNameSplit(rv$modNames_SE, 2)
    rv$modSet_SE <- rv$mods_SE[[rv$sizeclass_SE]]
    rv$best_SE <- (names(rv$modSet_SE)[rv$modOrder_SE])[1]
    rv$modTab_SE <- rv$mods_SE[[rv$sizeclass_SE]][[rv$best_SE]]$cellwiseTable
    rv$modTabPretty_SE <- prettyModTabSE(rv$modTab_SE, rv$CL)
    rv$modTabDL_SE <- dlModTabSE(rv$modTab_SE, rv$CL)
    rv$figH_SE <- setFigH(rv$modSet_SE)
    rv$figW_SE <- setFigW(rv$modSet_SE)
  }
  return(rv)
}

#' @title Update the SE reactive values when a p or k model is chosen
#'
#' @description Update the SE reactive values when a p or k model is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_outpk_SE <- function(rv, input){
  if (length(rv$mods_SE) > 0){
    rv$outSEpk <- modNamePaste(c(input$outSEp, input$outSEk))
    rv$modSet_SE <- rv$mods_SE[[rv$sizeclass]]

    if (rv$outSEpk %in% names(rv$modSet_SE)){
      rv$modTab_SE <- rv$modSet_SE[[rv$outSEpk]]$cellwiseTable
      rv$modTabPretty_SE <- prettyModTabSE(rv$modTab_SE, rv$CL)
      rv$modTabDL_SE <- dlModTabSE(rv$modTab_SE, rv$CL)
    } else {
      rv$modTab_SE <- NULL
      holder <- data.frame(msg = "Selected model was not successfully fit.")
      rv$modTabPretty_SE <- holder
      rv$modTabDL_SE <- holder
    }
  }
  return(rv)
}

#' @title Run the CP Models
#'
#' @description Use the inputs to run the CP models requested by the UI
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_run_CP <- function(rv, input){

  rv$ltp <- input$ltp
  rv$fta <- input$fta
  rv$preds_CP <- input$preds_CP
  rv$dists <- input$dists  
  rv$nsim <- input$nsim
  rv$CL <- input$CL
  rv$sizeclassCol <- input$sizeclassCol
  rv$predictors_CP <- prepPredictors(rv$preds_CP)
  rv$formula_l <- formula(paste("l~", rv$predictors_CP, sep = ""))
  rv$formula_s <- formula(paste("s~", rv$predictors_CP, sep = "")) 

  rv$mods_CP <- suppressWarnings(
                  cpmSetSize(formula_l = rv$formula_l,
                    formula_s = rv$formula_s, data = rv$data_CP, 
                    left = rv$ltp, right = rv$fta, dists = rv$dists,
                    sizeclassCol = rv$sizeclassCol, CL = rv$CL, quiet = TRUE
                  )
                )
  rv$mods_CP_og <- rv$mods_CP
  rv$mods_CP <- cpmSetSizeFailRemove(rv$mods_CP)

  if (!all(unlist(cpmSetSizeFail(rv$mods_CP)))){
    rv$sizeclasses <- updateSizeclasses(rv$data_CP, rv$sizeclassCol)
    rv$sizeclasses_CP <- rv$sizeclasses
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$outsizeclassCP)
    rv$sizeclass_CP <- rv$sizeclass
    rv$AICcTab_CP <- cpmSetAICcTab(rv$mods_CP[[rv$sizeclass_CP]], TRUE)
    rv$AICcTab_CP[ , "Scale Formula"] <- gsub("NULL", "", 
                                           rv$AICcTab_CP[ , "Scale Formula"]
                                         )
    rv$modOrder_CP <- as.numeric(row.names(rv$AICcTab_CP))
    rv$modNames_CP <- names(rv$mods_CP[[rv$sizeclass_CP]])[rv$modOrder_CP]
    rv$modNames_CPdist <- modNameSplit(rv$modNames_CP, 1)
    rv$modNames_CPl <- modNameSplit(rv$modNames_CP, 2)
    rv$modNames_CPs <- modNameSplit(rv$modNames_CP, 3)
    rv$modSet_CP <- rv$mods_CP[[rv$sizeclass_CP]]
    rv$best_CP <- (names(rv$modSet_CP)[rv$modOrder_CP])[1]
    rv$modTab_CP <- 
                  rv$mods_CP[[rv$sizeclass_CP]][[rv$best_CP]]$cellwiseTable_ls
    rv$modTabPretty_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)
    rv$modTabDL_CP <- dlModTabCP(rv$modTab_CP, rv$CL)
    rv$best_CP <- gsub("NULL", "s ~ 1", rv$best_CP)
    rv$figH_CP <- setFigH(rv$modSet_CP, "CP")
    rv$figW_CP <- setFigW(rv$modSet_CP)
  }
  return(rv)
}

#' @title Update the SE reactive values when the size class is chosen
#'
#' @description Update the SE reactive values when the size class is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_outsc_CP <- function(rv, input){
  if (length(rv$mods_CP) > 0){
    rv$sizeclass <- pickSizeclass(rv$sizeclasses, input$outsizeclassCP)
    rv$sizeclass_CP <- rv$sizeclass
    rv$AICcTab_CP <- cpmSetAICcTab(rv$mods_CP[[rv$sizeclass_CP]], TRUE)
    rv$modOrder_CP <- as.numeric(row.names(rv$AICcTab_CP))
    rv$modNames_CP <- names(rv$mods_CP[[rv$sizeclass_CP]])[rv$modOrder_CP]
    rv$modNames_CPdist <- modNameSplit(rv$modNames_CP, 1)
    rv$modNames_CPl <- modNameSplit(rv$modNames_CP, 2)
    rv$modNames_CPs <- modNameSplit(rv$modNames_CP, 3)
    rv$modSet_CP <- rv$mods_CP[[rv$sizeclass_CP]]
    rv$best_CP <- (names(rv$modSet_CP)[rv$modOrder_CP])[1]
    rv$modTab_CP <- 
                  rv$mods_CP[[rv$sizeclass_CP]][[rv$best_CP]]$cellwiseTable_ls
    rv$modTabPretty_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)
    rv$modTabDL_CP <- dlModTabCP(rv$modTab_CP, rv$CL)
    rv$figH_CP <- setFigH(rv$modSet_CP, "CP")
    rv$figW_CP <- setFigW(rv$modSet_CP)
    rv$best_CP <- gsub("NULL", "s ~ 1", rv$best_CP)
  }
  return(rv)
}

#' @title Update the CP reactive values when a distribution or l or s model 
#'   is chosen
#'
#' @description Update the CP reactive values when a distribution or l or s 
#'   model is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_outdls_CP <- function(rv, input){
  if (length(rv$mods_CP) > 0){
    rv$CPdls <- c(input$outCPdist, input$outCPl, input$outCPs)
    rv$outCPdlsfig <- modNamePaste(rv$CPdls, "CP")
    rv$outCPdlstab <- modNamePaste(rv$CPdls, "CP", tab = TRUE)
    rv$modSet_CP <- rv$mods_CP[[rv$sizeclass]]

    if (rv$outCPdlstab %in% names(rv$modSet_CP)){
      rv$modTab_CP <- rv$modSet_CP[[rv$outCPdlstab]]$cellwiseTable_ls
      rv$modTabPretty_CP <- prettyModTabCP(rv$modTab_CP, rv$CL)
      rv$modTabDL_CP <- dlModTabCP(rv$modTab_CP, rv$CL)
    } else {
      rv$modTab_CP <- NULL
      holder <- data.frame(msg = "Selected model was not successfully fit.")
      rv$modTabPretty_CP <- holder
      rv$modTabDL_CP <- holder
    }
  }
  return(rv)
}

#' @title Update the SS reactive values when the SS are chosen
#'
#' @description Update the SS reactive values when the SS are chosen
#'
#' @param rv the reactive values list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_useSSdata <- function(rv){
  rv$SS <- NULL
  rv$SStemp <- tryCatch(averageSS(rv$data_SS), error = function(x){NA})
  if (!is.na(rv$SStemp[1])){
    rv$SS <- rv$SStemp
    rv$avgSI <-  mean(diff(rv$SS[-length(rv$SS)]))
    rv$SStext <- paste(rv$SS, collapse = ", ")
  }
  rv
}

#' @title Update the SS reactive values when the average SS is chosen
#'
#' @description Update the SS reactive values when the average SS is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_useSSinputs <- function(rv, input){
  rv$SStemp <- NA
  rv$gSearchInterval <- input$gSearchInterval
  rv$gSearchMax <- input$gSearchMax
  if (rv$gSearchInterval > 0){
    rv$SStemp <- seq(0, rv$gSearchMax, by = rv$gSearchInterval)
  }
  if (any(is.na(rv$SStemp)) || any(rv$SStemp < 0) | any(rv$SStemp %% 1 != 0)){
    rv$SStemp <- NA
  }
  if (!is.na(rv$SStemp[1])){
    rv$SS <- seq(0, rv$gSearchMax, by = rv$gSearchInterval)
    if (max(rv$SS) != rv$gSearchMax){
      rv$SS <- c(rv$SS, rv$gSearchMax)
    }
    rv$SStext <- paste(rv$SS, collapse = ", ")
  }
  rv
}

#' @title Run the g Model
#'
#' @description Use the inputs to run the g model requested by the UI
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_run_g <- function(rv, input){
  rv$CL <- input$CL
  rv$kFill_g <- NA
  if (length(rv$obsCols_SE) == 1){
    rv$kFill_g <- input$kFill_g
  }
  rv$sizeclasses_g <- rv$sizeclasses
  rv$nsizeclasses_g <- length(rv$sizeclasses_g)
  if (length(rv$nsizeclasses_g) == 1){
    if (is.null(rv$sizeclasses_g)){
      rv$sizeclasses_g <- "all"
      rv$nsizeclasses_g <- 1
    }
  }

  rv$nsim <- input$nsim
  rv$gGeneric <- vector("list", length = rv$nsizeclasses_g)
  for (sci in 1:rv$nsizeclasses_g){

    rv$SEmodToUse_g <- input[[sprintf("modelChoices_SE%d", sci)]]
    rv$CPmodToUse_g <- input[[sprintf("modelChoices_CP%d", sci)]]
    if (!grepl("s ~", rv$CPmodToUse_g)){
      rv$CPmodToUse_g <- paste(rv$CPmodToUse_g, "; NULL", sep = "")
    }
    rv$CPmodToUse_g <- paste("dist: ", rv$CPmodToUse_g, sep = "")

    rv$gGeneric[[sci]] <- tryCatch(
                            estgGeneric(nsim = rv$nsim, days = rv$SS,
                              model_SE = rv$mods_SE[[sci]][[rv$SEmodToUse_g]],
                              model_CP = rv$mods_CP[[sci]][[rv$CPmodToUse_g]],
                              kFill = rv$kFill_g
                            ), 
                            error = function(x){NULL}
                          )
  }
  names(rv$gGeneric) <- rv$sizeclasses_g
  rv$sizeclass_g <- rv$sizeclasses_g[1]
  rv
}

#' @title Update the g reactive values when the size class is chosen
#'
#' @description Update the g reactive values when the size class is chosen
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_outsc_g <- function(rv, input){
  rv$sizeclass_g <- pickSizeclass(rv$sizeclasses_g, input$outsizeclassg)
  rv$CL <- input$CL
  rv
}

#' @title Run the M Model
#'
#' @description Use the inputs to run the M model requested by the UI
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_run_M <- function(rv, input){

  rv$M <- NULL
  rv$kFill <- NULL
  if (length(rv$obsCols_SE) == 1){
    rv$kFill <- input$kFill
  }
  rv$nsizeclasses <- length(rv$sizeclasses)
  if (length(rv$nsizeclasses) == 1){
    if (is.null(rv$sizeclasses)){
      rv$sizeclasses <- "all"
     }
  }
  rv$dateFoundCol <- input$dateFoundCol
  rv$nsim <- input$nsim
  rv$frac <- input$frac
  rv$SEmodToUse <- rep(NA, rv$nsizeclasses)
  rv$CPmodToUse <- rep(NA, rv$nsizeclasses)

  for (sci in 1:rv$nsizeclasses){
    rv$SEmodToUse[sci] <- input[[sprintf("modelChoices_SE%d", sci)]]
    rv$CPmodToUse[sci] <- input[[sprintf("modelChoices_CP%d", sci)]]
    if (!grepl("s ~", rv$CPmodToUse[sci])){
      rv$CPmodToUse[sci] <- paste(rv$CPmodToUse[sci], "; NULL", sep = "")
    }
    rv$CPmodToUse[sci] <- paste("dist: ", rv$CPmodToUse[sci], sep = "")
  }
  names(rv$SEmodToUse) <- rv$sizeclasses
  names(rv$CPmodToUse) <- rv$sizeclasses

  rv$models_SE <- trimSetSize(rv$mods_SE, rv$SEmodToUse)
  rv$models_CP <- trimSetSize(rv$mods_CP, rv$CPmodToUse)

  if (rv$nsizeclasses > 1){
    rv$DWPCol <- NULL
    rv$sizeclassCol_M <- rv$sizeclassCol
  } else{
    rv$DWPCol <- input$DWPCol  
    rv$sizeclassCol_M <- NULL
    rv$models_SE <- rv$models_SE[[1]]
    rv$models_CP <- rv$models_CP[[1]]
  }

  rv$M <- tryCatch(
            estM(data_CO = rv$data_CO, data_SS = rv$data_SS, rv$data_DWP,
              frac = rv$frac, model_SE = rv$models_SE, 
              model_CP = rv$models_CP, kFill = rv$kFill, 
              dateFoundCol = rv$dateFoundCol, DWPCol = rv$DWPCol,
              sizeclassCol = rv$sizeclassCol_M, nsim = rv$nsim, 
              max_intervals = 8
            ), error = function(x){NULL}, warning = function(x){NULL}
          )

  if (!is.null(rv$M)){
    rv$Msplit <- tryCatch(
                   calcSplits(M = rv$M$Mhat, Aj = rv$M$Aj,
                     split_SS = NULL, split_CO = NULL,
                     data_SS = rv$data_SS, data_CO = rv$data_CO
                   ), error = function(x){NULL}, warning = function(x){NULL}
                 )
    rv$unitCol <- intersect(rv$colNames_CO, rv$colNames_DWP)  
    rv$colNames_SS_sel <- colnames(rv$data_SS) %in% rv$data_CO[ , rv$unitCol]
    rv$colNames_SS_nosel <- rv$colNames_SS[rv$colNames_SS_sel == FALSE]  
  }

  rv
}


#' @title Update the M reactive values when M is split
#'
#' @description Update the M reactive values when M is split
#'
#' @param rv the reactive values list
#'
#' @param input the input list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_split_M <- function(rv, input){
  rv$Msplit <- NULL
  rv$split_CO <- input$split_CO
  rv$split_SS <- input$split_SS
  rv$nsplit_CO <- length(rv$split_CO)
  rv$nsplit_SS <- length(rv$split_SS)
  rv$dateFoundCol <- input$dateFoundCol

  rv$Msplit <- tryCatch(
                 calcSplits(M = rv$M$Mhat, Aj = rv$M$Aj,
                   split_SS = rv$split_SS, split_CO = rv$split_CO,
                   data_SS = rv$data_SS, data_CO = rv$data_CO
                 ), error = function(x){NULL}, warning = function(x){NULL}
               )
  if (!is.null(rv$Msplit)){
    rv$figH_M <- 600
    if (length(attr(rv$Msplit, "vars")) > 1){
      rv$figH_M <- max(600, 300 * length(rv$Msplit))
    }
  }
  rv
}


#' @title Update the M reactive values when M split is transposed
#'
#' @description Update the M reactive values when M split is transposed
#'
#' @param rv the reactive values list
#'
#' @return an updated reactive values list
#'
#' @export
#'
update_rv_transpose_split <- function(rv){
  if (rv$nsplit_CO + rv$nsplit_SS == 2){
    rv$Msplit <- transposeSplits(rv$Msplit)
  } 
  rv
}
#' @title Update the size classes
#'
#' @description Determine the options for size classes, based on a data table
#'   and column name, returning \code{NULL} if no size class column is
#'   provided
#'
#' @param data data table to draw sizes from
#'
#' @param sizeclassCol size class column name 
#'
#' @return unique size classes
#'
#' @export
#'
updateSizeclasses <- function(data, sizeclassCol){
  if (is.null(sizeclassCol)){
    return("all")
  }
  return(as.character(unique(data[ , sizeclassCol])))
}

#' @title Locate the sizeclass selected by the inputs
#'
#' @description Locate the selection of a size class from the size class 
#'   column, retuning the first option from the size classes if the selection
#'   is not available. 
#'
#' @param sizeclasses size class options
#'
#' @param choice size class chosen
#'
#' @return location of the size class chosen
#'
#' @export
#'
pickSizeclass <- function(sizeclasses, choice){

  sizeclass <- NULL
  if (!(choice %in% sizeclasses)){
    choice <- sizeclasses[1]
  }
  sizeclass <- sizeclasses[which(sizeclasses == choice)]
  return(sizeclass)
}

#' @title Update the name of the size class column based on available names
#'
#' @description Update the size class column name based on the available
#'   options. If the existing size class column name is no longer in the
#'   set of available names, a NULL is returned to reset the column name.
#'
#' @param sizeclassCol current size class column name
#'
#' @param colNames_all updated vector of column names in all needed tables
#'
#' @return updated sizeclassCol
#'
#' @export
#'
updateSizeclassCol <- function(sizeclassCol, colNames_all){
  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colNames_all)){
      NULL
    } else{
      sizeclassCol
    }
  } else{
    NULL
  }
}
#' @title Create the pretty version of the Searcher Efficiency model table
#'
#' @description Format a reader-friendly version of the parameter table from
#'   a Searcher Efficiency model, based on confidence level of interest
#'
#' @param modTab model table
#'
#' @param CL Confidence level
#'
#' @return pretty version of the SE model table
#'
#' @export
#'
prettyModTabSE <- function(modTab, CL = 0.95){

  kFit <- any(grepl("k_median", colnames(modTab)))
  
  if (!kFit){
    modTab$k_median <- NA
    modTab$k_lower <- NA
    modTab$k_upper <- NA
  }

  out <- modTab[ , c("cell", "p_median", "k_median")]
  ncell <- nrow(out)

  for (celli in 1:ncell){
    p_m <- modTab[celli, "p_median"]
    p_l <- modTab[celli, "p_lower"]
    p_u <- modTab[celli, "p_upper"]
    k_m <- modTab[celli, "k_median"]
    k_l <- modTab[celli, "k_lower"]
    k_u <- modTab[celli, "k_upper"]
    out[celli, "p_median"] <- paste0(p_m, " [", p_l, " - ", p_u, "]")

    if (is.na(k_m)){
      out[celli, "k_median"] <- ""
    } else{
      out[celli, "k_median"] <- paste0(k_m, " [", k_l, " - ", k_u, "]")
    }
  }

  coltxt <- paste0(" (Median [", 100 * (1 - CL) / 2, "% - ", 
              100 - 100 * (1 - CL) / 2, "%])"
            )
  colnames(out) <- c("Cell", paste0(c("p", "k"), coltxt))
  return(out)
}

#' @title Create the download version of the Searcher Efficiency model table
#'
#' @description Format a user-friendly version of the parameter table from
#'   a Searcher Efficiency model, based on confidence level of interest
#'
#' @param modTab model table
#'
#' @param CL Confidence level
#'
#' @return download version of the SE model table
#'
#' @export
#'
dlModTabSE <- function(modTab, CL = 0.95){

  kFit <- any(grepl("k_median", colnames(modTab)))
  if (!kFit){
    modTab$k_median <- NA
    modTab$k_lower <- NA
    modTab$k_upper <- NA
  }

  out <- modTab
  lo <- 100 * (1 - CL) / 2
  up <- 100 - 100 * (1 - CL) / 2
  coltypes <- c("Median", paste0(lo, "%"), paste0(up, "%"))
  colnames(out) <- c("Cell", paste0("p ", coltypes), paste0("k ", coltypes))
  return(out)
}

#' @title Create the pretty version of the Carcass Persistence model table
#'
#' @description Format a reader-friendly version of the parameter table from
#'   a Carcass Persistence model, based on confidence level of interest
#'
#' @param modTab model table
#'
#' @param CL Confidence level
#'
#' @return pretty version of the CP model table
#'
#' @export
#'
prettyModTabCP <- function(modTab, CL = 0.95){
  out <- modTab[ , c("cell", "l_median", "s_median")]
  ncell <- nrow(out)

  for (celli in 1:ncell){
    l_m <- modTab[celli, "l_median"]
    l_l <- modTab[celli, "l_lower"]
    l_u <- modTab[celli, "l_upper"]
    s_m <- modTab[celli, "s_median"]
    s_l <- modTab[celli, "s_lower"]
    s_u <- modTab[celli, "s_upper"]
    out[celli, "l_median"] <- paste0(l_m, " [", l_l, " - ", l_u, "]")

    if (s_m == s_l & s_m == s_u & s_m == 1){
      out[celli, "s_median"] <- "1"
    } else{
      out[celli, "s_median"] <- paste0(s_m, " [", s_l, " - ", s_u, "]")
    }
  }

  coltxt <- paste0(" (Median [", 100 * (1 - CL) / 2, "% - ", 
              100 - 100 * (1 - CL) / 2, "%])"
            )
  colnames(out) <- c("Cell", paste0(c("Location", "Scale"), coltxt))
  return(out)
}

#' @title Create the download version of the Carcass Persistence model table
#'
#' @description Format a user-friendly version of the parameter table from
#'   a Carcass Persistence model, based on confidence level of interest
#'
#' @param modTab model table
#'
#' @param CL Confidence level
#'
#' @return download version of the SE model table
#'
#' @export
#'
dlModTabCP <- function(modTab, CL = 0.95){

  out <- modTab
  lo <- 100 * (1 - CL) / 2
  up <- 100 - 100 * (1 - CL) / 2
  coltypes <- c("Median", paste0(lo, "%"), paste0(up, "%"))
  colnames(out) <- c("Cell", paste0("Location ", coltypes), 
                     paste0("Scale ", coltypes))
  return(out)
}

#' @title Create the pretty version of the split summary table
#'
#' @description Format a reader-friendly version of the split summary table
#'   a mortality estimation
#'
#' @param splitSummary a split summary
#'
#' @return split pretty table 
#'
#' @export
#'
prettySplitTab <- function(splitSummary){

  vectored <- as.vector(splitSummary)

  if ("splitSummary" %in% attr(vectored, "class")){

    nspec <- length(splitSummary)
    Out <- prettySplitSpecTab(splitSummary[[1]])
    Out[, 1] <- row.names(splitSummary[[1]])
    colnames(Out)[1] <- attr(splitSummary, "vars")[1]
    extralev <- rep(names(splitSummary)[1], nrow(Out))
    Out <- cbind(extralev, Out)    
    colnames(Out)[1] <- attr(splitSummary, "vars")[2]

    for (speci in 2:nspec){
      specOut <- prettySplitSpecTab(splitSummary[[speci]])
      colnames(specOut)[1] <- attr(splitSummary, "vars")[1]
      specOut[, 1] <- row.names(splitSummary[[speci]])
      colnames(specOut)[1] <- attr(splitSummary, "vars")[1]
      extralev <- rep(names(splitSummary)[speci], nrow(specOut))
      specOut <- cbind(extralev, specOut)    
      colnames(specOut)[1] <- attr(splitSummary, "vars")[2]
      Out <- rbind(Out, specOut)
    }
    return(Out)
  } else{
    return(prettySplitSpecTab(splitSummary))
  }
}

#' @title Create the pretty version of a specific split summary table
#'
#' @description Format a reader-friendly version of a specific split summary 
#'   table from a mortality estimation
#'
#' @param splitSummarySpec a specific split summary
#'
#' @return specific split pretty table  
#'
#' @export
#'
prettySplitSpecTab <- function(splitSummarySpec){
  vectored <- as.vector(splitSummarySpec)
  nrows <- nrow(splitSummarySpec)
  ncols <- ncol(splitSummarySpec)
  if (is.null(nrows)){
    nrows <- 1
    ncols <- length(splitSummarySpec)
    rnames <- "all"
    cnames <- names(splitSummarySpec)
  } else{
    rnames <- rownames(splitSummarySpec)
    cnames <- colnames(splitSummarySpec)
  }
  prettyTab <- matrix(round(vectored, 2), nrow = nrows, ncol = ncols)
    
  colnames(prettyTab) <- cnames
  varname <- attr(splitSummarySpec, "vars")
  if (is.null(varname)){
    return(prettyTab)
  }
  prettyTab <- cbind(rnames, prettyTab)
  colnames(prettyTab)[1] <- varname
  return(prettyTab)
}#' @title About Main Panel UI element
#'
#' @description create the HTML code for the About main panel
#'
#' @return HTML for the about panel
#'
#' @export
#'
aboutPanel <- function(){
  fluidRow(
    column(5, offset = 2,
      br(), br(), 
      HTML("<b>Authors:</b>  
        Daniel Dalthorp
          <a href = 'http://www.USGS.gov'>(USGS)</a>,
        Juniper Simonis 
          <a href = 'http://www.dapperstats.com'>(DAPPER Stats)</a>,
        Manuela Huso
          <a href = 'http://www.USGS.gov'>(USGS)</a>,
        Lisa Madsen
          <a href = 'http://www.oregonstate.edu'>(OSU)</a>,
        Paul Rabie 
          <a href = 'http://www.west-inc.com'>(WEST)</a>, 
        Jeffrey Mintz
          <a href = 'http://www.USGS.gov'>(USGS)</a>,
        Robert Wolpert
          <a href = 'http://www2.stat.duke.edu/~rlw/'>(Duke)</a>,
        Jared Studyvin
          <a href = 'http://www.west-inc.com'>(WEST)</a>, 
        and Franzi Korner-Nievergelt
          <a href = 'http://www.oikostat.ch/'>(oikostat)</a>"),
      br(), br(),
      HTML("<b>Web Design and Graphics User Interface Programming:</b>  
        Juniper Simonis 
          <a href = 'http://www.dapperstats.com'>(DAPPER Stats)</a>"
      ),
      br(), br(),
      HTML("GenEst is a tool for estimating mortality from efficiency, 
        persistence, search schedule, and carcass data."
      ),
      br(), br(),
      HTML("GenEst is currently in development and should be considered
        provisional."
      ),
      br(), br(),
      textOutput("versionInfo"),
      br(), 
      HTML("The development of GenEst is being supported by 
        <a href = 'https://www.blm.gov/'>The US Bureau of Land Management</a>,
        <a href = 'https://www.usgs.gov/'>The US Geological Survey</a>,
        <a href = 'https://www.nrel.gov/'>National Renewable Energy 
          Laboratory</a>, 
        <a href = 'http://www.westconsultants.com/'>WEST, Inc.</a>, 
        <a href = 'http://www.batcon.org/'>Bat Conservation International</a>,
        <a href = 'http://www.avangridrenewables.us/'>Avangrid Renewables</a>,
        <a href = 'https://awwi.org/'>American Wind Wildlife Institute</a>, 
        and
        <a href = 'https://oregonstate.edu/'>Oregon State University</a>."),
      br(), br(),
      HTML("GenEst is provided under GNU GPL v3 (and later versions)."),
      br(), br(), br(), br(),
      HTML("<a href='https://www.blm.gov/'>
         <img src = 'blm.jpg' height = '60'></a>
         <a href='https://www.usgs.gov/'>
         <img src = 'usgs.png' height = '60'></a>
         <a href='https://www.nrel.gov/'>
         <img src = 'nrel.jpg' height = '60'> </a>
         <a href='http://www.westconsultants.com/'>
         <img src = 'west.png' height = '60'></a>
         <a href='http://www.batcon.org/'>
         <img src = 'bci.jpg' height = '60'></a>
         <a href='https://awwi.org/'>
         <img src = 'awwi.png' height = '60'></a>
         <a href='http://www.avangridrenewables.us/'>
         <img src = 'avangrid.png' height = '60'></a>
         <a href='http://www.dapperstats.com'>
         <img src = 'dapper.png' height = '60'></a>
         <a href='http://www.oikostat.ch/'>
         <img src = 'oikostat.jpg' height = '60'> </a>
         <a href='https://www.oregonstate.edu/'>
         <img src = 'osu.jpg' height = '60'> </a>
         <a href='https://www.duke.edu/'>
         <img src = 'duke.png' height = '60'></a>"
       )
    )
  )
}

#' @title Analysis Main Panel UI element 
#'
#' @description create the HTML code for the Analysis panel
#'
#' @return HTML for the Analysis panel
#'
#' @export
#'
analysisPanel <- function(){
  tabsetPanel(
    GeneralInputsPanel(),
    SEPanel(),
    CPPanel(),
    MPanel(),
    gPanel()
  )
}

#' @title Analysis General Inputs Main Panel UI element 
#'
#' @description create the HTML code for the Analysis General Inputs panel
#'
#' @return HTML for the Analysis General Inputs panel
#'
#' @export
#'
GeneralInputsPanel <- function(){
  tabPanel("General Inputs", br(), br(), GeneralInputSidebar())
}

#' @title Analysis General Inputs sidebar panel UI element 
#'
#' @description create the HTML code for the Analysis General Inputs sidebar
#'
#' @return HTML for the Analysis General Inputs sidebar
#'
#' @export
#'
GeneralInputSidebar <- function(){
  sidebarPanel(width = 3,
    numericInput("nsim", "Number of Iterations:", value = 1000,
      min = 1, max = 10000, step = 1
    ),
    numericInput("CL", "Confidence Level:", value = 0.95, min = 0, 
      max = 1, step = 0.001
    ),
    selectizeInput("sizeclassCol", "Size Class Column (optional):", 
      c("No data input yet"), multiple = TRUE, 
      options = list(maxItems = 1)
    )
  )
}
#' @title Analysis Carcass Persistence Main Panel UI element 
#'
#' @description create the HTML code for the Analysis CP panel
#'
#' @return HTML for the Analysis CP panel
#'
#' @export
#'
CPPanel <- function(){
  tabPanel("Carcass Persistence", br(), br(), 
    CPSidebar(), 
    CPMainPanel()
  )
}

#' @title Analysis Carcass Persistence sidebar panel UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   sidebar
#'
#' @return HTML for the Analysis Carcass Persistence sidebar
#'
#' @export
#'
CPSidebar <- function(){
  sidebarPanel(width = 3,
    HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
    br(), br(),
    selectizeInput("ltp", "Last Time Present:", c("No data input yet"), 
      multiple = TRUE, options = list(maxItems = 1)
    ),
    selectizeInput("fta", "First Time Absent:", c("No data input yet"),
      multiple = TRUE, options = list(maxItems = 1)
    ),
    selectizeInput("preds_CP", "Predictor Variables:", 
      c("No data input yet"), multiple = TRUE
    ),
    checkboxGroupInput("dists", label = "Distributions to Include",
      choices = CPdistOptions(), 
      selected = c("exponential", "weibull", "lognormal", "loglogistic"),
      inline = TRUE
    ),
    conditionalPanel(
       condition = "input.ltp == null | input.fta == null",
      br(), 
      HTML("<center><em>Select observation columns to run model</center></em>"
      )          
    ),
    conditionalPanel(
      condition = "input.ltp != null & input.fta != null",
      br(),
      actionButton("runMod_CP", "Run Model")
    ),
    conditionalPanel(condition = "output.CPModDone == 'OK'", 
      br(), br(), 
      HTML("<big><strong><u> Table & Figure Selection: </u></strong></big>"),
      br(), br(),
      selectizeInput("outsizeclassCP", "Size Class:",  " ", multiple = FALSE),
      selectizeInput("outCPdist", "Distribution:", " ", multiple = FALSE),
      selectizeInput("outCPl", "Location Model:", " ", multiple = FALSE),
      selectizeInput("outCPs", "Scale Model:", " ", multiple = FALSE)        
    )
  )
}

#' @title Analysis Carcass Persistence main panel UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main
#'
#' @return HTML for the Analysis Carcass Persistence main panel
#'
#' @export
#'
CPMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_CP",
      CPSelectedDataPanel(),
      CPFiguresPanel(),
      CPEstimatesPanel(),
      CPModComparisonPanel(),
      CPModSelectionPanel()
    )
  )
}

#' @title Analysis Carcass Persistence main panel selected data UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main panel selected data element
#'
#' @return HTML for the Analysis Carcass Persistence selected data panel
#'
#' @export
#'
CPSelectedDataPanel <- function(){
  tabPanel("Selected Data", br(), 
    conditionalPanel(condition = "input.ltp == null & input.fta == null",
      HTML("<em>Select observation columns to view data</em>")
    ), br(), 
    dataTableOutput("selected_CP")
  )
}

#' @title Analysis Carcass Persistence main panel figures UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main panel figures element
#'
#' @return HTML for the Analysis Carcass Persistence figures panel
#'
#' @export
#'
CPFiguresPanel <- function(){
  tabPanel("Figures", br(), 
    conditionalPanel(condition = "output.fig_CP == null",
      HTML("<em>Run model to view figures</em>")
    ),
    conditionalPanel(condition = "output.CPModDone == 'OK'",
      textOutput("sizeclass_CP1"), br(), 
      plotOutput("fig_CP", inline = TRUE), br(), br(),
      downloadButton("dlCPfig", "Download")
    )
  )
}

#' @title Analysis Carcass Persistence main panel estimates UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main panel model estimates element
#'
#' @return HTML for the Analysis Carcass Persistence model estimates panel
#'
#' @export
#'
CPEstimatesPanel <- function(){
  tabPanel("Estimates", br(),  
    conditionalPanel(condition = "output.modTab_CP == null",
      HTML("<em>Run model to view model estimates</em>")
    ),
    conditionalPanel(condition = "output.CPModDone == 'OK'",
      textOutput("sizeclass_CP2"), br(), 
      dataTableOutput("modTab_CP"), br(),
      downloadButton("dlCPest", "Download")
    )
  )
}

#' @title Analysis Carcass Persistence main panel model comparison UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main panel model comparison element
#'
#' @return HTML for the Analysis Carcass Persistence model comparison panel
#'
#' @export
#'
CPModComparisonPanel <- function(){
  tabPanel("Model Comparison", br(), 
    conditionalPanel(condition = "output.AICcTab_CP == null",
      HTML("<em>Run models to view model comparison</em>")
    ),
    conditionalPanel(condition = "output.CPModDone == 'OK'",
      textOutput("sizeclass_CP3"), br(), 
      dataTableOutput("AICcTab_CP"), br(),
      downloadButton("dlCPAICc", "Download")
    )
  )
}

#' @title Analysis Carcass Persistence main panel model selection UI element 
#'
#' @description create the HTML code for the Analysis Carcass Persistence
#'   main panel model selection element
#'
#' @return HTML for the Analysis Carcass Persistence model selection panel
#'
#' @export
#'
CPModSelectionPanel <- function(){
  tabPanel("Model Selection", br(), 
    conditionalPanel(condition = "output.modelMenu_CP == null",
      HTML("<em>Run models to select models</em>")
    ),
    htmlOutput("modelMenu_CP")
  )
}

#' @title Data Input Main Panel UI element 
#'
#' @description create the HTML code for the Data Input panel
#'
#' @return HTML for the Data Input panel
#'
#' @export
#'
dataInputPanel <- function(){
  sidebarLayout(dataInputSidebar(), loadedDataPanel())
}

#' @title Data Input sidebar panel UI element 
#'
#' @description create the HTML code for the Data Input sidebar
#'
#' @return HTML for the Data Input sidebar
#'
#' @export
#'
dataInputSidebar <- function(){
  sidebarPanel(width = 3,
    fileInput("file_SE", "Searcher Efficiency Data File",
      accept = c("text/csv", "text/comma-separated-values", ".csv")
    ), 
    fileInput("file_CP", "Carcass Persistence Data File",
      accept = c("text/csv", "text/comma-separated-values", ".csv")
    ), 
    fileInput("file_SS", "Search Schedule Data File",
      accept = c("text/csv", "text/comma-separated-values", ".csv")
    ), 
    fileInput("file_DWP", "Density Weighted Proportion Data File",
      accept = c("text/csv", "text/comma-separated-values", ".csv")
    ), 
    fileInput("file_CO", "Carcass Observation Data File",
      accept = c("text/csv", "text/comma-separated-values", ".csv")
    )
  )
}

#' @title Data Input loaded data UI element
#'
#' @description create the HTML code for the Data Input loaded data tables
#'
#' @return HTML for the Data Input data table ouput
#'
#' @export
#'
loadedDataPanel <- function(){
  mainPanel(
    tabsetPanel(id = "LoadedDataViz",
      dataTabPanel("Searcher Efficiency", "data_SE"),
      dataTabPanel("Carcass Persistence", "data_CP"),
      dataTabPanel("Search Schedule", "data_SS"),
      dataTabPanel("Density Weighted Proportion", "data_DWP"),
      dataTabPanel("Carcass Observations", "data_CO")
    )
  )
}

#' @title Data Input loaded data UI element for a single table
#'
#' @description create the HTML code for a single Data Input loaded data table
#'
#' @param tabname shiny ID name of the table 
#'
#' @param table table's name in input object
#'
#' @return HTML for the Data Input data table ouput
#'
#' @export
#'
dataTabPanel <- function(tabname, table){
  tabPanel(tabname, br(), dataTableOutput(table))
}
#' @title Analysis Detection Probability Main Panel UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability 
#'   panel
#'
#' @return HTML for the Analysis Detection Probability panel
#'
#' @export
#'
gPanel <- function(){
  tabPanel("Detection Probability", br(), br(),
    gSidebar(),
    gMainPanel()
  )
}

#' @title Analysis Detection Probability sidebar panel UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability 
#'   sidebar
#'
#' @return HTML for the Analysis Detection Probability sidebar
#'
#' @export
#'
gSidebar <- function(){
  sidebarPanel(width = 3, 
    HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
    br(), br(),
    HTML("<strong><u> Search Schedule Data: </u></strong>"),
    conditionalPanel(    
      condition = "output.data_SS == null",
      br(), 
      HTML("<center><em>Input search schedule data file</center></em>")
    ),
    conditionalPanel(    
      condition = "output.data_SS != null",
      br(), 
      actionButton("useSSdata", "Create Schedule")
    ),
    br(), br(),
    HTML("<strong><u> Generic Search Schedule Inputs: </u></strong>"),
    br(), br(),
    numericInput("gSearchInterval", "Search Interval (days):", 
      value = 7, min = 1, max = 400, step = 1),
    numericInput("gSearchMax", "Final Seach (day):", 
      value = 364, min = 1, max = 1000, step = 1),
    actionButton("useSSinputs", "Create Schedule"),
    conditionalPanel(
      condition = "output.kFillNeed == 'yes'",
      br(), br(),
      numericInput("kFill_g", "Assumed k:", value = 0.5, 
        min = 0, max = 1, step = 0.001
      )
    ),
    conditionalPanel(
      condition = 
        "input.modelChoices_SE1 == null | input.modelChoices_CP1 == null | 
         output.sizeclassesSE != output.sizeclassesCP",
      br(), 
      HTML("<center><em>Select SE and CP models fit to matching size
        classes to run model</center></em>"
      )
    ),
    conditionalPanel(
      condition = 
        "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
         output.sizeclassesSE == output.sizeclassesCP",
      br(), br(),
      actionButton("runMod_g", "Estimate")
    ),
    conditionalPanel(condition = "output.gModDone == 'OK'", 
      br(), br(), 
      HTML("<big><strong><u> Table & Figure Selection: 
        </u></strong></big>"
      ), br(), br(),
      selectizeInput("outsizeclassg", "Size Class:", 
        " ", multiple = FALSE
      )
    )
  )
}

#' @title Analysis Detection Probability main panel UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability 
#'   main panel
#'
#' @return HTML for the Analysis Detection Probability main panel
#'
#' @export
#'
gMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_g",
      gSchedulePanel(),
      gFigurePanel(),
      gSummaryPanel()
    )
  )
}

#' @title Analysis Detection Probability main panel schedule UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability
#'   main panel schedule element
#'
#' @return HTML for the Analysis Detection Probability schedule panel
#'
#' @export
#'
gSchedulePanel <- function(){
  tabPanel("Schedule",         
    br(), 
    HTML("<big><strong><u> Search Schedule: </u></strong></big>"),
    br(), br(), 
    textOutput("SStext")
  )
}

#' @title Analysis Detection Probability main panel figure UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability
#'   main panel figure element
#'
#' @return HTML for the Analysis Detection Probability figure panel
#'
#' @export
#'
gFigurePanel <- function(){
  tabPanel("Figure", br(), 
    conditionalPanel(condition = "output.fig_g == null",
      HTML("<em>Run estimate to view figure</em>")
    ), 
    conditionalPanel(condition = "output.gModDone == 'OK'",
      textOutput("sizeclass_g1"), br(), 
      plotOutput("fig_g", inline = TRUE), br(), br(),
      downloadButton("dlgfig", "Download")
    )
  )
}

#' @title Analysis Detection Probability main panel summary UI element 
#'
#' @description create the HTML code for the Analysis Detection Probability
#'   main panel summary element
#'
#' @return HTML for the Analysis Detection Probability summary panel
#'
#' @export
#'
gSummaryPanel <- function(){
  tabPanel("Summary", br(), 
    conditionalPanel(condition = "output.table_g == null",
      HTML("<em>Run estimate to view summary</em>")
    ), 
    conditionalPanel(condition = "output.gModDone == 'OK'",
      textOutput("sizeclass_g2"), br(), 
      br(), dataTableOutput("table_g"), br(),
      downloadButton("dlgtab", "Download")
    )
  )
}#' @title Analysis Mortality Main Panel UI element 
#'
#' @description create the HTML code for the Analysis Mortality panel
#'
#' @return HTML for the Analysis Mortality Estimation panel
#'
#' @export
#'
MPanel <- function(){
  tabPanel("Mortality Estimation", br(), br(), 
    MSidebar(),
    MMainPanel()
  )
}


#' @title Analysis Mortality sidebar panel UI element 
#'
#' @description create the HTML code for the Analysis Mortality sidebar
#'
#' @return HTML for the Analysis Mortality sidebar
#'
#' @export
#'
MSidebar <- function(){
  sidebarPanel(width = 3, 
    HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
    br(), br(),
    numericInput("frac", "Fraction of Facility Surveyed:", value = 1.0, 
      min = 0.01, max = 1.0, step = 0.01
    ),
    selectizeInput("dateFoundCol", "Date Found:", c("No data input yet"), 
      multiple = FALSE
    ),
    conditionalPanel(
      condition = "output.kFillNeed == 'yes'",
      numericInput("kFill", "Assumed k:", value = 0.5, 
        min = 0, max = 1, step = 0.001
      )
    ),
    conditionalPanel(
      condition = "output.DWPNeed == 'yes'",
      selectizeInput("DWPCol", "DWP Column", c("No data input yet"), 
        multiple = FALSE
      )
    ),
    conditionalPanel(
      condition = 
        "input.modelChoices_SE1 == null | input.modelChoices_CP1 == null | 
         output.sizeclassesSE != output.sizeclassesCP",
      br(), 
      HTML("<center><em>Select SE and CP models fit to matching size
        classes to run model</center></em>"
      )
    ),
    conditionalPanel(
      condition = 
        "input.modelChoices_SE1 != null & input.modelChoices_CP1 != null & 
         output.sizeclassesSE == output.sizeclassesCP",
      br(), 
      actionButton("runMod_M", "Estimate")
    ),
    conditionalPanel(
      condition = "output.MModDone == 'OK'",
      br(), br(), 
      HTML("<big><strong><u> Splitting Mortality: </u></strong></big>"),
      br(), br(), 
      HTML("<em>Max. two total splits, max. one schedule-based split</em>"),
      br(), br(),
      selectizeInput("split_SS", "Schedule Variable:", 
        " ", multiple = TRUE, options = list(maxItems = 1)
      ),
      selectizeInput("split_CO", "Observation Variable:", 
        " ", multiple = TRUE, options = list(maxItems = 2)
      ),
      br(),
      actionButton("splitM", "Split Estimate"),
      br(), br(), br(), br(), 
      actionButton("transposeSplit", "Transpose Split Plot")
    )
  )
}

#' @title Analysis Mortality main panel UI element 
#'
#' @description create the HTML code for the Analysis Mortality main panel
#'
#' @return HTML for the Analysis Mortality main panel
#'
#' @export
#'
MMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_M",
      MFigurePanel(),
      MSummaryPanel()
    )
  )
}


#' @title Analysis Mortality main panel figure UI element 
#'
#' @description create the HTML code for the Analysis Mortality
#'   main panel figure element
#'
#' @return HTML for the Analysis Mortality figure panel
#'
#' @export
#'
MFigurePanel <- function(){
  tabPanel("Figure", br(), 
    conditionalPanel(condition = "output.fig_M == null",
      HTML("<em>Run estimate to view figure</em>")
    ), 
    conditionalPanel(condition = "output.MModDone == 'OK'",
      plotOutput("fig_M", inline = TRUE), br(), br(),
      downloadButton("dlMfig", "Download")
    )
  )
}


#' @title Analysis Mortality main panel summary UI element 
#'
#' @description create the HTML code for the Analysis Mortality
#'   main panel summary element
#'
#' @return HTML for the Analysis Mortality summary panel
#'
#' @export
#'
MSummaryPanel <- function(){
  tabPanel("Summary", br(), 
    conditionalPanel(condition = "output.table_M == null",
      HTML("<em>Run estimate to view summary</em>")
    ), 
    conditionalPanel(condition = "output.MModDone == 'OK'",
      br(), dataTableOutput("table_M"), br(),
      downloadButton("dlMtab", "Download")
    )
  )
}

#' @title Analysis Searcher Efficiency Main Panel UI element 
#'
#' @description create the HTML code for the Analysis SE panel
#'
#' @return HTML for the Analysis SE panel
#'
#' @export
#'
SEPanel <- function(){
  tabPanel("Searcher Efficiency", br(), br(), 
    SESidebar(), 
    SEMainPanel()
  )
}

#' @title Analysis Searcher Efficiency sidebar panel UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   sidebar
#'
#' @return HTML for the Analysis Searcher Efficiency sidebar
#'
#' @export
#'
SESidebar <- function(){
  sidebarPanel(width = 3,
    HTML("<big><strong><u> Model Inputs: </u></strong></big>"), 
    br(), br(),
    selectizeInput("obsCols_SE", "Observations:", c("No data input yet"), 
      multiple = TRUE
    ),
    selectizeInput("preds_SE", "Predictor Variables:", 
      c("No data input yet"), multiple = TRUE
    ),
    radioButtons("kFixedChoice", "Fix k?",
      choices = list("No" = 0, "Yes" = 1), selected = 0
    ),
    conditionalPanel(condition = "input.kFixedChoice == 1",
      numericInput("kFixed", "Value for fixed k:", value = 0.5, 
        min = 0, max = 1, step = 0.001
      )
    ),
    conditionalPanel(condition = "input.obsCols_SE == null",
      br(), 
      HTML("<center><em>Select observation columns to run 
        model</center></em>"
      )          
    ),
    conditionalPanel(condition = "input.obsCols_SE != null",
      br(), 
      actionButton("runMod_SE", "Run Model")          
    ),
    conditionalPanel(condition = "output.SEModDone == 'OK'", 
      br(), br(),
      HTML("<big><strong><u> Table & Figure Selection:
      </u></strong></big>"), 
      br(), br(), 
      selectizeInput("outsizeclassSE",  "Size Class:", " ", multiple = FALSE),
      selectizeInput("outSEp", "p Model:", " ", multiple = FALSE), 
      selectizeInput("outSEk", "k Model:", " ", multiple = FALSE)
    )
  )
}

#' @title Analysis Searcher Efficiency main panel UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main
#'
#' @return HTML for the Analysis Searcher Efficiency main panel
#'
#' @export
#'
SEMainPanel <- function(){
  mainPanel(
    tabsetPanel(id = "analyses_SE",
      SESelectedDataPanel(),
      SEFiguresPanel(),
      SEEstimatesPanel(),
      SEModComparisonPanel(),
      SEModSelectionPanel()
    )
  )
}

#' @title Analysis Searcher Efficiency main panel selected data UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main panel selected data element
#'
#' @return HTML for the Analysis Searcher Efficiency selected data panel
#'
#' @export
#'
SESelectedDataPanel <- function(){
  tabPanel("Selected Data", br(), 
    conditionalPanel(condition = "input.obsCols_SE == null",
      HTML("<em>Select observation columns to view data</em>")
    ), br(), 
    dataTableOutput("selected_SE")
  )
}

#' @title Analysis Searcher Efficiency main panel figures UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main panel figures element
#'
#' @return HTML for the Analysis Searcher Efficiency figures panel
#'
#' @export
#'
SEFiguresPanel <- function(){
  tabPanel("Figures", br(), 
    conditionalPanel(condition = "output.fig_SE == null",
      HTML("<em>Run model to view figures</em>")
    ),
    conditionalPanel(condition = "output.SEModDone == 'OK'",
      textOutput("sizeclass_SE1"), br(), 
      plotOutput("fig_SE", inline = TRUE), br(), br(),
      downloadButton("dlSEfig", "Download")
    )
  )
}

#' @title Analysis Searcher Efficiency main panel estimates UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main panel model estimates element
#'
#' @return HTML for the Analysis Searcher Efficiency model estimates panel
#'
#' @export
#'
SEEstimatesPanel <- function(){
  tabPanel("Estimates", br(),  
    conditionalPanel(condition = "output.modTab_SE == null",
      HTML("<em>Run model to view model estimates</em>")
    ),
    conditionalPanel(condition = "output.SEModDone == 'OK'",
      textOutput("sizeclass_SE2"), br(), 
      dataTableOutput("modTab_SE"), br(),
      downloadButton("dlSEest", "Download")
    )
  )
}

#' @title Analysis Searcher Efficiency main panel model comparison UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main panel model comparison element
#'
#' @return HTML for the Analysis Searcher Efficiency model comparison panel
#'
#' @export
#'
SEModComparisonPanel <- function(){
  tabPanel("Model Comparison", br(), 
    conditionalPanel(condition = "output.AICcTab_SE == null",
      HTML("<em>Run models to view model comparison</em>")
    ),
    conditionalPanel(condition = "output.SEModDone == 'OK'",
      textOutput("sizeclass_SE3"), br(), 
      dataTableOutput("AICcTab_SE"), br(),
      downloadButton("dlSEAICc", "Download")
    )
  )
}

#' @title Analysis Searcher Efficiency main panel model selection UI element 
#'
#' @description create the HTML code for the Analysis Searcher Efficiency
#'   main panel model selection element
#'
#' @return HTML for the Analysis Searcher Efficiency model selection panel
#'
#' @export
#'
SEModSelectionPanel <- function(){
  tabPanel("Model Selection", br(), 
    conditionalPanel(condition = "output.modelMenu_SE == null",
      HTML("<em>Run models to select models</em>")
    ),
    htmlOutput("modelMenu_SE")
  )
}



#' @title navbar UI element 
#'
#' @description create the HTML code for the navbar element
#'
#' @return HTML for the navbar element
#'
#' @export
#'
navbar <- function(){
  div(
    a(href = "https://github.com/ddalthorp/GenEst", 
      img(src = "GenEst.png", style =  "margin-top: -8px;", height = 40)
    )
  )
}


#' @title Make a model menu
#'
#' @description Produce a size-based model-selection menu object based on
#'   model inputs
#'
#' @param mods size-indexed list of model sets
#'
#' @param sizeclasses size class names
#'
#' @param type model type, either "SE" or "CP"
#'
#' @return rendered HTML model selection menu object
#'
#' @export
#'
makeMenu <- function(mods, sizeclasses, type){

  modelMenu <- ""
  nsizeclasses <- length(sizeclasses)
  if (nsizeclasses > 0){
    for(sci in 1:nsizeclasses){
      if (type == "SE"){
        AICcTab <- pkmSetAICcTab(mods[[sci]], quiet = TRUE)
      }
      if (type == "CP"){
        AICcTab <- cpmSetAICcTab(mods[[sci]], quiet = TRUE)
      }
      modOrder <- as.numeric(row.names(AICcTab))
      modNames <- names(mods[[sci]])[modOrder]
      modNames <- gsub("; NULL", "", modNames)
      modNames <- gsub("dist: ", "", modNames)

      modNames_nchar <- nchar(modNames)
      modNames_maxchar <- max(modNames_nchar)
      modNames_nspaces <- modNames_maxchar - modNames_nchar + 10
      modSpaces <- sapply(modNames_nspaces, 
                     function(x){paste(rep(" ", x), collapse = "")}
                   )

      modDeltaAICcs <- AICcTab[ , "Delta AICc"]
      modLabels <- paste0(modNames, " (delta AICc: ", modDeltaAICcs, ")")
      names(modNames) <- modLabels
      labels_nchar <- nchar(modLabels)
      labels_maxchar <- max(labels_nchar)
      widthval <- max(c(400, labels_maxchar * 7 + 20))
      widthtxt <- paste0(widthval, "px")
      mtuText <- paste("modelChoices_", type, sci, sep = "") 
      scText <- paste("Model for ", sizeclasses[sci], sep = "")
      modSelect <- selectizeInput(mtuText, scText, modNames, width = widthtxt)
      modelMenu <- paste(modelMenu, modSelect)  
    }
  }
          
  return(renderUI({HTML(modelMenu)}))
}#' @title Prepare predictors based on inputs
#'
#' @description Prepare predictor inputs from the app for use in the model
#'   function
#'
#' @param preds predictors, as input to the app
#'
#' @return prepared predictors (or 1 if no predictors)
#'
#' @export
#'
prepPredictors <- function(preds = NULL){

 out <- paste(preds, collapse = "*")
 if (is.null(preds)){
   out <- 1
 }
 return(out)
}

#' @title Create the kFillNeed text
#'
#' @description Based on the number of observation columns, create text output
#'   of "yes" or "no"
#'
#' @param obsCols vector of observation column names
#'
#' @return kFillNeed character of "yes" or "no"
#'
#' @export
#'
setkFillNeed <- function(obsCols){
  if(length(obsCols) == 1){
    return(renderText("yes"))
  }
  if(length(obsCols) > 1){
    return(renderText("no"))
  }
}

#' @title Update the fixed k value
#' 
#' @description Update the value for \code{kFixed} is chosen and available
#'
#' @param kFixedChoice choice to fix k (1) or not (anything else)
#'
#' @param kFixed existing kFixed value
#'
#' @return new kFixed value
#'
#' @export
#'
setkFix <- function(kFixedChoice, kFixed){
  if (kFixedChoice == 1 & is.numeric(kFixed)){
    return(kFixed)
  }else{
    return(NULL)
  }
}

#' @title Select the date columns from a data table
#'
#' @description Simple function to facilitate selection of date columns from
#'   a data table
#'
#' @param data data table potentially containing columns that could be 
#'   coerced (via \code{as.Date(yyyymmmdd())}) into a date
#'
#' @return column names of columns that can be coerced to dates
#'
#' @export
#'
dateCols <- function(data){

  ncols <- ncol(data)
  dateTF <- rep(NA, ncols)
  for (coli in 1:ncols){
    temp <- tryCatch(
              as.Date(yyyymmdd(data[ , coli])),
              error = function(x){FALSE}
            )
    dateTF[coli] <- lubridate::is.Date(temp)
  }
  out <- colnames(data)[dateTF]
  return(out)
}

#' @title Remove selected columns from column names
#'
#' @description Simple function to facilitate removal of columns selected
#'
#' @param colNames column names from which some could be removed
#'
#' @param selCols selected columns to be removed
#'
#' @return column names without selected columns
#'
#' @export
#'
removeSelCols <- function(colNames, selCols){
  which_sel <- which(colNames %in% selCols)
  if (length(which_sel) > 0){
    out <- colNames[-which_sel]
  } else{
    out <- colNames
  }
  return(out)
}


#' @title Update the string of column names that are in all the needed tables
#'
#' @description Determine the overlap between the column names in the SE, CP,
#'    and CO data tables.
#'
#' @param rv reactive values list with elements named \code{colnames_SE},
#'    \code{colnames_CP}, and \code{colnames_CO}
#'
#' @return possible column names
#'
#' @export
#'
updateColNames_all <- function(rv){

  SECPCO <- NULL
  SE <- rv$colNames_SE
  CP <- rv$colNames_CP
  CO <- rv$colNames_CO

  SECP <- which(SE %in% CP)
  SECO <- which(SE %in% CO)
  CPSE <- which(CP %in% SE)
  CPCO <- which(CP %in% CO)
  COSE <- which(CO %in% SE)
  COCP <- which(CO %in% CP)
  alltogether <- c(SECP, SECO, CPSE, CPCO, COSE, COCP)

  if (length(alltogether) == 0){
    if (is.null(SE) + is.null(CP) + is.null(CO) == 2){
      SECPCO <- unique(c(SE, CP, CO))
    } 
  } else{
    if (is.null(SE) + is.null(CP) + is.null(CO) == 1){
      SECPCOa <- c(SE[SECP], SE[SECO], CP[CPSE], CP[CPCO], CO[COSE], CO[COCP])
      SECPCO <- unique(SECPCOa)
    } else{
      SECP <- SE[SE %in% CP]
      SECPCO <- CO[CO %in% SECP]
    }
  }

  return(SECPCO)
}

#' @title Select particular columns from a data set
#'
#' @description Convenience function for selecting specific columns from a 
#'   data table
#'
#' @param data data table to select from
#'
#' @param cols column names to select
#'
#' @return selected data
#'
#' @export
#'
selectData <- function(data, cols){
  colNames <- colnames(data)
  selectedTab <- data[ , which(colNames %in% cols)]
  selectedDF <- data.frame(selectedTab)
  if (length(cols) == 1){
    colnames(selectedDF) <- cols
  }
  return(selectedDF)
}

#' @title Split model names into their components and remove only a desired 
#'   one
#'
#' @description Split a model name to return a specific component. Splitting 
#'   is done based on the semicolon in the name
#'
#' @param modNames names of the model to be split off
#'
#' @param pos position in the name to split off
#'
#' @return vector of split-off model names
#'
#' @export
#'
modNameSplit <- function(modNames, pos){
  modNames_split <- modNames
  nmod <- length(modNames)
  if (nmod > 0){
    for (modi in 1:nmod){
      modNames_split[modi] <- strsplit(modNames[modi], "; ")[[1]][pos]
    }
  }
  modNames_split <- gsub("NULL", "s ~ 1", modNames_split)
  modNames_split <- gsub("dist:", "", modNames_split)
  return(modNames_split)
}

#' @title Count the minimum number of carcasses in the cells
#'
#' @description Count the minimum number of carcasses in all of the cells
#'   within a \code{_SetSize} model complex
#'
#' @param mods model output from the \code{_SetSize} version of a function
#'
#' @return the minimum number of carcasses in the cells
#'
#' @export
#'
countCarcs <- function(mods){
  nsizeclasses <- length(mods)
  nmods <- length(mods[[1]])
  if (nsizeclasses > 0 & nmods > 0){
    ncarc <- rep(NA, nsizeclasses * nmods)
    counter <- 0
    for (sci in 1:nsizeclasses){
      for (modi in 1:nmods){
        counter <- counter + 1
        if (!grepl("Failed model fit", mods[[sci]][[modi]][1])){
          ncarc[counter] <- min(table(mods[[sci]][[modi]]$carcCell))
        }
      }
    }
    ncarc <- min(na.omit(ncarc))
  }else{
    ncarc <- Inf
  }
  return(ncarc)
}

#' @title Prepare text for size classes 
#'
#' @description Prepare and render text of the size class names
#'
#' @param sizeclasses names of the size classes
#'
#' @return prepared and render name text
#'
#' @export
#'
prepSizeclassText <- function(sizeclasses){
  return(renderText(paste(sizeclasses, collapse = " ")))
}

#' @title Paste the parts of a model's name back together
#'
#' @description Paste the component parts of a model's name back together
#'   for presentation
#'
#' @param parts the component parts of the model's name
#'
#' @param type "SE" or "CP"
#'
#' @param tab logical for if it's the table output for CP
#'
#' @return the pasted name
#'
#' @export
#'
modNamePaste <- function(parts, type = "SE", tab = FALSE){
  if (tab & parts[1] == " exponential"){
    out <- paste(c(parts[1:2], "NULL"), collapse = "; ")
  } else{
    out <- paste(parts, collapse = "; ")
  }
  if (type == "CP"){
    out <- paste("dist:", out, sep = "")
  }
  return(out)
}
  
#' @title Produce the options for the distributions in the CP model
#'
#' @description Simply make the named list for the disributions in the CP
#'   model
#'
#' @return list with named elements of the distributions 
#'
#' @export
#'
CPdistOptions <- function(){
  list("exponential" = "exponential", "weibull" = "weibull",
    "lognormal" = "lognormal", "loglogistic" = "loglogistic"
  )
}


#' @title Produce a blank plot for unsucessful fits
#'
#' @description Simply make a blank plot with descriptive text
#'
#' @return dummy plot
#'
#' @export
#'
plotNA <- function(){
  plot(1, 1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", 
    ylab = "", ylim = c(0, 1), xlim = c(0,1))
  text(0.01, 0.9, "Selected model was not fit successfully.", adj = 0)
}

#' @title Create the modal welcome for GenEst 
#'
#' @description Create a modal welcome and basic info screen for the GenEst
#'   application
#'
#' @param type "base" (just USGS text) or "deploy" (USGS and West text)
#'
#' @return Nothing
#'
#' @export
#'
modalWelcome <- function(type = "base"){  
  if (type == "base"){
    vnumber <- packageDescription("GenEst", fields = "Version")
    vdate <- packageDescription("GenEst", fields = "Date")
    disclaimer <- paste("GenEst v", vnumber, " (", vdate, ")", sep = "")
    showModal(modalDialog(title = disclaimer, modalTextUSGS(),
      easyClose = FALSE, footer = modalButton("OK"))
    )
  }
  if (type == "deploy"){
    vnumber <- packageDescription("GenEst", fields = "Version")
    vdate <- packageDescription("GenEst", fields = "Date")
    disclaimer <- paste("GenEst v", vnumber, " (", vdate, ")", sep = "")
    showModal(modalDialog(title = disclaimer, modalTextUSGS(), br(), br(),
      modalTextWest(), easyClose = FALSE, footer = modalButton("OK"))
    )
  }
}

#' @title Create USGS text for the modal welcome
#'
#' @description Create USGS text for the modal welcome
#'
#' @return HTML element of the USGS text
#'
#' @export
#'
modalTextUSGS <- function(){
  HTML("This software is preliminary or provisional and is subject to
    revision. It is being provided to meet the need for timely best science.
    The software has not received final approval by the U.S. Geological
    Survey (USGS). No warranty, expressed or implied, is made by the USGS or
    the U.S. Government as to the functionality of the software and related
    material nor shall the fact of release constitute any such warranty. The
    software is provided on the condition that neither the USGS nor the U.S.
    Government shall be held liable for any damages resulting from the
    authorized or unauthorized use of the software."
  )
}

#' @title Create West text for the modal welcome
#'
#' @description Create West text for the modal welcome
#'
#' @return HTML element of the West text
#'
#' @export
#'
modalTextWest <- function(){
  HTML("Western EcoSystems Technology, Inc. does not host nor maintain the 
    Shinyapp.io website. It is advised that users not upload sensitive data 
    containing personally identifiable information (SSN, birthdates, medical 
    information, etc.). Western EcoSystems Technology, Inc. is not liable for 
    any damages, including but not limited to general, compensatory, special 
    or punitive damages, sustained by user arising out of another party or 
    entity using said sensitive data or for the use of any data by another 
    party or entity which is obtained from viruses, Trojans or other malware. 
    Shinyapp.io is actively maintained by the RStudio Company on Amazon Web 
    Services.", "<br>", "<br>", 
    "This program is an 'AS IS' without warranty of any kind, either expressed
    or implied, including but not limited to, the implied warranties of 
    merchantability and fitness for a particular purpose. The entire risk as
    to the quality and performance of the program is with you. Should the 
    program prove defective, you assume all cost of all necessary servicing,
    repair or correction. If this program is modified and/or redistributed, 
    Western EcoSystems Technology, Inc. is not liable for any damages,
    including any general, special, incidental or consequential damages 
    arising out of the use or inability to use this program (including but not
    limited to loss of data or data being rendered inaccurate or losses 
    sustained by you or third parties or a failure of the program to operate 
    with any other programs), even if such holder or other party has been
    advised of the possibility of such damages."
  )
}

#' @title Create the version text for GenEst 
#'
#' @description Create a text string of the version number and date
#'
#' @return version text
#'
#' @export
#'
createvtext <- function(){
  vnumber <- packageDescription("GenEst", fields = "Version")
  vdate <- packageDescription("GenEst", fields = "Date")
  vtext <- paste("This is version ", vnumber, " (", vdate, ")", sep = "")
  return(vtext)
}#' @title Download the CP figure
#'
#' @description Handle the CP figure downloading
#'
#' @param rv the reactive values list
#'
#' @return an updated reactive values list
#'
#' @export
#'
downloadCPFig <- function(rv){
  downloadHandler(filename = "CP_fig.png",
      content = function(file){
        png(file, height = rv$figH_CP, width = rv$figW_CP, units = "px")
        plot(rv$modSet_CP, specificModel = rv$outCPdlsfig)
        dev.off()
      }
  )
}

#' @title Download the SE figure
#'
#' @description Handle the SE figure downloading
#'
#' @param rv the reactive values list
#'
#' @return an updated reactive values list
#'
#' @export
#'
downloadSEFig <- function(rv){
  downloadHandler(filename = "SE_fig.png",
      content = function(file){
        png(file, height = rv$figH_SE, width = rv$figW_SE, units = "px")
        tryCatch(
          plot(rv$modSet_SE, specificModel = rv$outSEpk),
          error = function(x){plotNA()}
        )
        dev.off()
      }
  )
}

#' @title Download the g figure
#'
#' @description Handle the g figure downloading
#'
#' @param rv the reactive values list
#'
#' @param sc size class
#'
#' @return an updated reactive values list
#'
#' @export
#'
downloadgFig <- function(rv, sc){
  downloadHandler(filename = "g_fig.png",
      content = function(file){
        png(file, height = rv$figH_g, width = rv$figW_g, units = "px")
        plot(rv$gGeneric[[sc]], CL = rv$CL)
        dev.off()
      }
  )
}

#' @title Download the M figure
#'
#' @description Handle the M figure downloading
#'
#' @param rv the reactive values list
#'
#' @param split logical indicator to use the split or not
#'
#' @param transpose logical indicator if to transpose the output or not
#'
#' @return an updated reactive values list
#'
#' @export
#'
downloadMFig <- function(rv, split = TRUE, transpose = FALSE){

  if (split){
    if (transpose){
      downloadHandler(filename = "M_fig.png",
          content = function(file){
            png(file, height = rv$figH_M, width = rv$figW_M, units = "px")
            tryCatch(
              plot(transposeSplits(rv$Msplit)),
              error = function(x){plotNA()}
            )
            dev.off()
          }
      )
    } else {
      downloadHandler(filename = "M_fig.png",
          content = function(file){
            png(file, height = rv$figH_M, width = rv$figW_M, units = "px")
            tryCatch(
              plot(rv$Msplit),
              error = function(x){plotNA()}
            )
            dev.off()
          }
      )
    }
  } else{
    downloadHandler(filename = "M_fig.png",
        content = function(file){
          png(file, height = rv$figH_M, width = rv$figW_M, units = "px")
          tryCatch(
            plot(rv$M),
            error = function(x){plotNA()}
          )
          dev.off()
        }
    )
  }
}

#' @title Download a table
#'
#' @description Handle the downloading of a table
#'
#' @param filename the name for the file writing out
#'
#' @param tablename the name of the table in the rv list
#'
#' @return an updated reactive values list
#'
#' @export
#'
downloadTable <- function(filename, tablename){
  downloadHandler(filename = filename,
                  content = function(file){
                              write.csv(tablename, file, row.names = FALSE)
                            }
                  )
}

#' @title Set the figure width based on the number of cells
#'
#' @description Convenience function for determining the needed figure width
#'
#' @param modelSet model set
#'
#' @return figure width
#'
#' @export
#'
setFigW <- function(modelSet){
  ncell <- nrow(modelSetCells(modelSet))
  if (ncell > 6){
    return(1200)
  } else{
    return(800)
  }
}

#' @title Set the figure height based on the number of cells
#'
#' @description Convenience function for determining the needed figure height
#'
#' @param modelSet model set
#'
#' @param type "SE" or "CP"
#'
#' @return figure height
#'
#' @export
#'
setFigH <- function(modelSet, type = "SE"){
  ncell <- nrow(modelSetCells(modelSet))
  nRow <- ceiling(ncell / 3)
  mult <- 200
  if (ncell > 6){
    mult <- 300
  }
  proposed <- nRow * mult + 400
  out <- max(c(proposed, 800))
  if (type == "CP"){
    out <- out - 100
  }
  out
}#' @title Launch the app
#' 
#' @description Launches a local version of the GenEst application by running
#'   \code{shinyAppDir} pointed to the \code{app} subdirectory in the 
#'   local \code{GenEst} package folder.
#'
#' @export
#'
runGenEst <- function(){

  appDir <- system.file("app", package = "GenEst")
  runApp(appDir)
  
}


#' @title Update the inputs when SE data are read in
#'
#' @description Update the inputs when the SE data file is input
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_SE <- function(rv, session){
  updateSelectizeInput(session, "preds_SE", choices = rv$colNames_SE_nosel)
  updateSelectizeInput(session, "obsCols_SE", choices = rv$colNames_SE_nosel)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_all,
    selected = rv$sizeclassCol
  )
  updateTabsetPanel(session, "LoadedDataViz", "Searcher Efficiency")
}

#' @title Update the inputs when CP data are read in
#'
#' @description Update the inputs when the CP data file is input
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_CP <- function(rv, session){
  updateSelectizeInput(session, "preds_CP", choices = rv$colNames_CP_nosel)
  updateSelectizeInput(session, "ltp", choices = rv$colNames_CP_nosel)
  updateSelectizeInput(session, "fta", choices = rv$colNames_CP_nosel)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_all,
    selected = rv$sizeclassCol
  )
  updateTabsetPanel(session, "LoadedDataViz", "Carcass Persistence")
}

#' @title Update the inputs when SS data are read in
#'
#' @description Update the inputs when the SS data file is input
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_SS <- function(rv, session){
  updateTabsetPanel(session, "LoadedDataViz", "Search Schedule")
}

#' @title Update the inputs when DWP data are read in
#'
#' @description Update the inputs when the DWP data file is input
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_DWP <- function(rv, session){
  updateSelectizeInput(session, "DWPCol", choices = rv$colNames_DWP)
  updateTabsetPanel(session, "LoadedDataViz", "Density Weighted Proportion")
}

#' @title Update the inputs when CO data are read in
#'
#' @description Update the inputs when the CO data file is input
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_data_CO <- function(rv, session){
  updateTabsetPanel(session, "LoadedDataViz", "Carcass Observations")
  updateSelectizeInput(session, "splitCol", choices = rv$colNames_CO)
  updateSelectizeInput(session, "dateFoundCol", choices = rv$colNames_COdates)
  updateSelectizeInput(session, "sizeclassCol", choices = rv$colNames_all,
    selected = rv$sizeclassCol
  )
  updateTabsetPanel(session, "LoadedDataViz", "Carcass Observations")
}

#' @title Update the inputs when the size class column is selected
#'
#' @description Update the inputs when the SE data file is input
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param session session
#'
#' @export
#'
update_input_sizeclassCol <- function(rv, input, session){
  rv$colNames_SE_sel <- c(input$obsCols_SE, input$sizeclassCol)
  rv$colNames_SE_nosel <- removeSelCols(rv$colNames_SE, rv$colNames_SE_sel)
  updateSelectizeInput(session, "preds_SE", choices = rv$colNames_SE_nosel,
    selected = input$preds_SE)
  rv$colNames_SE_sel <- c(input$preds_SE, input$sizeclassCol)
  rv$colNames_SE_nosel <- removeSelCols(rv$colNames_SE, rv$colNames_SE_sel)
  updateSelectizeInput(session, "obsCols_SE", choices = rv$colNames_SE_nosel,
    selected = input$obsCols_SE)
  rv$colNames_CP_sel <- c(input$preds_CP, input$fta, input$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  updateSelectizeInput(session, "ltp", choices = rv$colNames_CP_nosel,
    selected = input$ltp)
  rv$colNames_CP_sel <- c(input$preds_CP, input$ltp, input$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  updateSelectizeInput(session, "fta", choices = rv$colNames_CP_nosel,
    selected = input$fta)
  rv$colNames_CP_sel <- c(input$ltp, input$fta, input$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  updateSelectizeInput(session, "preds_CP", choices = rv$colNames_CP_nosel,
    selected = input$preds_CP)
}

#' @title Update the remaining columns when SE data columns are selected
#'
#' @description Update the inputs when the SE data columns are selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param session session
#'
#' @param x specific column
#'
#' @export
#'
update_input_cols_SE <- function(rv, input, session, x = "obsCols"){

  notx <- switch(x, "obsCols" = "preds_SE", "preds" = "obsCols_SE")
  x <- paste0(x, "_SE")
  rv$colNames_SE_sel <- c(input[[x]], input$sizeclassCol)
  rv$colNames_SE_nosel <- removeSelCols(rv$colNames_SE, rv$colNames_SE_sel)
  updateSelectizeInput(session, notx, choices = rv$colNames_SE_nosel,
    selected = input[[notx]])
}

#' @title Update the remaining columns when CP data columns are selected
#'
#' @description Update the inputs when the CP data columns are selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param session session
#'
#' @param x specific column
#'
#' @export
#'
update_input_cols_CP <- function(rv, input, session, x = "ltp"){

  notx1 <- switch(x, "ltp" = "fta", "fta" = "ltp", "preds" = "ltp")
  notx2 <- switch(x, "ltp" = "preds", "fta" = "preds", "preds" = "fta")

  x <- gsub("preds", "preds_CP", x)
  notx1 <- gsub("preds", "preds_CP", notx1)
  notx2 <- gsub("preds", "preds_CP", notx2)

  rv$colNames_CP_sel <- c(input[[x]], input[[notx1]], input$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  updateSelectizeInput(session, notx2, choices = rv$colNames_CP_nosel,
    selected = input[[notx2]])

  rv$colNames_CP_sel <- c(input[[x]], input[[notx2]], input$sizeclassCol)
  rv$colNames_CP_nosel <- removeSelCols(rv$colNames_CP, rv$colNames_CP_sel)
  updateSelectizeInput(session, notx1, choices = rv$colNames_CP_nosel,
    selected = input[[notx1]])
}

#' @title Update the SE output dropdown selections when the model is run
#'
#' @description Update the SE output dropdown selections when the model is run
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_SE <- function(rv, session){
  updateTabsetPanel(session, "analyses_SE", "Model Comparison")
  updateSelectizeInput(session, "outSEp", choices = rv$modNames_SEp)
  updateSelectizeInput(session, "outSEk", choices = rv$modNames_SEk)
  updateSelectizeInput(session, "outsizeclassSE", choices = rv$sizeclasses)

  if (rv$kFixedChoice == 1){
    updateNumericInput(session, "kFill", value = rv$kFixed)
  }
}

#' @title Update the SE output dropdown selections when the size class is 
#'   chosen
#'
#' @description Update the SE output dropdown selections when the size 
#'   class is chosen
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_outsc_SE <- function(rv, session){
  updateSelectizeInput(session, "outSEp", choices = rv$modNames_SEp)
  updateSelectizeInput(session, "outSEk", choices = rv$modNames_SEk)
}

#' @title Update the CP output dropdown selections when the model is run
#'
#' @description Update the CP output dropdown selections when the model is run
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_CP <- function(rv, session){
  updateTabsetPanel(session, "analyses_CP", "Model Comparison")
  updateSelectizeInput(session, "outCPl", choices = rv$modNames_CPl)
  updateSelectizeInput(session, "outCPs", choices = rv$modNames_CPs)
  updateSelectizeInput(session, "outCPdist", choices = rv$modNames_CPdist)
  updateSelectizeInput(session, "outsizeclassCP", choices = rv$sizeclasses)
}

#' @title Update the CP output dropdown selections when the size class is 
#'   chosen
#'
#' @description Update the CP output dropdown selections when the size 
#'   class is chosen
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_outsc_CP <- function(rv, session){
  updateSelectizeInput(session, "outCPl", choices = rv$modNames_CPl)
  updateSelectizeInput(session, "outCPs", choices = rv$modNames_CPs)
  updateSelectizeInput(session, "outCPdist", choices = rv$modNames_CPdist)
}

#' @title Update the SS average dropdown selections when the SS data are used
#'
#' @description Update the SS average dropdown selections when the SS data 
#'   are used
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_useSSdata <- function(rv, session){
  if (!is.na(rv$SStemp[1])){
    updateNumericInput(session, "gSearchInterval", value = rv$avgSI)
    updateNumericInput(session, "gSearchMax", value = max(rv$SS))
  }
}

#' @title Update the g output dropdown selections when the model is run
#'
#' @description Update the g output dropdown selections when the model is run
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_g <- function(rv, session){
  updateSelectizeInput(session, "outsizeclassg", choices = rv$sizeclasses_g)
  updateTabsetPanel(session, "analyses_g", "Summary")
}


#' @title Update the M output dropdown selections when the model is run
#'
#' @description Update the M output dropdown selections when the model is run
#'
#' @param rv reactive values list
#'
#' @param session session
#'
#' @export
#'
update_input_run_M <- function(rv, session){
  updateSelectizeInput(session, "split_SS", choices = rv$colNames_SS_nosel)
  updateSelectizeInput(session, "split_CO", choices = rv$colNames_CO)
}
#' @title Produce the message list
#'
#' @description Produce the message list, currently restricted to a NULL value
#'   starting for each value
#'
#' @return list of message elements
#'
#' @export
#'
msgList <- function(){
  list(ModSE = NULL, ModCP = NULL, ModM = NULL, SS = NULL, Modg = NULL)
}

#' @title Clear all notifications
#' 
#' @description Clear all messages in the message list
#'
#' @param msgs message list
#'
#' @export
#'
#'
clearNotifications <- function(msgs = msgList()){
  if (!is.null(msgs$ModSE)){
    removeNotification(msgs$ModSE)
  }
  if (!is.null(msgs$ModCP)){
    removeNotification(msgs$ModCP)
  }
  if (!is.null(msgs$ModM)){
    removeNotification(msgs$ModM)
  }
  if (!is.null(msgs$SS)){
    removeNotification(msgs$SS)
  }
  if (!is.null(msgs$Modg)){
    removeNotification(msgs$Modg)
  }
}

#' @title Create a model running message
#'
#' @description Produces a model-running notification
#'
#' @param msgs message list
#'
#' @param modelType "SE", "CP", "g", or "M"
#'
#' @param clear if all notifications should be cleared or not
#'
#' @return a model running message
#'
#' @export
#'
msgModRun <- function(msgs, modelType, clear = TRUE){
  if (clear){
    clearNotifications(msgs)
  }
  msg <- NULL
  if (modelType == "SE"){
    msg <- ("Running Searcher Efficiency Model")
  }
  if (modelType == "CP"){
    msg <- ("Running Carcass Persistence Model")
  }
  if (modelType == "g"){
    msg <- ("Running Detection Probability Model")
  }
  if (modelType == "M"){
    msg <- ("Estimating Mortality")
  }
  if(!is.null(msg)){
    return(showNotification(msg, duration = NULL))
  }
}

#' @title Create a model done message
#'
#' @description Produces a SE or CP model-done notification
#'
#' @param msgs message list
#'
#' @param rv reactive values list
#'
#' @param type "SE", "CP", "M", "split", or "g"
#'
#' @param clear if all notifications should be cleared or not
#'
#' @return an SE model done message
#'
#' @export
#'
msgModDone <- function(msgs, rv, type = "SE", clear = TRUE){
  if (clear){
    clearNotifications(msgs)
  }
  if (type == "SE"){
    if (all(unlist(pkmSetSizeFail(rv$mods_SE_og)))){
      return(msgModFail(rv$mods_SE_og, "SE"))
    } else if(any(unlist(lapply(rv$mods_SE_og, pkmSetAllFail)))){
      return(msgModFail(rv$mods_SE_og, "SE", "size_k"))
    } else {
      return(msgModWarning(rv$mods_SE_og, "SE", rv))
    }
  }
  if (type == "CP"){
    if (all(unlist(cpmSetSizeFail(rv$mods_CP_og)))){
      return(msgModFail(rv$mods_CP_og, "CP"))
    } else{
      return(msgModWarning(rv$mods_CP_og, "CP"))
    }
  }  
  if (type == "M"){
    if (is.null(rv$M)){
      return(msgModFail(rv$M, "M"))
    }
  }
  if (type == "split"){
    if (rv$nsplit_CO + rv$nsplit_SS > 2 | rv$nsplit_SS > 1){
      return(msgsplitFail("setup"))
    }
    if (is.null(rv$Msplit)){
      return(msgsplitFail("run"))
    }
  }
  if (type == "g"){
    if (is.null(rv$gGeneric[[1]])){    
      return(msgModFail(rv$gGeneric, "g"))
    }
  }
  NULL
}

#' @title Create the warning message text for when only some models are fit 
#'   successfully
#'
#' @description Produces text for a notification for partial model failure
#'
#' @param mods Set Size list of models
#'
#' @param type "SE" or "CP"
#'
#' @return a partial model fail warning text
#'
#' @export
#'
msgModPartialFail <- function(mods, type = "SE"){

  anyFail <- FALSE
  if (type == "SE"){
    if (any(unlist(pkmSetSizeFail(mods)))){
      anyFail <- TRUE
    }
  }
  if (type == "CP"){
    if (any(unlist(cpmSetSizeFail(mods)))){
      anyFail <- TRUE
    }
  }
  if (!anyFail){
    return(NULL)
  }

  nsizeclass <- length(mods)
  uniquemsgs <- NULL
  for (sci in 1:nsizeclass){
    newmsgs <- NULL
    if (type == "SE"){
      failedmods <- which(pkmSetFail(mods[[sci]]))
    }
    if (type == "CP"){
      failedmods <- which(cpmSetFail(mods[[sci]]))
    }
    nfailedmods <- length(failedmods)
    if (nfailedmods > 0){
      for(fmodi in 1:nfailedmods){
        newmsg <- mods[[sci]][[names(failedmods)[fmodi]]]
        if (length(newmsg) == 1){
          newmsg <- gsub("Failed model fit: ", "", newmsg)
        } else{
          newmsg <- "Failed fit for k."
        }
        newmsgs <- unique(c(newmsgs, newmsg))
      }
    }
    uniquemsgs <- unique(c(uniquemsgs, newmsgs))
  }
  paste0(
    "Some models were not successfully fit. Failed models were removed. ",
    paste(uniquemsgs, collapse = " ")
  )
}

#' @title Create the warning message text for small sample sizes
#'
#' @description Produces text for a notification for small sample sizes
#'
#' @param mods Set Size list of models
#'
#' @return small sample sizes warning text (if needed)
#'
#' @export
#'
msgSampleSize <- function(mods){
  cellCounts <- countCarcs(mods)
  minCellCount <- min(na.omit(cellCounts))
  if (minCellCount < 10){
    return(
      paste0("Small (< 10) sample sizes in some cells. ",
             "Consider simplifying the model; ",
             "parameter estimates may be unstable."
      )
    )
  }
  NULL
}

#' @title Create the warning message for a model run (if needed)
#'
#' @description Produces a notification for partial model failures if needed
#'
#' @param mods Set Size list of models
#'
#' @param type "SE" or "CP"
#'
#' @param rv reactive values list
#'
#' @return a partial model warning (if needed)
#'
#' @export
#'
msgModWarning <- function(mods, type = "SE", rv = NULL){
  msg <- paste(msgModPartialFail(mods, type), msgSampleSize(mods), sep = " ")
  if (type == "SE"){
    msg <- paste(msg, msgModSENobs(rv), sep = " ")
  }
  if (length(msg) > 0){
    return(showNotification(msg, type = "warning", duration = NULL))    
  }
  NULL
}

#' @title Create the SE data size notification 
#'
#' @description Produces a notification for SE data sizes (associated with k)
#'
#' @param rv reactive values list
#'
#' @return data size message
#'
#' @export
#'
msgModSENobs <- function(rv){
  if (length(rv$obsCols_SE) == 1){
    if(length(rv$formula_k) > 0 & length(rv$kFixed) == 0){
      return("Only one observation, k not estimated.")
    }
    if (length(rv$kFixed) == 1){
      return("Only one observation, fix k input ignored.")
    }
  }
  NULL
}


#' @title Create the error message for when no models are fit successfully
#'
#' @description Produces a notification for complete model failure
#'
#' @param mods (fully failed) model object
#'
#' @param type "SE", "CP", "M", or "g"
#'
#' @param special indicator of a special type of message
#'
#' @return a model fail error message
#'
#' @export
#'
msgModFail <- function(mods, type = "SE", special = NULL){
  if (type %in% c("SE", "CP")){
    if (is.null(special)){
      msg <- paste(
               "No models were successfully fit.", 
                gsub("Failed model fit: ", "", unique(unlist(mods))),
                sep = " "
             )
    } else if (special == "size_k"){
      msg <- "Some size classes had no successful models. Consider a fixed k."
    }
  }
  if (type == "g"){
    msg <- "Detection probability not able to be estimated"
  }
  if (type == "M"){
    msg <- "Mortality not able to be estimated"
  }
  if(!is.null(msg)){
    return(showNotification(msg, type = "error", duration = NULL))
  }
}

#' @title Create the warning message for when the SS average doesn't work
#'
#' @description Produces a notification for when an average search schedule
#'   can't be created
#'
#' @param msgs message list
#'
#' @param rv reactive values list
#'
#' @param clear if all notifications should be cleared or not
#'
#' @return an average SS fail warning
#'
#' @export
#'
msgSSavgFail <- function(msgs, rv, clear = TRUE){
  if (clear){
    clearNotifications(msgs)
  }
  if (is.na(rv$SStemp[1])){
    msg <- "Search Schedule can't be averaged using date searched column"
    return(showNotification(msg, type = "warning", duration = NULL))
  }
  NULL
}

#' @title Create the warning message for when the SS based on inputs doesn't 
#'   work
#'
#' @description Produces a notification for when an input-based search 
#'   schedule can't be created
#'
#' @param msgs message list
#'
#' @param rv reactive values list
#'
#' @param clear if all notifications should be cleared or not
#'
#' @return an average SS fail warning
#'
#' @export
#'
msgSSinputFail <- function(msgs, rv, clear = TRUE){
  if (clear){
    clearNotifications(msgs)
  }
  if (is.na(rv$SStemp[1])){
    msg <- "Search Schedule can't be created using inputs"
    return(showNotification(msg, type = "warning", duration = NULL))
  }
  NULL
}

#' @title Create the fail message for when splits aren't done correctly
#'
#' @description Produces a notification for failed mortality splits
#'
#' @param type "setup" or "run"
#'
#' @return a split fail warning
#'
#' @export
#'
msgsplitFail <- function(type = NULL){

  if (is.null(type)){
    return(NULL)
  }
  if (type == "setup"){
    msg <- paste0(
             "Improper splits setup. Maximum two splits, only one of which ",
             "can be associated with the search schedule."
           )
  }
  if (type == "run"){
    msg <- "Splits calculation failed. Check split selections."
  }
  return(showNotification(msg, type = "error", duration = NULL))
}#' @title Update the output list when SE data are read in
#'
#' @description Update the output list when the SE data file is input
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_data_SE <- function(rv, output){
  output$data_SE <- renderDataTable(datatable(rv$data_SE))
  return(output)
}

#' @title Update the output list when CP data are read in
#'
#' @description Update the output list when the CP data file is input
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_data_CP <- function(rv, output){
  output$data_CP <- renderDataTable(datatable(rv$data_CP))
  return(output)
}

#' @title Update the output list when SS data are read in
#'
#' @description Update the output list when the SS data file is input
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_data_SS <- function(rv, output){
  output$data_SS <- renderDataTable(datatable(rv$data_SS))
  return(output)
}

#' @title Update the output list when DWP data are read in
#'
#' @description Update the output list when the DWP data file is input
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_data_DWP <- function(rv, output){
  output$data_DWP <- renderDataTable(datatable(rv$data_DWP))
  return(output)
}

#' @title Update the output list when CO data are read in
#'
#' @description Update the output list when the CO data file is input
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_data_CO <- function(rv, output){
  output$data_CO <- renderDataTable(datatable(rv$data_CO))
  return(output)
}

#' @title Update the selected CP and SE data when a size class column is 
#'   selected
#'
#' @description Update the output tables when the size class column is
#'   selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_sizeclassCol <- function(rv, input, output){
  if (!is.null(input$obsCols_SE)){
    selectedCols <- c(input$obsCols_SE, input$sizeclassCol, input$preds_SE)
    selectedData <- selectData(rv$data_SE, selectedCols)
    output$selected_SE <- renderDataTable(datatable(selectedData))
  }
  if (!is.null(c(input$ltp, input$fta))){
    obsColsSelected <- c(input$ltp, input$fta)
    selectedCols <- c(obsColsSelected, input$sizeclassCol, input$preds_CP)
    selectedData <- selectData(rv$data_CP, selectedCols)
    output$selected_CP <- renderDataTable(datatable(selectedData))
  }
  return(output)
}

#' @title Update the selected SE data when a column is selected
#'
#' @description Update the output table when the SE columns are selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_cols_SE <- function(rv, input, output){
  selectedCols <- c(input$obsCols_SE, input$sizeclassCol, input$preds_SE)
  selectedData <- selectData(rv$data_SE, selectedCols)
  output$selected_SE <- renderDataTable(datatable(selectedData))
  return(output)
}

#' @title Update the selected CP data when a column is selected
#'
#' @description Update the output table when the CP columns are selected
#'
#' @param rv reactive values list
#'
#' @param input input list
#'
#' @param output output list
#'
#' @return an updated output list
#'
#' @export
#'
update_output_cols_CP <- function(rv, input, output){
  obsColsSelected <- c(input$ltp, input$fta)
  selectedCols <- c(obsColsSelected, input$sizeclassCol, input$preds_CP)
  selectedData <- selectData(rv$data_CP, selectedCols)
  output$selected_CP <- renderDataTable(datatable(selectedData))
  return(output)
}

#' @title Update the output when an SE model has been run
#'
#' @description Update the output table when an SE model has been run
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_run_SE <- function(rv, output, session){

  if (!all(unlist(pkmSetSizeFail(rv$mods_SE))) &&
      !any(unlist(lapply(rv$mods_SE_og, pkmSetAllFail)))){

    output$SEModDone <- renderText("OK")
    output$kFillNeed <- setkFillNeed(rv$obsCols_SE)
    if (length(rv$sizeclasses) == 1){
      output$DWPNeed <- renderText("yes")
    } else{
      output$DWPNeed <- renderText("no")
    }
    outputOptions(output, "SEModDone", suspendWhenHidden = FALSE)
    outputOptions(output, "kFillNeed", suspendWhenHidden = FALSE)
    outputOptions(output, "DWPNeed", suspendWhenHidden = FALSE)

    output$AICcTab_SE <- renderDataTable({rv$AICcTab_SE})    
    output$modTab_SE <- renderDataTable({rv$modTabPretty_SE})
    output$fig_SE <- renderPlot({ 
                       plot(rv$modSet_SE, specificModel = rv$best_SE)
                     }, height = rv$figH_SE, width = rv$figW_SE)

    isolate({
      output$sizeclasses_SE <- prepSizeclassText(rv$sizeclasses_SE)
      outputOptions(output, "sizeclasses_SE", suspendWhenHidden = FALSE)
      output$modelMenu_SE <- makeMenu(rv$mods_SE, rv$sizeclasses_SE, "SE")
    })

    scText <- renderText(paste0("Size class: ", rv$sizeclass_SE))
    output$sizeclass_SE1 <- scText
    output$sizeclass_SE2 <- scText
    output$sizeclass_SE3 <- scText

    output$dlSEest <- downloadTable("SE_estimates.csv", rv$modTabDL_SE)
    output$dlSEAICc <- downloadTable("SE_AICc.csv", rv$AICcTab_SE)
    output$dlSEfig <- downloadSEFig(rv)
  }
  return(output)
}

#' @title Update the SE output when a size class is chosen
#'
#' @description Update the SE output when a size class is chosen
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_outsc_SE <- function(rv, output, session){
  if (length(rv$mods_SE) > 0){
    output$AICcTab_SE <- renderDataTable({rv$AICcTab_SE})    
    output$modTab_SE <- renderDataTable({rv$modTabPretty_SE})
    output$fig_SE <- renderPlot({ 
                       plot(rv$modSet_SE, specificModel = rv$best_SE)
                     }, height = rv$figH_SE, width = rv$figW_SE)

    scText <- renderText(paste0("Size class: ", rv$sizeclass_SE))
    output$sizeclass_SE1 <- scText
    output$sizeclass_SE2 <- scText
    output$sizeclass_SE3 <- scText

    output$dlSEest <- downloadTable("SE_estimates.csv", rv$modTabDL_SE)
    output$dlSEAICc <- downloadTable("SE_AICc.csv", rv$AICcTab_SE)
    output$dlSEfig <- downloadSEFig(rv)
  }
  return(output)
}

#' @title Update the SE output when a p or k equation is chosen
#'
#' @description Update the SE output when a p or k equation is chosen
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_outpk_SE <- function(rv, output, session){
  if (length(rv$mods_SE) > 0){
    output$fig_SE <- renderPlot({ 
                       tryCatch(
                         plot(rv$modSet_SE, specificModel = rv$outSEpk),
                         error = function(x){plotNA()}
                       )
                     }, height = rv$figH_SE, width = rv$figW_SE)
    output$dlSEfig <- downloadSEFig(rv)
    if (!is.null(rv$modTab_SE)){
      output$modTab_SE <- renderDataTable({rv$modTabPretty_SE})
      output$dlSEest <- downloadTable("SE_estimates.csv", rv$modTabDL_SE)
    }
  }
  return(output)
}

#' @title Update the output when an CP model has been run
#'
#' @description Update the output table when an CP model has been run
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_run_CP <- function(rv, output, session){

  if (!all(unlist(cpmSetSizeFail(rv$mods_CP)))){

    output$CPModDone <- renderText("OK")
    outputOptions(output, "CPModDone", suspendWhenHidden = FALSE)

    output$AICcTab_CP <- renderDataTable({rv$AICcTab_CP})    
    output$modTab_CP <- renderDataTable({rv$modTabPretty_CP})
    output$fig_CP <- renderPlot({ 
                       plot(rv$modSet_CP, specificModel = rv$best_CP)
                     }, height = rv$figH_CP, width = rv$figW_CP)

    isolate({
      output$sizeclasses_CP <- prepSizeclassText(rv$sizeclasses_CP)
      outputOptions(output, "sizeclasses_CP", suspendWhenHidden = FALSE)
      output$modelMenu_CP <- makeMenu(rv$mods_CP, rv$sizeclasses_CP, "CP")
    })

    scText <- renderText(paste0("Size class: ", rv$sizeclass_CP))
    output$sizeclass_CP1 <- scText
    output$sizeclass_CP2 <- scText
    output$sizeclass_CP3 <- scText

    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP)
    output$dlCPAICc <- downloadTable("CP_AICc.csv", rv$AICcTab_CP)
    output$dlCPfig <- downloadCPFig(rv)
  }
  return(output)
}

#' @title Update the CP output when a size class is chosen
#'
#' @description Update the CP output when a size class is chosen
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_outsc_CP <- function(rv, output, session){
  if (length(rv$mods_CP) > 0){
    output$modTab_CP <- renderDataTable(rv$modTabPretty_CP)
    output$fig_CP <- renderPlot({ 
                       plot(rv$modSet_CP, specificModel = rv$best_CP)
                     }, height = rv$figH_CP, width = rv$figW_CP)

    scText <- renderText(paste0("Size class: ", rv$sizeclass_CP))
    output$sizeclass_CP1 <- scText
    output$sizeclass_CP2 <- scText
    output$sizeclass_CP3 <- scText

    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP)
    output$dlCPAICc <- downloadTable("CP_AICc.csv", rv$AICcTab_CP)
    output$dlCPfig <- downloadCPFig(rv)
  }
  return(output)
}

#' @title Update the CP output when a distribuition or l or s equation is 
#'   chosen
#'
#' @description Update the SE output when a distribution or l or s equation is 
#'   chosen
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_outdls_CP <- function(rv, output, session){
  if (length(rv$mods_CP) > 0){
    output$modTab_CP <- renderDataTable(rv$modTabPretty_CP)
    output$fig_CP <- renderPlot({ 
                       tryCatch(
                       plot(rv$modSet_CP, specificModel = rv$outCPdlsfig),
                         error = function(x){plotNA()}
                       )
                     }, height = rv$figH_CP, width = rv$figW_CP)
    output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP)
    output$dlCPfig <- downloadCPFig(rv)

    if (!is.null(rv$modTab_CP)){
      output$modTab_CP <- renderDataTable({rv$modTabPretty_CP})
      output$dlCPest <- downloadTable("CP_estimates.csv", rv$modTabDL_CP)
    }

  }
  return(output)
}

#' @title Update the SS text output when the SS is updated
#'
#' @description Update the SS text output when the SS is updated
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_SS <- function(rv, output, session){
  output$SStext <- renderText(rv$SStext)
  output
}

#' @title Update the output when a generic g model has been run
#'
#' @description Update the output table when a generic g model has been run
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_run_g <- function(rv, output, session){

  if (!is.null(rv$gGeneric[[1]])){
    summaryTab <- summary(rv$gGeneric[[1]], CL = rv$CL)
    output$table_g <- renderDataTable(summaryTab)
    output$fig_g <- renderPlot({
                      tryCatch(
                        plot(rv$gGeneric[[1]], CL = rv$CL),
                        error = function(x){plot(1,1)},
                        warning = function(x){plot(1,1)}
                      )
                    }, height = rv$figH_g, width = rv$figW_g)
    output$gModDone <- renderText("OK")
    outputOptions(output, "gModDone", suspendWhenHidden = FALSE)

    scText <- renderText(paste0("Size class: ", rv$sizeclass_g))
    output$sizeclass_g1 <- scText
    output$sizeclass_g2 <- scText

    output$dlgtab <- downloadTable("g_estimates.csv", summaryTab)
    output$dlgfig <- downloadgFig(rv, 1)
  }
  return(output)
}

#' @title Update the output when a generic g size class is chosen
#'
#' @description Update the output table when a generic g size class is chosen
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_outsc_g <- function(rv, output, session){

  if (class(rv$gGeneric[[rv$sizeclass_g]])[1] == "gGeneric"){
    summaryTab <- summary(rv$gGeneric[[rv$sizeclass_g]], CL = rv$CL)
    output$table_g <- renderDataTable(summaryTab)
    output$fig_g <- renderPlot({
                      tryCatch(
                        plot(rv$gGeneric[[rv$sizeclass_g]], CL = rv$CL),
                        error = function(x){plot(1,1)},
                        warning = function(x){plot(1,1)}
                      )
                    }, height = rv$figH_g, width = rv$figW_g)
    output$gModDone <- renderText("OK")
    outputOptions(output, "gModDone", suspendWhenHidden = FALSE)

    scText <- renderText(paste0("Size class: ", rv$sizeclass_g))
    output$sizeclass_g1 <- scText
    output$sizeclass_g2 <- scText

    output$dlgtab <- downloadTable("g_estimates.csv", summaryTab)
    output$dlgfig <- downloadgFig(rv, rv$sizeclass_g)
  }
  return(output)
}

#' @title Update the output when an M model has been run
#'
#' @description Update the output table when an M model has been run
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_run_M <- function(rv, output, session){

  if (!is.null(rv$Msplit)){
    output$MModDone <- renderText("OK")
    outputOptions(output, "MModDone", suspendWhenHidden = FALSE)

    output$fig_M <- renderPlot({plot(rv$Msplit)},
                      height = rv$figH_M, width = rv$figW_M
                    )
    summaryTab <-  prettySplitTab(summary(rv$Msplit, CL = rv$CL))
    output$table_M <- renderDataTable(datatable(summaryTab))
    output$dlMtab <- downloadTable("M_table.csv", summaryTab)
    output$dlMfig <- downloadMFig(rv)
  }
  output
}

#' @title Update the output when M has been split
#'
#' @description Update the output table when M has been split
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_split_M <- function(rv, output, session){

  if (is.null(rv$Msplit)){
    output$fig_M <- renderPlot({
                      tryCatch(plot(rv$M),
                        error = function(x){plotNA()}
                      )
                    }, height = rv$figH_M, width = rv$figW_M)
    output$dlMfig <- downloadMFig(rv, split = FALSE)

  } else{
    output$fig_M <- renderPlot({plot(rv$Msplit)},
                      height = rv$figH_M, width = rv$figW_M
                    )
    summaryTab <-  prettySplitTab(summary(rv$Msplit, CL = rv$CL))
    output$table_M <- renderDataTable(datatable(summaryTab))
    output$dlMtab <- downloadTable("M_table.csv", summaryTab)
    output$dlMfig <- downloadMFig(rv)
  }
  output
}


#' @title Update the output when the M split is transposed
#'
#' @description Update the output when the M split is transposed
#'
#' @param rv reactive values list
#'
#' @param output output list
#'
#' @param session session
#'
#' @return an updated output list
#'
#' @export
#'
update_output_transpose_split <- function(rv, output, session){

  if (!is.null(rv$Msplit)){
      output$fig_M <- renderPlot({plot(rv$Msplit)},
                        height = rv$figH_M, width = rv$figW_M
                      )
      output$dlMfig <- downloadMFig(rv, TRUE, TRUE)

  }
  output
}#' @title Create the main reactive value list for GenEst 
#'
#' @description Create a list of reactive values as used across the components
#'   of the GenEst application
#'
#' @return a reactive values list
#'
#' @export
#'
createReactiveValues <- function(){
  reactiveValues(
    data_SE = NULL, data_CP = NULL, data_SS = NULL, data_DWP = NULL, 
    data_CO = NULL,
    colNames_SE = NULL, colNames_SE_sel = NULL, colNames_SE_nosel = NULL, 
    colNames_CP = NULL, colNames_CP_sel = NULL, colNames_CP_nosel = NULL,    
    colNames_SS = NULL, colNames_SS_sel = NULL, colNames_SS_nosel = NULL, 
    colNames_DWP = NULL,
    colNames_CO = NULL, colNames_COdates = NULL,
    colNames_all = NULL, 

    nsim = 1000, CL = 0.95, 

    sizeclassCol = NULL, sizeclasses = NULL, sizeclass = NULL, 
    sizeclass_SE = NULL, sizeclass_CP = NULL, sizeclass_g = NULL,
    sizeclass_M = NULL,

    obsCols_SE = NULL, preds_SE = NULL, predictors_SE = NULL, 
    formula_p = NULL, formula_k = NULL, kFixedChoice = NULL, kFixed = NULL, 
    mods_SE = NULL, mods_SE_og = NULL, sizeclasses_SE = NULL, 
    outSEpk = NULL, AICcTab_SE = NULL, modOrder_SE = NULL, modNames_SE = NULL,
    modNames_SEp = NULL, modNames_SEk = NULL, modSet_SE = NULL,
    best_SE = NULL, modTab_SE = NULL, modTabPretty_SE = NULL,
    modTabDL_SE = NULL, figH_SE = 800, figW_SE = 800,
    kFill = NULL, 

    ltp = NULL, fta = NULL, preds_CP = NULL, dists = NULL, 
    predictors_CP = NULL, formula_l = NULL, formula_s = NULL, 
    mods_CP = NULL, mods_CP_og = NULL, CPdls = NULL, outCPdlsfig = NULL, 
    outCPdlstab = NULL, sizeclasses_CP = NULL, AICcTab_CP = NULL, 
    modOrder_CP = NULL, modNames_CP = NULL, modNames_CPdist = NULL, 
    modNames_CPl = NULL, modNames_CPs = NULL, modSet_CP = NULL, 
    best_CP = NULL, modTab_CP = NULL, modTabPretty_CP = NULL, 
    modTabDL_CP = NULL, figH_CP = 700, figW_CP = 800,

    M = NULL, Msplit = NULL, unitCol = NULL, frac = 1, 
    sizeclassCol_M = NULL, DWPCol = NULL, dateFoundCol = NULL, 
    SEmodToUse = NULL, CPmodToUse = NULL,
    split_CO = NULL, split_SS = NULL, nsplit_CO = 0, nsplit_SS = 0, 
    figH_M = 800, figW_M = 800,

    SS = seq(0, 364, 7), SStext = paste(seq(0, 364, 7), collapse = ", "),
    kFill_g = NULL, sizeclasses_g = NULL, nsizeclasses_g = NULL,
    gGeneric = NULL, SEmodToUse_g = NULL, CPmodToUse_g = NULL,
    figH_g = 400, figW_g = 800
  )
}

server = function(input, output, session){

require(GenEst)

modalWelcome("base")
rv <- createReactiveValues()
output$versionInfo <- renderText(createvtext())
output$SStext <- renderText(rv$SStext)
msgs <- msgList()

observeEvent(input$file_SE, {
  rv <- update_rv_data_SE(rv, input)
  output <- update_output_data_SE(rv, output)
  update_input_data_SE(rv, session)
})
observeEvent(input$file_CP, {
  rv <- update_rv_data_CP(rv, input)
  output <- update_output_data_CP(rv, output)
  update_input_data_CP(rv, session)
})
observeEvent(input$file_SS, {
  rv <- update_rv_data_SS(rv, input)
  output <- update_output_data_SS(rv, output)
  update_input_data_SS(rv, session)
})
observeEvent(input$file_DWP, {
  rv <- update_rv_data_DWP(rv, input)
  output <- update_output_data_DWP(rv, output)
  update_input_data_DWP(rv, session)
})
observeEvent(input$file_CO, {
  rv <- update_rv_data_CO(rv, input)
  output <- update_output_data_CO(rv, output)
  update_input_data_CO(rv, session)
})

observeEvent(input$sizeclassCol, {
  output <- update_output_sizeclassCol(rv, input, output)
  update_input_sizeclassCol(rv, input, session)
})

observeEvent(input$obsCols_SE, {
  output <- update_output_cols_SE(rv, input, output)
  update_input_cols_SE(rv, input, session, "obsCols")
})
observeEvent(input$preds_SE, {
  output <- update_output_cols_SE(rv, input, output)
  update_input_cols_SE(rv, input, session, "preds")
})
observeEvent(input$runMod_SE, {
  msgs$ModSE <<- msgModRun(msgs, "SE")
  rv <- update_rv_run_SE(rv, input)
  output <- update_output_run_SE(rv, output, session)
  update_input_run_SE(rv, session)
  msgs$ModSE <<- msgModDone(msgs, rv, "SE")
})
observeEvent(input$outsizeclassSE, {
  rv <- update_rv_outsc_SE(rv, input)
  output <- update_output_outsc_SE(rv, output, session)
  update_input_outsc_SE(rv, session)
})
observeEvent(input$outSEp, {
  rv <- update_rv_outpk_SE(rv, input)
  output <- update_output_outpk_SE(rv, output, session)
})
observeEvent(input$outSEk, {
  rv <- update_rv_outpk_SE(rv, input)
  output <- update_output_outpk_SE(rv, output, session)
})

observeEvent(input$ltp, {
  output <- update_output_cols_CP(rv, input, output)
  update_input_cols_CP(rv, input, session, "ltp")
})
observeEvent(input$fta, {
  output <- update_output_cols_CP(rv, input, output)
  update_input_cols_CP(rv, input, session, "fta")
})
observeEvent(input$preds_CP, {
  output <- update_output_cols_CP(rv, input, output)
  update_input_cols_CP(rv, input, session, "preds")
})
observeEvent(input$runMod_CP, {
  msgs$ModCP <<- msgModRun(msgs, "CP")
  rv <- update_rv_run_CP(rv, input)
  output <- update_output_run_CP(rv, output, session)
  update_input_run_CP(rv, session)
  msgs$ModCP <<- msgModDone(msgs, rv, "CP")
})
observeEvent(input$outsizeclassCP, {
  rv <- update_rv_outsc_CP(rv, input)
  output <- update_output_outsc_CP(rv, output, session)
  update_input_outsc_CP(rv, session)
})
observeEvent(input$outCPdist, {
  rv <- update_rv_outdls_CP(rv, input)
  output <- update_output_outdls_CP(rv, output, session)
})
observeEvent(input$outCPl, {
  rv <- update_rv_outdls_CP(rv, input)
  output <- update_output_outdls_CP(rv, output, session)
})
observeEvent(input$outCPs, {
  rv <- update_rv_outdls_CP(rv, input)
  output <- update_output_outdls_CP(rv, output, session)
})

observeEvent(input$runMod_M, {
  msgs$ModM <<- msgModRun(msgs, "M")
  rv <- update_rv_run_M(rv, input)
  output <- update_output_run_M(rv, output, session)
  update_input_run_M(rv, session)
  msgs$ModM <<- msgModDone(msgs, rv, "M")
})
observeEvent(input$splitM, {
  rv <- update_rv_split_M(rv, input)
  output <- update_output_split_M(rv, output, session)
  msgs$ModM <<- msgModDone(msgs, rv, "split")
})
observeEvent(input$transposeSplit, {
  rv <- update_rv_transpose_split(rv)
  output <- update_output_transpose_split(rv, output, session)
})

observeEvent(input$useSSdata, {
  rv <- update_rv_useSSdata(rv)
  msgs$SS <<- msgSSavgFail(msgs, rv)
  output <- update_output_SS(rv, output, session)
  update_input_useSSdata(rv, session)
})
observeEvent(input$useSSinputs, {
  rv <- update_rv_useSSinputs(rv, input)
  msgs$SS <<- msgSSinputFail(msgs, rv)
  output <- update_output_SS(rv, output, session)
})
observeEvent(input$runMod_g, {
  msgs$Modg <<- msgModRun(msgs, "g")
  rv <- update_rv_run_g(rv, input)
  output <- update_output_run_g(rv, output, session)
  update_input_run_g(rv, session)
  msgs$Modg <<- msgModDone(msgs, rv, "g")
})
observeEvent(input$outsizeclassg, {
  rv <- update_rv_outsc_g(rv, input)
  output <- update_output_outsc_g(rv, output, session)
})

}
ui = function(){

navbarPage(navbar(), collapsible = TRUE, windowTitle = "GenEst", 

  tabPanel("WEST, Inc. Disclaimers",
    helpText("Western EcoSystems Technology, Inc. does not host nor maintain the Shinyapp.io website. It is advised that users not upload sensitive data containing personally identifiable information (SSN, birthdates, medical information, etc.). Western EcoSystems Technology, Inc. is not liable for any damages, including but not limited to general, compensatory, special or punitive damages, sustained by user arising out of another party or entity using said sensitive data or for the use of any data by another party or entity which is obtained from viruses, Trojans or other malware. Shinyapp.io is actively maintained by the RStudio Company on Amazon Web Services."),
    helpText("This program is an “AS IS” without warranty of any kind, either expressed or implied, including but not limited to, the implied warranties of merchantability and fitness for a particular purpose. The entire risk as to the quality and performance of the program is with you. Should the program prove defective, you assume all cost of all necessary servicing, repair or correction. If this program is modified and/or redistributed, Western EcoSystems Technology, Inc. is not liable for any damages, including any general, special, incidental or consequential damages arising out of the use or inability to use this program (including but not limited to loss of data or data being rendered inaccurate or losses sustained by you or third parties or a failure of the program to operate with any other programs), even if such holder or other party has been advised of the possibility of such damages.")
    ),
  tabPanel("Data Input", dataInputPanel()),
  tabPanel("Analyses", analysisPanel()),
  tabPanel("Submit feedback", 
	helpText(a(
		"Please provide any feedback you may have about the GenEst web app.  (Links to external page)",
		href="https://goo.gl/forms/2tfvqQ4hpcFax4j13", target = "_blank"))
  ),
  tabPanel("About", aboutPanel())

)

}

shinyApp(ui = ui, server = server)