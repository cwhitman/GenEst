#' @title Estimate mortality 
#'
#' @description Given given fitted Searcher Efficiency and Carcass 
#'   Persistence models; Search Schedule, Density Weighted Proportion,
#'   and Carcass Observation data; and information about the fraction of the
#'   the facility that was surveyed. 
#'
#' @param data_CO Carcass Observation data
#'
#' @param data_SS Search Schedule data
#'
#' @param data_DWP Survey unit (rows) by size (columns) density weighted
#'   proportion table
#'
#' @param frac fraction of facility (by units or by area) surveyed
#'
#' @param dateFoundCol Column name for the date found data
#'
#' @param model_SE Searcher Efficiency model (or list of models if there are
#'   multiple size classes)
#'
#' @param model_CP Carcass Persistence model (or list of models if there are
#'   multiple size classes)
#'
#' @param kFill value to fill in for missing k when not existing in the model
#'
#' @param unitCol Column name for the unit indicator (optional)
#'
#' @param datesSearchedCol Column name for the date searched data
#'
#' @param sizeclassCol Name of colum in \code{data_CO} where the size classes
#'  are recorded. Optional. If none provided, it is assumed there is no
#'  distinctions among size classes.
#'
#' @param DWPCol Column name for the DWP values in the DWP table when no
#'   size class is used and there is more than one column in \code{data_DWP}
#'   that could be interpreted as DWP.
#'
#' @param seed_SE seed for random draws of the SE model
#'
#' @param seed_CP seed for random draws of the CP model
#'
#' @param seed_g seed for random draws of the gs
#'
#' @param seed_M seed for the random draws of the Mhats
#'
#' @param nsim the number of simulation draws
#'
#' @param max_intervals maximum number of arrival intervals to consider
#'  for each carcass
#'
#' @return list of Mhat, Aj, ghat
#'
#' @examples 
#'  \dontrun{
#'  data(mock)
#'  model_SE <- pkm(formula_p = p ~ HabitatType, formula_k = k ~ 1,
#'               data = mock$SE
#'              )
#'  model_CP <- cpm(formula_l = l ~ Visibility, formula_s = s ~ Visibility, 
#'                data = mock$CP, dist = "weibull",
#'                left = "LastPresentDecimalDays", 
#'                right = "FirstAbsentDecimalDays"
#'              )
#'  eM <- estM(nsim = 1000, data_CO = mock$CO, data_SS = mock$SS, 
#'          data_DWP = mock$DWP, frac = 1, model_SE = model_SE, 
#'          model_CP = model_CP, dateFoundCol = "DateFound", 
#'          DWPCol = "S", sizeclassCol = NULL
#'        )
#'  }
#'
#' @export 
#'
estM <- function(data_CO, data_SS, data_DWP, frac = 1,
                 dateFoundCol = "DateFound", model_SE, model_CP, kFill = NULL,
                 unitCol = NULL, datesSearchedCol = NULL, sizeclassCol = NULL,
                 DWPCol = NULL, seed_SE = NULL, seed_CP = NULL, seed_g = NULL,
                 seed_M = NULL, nsim = 1000, max_intervals = 8){


  if (!(dateFoundCol %in% colnames(data_CO))){
    stop("dateFoundCol not found in data_CO")
  }
  data_CO[ , dateFoundCol] <- checkDate(data_CO[ , dateFoundCol])
  if (is.null(data_CO[ , dateFoundCol]))
    stop("dates_CO not unambiguously intepretable as dates")
  # attempted auto-parsing for unitCol:
  #  find common cols in CO and DWP as the candidate unitCol
  #  if unique, then use that as unitCol
  #  if not present or not unique, then error
  if (is.null(unitCol)){ 
    unitCol <- intersect(colnames(data_CO), colnames(data_DWP))
    if (length(unitCol) == 0){
      stop(
        "no columns in data_CO and data_DWP share a common name ",
        "to use as a unit column."
      )
    }
    if (length(unitCol) > 1){
      stop(
        "multiple matching column names in data_CO and data_DWP. ",
        "Provide a value for unitCol in estM arg list."
      )
    }
  }
  # if no sizeclassCol is provided, then the later analysis is done without
  #   making distinctions between sizes; no error-checking here
  # if sizeclassCol is provided, it must be present in CO. It's levels must 
  #  also all be present in DWP, but the check is done in the DWPbyCarcass 
  #  function, which allow DWPbyCarcass to more readily be used as a 
  #  standalone function if user wishes.
  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colnames(data_CO))){
      stop("size class column not in carcass data.")
    } else if (!all(data_CO[, sizeclassCol] %in% names(data_DWP))){
      stop("a size class in data_CO is missing from data_DWP")
    } else {
      sizeclasses <- as.character(unique(data_CO[ , sizeclassCol]))
      nsizeclass <- length(sizeclasses)
    }
  }

  # error-checking for match b/t DWP and CO data is done in DWPbyCarcass
  DWP <- DWPbyCarcass(data_DWP = data_DWP, data_CO = data_CO,
           sizeclassCol = sizeclassCol, unitCol = unitCol, DWPCol = DWPCol)

  est <- estg(data_CO = data_CO, data_SS = data_SS, 
           dateFoundCol = dateFoundCol, model_SE = model_SE, 
           model_CP = model_CP, kFill = kFill, unitCol = unitCol,
           datesSearchedCol = datesSearchedCol, sizeclassCol = sizeclassCol,
           seed_SE = seed_SE, seed_CP = seed_CP, seed_g = seed_g,
           nsim = nsim, max_intervals = max_intervals
         )

  gDf <- est$ghat * DWP * frac
  set.seed(seed_M)
  c_out <- which(rowSums(gDf) == 0)
  if (length(c_out) == 0){
    n <- length(gDf)
    Mhat <- ((rcbinom(n, 1/gDf, gDf)) - (Ecbinom(gDf) - 1))/gDf
  } else {
    Mhat <- array(0, dim = c(dim(data_CO)[1], nsim))
    gDf <- gDf[-c_out, ]
    n <- length(gDf)
    Mhat[-c_out,] <- ((rcbinom(n, 1/gDf, gDf)) - (Ecbinom(gDf) - 1))/gDf
  }
  out <- list(Mhat = Mhat, Aj = est$Aj, ghat = est$ghat, Xtot = nrow(data_CO))
  class(out) <- c("estM", "list")
  return(out)
}

#' @title Assign DWP value to each carcass
#'
#' @description Expand the density weighted proportion table to a value for 
#'   each carcass (across multiple classes if desired) based on the unit where 
#'   they were found
#'
#' @param data_DWP Survey unit (rows) by size (columns) density weighted 
#'   proportion table 
#'
#' @param data_CO Carcass observation data
#'
#' @param unitCol Column name for the unit indicator (optional). If 
#'   \code{NULL}, then is assumed to be the column that \code{data_DWP} and
#'   \code{data_CO} share. If none are in common, error is thrown with no 
#'   remedy. If data sets share more than one column, user is asked to input 
#'   \code{unitCol}.
#'
#' @param sizeclassCol Name of colum in \code{data_CO} where the size classes
#'  are recorded. Optional.
#'
#' @param DWPCol Name of column where DWP values are stored (optional). Used
#'  when there is more than one DWP column in \code{data_DWP} but analysis is
#'  intended for a single class (i.e., no "size" is specified in data_CO).
#'  If \code{sizeclassCol} is \code{NULL} and \code{DWPCol} is not provided,
#'  there is a check for possible DWPCols. If there is only one column with
#'  values in (0, 1], that's DWPCol. If there is not a unique column with
#'  values in (0, 1], an error is returned.
#'
#' @return DWP value for each carcass 
#'
#' @examples 
#'  data(mock)
#'  DWP <- DWPbyCarcass(data_DWP = mock$DWP, data_CO = mock$CO,
#'           sizeclassCol = "Size", unitCol = "Unit")
#'  DWP <- DWPbyCarcass(data_DWP = mock$DWP, data_CO = mock$CO,
#'           unitCol = "Unit", DWPCol = "S")
#'
#' @export 
#'
DWPbyCarcass <- function(data_DWP, data_CO, unitCol = NULL, 
                         sizeclassCol = NULL, DWPCol = NULL){

  if (is.null(unitCol)){
    unitCol <- colnames(data_DWP)[colnames(data_DWP) %in% colnames(data_CO)]
    if (length(unitCol) == 0){
      stop("data_DWP and data_CO must have identically-named unit columns")
    }
    if (length(unitCol) > 1){
      stop(paste("more than one possibility for unitCol (", unitCol, ")",
        collapse = " "))
    }
  } else {
    if (length(unitCol) > 1) stop("length(unitCol) must be 1 if provided")
    if (!(unitCol %in% colnames(data_DWP))){
      stop(unitCol, " not found in data_DWP")
    }
  }
  # length(unitCol) = 1 and has been found in both CO and DWP;
  # are all units in data_CO also found in DWP?
  if (!all(data_CO[, unitCol] %in% data_DWP[ , unitCol])){
    stop("Some units present in carcass table not in DWP table")
  } # error-checking on units is complete

  # parse w.r.t. sizeclassCol arg and assign DWP to carcasses
  if (!is.null(sizeclassCol)){
    if (!(sizeclassCol %in% colnames(data_CO))){
      stop("size class column not found in carcass data.")
    }
    if (!all(unique(data_CO[,sizeclassCol]) %in% colnames(data_DWP))){
      stop("not all sizes in data_CO are represented in data_DWP.")
    }
    # size classes and units have been error-checked, and assigning DWP to
    #  carcasses is a simple extraction of DWP from the appropriate row and
    #  column for each carcass in CO.
    # unit in CO defines the desired row in DWP:
    rowind <- match(data_CO[, unitCol], data_DWP[, unitCol])
    # size in CO defines the desired column in DWP:
    colind <- match(data_CO[ , sizeclassCol], names(data_DWP))
    # and DWPbyCarc falls out naturally
    DWPbyCarc <- as.numeric(data_DWP[cbind(rowind, colind)])
    if (!is.null(DWPCol) && !is.na(DWPCol)){
      message("\nBoth sizeclassCol and DWPCol were provided by user. ",
        "Assigning DWP by size.\n")
    }
  } else { # assume same DWP for all sizes, so matching is by unit only
    # which column has the DWP data?
    if (!is.null(DWPCol)){
      # DWPCol has been provided, but must be error-checked (as below)
      if (length(DWPCol) > 1 || !is.character(DWPCol)){
        stop("DWPCol must be the name of a single column in data_DWP or NULL")
      }
      if (!(DWPCol %in% colnames(data_DWP))){
        stop("DWPCol must be the name of column in data_DWP or NULL")
      }
      if (!is.numeric(data_DWP[ , DWPCol]) || anyNA(data_DWP[ , DWPCol])){
        stop("data_DWP[, DWPCol] must be numeric with no NA's")
      }
      if (sum(data_DWP[, DWPCol] <= 0 | data_DWP[, DWPCol] > 1) > 0){
        stop("data_DWP[, DWPCol] values must be in (0, 1]")
      }
      rowind <- match(data_CO[, unitCol], data_DWP[, unitCol])
      DWPbyCarc <- data_DWP[rowind, DWPCol]
    } else {
      possibleNames <- colnames(data_DWP)
      for (coli in possibleNames){
        if (is.numeric(data_DWP[ , coli]) && !anyNA(data_DWP[ , coli]) &&
            all(data_DWP[ , coli] <= 1) && all(data_DWP[ , coli] > 0)){
          # candidate DWP has been discovered
          if (is.null(DWPCol)){
            DWPCol <- coli
          } else { # it is the second one found => conflict
            stop(
              "data_DWP must have columns corresponding to data_CO sizes\n",
              "or have a single column of DWP's to associate with units.\n",
              "Alternatively, user may specify name of DWP column as DWPCol"
            )
          }
        }
      }
      # DWPCol and unitCol have been error-checked, so
      rowind <- match(data_CO[, unitCol], data_DWP[, unitCol])
      DWPbyCarc <- as.numeric(data_DWP[rowind, DWPCol])
    }
  }
  return(DWPbyCarc)
}

#' @title Summarize total mortality estimation
#'
#' @description \code{summary} defined for class \code{estM} objects
#'
#' @param object \code{estM} object
#'
#' @param ... arguments to pass down
#'
#' @param CL confidence level
#'
#' @export
#'
summary.estM <- function(object, ..., CL = 0.90){
  alpha <- 1 - CL
  Mtot <- colSums(object$Mhat) 
  Mtot[Mtot < object$Xtot] <- object$Xtot
  out <- round(quantile(Mtot, probs = c(0.5, alpha/2, 1 - alpha/2)), 2)
  names(out) <- c("median", paste0(100*c(alpha/2, 1- alpha/2), "%"))
  return(out)
}
