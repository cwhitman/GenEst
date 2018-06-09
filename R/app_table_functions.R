#' @title Create the pretty version of the split summary table
#'
#' @description Format a reader-friendly version of the split summary table
#'   a mortality estimation
#'
#' @param splitSummary a split summary
#'
#' @param CL Confidence level
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
    colnames(Out)[1] <- attr(splitSummary, "vars")[1]
    extralev <- rep(names(splitSummary)[1], nrow(Out))
    Out <- cbind(extralev, Out)    
    colnames(Out)[1] <- attr(splitSummary, "vars")[2]

    for (speci in 2:nspec){
      specOut <- prettySplitSpecTab(splitSummary[[speci]])
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
  prettyTab <- matrix(NA, nrow = nrows, ncol = ncols)
  counter <- 0
  for (ri in 1:nrows){
    spots <- counter + 1:ncols
    prettyTab[ri, ] <- round(vectored[spots], 2)
    counter <- spots[ncols]
  }     
  colnames(prettyTab) <- cnames
  varname <- attr(splitSummarySpec, "vars")
  if (is.null(varname)){
    return(prettyTab)
  }
  prettyTab <- cbind(rnames, prettyTab)
  colnames(prettyTab)[1] <- varname
  return(prettyTab)
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
prettyModTabSE <- function(modTab, CL = 0.9){

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
prettyModTabCP <- function(modTab, CL = 0.9){
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