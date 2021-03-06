% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detection_probability_functions.R
\name{estgGenericSize}
\alias{estgGenericSize}
\title{Estimate generic detection probability for multiple size classes}
\usage{
estgGenericSize(nsim = 1000, days, modelSetSize_SE, modelSetSize_CP,
  modelSizeSelections_SE, modelSizeSelections_CP, seed_SE = NULL,
  seed_CP = NULL, kFill = NULL)
}
\arguments{
\item{nsim}{the number of simulation draws}

\item{days}{Search schedule data as a vector of days searched}

\item{modelSetSize_SE}{Searcher Efficiency model set for multiple sizes}

\item{modelSetSize_CP}{Carcass Persistence model set for multiple sizes}

\item{modelSizeSelections_SE}{vector of SE models to use, one for each 
size. Size names are required, and names must match those of
modelSetSize_SE. E.g., 
\code{c(lrg = "p ~ Visibility; k ~ 1", sml = "p ~ 1; k ~ 1")}.
Model formulas are read as text and must have exact matches among models
listed in modelSetSize_SE. For example, if one of the
\code{modelSizeSelections_SE} elements is
\code{lrg = "p ~ Visibility; k ~ 1"}, then \code{"p ~ Visibility; k ~ 1"}
must be in \code{names(modelSizeSelections_SE)[["lrg"]]}.}

\item{modelSizeSelections_CP}{vector of CP models to use, one for each size}

\item{seed_SE}{seed for random draws of the SE model}

\item{seed_CP}{seed for random draws of the CP model}

\item{kFill}{values to fill in for missing k when the selected SE model 
does not include an estimated k. If all of the selected models include 
estimated k's, set \code{kFill = NULL}. Models that do not include 
estimated k's require named kFill values in a vector with names matching 
the names of size classes, e.g., \code{kFill = c(lrg = 0.7, sml = 0.4)},
or, if only one model requires a \code{kFill} value, 
\code{kFill = c(lrg = 0.7)}.}
}
\value{
list of g estimates, with one element in the list corresponding
   to each of the cells from the cross-model combination
}
\description{
Generic g estimation for a combination of SE model and CP
  model under a given search schedule

The g estimated by \code{estgGenericSize} is a generic aggregate detection
  probability and represents the probability of detecting a carcass that 
  arrives at a (uniform) random time during the period monitored, for each
  of the possible cell combinations, given the SE and CP models. This 
  is somethat different from the GenEst estimation of g when the purpose 
  is to estimate total mortality (M), in which case the detection 
  probability varies with carcass arrival interval and is difficult to 
  summarize statistically. The \code{estgGeneric} estimate is a useful 
  "big picture" summary of detection probability, but would be difficult
  to work with for estimating M with precision.
}
\examples{
  data(mock)
  pkmModsSize <- pkmSetSize(formula_p = p ~ HabitatType,
                   formula_k = k ~ HabitatType, data = mock$SE,
                   obsCol = c("Search1", "Search2", "Search3", "Search4"),
                   sizeclassCol = "Size"
                 )
  cpmModsSize <- cpmSetSize(formula_l = l ~ Visibility,
                   formula_s = s ~ Visibility, data = mock$CP,
                   left = "LastPresentDecimalDays",
                   right = "FirstAbsentDecimalDays",
                   dist = c("exponential", "lognormal"),
                   sizeclassCol = "Size"
                   )
  pkMods <- c("S" = "p ~ 1; k ~ 1", "L" = "p ~ 1; k ~ 1",
             "M" = "p ~ 1; k ~ 1", "XL" = "p ~ 1; k ~ 1"
            )
  cpMods <- c("S" = "dist: exponential; l ~ 1; NULL", 
              "L" = "dist: exponential; l ~ 1; NULL",
              "M" = "dist: exponential; l ~ 1; NULL",
              "XL" = "dist: exponential; l ~ 1; NULL"
            )
  avgSS <- averageSS(mock$SS)
  gsGeneric <- estgGenericSize(nsim = 1000, days = avgSS,
                 modelSetSize_SE = pkmModsSize,
                 modelSetSize_CP = cpmModsSize,
                 modelSizeSelections_SE = pkMods,
                 modelSizeSelections_CP = cpMods
               )

}
