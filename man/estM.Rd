% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mortality_functions.R
\name{estM}
\alias{estM}
\title{Estimate mortality}
\usage{
estM(data_CO, data_SS, data_DWP, frac = 1, dateFoundCol = "DateFound",
  model_SE, model_CP, kFill = NULL, unitCol = NULL,
  datesSearchedCol = NULL, sizeclassCol = NULL, DWPCol = NULL,
  seed_SE = NULL, seed_CP = NULL, seed_g = NULL, seed_M = NULL,
  nsim = 1000, max_intervals = 8)
}
\arguments{
\item{data_CO}{Carcass Observation data}

\item{data_SS}{Search Schedule data}

\item{data_DWP}{Survey unit (rows) by size (columns) density weighted
proportion table}

\item{frac}{fraction of facility (by units or by area) surveyed}

\item{dateFoundCol}{Column name for the date found data}

\item{model_SE}{Searcher Efficiency model (or list of models if there are
multiple size classes)}

\item{model_CP}{Carcass Persistence model (or list of models if there are
multiple size classes)}

\item{kFill}{value to fill in for missing k when not existing in the model}

\item{unitCol}{Column name for the unit indicator (optional)}

\item{datesSearchedCol}{Column name for the date searched data}

\item{sizeclassCol}{Name of colum in \code{data_CO} where the size classes
are recorded. Optional. If none provided, it is assumed there is no
distinctions among size classes.}

\item{DWPCol}{Column name for the DWP values in the DWP table when no
size class is used and there is more than one column in \code{data_DWP}
that could be interpreted as DWP.}

\item{seed_SE}{seed for random draws of the SE model}

\item{seed_CP}{seed for random draws of the CP model}

\item{seed_g}{seed for random draws of the gs}

\item{seed_M}{seed for the random draws of the Mhats}

\item{nsim}{the number of simulation draws}

\item{max_intervals}{maximum number of arrival intervals to consider
for each carcass}
}
\value{
list of Mhat, Aj, ghat
}
\description{
Given given fitted Searcher Efficiency and Carcass 
  Persistence models; Search Schedule, Density Weighted Proportion,
  and Carcass Observation data; and information about the fraction of the
  the facility that was surveyed.
}
\examples{
 \dontrun{
 data(mock)
 model_SE <- pkm(formula_p = p ~ HabitatType, formula_k = k ~ 1,
              data = mock$SE
             )
 model_CP <- cpm(formula_l = l ~ Visibility, formula_s = s ~ Visibility, 
               data = mock$CP, dist = "weibull",
               left = "LastPresentDecimalDays", 
               right = "FirstAbsentDecimalDays"
             )
 eM <- estM(nsim = 1000, data_CO = mock$CO, data_SS = mock$SS, 
         data_DWP = mock$DWP, frac = 1, model_SE = model_SE, 
         model_CP = model_CP, dateFoundCol = "DateFound", 
         DWPCol = "S", sizeclassCol = NULL
       )
 }

}
