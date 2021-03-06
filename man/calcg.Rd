% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detection_probability_functions.R
\name{calcg}
\alias{calcg}
\title{Calculate cell-level generic detection probability}
\usage{
calcg(days, param_SE, param_CP, dist)
}
\arguments{
\item{days}{Search schedule (vector of days searched)}

\item{param_SE}{numeric array of searcher efficiency parameters (p and k)}

\item{param_CP}{numeric array of carcass persistence parameters (a and b)}

\item{dist}{distribution for the CP model}
}
\description{
Calculate detection probability (g) given SE and CP parameters
 and a search schedule.

The g given by \code{calcg} is a generic aggregate detection
 probability and represents the probability of detecting a carcass that
 arrives at a (uniform) random time during the time spanned by the search
 schedule for the the given SE and CP parameters. This differs the GenEst
 estimation of g when the purpose is to estimate total mortality (M), in
 which case the detection probability varies with carcass arrival interval
 and is difficult to summarize statistically. \code{calcg} provides a 
 useful "big picture" summary of detection probability, but would be 
 difficult to work with for estimating M with precision.
}
