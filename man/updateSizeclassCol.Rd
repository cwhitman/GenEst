% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_utility_functions.R
\name{updateSizeclassCol}
\alias{updateSizeclassCol}
\title{Update the name of the size class column based on available names}
\usage{
updateSizeclassCol(sizeclassCol, colNames_size)
}
\arguments{
\item{sizeclassCol}{current size class column name}

\item{colNames_size}{updated vector of size column names in all needed 
tables}
}
\value{
updated sizeclassCol
}
\description{
Update the size class column name based on the available
  options. If the existing size class column name is no longer in the
  set of available names, a NULL is returned to reset the column name.
}
