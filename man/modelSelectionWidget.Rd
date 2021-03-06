% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_widgets.R
\name{modelSelectionWidget}
\alias{modelSelectionWidget}
\alias{modelSelectionWidgetHeader}
\alias{modelSelectionWidgetRow}
\title{Make a Model Selection Widget}
\usage{
modelSelectionWidget(mods, modType)

modelSelectionWidgetHeader(mods)

modelSelectionWidgetRow(mods, modType, sci)
}
\arguments{
\item{mods}{Model Set Size object (from the reactive values list).}

\item{modType}{Model type, either "SE" or "CP".}

\item{sci}{Numeric size class element index.}
}
\value{
Rendered HTML model selection menu widget.
}
\description{
Produce a Size-Class-based model selection widget based 
  on the model inputs.
}
\details{
\code{modelSelectionWidgetHeader} creates header text depending on
  if there is one size class or more than one.

\code{modelSelectionWidgetRow} creates a row of the widget (input
  for one size class).
}
