% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gap.R
\name{print.gap}
\alias{print.gap}
\alias{print.gapList}
\title{Gap Analysis Printing}
\usage{
\method{print}{gap}(
  x,
  ...,
  printPercentage = TRUE,
  use_es_round = getOption("EdSurvey_round_output")
)

\method{print}{gapList}(x, ..., printPercentage = TRUE)
}
\arguments{
\item{x}{an \code{R} object representing a \code{gap} of class \code{gap} or \code{gapList}}

\item{...}{these arguments are not passed anywhere and are included only for compatibility}

\item{printPercentage}{a logical value set to \code{TRUE} to request printing
of the percentage in the groups. Defaults to \code{TRUE}.}

\item{use_es_round}{use the EdSurvey rounding methods for gap}
}
\description{
Prints labels and a results vector of a gap analysis.
}
\author{
Paul Bailey
}
