% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showPlausibleValues.R
\name{showPlausibleValues}
\alias{showPlausibleValues}
\title{Plausible Value Variable Names}
\usage{
showPlausibleValues(data, verbose = FALSE)
}
\arguments{
\item{data}{an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
an \code{edsurvey.data.frame.list}}

\item{verbose}{a logical value; set to \code{TRUE} to get the variable names for plausible values.
Otherwise, prints only the subject scale or subscale names for variables
that use plausible values.}
}
\description{
Prints a summary of the subject scale or subscale and the associated variables for their
             plausible values for an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
}
\examples{
\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# show the plausible values
showPlausibleValues(data=sdf, verbose=TRUE)
}
}
\author{
Michael Lee and Paul Bailey
}
