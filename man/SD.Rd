% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary2.R
\name{SD}
\alias{SD}
\title{EdSurvey Standard Deviation}
\usage{
SD(
  data,
  variable,
  weightVar = NULL,
  jrrIMax = 1,
  varMethod = "jackknife",
  dropOmittedLevels = TRUE,
  defaultConditions = TRUE,
  recode = NULL,
  targetLevel = NULL,
  jkSumMultiplier = getAttributes(data, "jkSumMultiplier"),
  returnVarEstInputs = FALSE,
  omittedLevels = deprecated()
)
}
\arguments{
\item{data}{an \code{edsurvey.data.frame}, an \code{edsurvey.data.frame.list}, or a \code{light.edsurvey.data.frame}}

\item{variable}{character vector of variable names}

\item{weightVar}{character weight variable name. Default is the default weight of \code{data} if it exists.
If the given survey data do not have a default weight,
the function will produce unweighted statistics instead.
Can be set to \code{NULL} to return unweighted statistics.}

\item{jrrIMax}{a numeric value; when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the
                  sampling variance from the first plausible value as the component for sampling variance estimation. The \code{Vjrr}
                  term (see
\href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}})
                  can be estimated with any number of plausible values, and values larger than the number of
                  plausible values on the survey (including \code{Inf}) will result in all plausible values being used.
                  Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.}

\item{varMethod}{deprecated parameter; \code{gap} always uses the jackknife variance estimation}

\item{dropOmittedLevels}{a logical value. When set to \code{TRUE}, drops those levels of the specified \code{variable}.
Use print on an \code{edsurvey.data.frame} to see the omitted levels. Defaults to \code{FALSE}.}

\item{defaultConditions}{a logical value. When set to the default value of
\code{TRUE}, uses the default conditions stored in
 an \code{edsurvey.data.frame} to subset the data. Use
\code{print} on an \code{edsurvey.data.frame} to
see the default conditions.}

\item{recode}{a list of lists to recode variables. Defaults to \code{NULL}.
Can be set as \code{recode} \code{=} \code{list(var1}
\code{=} \code{list(from} \code{=} \code{c("a","b","c"), to}
\code{=} \code{"d"))}.}

\item{targetLevel}{a character string. When specified, calculates the gap in
the percentage of students at
\code{targetLevel} in the \code{variable} argument, which is useful for
comparing the gap in the percentage of students at a
survey response level.}

\item{jkSumMultiplier}{when the jackknife variance estimation method---or
balanced repeated replication (BRR)
method---multiplies the final jackknife variance estimate by a value,
set \code{jkSumMultiplier} to that value.
For an \code{edsurvey.data.frame}, or
a \code{light.edsurvey.data.frame},
the recommended value can be recovered with
\code{EdSurvey::getAttributes(}\code{myData,} \code{"jkSumMultiplier")}.}

\item{returnVarEstInputs}{a logical value set to \code{TRUE} to return the
inputs to the jackknife and imputation variance
estimates, which allows for
the computation
of covariances between estimates.}

\item{omittedLevels}{this argument is deprecated. Use \code{dropOmittedLevels}}
}
\value{
a list object with elements:
 \item{mean}{the mean assessment score for \code{variable}, calculated according to the vignette titled
            \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistical Methods Used in EdSurvey}}
 \item{std}{the standard deviation of the \code{mean}}
 \item{stdSE}{the standard error of the \code{std}}
 \item{df}{the degrees of freedom of the \code{std}}
 \item{varEstInputs}{the variance estimate inputs used for calculating covariances with \code{\link{varEstToCov}}. Only returned with \code{returnVarEstInputs} is \code{TRUE}}
}
\description{
Calculate the standard deviation of a numeric variable in an \code{edsurvey.data.frame}.
}
\examples{
\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# get standard deviation for Male's composite score
SD(data = subset(sdf, dsex == "Male"), variable = "composite")

# get several standard deviations

# build an edsurvey.data.frame.list
sdfA <- subset(sdf, scrpsu \%in\% c(5,45,56))
sdfB <- subset(sdf, scrpsu \%in\% c(75,76,78))
sdfC <- subset(sdf, scrpsu \%in\% 100:200)
sdfD <- subset(sdf, scrpsu \%in\% 201:300)

sdfl <- edsurvey.data.frame.list(datalist=list(sdfA, sdfB, sdfC, sdfD),
                                 labels=c("A locations",
                                          "B locations",
                                          "C locations",
                                          "D locations"))

# this shows how these datasets will be described:
sdfl$covs

# SD results for each survey
SD(data = sdfl, variable = "composite")
# SD results more compactly and with comparisons
gap(variable="composite", data=sdfl, stDev=TRUE, returnSimpleDoF=TRUE)
}
}
\author{
Paul Bailey and Huade Huo
}
