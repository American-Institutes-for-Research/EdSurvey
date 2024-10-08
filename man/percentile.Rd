% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percentile.R
\name{percentile}
\alias{percentile}
\title{EdSurvey Percentiles}
\usage{
percentile(
  variable,
  percentiles,
  data,
  weightVar = NULL,
  jrrIMax = 1,
  varMethod = c("jackknife", "Taylor"),
  alpha = 0.05,
  dropOmittedLevels = TRUE,
  defaultConditions = TRUE,
  recode = NULL,
  returnVarEstInputs = FALSE,
  returnNumberOfPSU = FALSE,
  pctMethod = c("symmetric", "unbiased", "simple"),
  confInt = TRUE,
  dofMethod = c("JR", "WS"),
  omittedLevels = deprecated()
)
}
\arguments{
\item{variable}{the character name of the variable to percentiles computed,
typically a subject scale or subscale}

\item{percentiles}{a numeric vector of percentiles in the range of 0 to 100
(inclusive)}

\item{data}{an \code{edsurvey.data.frame} or an
\code{edsurvey.data.frame.list}}

\item{weightVar}{a character indicating the weight variable to use.}

\item{jrrIMax}{a numeric value; when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the
                  sampling variance from the first plausible value as the component for sampling variance estimation. The \eqn{V_{jrr}}
                  term (see
\href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}})
                  can be estimated with any number of plausible values, and values larger than the number of
                  plausible values on the survey (including \code{Inf}) will result in all plausible values being used.
                  Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.}

\item{varMethod}{a character set to \code{jackknife} or \code{Taylor}
that indicates the variance estimation method used when
constructing the confidence intervals. The jackknife
variance estimation method is always
used to calculate the standard error.}

\item{alpha}{a numeric value between 0 and 1 indicating the confidence level.
An \code{alpha} value of 0.05 would indicate a 95\%
confidence interval and is the default.}

\item{dropOmittedLevels}{a logical value. When set to the default value of
\code{TRUE}, drops those levels of
all factor variables that are specified in
\code{achievementVars} and \code{aggregatBy}.
Use \code{print} on an \code{edsurvey.data.frame}
to see the omitted levels.}

\item{defaultConditions}{a logical value. When set to the default value
of \code{TRUE}, uses the default
conditions stored in an \code{edsurvey.data.frame}
to subset the data.
Use \code{print} on an \code{edsurvey.data.frame}
to see the default conditions.}

\item{recode}{a list of lists to recode variables. Defaults to
\code{NULL}. Can be set as
\code{recode=}\code{list(var1=} \code{list(from=} \code{c("a",}
\code{"b",} \code{"c"),}
\code{to=} \code{"d"))}.}

\item{returnVarEstInputs}{a logical value set to \code{TRUE} to return the
inputs to the jackknife and imputation variance
estimates which allows for the computation
of covariances between estimates.}

\item{returnNumberOfPSU}{a logical value set to \code{TRUE} to return the number of
primary sampling units (PSUs)}

\item{pctMethod}{one of \dQuote{unbiased}, \dQuote{symmetric}, \dQuote{simple};
unbiased produces a weighted median unbiased percentile estimate,
whereas simple uses a basic formula that matches previously
published results. Symmetric uses a more basic formula
but requires that the percentile is symetric to multiplying
the quantity by negative one.}

\item{confInt}{a Boolean indicating if the confidence interval should be returned}

\item{dofMethod}{passed to \code{\link{DoFCorrection}} as the \code{method} argument}

\item{omittedLevels}{this argument is deprecated. Use \code{dropOmittedLevels}}
}
\value{
The return type depends on whether the class of the \code{data} argument is an
\code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}.

\strong{The data argument is an edsurvey.data.frame}
  When the \code{data} argument is an \code{edsurvey.data.frame},
  \code{percentile} returns an S3 object of class \code{percentile}.
  This is a \code{data.frame} with typical attributes (\code{names},
  \code{row.names}, and \code{class}) and additional attributes as follows:
    \item{n0}{number of rows on \code{edsurvey.data.frame} before any conditions were applied}
    \item{nUsed}{number of observations with valid data and weights larger than zero}
    \item{nPSU}{number of PSUs used in the calculation}
    \item{call}{the call used to generate these results}

  The columns of the \code{data.frame} are as follows:
    \item{percentile}{the percentile of this row}
    \item{estimate}{the estimated value of the percentile}
    \item{se}{the jackknife standard error of the estimated percentile}
    \item{df}{degrees of freedom}
    \item{confInt.ci_lower}{the lower bound
                     of the confidence interval}
    \item{confInt.ci_upper}{the upper bound
                     of the confidence interval}
    \item{nsmall}{the number of units with more extreme results, averaged
                  across plausible values}
  When the \code{confInt} argument is set to \code{FALSE}, the confidence
  intervals are not returned.

\strong{The data argument is an edsurvey.data.frame.list}
  When the \code{data} argument is an \code{edsurvey.data.frame.list},
  \code{percentile} returns an S3 object of class \code{percentileList}.
  This is a data.frame with a \code{call} attribute.
  The columns in the \code{data.frame} are identical to those in the previous
  section, but there also are columns from the \code{edsurvey.data.frame.list}.

    \item{covs}{a column for each column in the \code{covs} value of the
                \code{edsurvey.data.frame.list}.
                See Examples.}

When \code{returnVarEstInputs} is \code{TRUE}, an attribute
\code{varEstInputs} also is returned that includes the variance estimate
inputs used for calculating covariances with \code{\link{varEstToCov}}.
}
\description{
Calculates the percentiles of a numeric variable in an
             \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
             or an \code{edsurvey.data.frame.list}.
}
\details{
Percentiles, their standard errors, and confidence intervals
are calculated according to the vignette titled
\href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}.
The standard errors and confidence intervals are based
on separate formulas and assumptions.

The Taylor series variance estimation procedure is not relevant to percentiles
because percentiles are not continuously differentiable.
}
\examples{
\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# get the median of the composite
percentile(variable="composite", percentiles=50, data=sdf)

# get several percentiles
percentile(variable="composite", percentiles=c(0,1,25,50,75,99,100), data=sdf)
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

percentile(variable="composite", percentiles=50, data=sdfl)
percentile(variable="composite", percentiles=c(25, 50, 75), data=sdfl)
}
}
\references{
Hyndman, R. J., & Fan, Y. (1996). Sample quantiles in statistical packages. \emph{American Statistician}, \emph{50}, 361--365.
}
\author{
Paul Bailey
}
