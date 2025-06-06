% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waldTest.R
\name{waldTest}
\alias{waldTest}
\title{Wald Tests}
\usage{
waldTest(model, coefficients, H0 = NULL)
}
\arguments{
\item{model}{an \code{edsurveyGlm} and \code{edsurveyLm}}

\item{coefficients}{coefficients to be tested, by name or position in
\code{coef} vector. See Details.}

\item{H0}{reference values to test coefficients against, default = 0}
}
\value{
An \code{edsurveyWaldTest} object with the following elements:
   \item{Sigma}{coefficient covariance matrix}
   \item{coefficients}{indices of the coefficients tested}
   \item{H0}{null hypothesis values of coefficients tested}
   \item{result}{result object containing the values of the chi-square and \emph{F}-tests}
   \item{hypoMatrix}{hypothesis matrix used for the Wald Test}
}
\description{
Tests on coefficient(s) of \code{edsurveyGlm}
             and \code{edsurveyLm} models.
}
\details{
When plausible values are present, likelihood ratio tests cannot be used.
However, the Wald test can be used to test estimated parameters in a model,
with the null hypothesis being that a parameter(s) is equal to some value(s).
In the default case where the null hypothesis value of the parameters is 0,
if the test fails to reject the null hypothesis, removing the variables from
the model will not substantially harm the fit of that model. Alternative null
hypothesis values also can be specified with the \code{H0} argument.
See Examples.

Coefficients to test can be specified by an integer (or integer vector)
corresponding to the order of coefficients in the summary output. Coefficients
also can be specified using a character vector, to specify coefficient names
to test. The name of a factor variable can be used to test all levels of that
variable.

This test produces both chi-square and \emph{F}-tests; their calculation is described
in the vignette titled
\href{https://www.air.org/sites/default/files/EdSurvey-WaldTest.pdf}{\emph{Methods and Overview of Using EdSurvey for Running Wald Tests}}.
}
\examples{
\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# example with glm model
myLogit <- logit.sdf(formula=dsex ~ b017451 + b003501, data = sdf,
                     returnVarEstInputs = TRUE)

# single coefficient integer
waldTest(model = myLogit, coefficients = 2)

# set of coefficients integer vector
waldTest(model = myLogit, coefficients = 2:5)

# specify levels of factor variable to test
waldTest(myLogit, c("b017451Every day", "b017451About once a week"))

# specify all levels of factor variable to test
waldTest(myLogit, "b017451")

# example with lm model
fit <- lm.sdf(formula=composite ~ dsex + b017451, data = sdf,
              returnVarEstInputs = TRUE)

waldTest(model = fit, coefficients = "b017451")

# examples with alternative (nonzero) null hypothesis values
waldTest(model = myLogit, coefficients = 2, H0 = 0.5)

waldTest(model = myLogit, coefficients = 2:5, H0 = c(0.5, 0.6, 0.7, 0.8))

waldTest(model = myLogit, coefficients = "b017451", H0 = c(0.5, 0.6, 0.7, 0.8))

waldTest(model = myLogit, coefficients = c("b017451Every day", "b017451About once a week"),
         H0 = c(0.1, 0.2))
}
}
\references{
Diggle, P. J., Liang, K.-Y., & Zeger, S. L. (1994). \emph{Analysis of longitudinal data}. Oxford, UK: Clarendon Press.

Draper, N. R., & Smith, H. (1998). \emph{Applied regression analysis}. New York, NY: Wiley.

Fox, J. (1997). \emph{Applied regression analysis, linear models, and related methods}. Thousand Oaks, CA: SAGE.

[Institute for Digital Research and Education. (n.d.). FAQ: How are the likelihood ratio, Wald, and LaGrange multiplier (score) tests different and/or similar?](https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-are-the-likelihood-ratio-wald-and-lagrange-multiplier-score-tests-different-andor-similar/). Los Angeles: University of California at Los Angeles. Retrieved from [https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-are-the-likelihood-ratio-wald-and-lagrange-multiplier-score-tests-different-andor-similar/](https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-are-the-likelihood-ratio-wald-and-lagrange-multiplier-score-tests-different-andor-similar/)

Korn, E., & Graubard, B. (1990). Simultaneous testing of regression coefficients with complex survey data: Use of Bonferroni t statistics. \emph{The American Statistician}, \emph{44}(4), 270--276.
}
\author{
Alex Lishinski and Paul Bailey
}
