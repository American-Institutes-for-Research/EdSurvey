\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# estimate a regression
lm1 <- lm.sdf(formula=composite ~ dsex + b017451, data=sdf, returnVarEstInputs=TRUE)
summary(lm1)
# estimate the covariance between two regression coefficients
# note that the variable names are parallel to what they are called in lm1 output
jkSumMultiplier <- EdSurvey:::getAttributes(data=sdf, attribute="jkSumMultiplier")
covFEveryDay <- varEstToCov(varEstA=lm1$varEstInputs,
                            varA="dsexFemale",
                            varB="b017451Every day",
                            jkSumMultiplier=jkSumMultiplier)
# the estimated difference between the two coefficients
# note: unname prevents output from being named after the first coefficient
unname(coef(lm1)["dsexFemale"] - coef(lm1)["b017451Every day"])
# the standard error of the difference
# uses the formula SE(A-B) = sqrt(var(A) + var(B) - 2*cov(A,B))
sqrt(lm1$coefmat["dsexFemale", "se"]^2
     + lm1$coefmat["b017451Every day", "se"]^2
     - 2 * covFEveryDay)
}
