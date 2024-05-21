\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# by default uses the jackknife variance method using replicate weights
table(sdf$b013801)
logit1 <- logit.sdf(formula=I(b013801 %in% c("26-100", ">100")) ~ dsex + b017451, data=sdf)
# use summary to get detailed results
summary(logit1)

# Taylor series variance estimation
logit1t <- logit.sdf(formula=I(b013801 %in% c("26-100", ">100")) ~ dsex + b017451, data=sdf,
                     varMethod="Taylor")
summary(logit1t)

logit2 <- logit.sdf(formula=I(composite >= 300) ~ dsex + b013801, data=sdf)
summary(logit2)

logit3 <- glm.sdf(formula=I(composite >= 300) ~ dsex + b013801, data=sdf, 
                  family=quasibinomial(link="logit"))

# Wald test for joint hypothesis that all coefficients in b013801 are zero
waldTest(model=logit3, coefficients="b013801")

summary(logit3)
}
