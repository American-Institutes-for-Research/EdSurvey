\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# by default uses jackknife variance method using replicate weights
lm1 <- lm.sdf(formula=composite ~ dsex + b017451, data=sdf)
lm1

# the summary function displays detailed results
summary(lm1)

# to show standardized regression coefficients
summary(lm1, src=TRUE)

# to specify a variance method, use varMethod
lm2 <- lm.sdf(formula=composite ~ dsex + b017451, data=sdf, varMethod="Taylor")
lm2
summary(lm2)

# use relevel to set a new omitted category
lm3 <- lm.sdf(formula=composite ~ dsex + b017451, data=sdf, relevels=list(dsex="Female"))
summary(lm3)
# test of a simple joint hypothesis
waldTest(lm3, "b017451")

# use recode to change values for specified variables
lm4 <- lm.sdf(formula=composite ~ dsex + b017451, data=sdf,
              recode=list(b017451=list(from=c("Never or hardly ever",
                                              "Once every few weeks",
                                              "About once a week"),
                                       to=c("Infrequently")),
                          b017451=list(from=c("2 or 3 times a week","Every day"),
                                       to=c("Frequently"))))
# Note: "Infrequently" is the dropped level for the recoded b017451
summary(lm4)
}
