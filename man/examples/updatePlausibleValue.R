\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# get the PVs before
showPlausibleValues(data=sdf)
sdf2 <- updatePlausibleValue(oldVar="composite", newVar="overall", data=sdf)
showPlausibleValues(data=sdf2)
lm1 <- lm.sdf(formula=overall ~ b017451, data=sdf2)
summary(lm1)
}
