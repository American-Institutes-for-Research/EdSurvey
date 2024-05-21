\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# conduct quantile regression at a given tau value (by default, tau is set to be 0.5) 
rq1 <- rq.sdf(formula=composite ~ dsex + b017451, data=sdf, tau = 0.8)
summary(rq1)
}
