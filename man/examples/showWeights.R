\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# show the weights
showWeights(data=sdf, verbose=TRUE)
}
