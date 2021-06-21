\dontrun{
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
myLogit <- logit.sdf(dsex ~ b017451 + b003501, data = sdf, returnVarEstInputs = T)
oddsRatio(myLogit)
or <- oddsRatio(myLogit)
str(or)
}
