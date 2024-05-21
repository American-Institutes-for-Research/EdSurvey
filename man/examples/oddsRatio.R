\dontrun{
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
myLogit <- logit.sdf(formula=dsex ~ b017451 + b003501, data = sdf, returnVarEstInputs = T)
oddsRatio(model=myLogit)
or <- oddsRatio(model=myLogit)
str(or)
}
