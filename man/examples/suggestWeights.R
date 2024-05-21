\dontrun{
# read-in ECLS-K:2011 data file with parameters specified
eclsk11 <- readECLS_K2011(path=file.path("~/", "ECLS_K", "2011"), filename = "childK5p.dat",
                           layoutFilename = "ECLSK2011_K5PUF.sps", verbose = FALSE)

# suggest weight for individual variable
suggestWeights(varnames="x8mscalk5", data=eclsk11)

# suggest weight for multiple variables
suggestWeights(varnames=c("x8mscalk5", "x_chsex_r", "x12sesl"), data=eclsk11)
}
