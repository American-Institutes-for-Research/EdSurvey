\dontrun{
## Direct Estimation with NAEP 
# Load data 
sdfNAEP <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# Inspect scoring guidelines
defaultNAEPScoreCard()

# example output: 
#          resCat pointMult pointConst
# 1     Multiple         8          0
# 2  Not Reached        NA         NA
# 3      Missing        NA         NA
# 4      Omitted         8          0
# 5    Illegible         0          0
# 6 Non-Rateable         0          0
# 7     Off Task         0          0

# Run NAEP model, warnings are about item codings
mmlNAEP <- mml.sdf(algebra ~ dsex + b013801, sdfNAEP, weightVar='origwt')

# Call with Taylor
summary(mmlNAEP, varType="Taylor", strataVar="repgrp1", PSUVar="jkunit")

## Direct Estimation with TIMSS 
# Load data 
downloadTIMSS("~/", year=2015)
sdfTIMSS <- readTIMSS("~/TIMSS/2015", countries="usa", grade = "4")

# Run TIMSS model, warnings are about item codings 
mmlTIMSS <- mml.sdf(mmat ~ itsex + asbg04, sdfTIMSS, weightVar='totwgt')

# Call with Taylor
summary(mmlTIMSS, varType="Taylor", strataVar="jkzone", PSUVar="jkrep")
}


