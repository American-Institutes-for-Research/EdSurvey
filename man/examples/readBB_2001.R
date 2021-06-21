\dontrun{
setwd("~/B&B2001/ecbw/B01")

dervFP <- file.path(getwd(), "Data", "B01DER.dat")
wgtFP <- file.path(getwd(), "Data", "B01WT.dat")
mstFP <- file.path(getwd(), "master.txt")

# with verbose output as default
bb2001 <- readBB_2001(dervFP, wgtFP, mstFP)

# silent output
bb2001 <- readBB_2001(dervFP, wgtFP, mstFP, verbose=FALSE)

# force cache update
bb2001 <- readBB_2001(dervFP, wgtFP, mstFP, forceReread = TRUE)
}
