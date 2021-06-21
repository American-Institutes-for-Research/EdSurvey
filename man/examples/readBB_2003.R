\dontrun{
setwd("~/B&B2003/ecbw/B03")

dasFP <- file.path(getwd(), "Data", "B03DAS.dat")
wgtFP <- file.path(getwd(), "Data", "B03WEIGHT.dat")
mstFP <- file.path(getwd(), "master.txt")

# with verbose output as default
bb2003 <- readBB_2003(dasFP, wgtFP, mstFP)

# silent output
bb2003 <- readBB_2003(dasFP, wgtFP, mstFP, verbose=FALSE)

# force data cache update
bb2003 <- readBB_2003(dasFP, wgtFP, mstFP, forceReread = TRUE)
}
