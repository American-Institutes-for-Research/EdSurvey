\dontrun{
setwd("~/BPS1994/")

datFP <- file.path(getwd(), "BPS94", "STUDENT.DAT")
mstFP <- file.path(getwd(), "ECBW", "BP4", "MASTER.TXT")

#with verbose output as default
bps94 <- readBPS_1994(datFP, mstFP)

#silent output
bps94 <- readBPS_1994(datFP, mstFP, verbose=FALSE)

#force data cache update
bps94 <- readBPS_1994(datFP, mstFP, forceReread = TRUE)
}
