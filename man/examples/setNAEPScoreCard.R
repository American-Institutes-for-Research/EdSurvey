\dontrun{
datFP <- "~/NAEP_Folder/Data/M50NT3AT.dat"
sdf <- readNAEP(datFP)

#how to set NAEP mml attributes
#if readNAEP does not detect them automatically
dctFP <- "~/NAEP_Folder/AM/M50NT3AT.dct"
sdf <- setNAEPScoreCard(sdf, dctFP)
}
