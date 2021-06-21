\dontrun{
wrkFld <- "~/HSB/SOPHOMORE"

dataPath <- file.path(wrkFld, "REVISED_ASCII", "HSO8092_REV.PRI")
sasPath <- file.path(wrkFld, "SAS_EXTRACT_LOGIC", "HSBso_READ_HSO8092.SAS")

# with verbose output as default
hsbSO <- readHSB_Sophomore(dataPath, sasPath)

# silent output
hsbSO <- readHSB_Sophomore(dataPath, sasPath, verbose = FALSE)

# force cache update
hsbSO <- readHSB_Sophomore(dataPath, sasPath, forceReread = TRUE)
}
