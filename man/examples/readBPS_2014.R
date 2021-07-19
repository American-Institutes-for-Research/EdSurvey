\dontrun{
# using default filenames
bps14 <- readBPS_2014(path = "~/BPS/2012")
dim(bps14)

# parameters specified without default filenames
bps14 <- readBPS_2014(path = getwd(),
                      csvFilename = "renamedData.csv",
                      formatFilename = "renamedFormat.txt",
                      metadataFilename = "renamedMeta.txt")
dim(bps14)
}
