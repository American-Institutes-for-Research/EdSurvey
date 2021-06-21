\dontrun{
# using default filenames
bps01 <- readBPS_2001(path = "~/BPS/2001")
dim(bps01)

# parameters specified without default filenames
bps01 <- readBPS_2001(path = getwd(),
                      csvFilename = "renamedData.csv",
                      formatFilename = "renamedFormat.txt",
                      metadataFilename = "renamedMeta.txt")
dim(bps01)
}
