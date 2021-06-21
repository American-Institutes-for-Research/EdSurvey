\dontrun{
# using default filenames
bps09 <- readBPS_2009(path = "~/BPS/2009")
dim(bps09)

# parameters specified without default filenames
bps09 <- readBPS_2009(path = getwd(),
                      csvFilename = "renamedData.csv",
                      formatFilename = "renamedFormat.txt",
                      metadataFilename = "renamedMeta.txt")
dim(bps09)
}
