\dontrun{
# using default filenames
bb <- readBB_2012(path = "~/BB/2012")
dim(bb)

# parameters specified without default filenames
bb <- readBB_2012(path = getwd(),
                  csvFilename = "renamedData.csv",
                  formatFilename = "renamedFormat.txt",
                  metadataFilename = "renamedMeta.txt")
dim(bb)
}
