copyDataToTemp <- function(f0="M35NT2PM") {
  sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
  sf <- system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer")
  sfs <- system.file("extdata/data", "M36NC2PM.dat", package = "NAEPprimer")
  d0 <- tempdir()
  d0 <- paste0(dirname(d0),"/", basename(d0))
  d1 <- file.path(d0,"data")
  dir.create(d1, showWarnings=FALSE)
  f1 <- file.path(d1, paste0(f0, ".dat"))
  file.create(f1)
  schFilename <- f0
  substr(schFilename, nchar(schFilename) - 3, nchar(schFilename) - 3) <- "C"
  f1s <- file.path(d1, paste0(schFilename, ".dat"))
  n <- nrow(sdf)
  smp <- sort(sample(1:n,n, replace=TRUE))
  line <- readLines(sf)
  line <- line[smp]
  writeChar(paste(line,collapse="\n"), f1)
  d2 <- file.path(d0,"select")
  dir.create(d2, showWarnings=FALSE)
  d3 <- file.path(d2,"parms")
  dir.create(d3, showWarnings=FALSE)
  od0 <- system.file("extdata/select/parms", "M36NT2PM.fr2", package = "NAEPprimer")
  file.copy(from=od0, to=file.path(d3, paste0(f0, ".fr2")))
  od0s <- system.file("extdata/select/parms", "M36NC2PM.fr2", package = "NAEPprimer")
  file.copy(from=od0s, to=file.path(d3, paste0(schFilename, ".fr2")))
  file.copy(from=sfs, to=f1s)
  readNAEP(f1)
}
