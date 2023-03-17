# merge.edsurvey.data.frame testing and merge.light.edsurvey.data.frame testing
require(testthat)

options(width = 500)
options(useFancyQuotes=FALSE)

context("merge testing edsurvey.data.frame")
test_that("merge testing edsurvey.data.frame",{
  sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
  lsdf <- getData(sdf, c(all.vars(composite ~ dsex + b017451), "ROWID", "origwt", "jkunit", "repgrp1"), addAttributes=TRUE)
  
  newdf <- data.frame(ROWID = lsdf$ROWID, newVar = lfactor(ifelse(lsdf$b017451 < 3, 1,2), levels = c(1,2), labels = c("Once a week or less", "More than once a week")))
  
  tblREF <- structure(c(`Once a week or less` = 6984L, `More than once a week` = 9347L
  ), dim = 2L, dimnames = structure(list(c("Once a week or less", 
                                           "More than once a week")), names = ""), class = "table")
  
  #merge.edsurvey.data
  sdfT1 <- merge(sdf, newdf, by = "ROWID")
  expect_is(sdfT1, "edsurvey.data.frame")
  expect_equal(table(sdfT1$newVar), tblREF)
  
  #merge.light.edsurvey.data
  sdfT2 <- merge(lsdf, newdf, by = "ROWID")
  expect_is(sdfT2, "light.edsurvey.data.frame")
  expect_equal(table(sdfT2$newVar), tblREF)
})
  