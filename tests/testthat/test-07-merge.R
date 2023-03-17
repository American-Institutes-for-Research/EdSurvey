require(testthat)
require(EdSurvey)
options(width = 500)
options(useFancyQuotes=FALSE)
options(digits=7)

context("merge with NAEPprimer") #When this fails all regression tests are invalid.
test_that("merge with NAEPprimer",{
  sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
  
  sdf$pseudoID <- 10 * seq_along(sdf$scrpsu) #file doesn't have a true 'ID' field. for NAEP be mindful of 'defaultConditions' as that impacts size of 'cache' assignment but not nrow/dim
  
  tmpDF <- data.frame(pseudoID = sdf$pseudoID, 
                      otherID = sdf$pseudoID,
                      tVar1 = ifelse((seq_along(sdf$pseudoID) %% 2) == 0, "Even", "Odd"),
                      tVar2 = ifelse((seq_along(sdf$pseudoID) %% 7) == 0, "Seven", "Not Seven"),
                      knownVar1 = sdf$dsex,
                      knownVar2 = sdf$origwt)
  
  #shuffle the dataframe by rows for testing
  tmpDF = tmpDF[sample(1:nrow(tmpDF)), ]
  
  #test - normal merge (ensure vectors are ordered properly on return)
  res <- merge(sdf, tmpDF, by = "pseudoID")
  expect_equal(res$knownVar1, res$dsex)
  expect_equal(res$knownVar2, res$origwt)
  skip_on_cran()
  
  #shuffle the dataframe by rows for testing
  tmpDF <- tmpDF[sample(1:nrow(tmpDF)), ]
  
  #test - different by.x and by.y
  res <- merge(sdf, tmpDF, by.x = "pseudoID", by.y = "otherID")
  expect_equal(res$knownVar1, res$dsex)
  expect_equal(res$knownVar2, res$origwt)
  
  #test - smaller sample
  smallerSamp <- sample(1:nrow(tmpDF), 100)
  tmpDF2 <- tmpDF[smallerSamp, ]

  withr::with_options(list(digits=7, scipen=999), {
    res <- merge(sdf, tmpDF2, by.x = "pseudoID", by.y = "otherID", all.x = FALSE, all.y = TRUE, suffixes = c("", ".dupe"))
    expect_equal(table(res$knownVar1), table(tmpDF2$knownVar1))
    expect_equal(summary(res$knownVar2[!is.na(res$knownVar2)]), summary(tmpDF2$knownVar2)) #res will have many NA values with small merge
  })
})

context("merge with TIMSS") #When this fails all regression tests are invalid.
test_that("merge with TIMSS",{
  skip_on_cran()
  usa <- readTIMSS(file.path(edsurveyHome, "TIMSS/2015"), "usa", 4, verbose = FALSE)
  
  #acbg20 = years principal at school
  schDat <- getData(usa, c("idschool", "acbg20"), omittedLevels = FALSE)
  schDat$Prin10 <- schDat$acbg20 > 10
  schDat <- unique(subset(schDat, Prin10==TRUE)) #drop to small subset (22 records)
  
  #test merge without idstud being the cache
  usa2 <- merge(usa, schDat, by = "idschool")

  expect_equal(ifelse(usa$acbg20 > 10, TRUE, NA), usa2$Prin10)
  
})