skip_on_cran()
require(testthat)
require(EdSurvey)

# set up directory for TIMSS checks 
if(!exists("edsurveyHome")) {
  if (Sys.info()[['sysname']] == "Windows") {
    edsurveyHome <- "C:/EdSurveyData/"
  } else {
    edsurveyHome <- "~/EdSurveyData/"
  }
}

if (!dir.exists(edsurveyHome)) {
  dir.create(edsurveyHome)
}

# TIMSS 
context("TIMSS getData w/ Items") 
test_that("TIMSS getData w/ Items",{
  # read-in TIMSS data 
  sdfTIMSS <<- readTIMSS(file.path(edsurveyHome, "TIMSS", "2015"), countries="usa", grade = c("4"), verbose=FALSE)

  # check that call works for ommittedLevels T/F
  suppressWarnings(gdat <- getData(sdfTIMSS, c("asbg01", "mmat"), omittedLevels = FALSE, returnItems = TRUE, addAttributes=TRUE))
  expect_is(gdat, "data.frame")
  suppressWarnings(gdat <<- getData(sdfTIMSS, c("asbg01", "mmat", "jk1", "totwgt", getStratumVar(sdfTIMSS), getPSUVar(sdfTIMSS), "ROWID"),omittedLevels = TRUE, returnItems = TRUE, addAttributes=TRUE))
  expect_is(gdat, "data.frame")
})


context("mml.sdf TIMSS") 
test_that("mml.sdf TIMSS",{
  # run TIMSS mml with light edsurvey 
  sdfTIMSS <<- setTIMSSScoreDict(sdfTIMSS, 'mmat')
  suppressWarnings(mmlTIMSS <<- mml.sdf(mmat ~ asbg01, sdfTIMSS, idVar="ROWID"))
  gdat <<- setTIMSSScoreDict(gdat, 'mmat')
  suppressWarnings(mmlTIMSSlight <<- mml.sdf(mmat ~ asbg01, gdat, idVar="ROWID"))
  expect_is(mmlTIMSS, "mml.TIMSS")
})

context("drawPVs TIMSS") 
test_that("drawPVs TIMSS",{
  skip_if_not( packageVersion("Dire") > "1.1" )
  # draw for edsurvey.data.frame
  pvsTIMSS <- drawPVs(sdfTIMSS, mmlTIMSS)
  # draw for light.edsurvey.data.frame
  pvsTIMSS <- drawPVs(gdat, mmlTIMSSlight)
  # Beta Stochastic
  smry <- summary(mmlTIMSS)
  pvsTIMSS <- drawPVs(gdat, smry, stochasticBeta=TRUE)
  # Beta, non-summary 
  expect_error(drawPVs(gdat, mmlTIMSS, stochasticBeta=TRUE))
})

# NAEP 
context("NAEP getData  w/ Items") 
test_that("NAEP getData  w/ Items",{
  # read-in TIMSS data 
  sdfNAEP <<- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
  # check that call works for ommittedLevels T/F
  suppressWarnings(gdat <<- getData(sdfNAEP, c("dsex", "algebra", getStratumVar(sdfNAEP), getPSUVar(sdfNAEP), "ROWID", "origwt"), omittedLevels = FALSE, returnItems = TRUE, addAttributes=TRUE))
  expect_s3_class(gdat, "data.frame")
})


context("mml.sdf NAEP") 
test_that("mml.sdf NAEP",{
  suppressWarnings(mmlNAEP <<- mml.sdf(algebra~dsex, gdat, idVar="ROWID"))
  expect_s3_class(mmlNAEP, "mml.NAEP")
  suppressWarnings(mmlNAEPlight <<- mml.sdf(algebra~dsex, gdat, idVar="ROWID", multiCore = TRUE, numberOfCores = 10))
  expect_s3_class(mmlNAEPlight, "mml.NAEP")
  # test that coefficients are the same
  expect_equal(mmlNAEP$mml$coef, mmlNAEPlight$mml$coef)
})


context("drawPVs NAEP") 
test_that("drawPVs NAEP",{
  skip_if_not( packageVersion("Dire") > "1.1" )
  # draw for edsurvey.data.frame
  pvsNAEP <- drawPVs(sdfNAEP, mmlNAEP)
  expect_s3_class(pvsNAEP, "edsurvey.data.frame")
  expect_true("algebra_dire1" %in% colnames(pvsNAEP))
  # draw for light.edsurvey.data.frame
  pvsNAEPlight <- drawPVs(gdat, mmlNAEPlight)
  expect_s3_class(pvsNAEPlight, "light.edsurvey.data.frame")
  expect_true("algebra_dire1" %in% colnames(pvsNAEPlight))
  # these should be about the same but are random, so not expected to be exactly the same
  expect_equal( mean(pvsNAEP$algebra_dire1), mean(pvsNAEPlight$algebra_dire1), tol=0.005)
  # Beta Stochastic
  # must use a summary
  expect_error(drawPVs(gdat, mmlNAEP, stochasticBeta=TRUE))
  smry <- summary(mmlNAEP)
  pvsNAEPs <- drawPVs(sdfNAEP, smry, stochasticBeta=TRUE)
  expect_s3_class(pvsNAEPs, "edsurvey.data.frame")
  expect_true("algebra_dire1" %in% colnames(pvsNAEPs))
  expect_equal( mean(pvsNAEP$algebra_dire1), mean(pvsNAEPs$algebra_dire1), tol=0.005)
  # light
  lsmry <- summary(mmlNAEPlight)
  pvsNAEPlight <- drawPVs(gdat, lsmry, stochasticBeta=TRUE)
  expect_true("algebra_dire1" %in% colnames(pvsNAEPs))
  expect_equal( mean(pvsNAEP$algebra_dire1), mean(pvsNAEPs$algebra_dire1), tol=0.005)
})

# # Other survey  
context("Other getData  w/ Items") 
test_that("Other getData  w/ Items",{
  # shouldn't work with other surveys
  sdfPIAAC <- readPIAAC(file.path(edsurveyHome, "PIAAC", "Cycle 1"), countries = c("usa12_14"), verbose=FALSE)
  expect_error(gdat <- getData(sdfPIAAC, c("gender_r", "g_q03h"), omittedLevels = TRUE, returnItems = TRUE, addAttributes=TRUE))
  
})

