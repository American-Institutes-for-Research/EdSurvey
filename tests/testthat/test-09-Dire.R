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
  suppressWarnings(gdat <- EdSurvey::getData(data=sdfTIMSS, varnames=c("asbg01", "mmat"), omittedLevels = FALSE, returnItems = TRUE, addAttributes=TRUE))
  expect_is(gdat, "data.frame")
  suppressWarnings(gdat <<- EdSurvey::getData(data=sdfTIMSS, varnames=c("asbg01", "mmat", "jk1", "totwgt", getStratumVar(sdfTIMSS), getPSUVar(sdfTIMSS), "ROWID"),omittedLevels = TRUE, returnItems = TRUE, addAttributes=TRUE))
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
  skip_if_not( packageVersion("Dire") > "2.0.1" )
  wt <- EdSurvey::waldTest(mmlTIMSSlight, 1:2)
  expect_s3_class(wt, "edsurveyWaldTest")
  wt <- EdSurvey::waldTest(mmlTIMSS, 1:2)
  expect_s3_class(wt, "edsurveyWaldTest")
})

context("drawPVs TIMSS") 
test_that("drawPVs TIMSS",{
  skip_if_not( packageVersion("Dire") >= "2.1.0" )
  # draw for edsurvey.data.frame
  set.seed(2)
  pvsTIMSS1 <- drawPVs(data=sdfTIMSS, x=mmlTIMSS)
  expect_s3_class(pvsTIMSS1, "edsurvey.data.frame")
  # should not drop rows
  expect_equal(nrow(sdfTIMSS), nrow(pvsTIMSS1))
  # draw for light.edsurvey.data.frame
  set.seed(2)
  pvsTIMSS2 <- drawPVs(data=gdat, x=mmlTIMSSlight)
  expect_s3_class(pvsTIMSS2, "light.edsurvey.data.frame")
  # should not drop rows
  expect_equal(nrow(gdat), nrow(pvsTIMSS2))
  # because we set the seed these should be the same
  expect_equal(pvsTIMSS1$mmat_dire1, pvsTIMSS2$mmat_dire1)

  # Beta Stochastic
  smry <- summary(mmlTIMSS)
  pvsTIMSS <- drawPVs(data=gdat, x=smry, stochasticBeta=TRUE)
  # Beta, non-summary 
  expect_error(drawPVs(data=gdat, x=mmlTIMSS, stochasticBeta=TRUE))
})

# NAEP 
context("NAEP getData  w/ Items") 
test_that("NAEP getData  w/ Items",{
  # read-in TIMSS data 
  sdfNAEP <<- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
  # check that call works for ommittedLevels T/F
  suppressWarnings(gdat <<- EdSurvey::getData(sdfNAEP, c("dsex", "algebra", "b018201", getStratumVar(sdfNAEP), getPSUVar(sdfNAEP), "ROWID", "origwt"), omittedLevels = FALSE, returnItems = TRUE, addAttributes=TRUE))
  expect_s3_class(gdat, "data.frame")
})


context("mml.sdf NAEP") 
test_that("mml.sdf NAEP",{
  suppressWarnings(mmlNAEP <<- mml.sdf(algebra ~ dsex + b018201, sdfNAEP))
  expect_s3_class(mmlNAEP, "mml.NAEP")
  suppressWarnings(mmlNAEPlight <<- mml.sdf(algebra ~ dsex + b018201, gdat, multiCore = TRUE, numberOfCores = 10))
  expect_s3_class(mmlNAEPlight, "mml.NAEP")
  # test that coefficients are the same
  expect_equal(mmlNAEP$mml$coef, mmlNAEPlight$mml$coef)
  summary(mmlNAEP)
  waldTest(mmlNAEP, 2)
})

context("drawPVs NAEP") 
test_that("drawPVs NAEP",{
  skip_if_not( packageVersion("Dire") > "2.1" )
  # draw for edsurvey.data.frame
  set.seed(2)
  pvsNAEP <- drawPVs(data=sdfNAEP, x=mmlNAEP)
  expect_s3_class(pvsNAEP, "edsurvey.data.frame")
  expect_equal(nrow(sdfNAEP), nrow(pvsNAEP))
  expect_true("algebra_dire1" %in% colnames(pvsNAEP))
  # draw for light.edsurvey.data.frame
  set.seed(2)
  pvsNAEPlight <- drawPVs(data=gdat, x=mmlNAEPlight)
  expect_equal(nrow(gdat), nrow(pvsNAEPlight))
  expect_s3_class(pvsNAEPlight, "light.edsurvey.data.frame")
  expect_true("algebra_dire1" %in% colnames(pvsNAEPlight))
  # these should be about the same but are random, so not expected to be exactly the same
  expect_equal( pvsNAEP$algebra_dire1, pvsNAEPlight$algebra_dire1)
  # Beta Stochastic
  # must use a summary
  expect_error(drawPVs(data=gdat, x=mmlNAEP, stochasticBeta=TRUE))
  smry <- summary(mmlNAEP)
  set.seed(2)
  pvsNAEPs <- drawPVs(data=sdfNAEP, x=smry, stochasticBeta=TRUE)
  expect_s3_class(pvsNAEPs, "edsurvey.data.frame")
  expect_equal(nrow(sdfNAEP), nrow(pvsNAEPs))
  expect_true("algebra_dire1" %in% colnames(pvsNAEPs))
  # stochastic beta and non-stochastic beta should, broadly, agree
  expect_equal( mean(pvsNAEP$algebra_dire1), mean(pvsNAEPs$algebra_dire1), tol=0.005)
  # light
  lsmry <- summary(mmlNAEPlight)
  set.seed(2)
  pvsNAEPslight <- drawPVs(data=gdat, x=lsmry, stochasticBeta=TRUE)
  expect_true("algebra_dire1" %in% colnames(pvsNAEPslight))
  # stochastic v non-stochastic should be close-ish
  expect_equal( mean(pvsNAEPlight$algebra_dire1), mean(pvsNAEPslight$algebra_dire1), tol=0.005)
  # sdf PVs agree with gdat PVs
  expect_equal( pvsNAEPs$algebra_dire1, pvsNAEPslight$algebra_dire1)
})

# # Other survey  
context("Other getData  w/ Items") 
test_that("Other getData  w/ Items",{
  # shouldn't work with other surveys
  downloadPIAAC(edsurveyHome, root=edsurveyHome, cycle=1)
  sdfPIAAC <- readPIAAC(file.path(edsurveyHome, "PIAAC", "Cycle 1"), countries = c("usa12_14"), verbose=FALSE)
  expect_error(gdat <- getData(sdfPIAAC, c("gender_r", "g_q03h"), omittedLevels = TRUE, returnItems = TRUE, addAttributes=TRUE))
  
})

