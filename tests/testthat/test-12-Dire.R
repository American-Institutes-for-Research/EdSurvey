skip_on_cran()
require(testthat)
require(EdSurvey)

if (!dir.exists(edsurveyHome)) {
  dir.create(edsurveyHome)
}

# TIMSS
context("TIMSS getData w/ Items")
test_that("TIMSS getData w/ Items", {
  # read-in TIMSS data
  sdfTIMSS <<- readTIMSS(file.path(edsurveyHome, "TIMSS", "2015"), countries = "usa", grade = c("4"), verbose = FALSE)

  # check that call works for ommittedLevels T/F
  suppressWarnings(gdat <- getData(data = sdfTIMSS, varnames = c("asbg01", "mmat"), dropOmittedLevels = FALSE, addAttributes = TRUE))
  expect_is(gdat, "data.frame")
  suppressWarnings(gdat <<- getData(data = sdfTIMSS, varnames = c("asbg01", "mmat", "jk1", "totwgt", getAllItems(sdfTIMSS, "mmat"), getStratumVar(sdfTIMSS), getPSUVar(sdfTIMSS), "ROWID"), dropOmittedLevels = TRUE, addAttributes = TRUE))
  expect_is(gdat, "data.frame")
})


context("mml.sdf TIMSS")
test_that("mml.sdf TIMSS", {
  # run TIMSS mml with light edsurvey
  suppressWarnings(mmlTIMSS <<- mml.sdf(mmat ~ asbg01, sdfTIMSS))
  suppressWarnings(mmlTIMSSlight <<- mml.sdf(mmat ~ asbg01, gdat))
  expect_is(mmlTIMSS, "mml.TIMSS")
  expect_equal(summary(mmlTIMSS)$Summary$coef, summary(mmlTIMSSlight)$Summary$coef)
  wt <- EdSurvey::waldTest(mmlTIMSSlight, 1:2)
  expect_s3_class(wt, "edsurveyWaldTest")
  wtl <- EdSurvey::waldTest(mmlTIMSS, 1:2)
  expect_s3_class(wtl, "edsurveyWaldTest")
  expect_equal(wt, wtl)
})

context("drawPVs TIMSS")
test_that("drawPVs TIMSS", {
  set.seed(2)
  pvsTIMSS1 <- drawPVs(data = sdfTIMSS, x = mmlTIMSS)
  expect_s3_class(pvsTIMSS1, "edsurvey.data.frame")
  # Dire does not draw PVs when there is no stuItems, so this is not true
  # expect_equal(sum(is.na(pvsTIMSS1$mmat_dire1)), sum(is.na(pvsTIMSS1$asmmat01)))
  BGContrast <- c()
  for (i in 1:5) {
    BGContrast <- c(BGContrast, mean(pvsTIMSS1[pvsTIMSS1$asbg01 == "BOY", paste0("mmat_dire", i)], na.rm = TRUE) -
      mean(pvsTIMSS1[pvsTIMSS1$asbg01 != "BOY", paste0("mmat_dire", i)], na.rm = TRUE))
  }
  # intentionally very broad
  expect_true(mean(BGContrast) + 4 * sd(BGContrast) > coef(mmlTIMSS)[2])
  expect_true(mean(BGContrast) - 4 * sd(BGContrast) < coef(mmlTIMSS)[2])
  # should not drop rows
  expect_equal(nrow(sdfTIMSS), nrow(pvsTIMSS1))
  # draw for light.edsurvey.data.frame
  set.seed(2)
  pvsTIMSS2 <- drawPVs(data = gdat, x = mmlTIMSSlight)
  # Dire does not draw PVs when there is no stuItems, so this is not true
  # expect_equal(sum(is.na(pvsTIMSS1$mmat_dire1)), sum(is.na(pvsTIMSS2$asmmat01)))
  expect_s3_class(pvsTIMSS2, "light.edsurvey.data.frame")
  # should not drop rows
  expect_equal(nrow(gdat), nrow(pvsTIMSS2))
  # because we set the seed these should be the same. filter out NA values as not all students have value for asbg01, they are already filtered out in the light.edsurvey.data.frame
  dx1 <- pvsTIMSS1$mmat_dire1
  dx1 <- dx1[!is.na(dx1)]
  expect_equal(dx1, pvsTIMSS2$mmat_dire1)

  # Beta Stochastic
  smry <- summary(mmlTIMSS)
  set.seed(2)
  pvsTIMSS <- drawPVs(data = gdat, x = smry, stochasticBeta = TRUE)
  # Beta, non-summary
  expect_error(drawPVs(data = gdat, x = mmlTIMSS, stochasticBeta = TRUE))
})

# NAEP
context("NAEP getData  w/ Items")
test_that("NAEP getData  w/ Items", {
  # read-in TIMSS data
  sdfNAEP <<- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
  # check that call works for ommittedLevels T/F
  suppressWarnings(gdat <<- EdSurvey::getData(sdfNAEP, c("dsex", "algebra", "b018201", getAllItems(sdfNAEP, "algebra"), getStratumVar(sdfNAEP), getPSUVar(sdfNAEP), "ROWID", "origwt"), dropOmittedLevels = FALSE, addAttributes = TRUE))
  expect_s3_class(gdat, "data.frame")
})


context("mml.sdf NAEP")
test_that("mml.sdf NAEP", {
  suppressWarnings(mmlNAEP <<- mml.sdf(algebra ~ dsex + b018201, sdfNAEP))
  expect_s3_class(mmlNAEP, "mml.NAEP")
  skip_if_not_installed("doParallel")
  suppressWarnings(mmlNAEPlight <<- mml.sdf(algebra ~ dsex + b018201, gdat, multiCore = TRUE))
  expect_s3_class(mmlNAEPlight, "mml.NAEP")
  # test that coefficients are the same
  expect_equal(mmlNAEP$mml$coef, mmlNAEPlight$mml$coef)
  expect_s3_class(summary(mmlNAEP), "summary.mml.sdf")
  expect_s3_class(waldTest(mmlNAEP, 2), "edsurveyWaldTest")
})

context("drawPVs NAEP")
test_that("drawPVs NAEP", {
  # draw for edsurvey.data.frame
  set.seed(2)
  suppressWarnings(pvsNAEP <- drawPVs(data = sdfNAEP, x = mmlNAEP))
  expect_s3_class(pvsNAEP, "edsurvey.data.frame")
  expect_equal(nrow(sdfNAEP), nrow(pvsNAEP))
  expect_true("algebra_dire1" %in% colnames(pvsNAEP))
  # draw for light.edsurvey.data.frame
  set.seed(2)
  suppressWarnings(pvsNAEPlight <- drawPVs(data = gdat, x = mmlNAEPlight))
  expect_equal(nrow(gdat), nrow(pvsNAEPlight))
  expect_s3_class(pvsNAEPlight, "light.edsurvey.data.frame")
  expect_true("algebra_dire1" %in% colnames(pvsNAEPlight))
  # these should be about the same but are random, so not expected to be exactly the same
  expect_equal(pvsNAEP$algebra_dire1, pvsNAEPlight$algebra_dire1)
  # Beta Stochastic
  # must use a summary
  expect_error(drawPVs(data = gdat, x = mmlNAEP, stochasticBeta = TRUE))
  smry <- summary(mmlNAEP)
  set.seed(2)
  suppressWarnings(pvsNAEPs <- drawPVs(data = sdfNAEP, x = smry, stochasticBeta = TRUE))
  expect_s3_class(pvsNAEPs, "edsurvey.data.frame")
  expect_equal(nrow(sdfNAEP), nrow(pvsNAEPs))
  expect_true("algebra_dire1" %in% colnames(pvsNAEPs))
  # stochastic beta and non-stochastic beta should, broadly, agree
  expect_equal(mean(pvsNAEP$algebra_dire1), mean(pvsNAEPs$algebra_dire1), tol = 0.005)
  # light
  lsmry <- summary(mmlNAEPlight)
  set.seed(2)
  suppressWarnings(pvsNAEPslight <- drawPVs(data = gdat, x = lsmry, stochasticBeta = TRUE))
  expect_true("algebra_dire1" %in% colnames(pvsNAEPslight))
  # stochastic v non-stochastic should be close-ish
  expect_equal(mean(pvsNAEPlight$algebra_dire1), mean(pvsNAEPslight$algebra_dire1), tol = 0.005)
  # sdf PVs agree with gdat PVs
  expect_equal(pvsNAEPs$algebra_dire1, pvsNAEPslight$algebra_dire1)
})
