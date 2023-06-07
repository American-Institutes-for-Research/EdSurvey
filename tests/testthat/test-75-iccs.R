skip_on_cran()
require(testthat)
context("ICCS test")
require(EdSurvey)
options(width = 500)
options(useFancyQuotes = FALSE)

if (!exists("edsurveyHome")) {
  edsurveyHome <- "~/EdSurveyData/"
}

if (!dir.exists(edsurveyHome)) {
  dir.create(edsurveyHome)
}

# able to toggle 'forceReread' for recaching the data if necessary
if (!exists("forceCacheUpdate")) {
  forceCacheUpdate <- FALSE
}

# Test =================

# 1. Check data read-in
test_that("ICCS data reads in correctly", {
  # download manually
  # downloadCivEDICCS(years=c(1999, 2009, 2016))
  expect_silent(dnk8 <<- readCivEDICCS(path = file.path(edsurveyHome, "ICCS", "2016"), countries = "dnk", gradeLvl = 8, verbose = FALSE, forceReread = forceCacheUpdate))
  expect_silent(mex8 <<- readCivEDICCS(path = file.path(edsurveyHome, "ICCS", "2016"), countries = "mex", gradeLvl = 8, verbose = FALSE, forceReread = forceCacheUpdate))
  # 2000 complains about the PSU variable not being present

  expect_is(dnk8, "edsurvey.data.frame")
  expect_is(mex8, "edsurvey.data.frame")
  expect_equal(dim(dnk8), c(6254, 814))
  expect_equal(dim(mex8), c(5526, 812))
})


context("ICCS analysis")
test_that("ICCS analysis", {
  e1REF <- c(
    "",
    "Formula: civ ~ is3g12a ",
    "",
    "Plausible values: 5",
    "jrrIMax: 1",
    "Weight variable: 'totwgts'",
    "Variance method: jackknife",
    "JK replicates: 75",
    "full data n: 5526",
    "n used: 4695",
    "",
    "",
    "Summary Table:",
    "       is3g12a    N  WTD_N PCT SE(PCT) MEAN SE(MEAN)", "          NONE 1424 621312  33    1.52  448      3.3",
    "           ONE 1748 688543  36    0.87  477      3.1", "           TWO  929 360600  19    0.84  499      3.2",
    " THREE OR MORE  594 234602  12    0.69  500      5.5"
  )

  e1 <- edsurveyTable(civ ~ is3g12a, data = mex8)
  withr::with_options(list(digits = 2), co <- capture.output(e1))
  expect_equal(co, e1REF)
})
