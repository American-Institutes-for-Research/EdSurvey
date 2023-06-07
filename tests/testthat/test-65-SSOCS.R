skip_on_cran()
require(testthat)
context("SSOCS data reads in correctly")
require(EdSurvey)
options(width = 500)
options(useFancyQuotes = FALSE)


source("REF-12-SSOCS.R") # has REF output in it
if (!exists("edsurveyHome")) {
  if (Sys.info()[["sysname"]] == "Windows") {
    edsurveyHome <- "C:/EdSurveyData/"
  } else {
    edsurveyHome <- "~/EdSurveyData/"
  }
}

if (!dir.exists(edsurveyHome)) {
  dir.create(edsurveyHome)
}

# able to toggle 'forceReread' for recaching the data if necessary
if (!exists("forceCacheUpdate")) {
  forceCacheUpdate <- FALSE
}

test_that("SSOCS data reads in correctly", {
  # expect_silent(downloadSSOCS(root = edsurveyHome, years = c(2000, 2004, 2006, 2008, 2010, 2016, 2018), cache = FALSE, verbose = FALSE))
  expect_silent(ssocs16 <<- readSSOCS(
    sasDataFiles = file.path(edsurveyHome, "SSOCS", "2016", "pu_ssocs16.sas7bdat"),
    years = 2016, verbose = FALSE, forceReread = forceCacheUpdate
  ))
  expect_equal(dim(ssocs16), c(2092, 477))
  expect_silent(ssocs18 <<- readSSOCS(
    sasDataFiles = file.path(edsurveyHome, "SSOCS", "2018", "pu_ssocs18.sas7bdat"),
    years = 2016, verbose = FALSE, forceReread = forceCacheUpdate
  ))
  expect_equal(dim(ssocs18), c(2762, 425))
})

context("SSOCS getData")
test_that("SSOCS getData", {
  dat1 <- getData(ssocs16, c("c0014_r", "c0526", "c0528"))
  expect_equal(dim(dat1), c(2069, 3))
  withr::with_options(list(digits = 7), co <- capture.output(summary(dat1)))
  expect_equal(co, ssocs_ref1)

  dat2 <- getData(ssocs18, c("c0134", "c0198", "c0534"))
  expect_equal(dim(dat2), c(2762, 3))
  withr::with_options(list(digits = 7), co <- capture.output(summary(dat2)))
})

context("SSOCS edsurveyTable")
test_that("SSOCS edsurveyTable", {
  # 2016 tests
  withr::with_options(list(digits = 7), co <- capture.output(edsurveyTable(
    formula = vioinc16 ~ c0600,
    data = ssocs16,
    varMethod = "jackknife",
    weightVar = "finalwgt"
  )))
  expect_equal(co, ssocs_refTbl1)

  withr::with_options(list(digits = 7), co <- capture.output(edsurveyTable(
    formula = ~ c0610 + c0134,
    data = ssocs16,
    varMethod = "jackknife",
    weightVar = "finalwgt"
  )))
  expect_equal(co, ssocs_refTbl2)

  # 2018 test
  withr::with_options(list(digits = 7), co <- capture.output(edsurveyTable(
    formula = incid18 ~ c0669 + c0560,
    data = ssocs18,
    varMethod = "jackknife",
    weightVar = "finalwgt"
  )))
  expect_equal(co, ssocs_refTbl3)
})

context("SSOCS correlation")
test_that("SSOCS correlation", {
  withr::with_options(list(digits = 7), co <- capture.output(cor.sdf(
    x = "viopol18", y = "vioinc18",
    data = ssocs18,
    method = "Pearson",
    weightVar = "finalwgt"
  )))
  expect_equal(co, ssocs_refCorr1)
})

context("SSOCS lm and logit")
test_that("SSOCS correlation", {
  withr::with_options(list(digits = 7), co <- capture.output(lm.sdf(
    formula = sec_ft16 ~ c0532 + vioinc16,
    data = ssocs16,
    weightVar = "finalwgt"
  )))
  expect_equal(co, ssocs_refLM1)

  withr::with_options(list(digits = 7), l1 <- logit.sdf(I(c0690_r == "Yes") ~ c0532,
    data = ssocs16,
    varMethod = "jackknife",
    weightVar = "finalwgt",
    omittedLevels = FALSE
  ))

  withr::with_options(list(digits = 7), co <- capture.output(l1))

  expect_equal(co, ssocs_refLogit1)
  w1 <- waldTest(model = l1, coefficients = 2)
  withr::with_options(list(digits = 2), co <- capture.output(w1))
  expect_equal(co, ssocs_refWald)
})
