skip_on_cran()
require(testthat)
context("ECLS_K data reads in correctly")
require(EdSurvey)
options(width = 500)
options(useFancyQuotes = FALSE)


source("REF-6-ECLS_K.R") # has REF output in it

if (!dir.exists(edsurveyHome)) {
  dir.create(edsurveyHome)
}

# able to toggle 'forceReread' for recaching the data if necessary
if (!exists("forceCacheUpdate")) {
  forceCacheUpdate <- FALSE
}

test_that("ECLS_K data reads in correctly", {
  expect_silent(downloadECLS_K(years = c(1998, 2011), root = edsurveyHome, verbose = FALSE))
  expect_silent(eclsk11 <<- readECLS_K2011(file.path(edsurveyHome, "ECLS_K", "2011"), filename = "childK5p.dat", layoutFilename = "ECLSK2011_K5PUF.sps", verbose = FALSE, forceReread = forceCacheUpdate))

  expect_is(eclsk11, "edsurvey.data.frame")
  expect_equal(dim(eclsk11), c(18174, 26061)) # 18174 obs::26060 cols

  expect_silent(eclsk98 <<- readECLS_K1998(file.path(edsurveyHome, "ECLS_K", "1998"), filename = "eclsk_98_99_k8_child_v1_0.dat", layoutFilename = "Layout_k8_child.txt", verbose = FALSE, forceReread = forceCacheUpdate))
  expect_is(eclsk98, "edsurvey.data.frame")
  expect_equal(dim(eclsk98), c(21409, 18929)) # 21409 obs::18928 cols
})

context("ECLS_K getData")
test_that("ECLS_K getData", {
  dat1 <- getData(eclsk98, c("childid", "gender", "race"))
  expect_equal(dim(dat1), c(21357, 3))
  withr::with_options(list(digits = 7), co <- capture.output(summary(dat1)))
  expect_equal(co, dat1Summary.Ref)

  # test continuous variable having only omitted levels
  dat2 <- getData(eclsk98, c("childid", "c1height", "c2height", "c3height", "c4height", "c5height", "c6height", "c7height"), dropOmittedLevels = FALSE)
  expect_equal(dim(dat2), c(21409, 8))
  withr::with_options(list(digits = 7), co <- capture.output(summary(dat2)))
  expect_equal(co, dat2Summary.Ref)
})

context("ECLS_K rebindAttributes")
test_that("ECLS_K rebindAttributes", {
  mvData <- getData(
    data = eclsk11, varnames = c(
      "p9hmwork", "p9hlphwk", "x_chsex_r",
      "x9rscalk5", "x9mscalk5", "w9c29p_9t90"
    ),
    dropOmittedLevels = FALSE, addAttributes = TRUE
  )
  mvData$p9hlphwk <- ifelse(mvData$p9hmwork == "1: NEVER" &
    mvData$p9hlphwk == "-1: NOT APPLICABLE", 0,
  mvData$p9hlphwk
  )
  mvData <- rebindAttributes(mvData, eclsk11)

  withr::with_options(list(digits = 7, useFancyQuotes = FALSE), {
    res <- suppressWarnings(lm.sdf(
      formula = x9rscalk5 ~ x_chsex_r + p9hlphwk, data = mvData,
      weightVar = "w9c29p_9t90"
    )) # warnings expected here for dropping 0 weights

    co <- capture.output(summary(res))
  })

  expect_equal(co, ecls_rebind_lmRef)
})

context("ECLS_K rename.sdf")
test_that("ECLS_K rename.sdf", {
  eclsk11dat <- rename.sdf(eclsk11,
    oldnames = "a2exasis",
    newnames = "extraassistancefrequency"
  )
  expect_equal("extraassistancefrequency" %in% colnames(eclsk11dat), TRUE)
})

context("ECLS_K Taylor Series")
test_that("ECLS_K Taylor Series", {
  lmTaylor <- lm.sdf(x8mscalk5 ~ x12sesl,
    weightVar = "w8c28p_8t280", varMethod = "Taylor",
    data = eclsk11
  )
  withr::with_options(list(digits = 7), lmTaylorOutput <- capture.output(summary(lmTaylor)))
  expect_equal(lmTaylorOutput, lmTaylorREF)
  wt1 <- waldTest(lmTaylor, coeff = 2)
  # the Wald chi-square test should equal t-test
  expect_equal(sqrt(wt1$result$chi2[["chi2"]]), lmTaylor$coefmat$t[2])
})

context("ECLS_K Wald test")
test_that("ECLS_K Wald test", {
  suppressWarnings(glmTaylor <- logit.sdf(I(x8mscalk5 > 122) ~ x12sesl, weightVar = "w8c28p_8t280", data = eclsk11))
  summary(glmTaylor)
  wt2 <- waldTest(glmTaylor, coeff = 2)
  # the Wald chi-square test should equal t-test
  expect_equal(sqrt(wt2$result$chi2[["chi2"]]), glmTaylor$coefmat$t[2])
})

context("ECLS_K summary2")
test_that("ECLS_K summary2", {
  withr::with_options(list(digits = 7), ecls_sum <- capture.output(summary2(eclsk98, "c7r4mscl")))
  expect_equal(ecls_sum, ecls_sumREF)
  suppressWarnings(withr::with_options(list(digits = 7), ecls_sum_w <- capture.output(summary2(eclsk98, "c7r4mscl", weightVar = "c7cpts0"))))
  expect_equal(ecls_sum_w, ecls_sum_wREF)
})

context("ECLS_K suggestWeights")
test_that("ECLS_K suggestWeights", {
  expect_equal(suppressMessages(suggestWeights("x8mscalk5", eclsk11)), "w8c8p_20")
  expect_equal(
    suppressMessages(suggestWeights(c("x7mscalk5", "x8mscalk5", "x_chsex_r", "x_raceth_r"), eclsk11, TRUE)),
    c("w8c28p_8a0", "w8c28p_2t80", "w8c28p_2t8z0", "w8c28p_8b0", "w8c28p_2t280", "w8cf8p_80", "w8c28p_8t280", "w8c28p_8t28z0", "w8c18p_8t180", "w8cf8p_2t180")
  )
  expect_equal(suppressMessages(suggestWeights(c("x8mscalk5", "x_chsex_r", "x12sesl"), eclsk11)), "w8c8p_20")
})
