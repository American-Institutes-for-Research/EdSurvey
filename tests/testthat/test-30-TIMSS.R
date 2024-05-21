skip_on_cran()
require(testthat)
context("TIMSS data reads in correctly")
require(EdSurvey)
options(width = 500)
options(useFancyQuotes = FALSE)
source("REF-3-TIMSS.R") # has REF output in it

# able to toggle 'forceReread' for recaching the data if necessary
if (!exists("forceCacheUpdate")) {
  forceCacheUpdate <- FALSE
}

if (!dir.exists(edsurveyHome)) {
  dir.create(edsurveyHome)
}

test_that("TIMSS data reads in correctly", {
  expect_silent(downloadTIMSS(root = edsurveyHome, year = c(2003, 2007, 2011, 2015, 2019), verbose = FALSE))
  expect_silent(sgp8.03 <<- readTIMSS(file.path(edsurveyHome, "TIMSS", "2003"), countries = "sgp", gradeLvl = 8, forceReread = forceCacheUpdate, verbose = FALSE))
  expect_silent(usa4.07 <<- readTIMSS(file.path(edsurveyHome, "TIMSS", "2007"), countries = "usa", gradeLvl = 4, forceReread = forceCacheUpdate, verbose = FALSE))
  expect_silent(usa8.11 <<- readTIMSS(file.path(edsurveyHome, "TIMSS", "2011"), countries = "usa", gradeLvl = 8, forceReread = forceCacheUpdate, verbose = FALSE))
  expect_silent(fin4.11 <<- readTIMSS(file.path(edsurveyHome, "TIMSS", "2011"), countries = "fin", gradeLvl = 4, forceReread = forceCacheUpdate, verbose = FALSE))
  expect_silent(kwt4.15 <<- readTIMSS(file.path(edsurveyHome, "TIMSS", "2015"), countries = "kwt", gradeLvl = 4, forceReread = forceCacheUpdate, verbose = FALSE)) # includes both numeracy and gr4
  expect_silent(zaf8.15 <<- readTIMSS(file.path(edsurveyHome, "TIMSS", "2015"), countries = "zaf", gradeLvl = 8, forceReread = forceCacheUpdate, verbose = FALSE))
  expect_silent(dnk4.19 <<- readTIMSS(file.path(edsurveyHome, "TIMSS", "2019"), countries = "dnk", gradeLvl = 4, forceReread = forceCacheUpdate, verbose = FALSE))

  expect_silent(multiESDFL <<- readTIMSS(
    path = c(
      file.path(edsurveyHome, "TIMSS", "2007"),
      file.path(edsurveyHome, "TIMSS", "2011")
    ),
    countries = c("usa", "fin"), gradeLvl = 8, verbose = FALSE
  )) # test multipath read

  expect_is(sgp8.03, "edsurvey.data.frame")
  expect_is(usa4.07, "edsurvey.data.frame")
  expect_is(usa8.11, "edsurvey.data.frame")
  expect_is(kwt4.15, "edsurvey.data.frame")
  expect_is(zaf8.15, "edsurvey.data.frame")
  expect_is(dnk4.19, "edsurvey.data.frame")
  expect_is(multiESDFL, "edsurvey.data.frame.list")
  expect_equal(dim(sgp8.03), c(12144, 2590))
  expect_equal(dim(usa4.07), c(12883, 1906))
  expect_equal(dim(usa8.11), c(20859, 2394))
  expect_equal(dim(kwt4.15), c(11318, 2262))
  expect_equal(dim(zaf8.15), c(25029, 2432))
  expect_equal(dim(dnk4.19), c(5131, 3674))
  expect_equal(length(multiESDFL$datalist), 3)

  co <- capture.output(multiESDFL$covs)
  co.ref <- c(
    "  year       country",
    "1 2007 United States",
    "2 2011 United States",
    "3 2011       Finland"
  )
  expect_equal(co, co.ref)
})

context("TIMSS esdfl")
test_that("TIMSS esdfl", {
  usa <- edsurvey.data.frame.list(list(usa4.07, usa8.11))
  expect_equal(usa$covs, structure(
    list(
      year = c("2007", "2011"),
      gradeLevel = c("Grade 4", "Grade 8")
    ),
    class = "data.frame",
    row.names = c(NA, -2L)
  ))
  usafin11 <- edsurvey.data.frame.list(list(usa8.11, fin4.11))
  usafinB <- append.edsurvey.data.frame.list(usafin11, usa4.07)
  expect_equal(usafinB$covs, structure(
    list(
      year = c("2011", "2011", "2007"),
      gradeLevel = c("Grade 8", "Grade 4", "Grade 4"),
      country = c("United States", "Finland", "United States")
    ),
    class = "data.frame",
    row.names = c(NA, -3L)
  ))
})

context("TIMSS $ assign")
test_that("TIMSS edsurveyTable", {
  kwt4.15$testVar1 <- ifelse(kwt4.15$idstud %% 2 == 0, 1, 0) # create a 'cache' var:: works good
  # veryify that the raw cache agrees with the EdSurvey::getData results
  tab0 <- table(kwt4.15$cache$testVar1)
  tab1 <- table(kwt4.15$testVar1)
  expect_equal(tab0, tab1)
})

context("TIMSS edsurveyTable")
test_that("TIMSS edsurveyTable", {
  estt <- edsurveyTable(srea ~ as4gsex, usa4.07, varMethod = "Taylor")
  withr::with_options(list(digits = 7), co <- capture.output(estt))
  co.ref <- c(
    "",
    "Formula: srea ~ as4gsex ",
    "",
    "Plausible values: 5",
    "Weight variable: 'totwgt'",
    "Variance method: Taylor series",
    "full data n: 12883",
    "n used: 7886",
    "",
    "",
    "Summary Table:",
    " as4gsex    N   WTD_N     PCT   SE(PCT)     MEAN SE(MEAN)",
    "    GIRL 4020 1712811 50.9237 0.5773892 536.5267 3.226353",
    "     BOY 3866 1650674 49.0763 0.5773892 534.0332 3.377425"
  )
  expect_equal(co, co.ref)

  withr::with_options(list(digits = 7), co <- capture.output(edsurveyTable(srea ~ bsbg01, zaf8.15)))
  co.ref <- c(
    "",
    "Formula: srea ~ bsbg01 ",
    "",
    "Plausible values: 5",
    "jrrIMax: 1",
    "Weight variable: 'totwgt'",
    "Variance method: jackknife",
    "JK replicates: 150",
    "full data n: 25029",
    "n used: 12506",
    "",
    "",
    "Summary Table:",
    " bsbg01    N    WTD_N      PCT  SE(PCT)     MEAN SE(MEAN)",
    "   GIRL 6422 445654.5 51.30222 1.136601 354.4961 6.907083",
    "    BOY 6084 423030.1 48.69778 1.136601 346.2686 5.690930"
  )
  expect_equal(co, co.ref)
  et1 <- edsurveyTable(formula = ssci ~ itlang, data = multiESDFL)
  expect_is(et1, "edsurveyTableList")
  expect_error(factor(et1$data$itlang), NA)
})

context("TIMSS lm.sdf")
test_that("TIMSS lm.sdf", {
  withr::with_options(list(digits = 7, scipen = 0), {
    lm1t <- lm.sdf(mmat ~ asbg01 + asbgssb, kwt4.15, varMethod = "Taylor", returnNumberOfPSU = TRUE)
    lm1t$nPSU <- NULL
    co1 <- capture.output(lm1t)

    lm1j <- lm.sdf(mmat ~ asbg01 + asbgssb, kwt4.15, varMethod = "jackknife", returnNumberOfPSU = TRUE)
    lm2t <- lm.sdf(itsex ~ asbgssb, kwt4.15, varMethod = "Taylor", returnNumberOfPSU = TRUE)
    lm2j <- lm.sdf(itsex ~ asbgssb, kwt4.15, varMethod = "jackknife", returnNumberOfPSU = TRUE)
    lm1jk <- lm.sdf(ssci ~ bsbg01 + bsbg12c, usa8.11, varMethod = "jackknife")

    co2 <- capture.output(lm1jk)
    co3 <- capture.output(summary(lm1t))
    co4 <- capture.output(summary(lm1jk))
  })

  co.ref <- c(
    "(Intercept)   asbg01BOY     asbgssb ",
    "364.9699220 -12.4878760  -0.5142928 "
  )
  expect_equal(co1, co.ref)
  expect_is(EdSurvey::waldTest(lm1t, "asbgssb"), "edsurveyWaldTest")
  expect_equal(unname(sqrt(diag(vcov(lm1t)))), lm1t$coefmat$se)



  expect_equal(unname(sqrt(diag(vcov(lm1j)))), lm1j$coefmat$se)
  expect_is(EdSurvey::waldTest(lm1j, "asbgssb"), "edsurveyWaldTest")


  expect_equal(unname(sqrt(diag(vcov(lm2t)))), lm2t$coefmat$se)
  expect_is(EdSurvey::waldTest(lm2t, "asbgssb"), "edsurveyWaldTest")


  expect_equal(unname(sqrt(diag(vcov(lm2j)))), lm2j$coefmat$se)
  expect_is(EdSurvey::waldTest(lm2j, "asbgssb"), "edsurveyWaldTest")


  co.ref1 <- c(
    "             (Intercept)                bsbg01BOY    bsbg12cAGREE A LITTLE bsbg12cDISAGREE A LITTLE    bsbg12cDISAGREE A LOT ",
    "              530.622228                11.701545                -8.917658               -20.192490               -40.085668 "
  )
  expect_equal(co2, co.ref1)


  co.ref2 <- c(
    "",
    "Formula: mmat ~ asbg01 + asbgssb",
    "",
    "Weight variable: 'totwgt'",
    "Variance method: Taylor series",
    "Plausible values: 5",
    "jrrIMax: 5",
    "full data n: 11318",
    "n used: 6704",
    "",
    "Coefficients:",
    "                 coef        se        t    dof Pr(>|t|)    ",
    "(Intercept) 364.96992  15.20664 24.00069 24.170  < 2e-16 ***",
    "asbg01BOY   -12.48788   6.00111 -2.08093 17.596  0.05234 .  ",
    "asbgssb      -0.51429   1.18887 -0.43259 38.286  0.66774    ",
    "---",
    "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
    "",
    "Multiple R-squared: 0.0036",
    ""
  )
  expect_equal(co3, co.ref2)

  co.ref3 <- c(
    "",
    "Formula: ssci ~ bsbg01 + bsbg12c",
    "",
    "Weight variable: 'totwgt'",
    "Variance method: jackknife",
    "JK replicates: 150",
    "Plausible values: 5",
    "jrrIMax: 1",
    "full data n: 20859",
    "n used: 10319",
    "",
    "Coefficients:",
    "                             coef       se        t     dof  Pr(>|t|)    ",
    "(Intercept)              530.6222   3.1016 171.0782 128.797 < 2.2e-16 ***",
    "bsbg01BOY                 11.7015   2.2822   5.1273 118.079 1.163e-06 ***",
    "bsbg12cAGREE A LITTLE     -8.9177   2.5260  -3.5303 143.706 0.0005581 ***",
    "bsbg12cDISAGREE A LITTLE -20.1925   3.4035  -5.9329  87.353 5.852e-08 ***",
    "bsbg12cDISAGREE A LOT    -40.0857   4.3027  -9.3164  89.639 7.815e-15 ***",
    "---",
    "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
    "",
    "Multiple R-squared: 0.0302",
    ""
  )
  expect_equal(co4, co.ref3)
})

context("TIMSS showPlausibleValues and showWeights verbose output agrees")
test_that("TIMSS showPlausibleValues and showWeights verbose output agrees", {
  co <- capture.output(showPlausibleValues(usa4.07, verbose = TRUE))
  expect_equal(co, spv)

  co <- capture.output(showWeights(usa4.07, verbose = TRUE))
  expect_equal(co, swREF)
})

context("TIMSS getData")
test_that("TIMSS getData", {
  expect_known_value(head(EdSurvey::getData(fin4.11, c("asbg01", "mmat"))), file = "TIMSSgd1.rds", update = FALSE)
  expect_known_value(head(EdSurvey::getData(usa4.07, c("as4gth03", "idschool", "ssci", "totwgt"), defaultConditions = FALSE)[,1:10]), file = "TIMSSgd2.rds", update = FALSE)

  # test a getData call with teacher level variable ensure warning is returned
  tchWrn <- capture_warnings(gd3 <- EdSurvey::getData(kwt4.15, c("mgeo", "idstud", "atbg10f"), dropUnusedLevels = FALSE))
  expect_known_value(head(gd3), file = "TIMSSgd3.rds", update = FALSE, check.attributes = FALSE)

  kwt4_males <- EdSurvey:::subset(kwt4.15, asbg01 %in% "BOY", verbose = FALSE)
  expect_equal(dim(kwt4_males), c(5252, 2262))
})


context("TIMSS showCutPoints")
test_that("TIMSS showCutPoints", {
  sw <- c(
    "Achievement Levels:",
    "  Low International Benchmark:  400",
    "  Intermediate International Benchmark:  475",
    "  High International Benchmark:  550",
    "  Advanced International Benchmark:  625"
  )
  co <- capture.output(showCutPoints(usa4.07))
  expect_equal(sw, co)
})
context("TIMSS userConditions")
test_that("TIMSS userConditions", {
  usa4.07.cleaned <- rename.sdf(usa4.07, "as4golan", "asbg03")
  usa4.07.cleaned <- recode.sdf(usa4.07.cleaned, recode = list(
    asbg03 = list(
      from = c("ALWAYS", "ALMOST ALWAYS"),
      to = "FREQUENTLY"
    ),
    asbg03 = list(
      from = c("SOMETIMES", "NEVER"),
      to = "INFREQUENTLY"
    )
  ))
  # the rename variable has NA but it shouldn't affect getData
  expect_equal(dim(usa4.07), dim(usa4.07.cleaned))
  expect_equal(EdSurvey::getData(usa4.07, "itsex"), EdSurvey::getData(usa4.07.cleaned, "itsex"))

  expect_equal(gap("mmat", usa4.07.cleaned)$results, gap("mmat", usa4.07)$results)
})
context("TIMSS percentile")
test_that("TIMSS percentile", {
  withr::with_options(
    list(digits = 5, scipen = 0),
    co <- suppressWarnings(capture.output(percentile("mmat", c(0, 1, 25, 50, 75, 99, 100), usa4.07, pctMethod = "unbiased")))
  )
  expect_equal(co, pctREF)
})

context("TIMSS glm")
test_that("TIMSS glm", {
  # varEstInputs
  usa4recode <- EdSurvey::getData(usa4.07, varnames = c("as4srbsc", "itsex", "at4gsex", "tchwgt"), addAttributes = TRUE, returnJKreplicates = TRUE)
  usa4recode$readSciOften <- ifelse(usa4recode$as4srbsc %in% c("AT LEAST ONCE A WEEK", "ONCE OR TWICE A MONTH"), 1, 0)
  logit1 <- logit.sdf(readSciOften ~ itsex + at4gsex, data = usa4recode, weight = "tchwgt")

  withr::with_options(list(digits = 7, scipen = 0), co <- capture.output(summary(logit1)))
  expect_equal(co, logit1REF)

  probit1 <- probit.sdf(readSciOften ~ itsex + at4gsex, data = usa4recode, weight = "tchwgt")

  withr::with_options(list(digits = 7, scipen = 0), co <- capture.output(summary(probit1)))
  expect_equal(co, probit1REF)
})

context("TIMSS student-teacher merge on recode")
test_that("TIMSS student-teacher merge on recode", {
  ## Adapted from a bug identified by Anders Astrup Christensen
  ## The bug is that a recode of a teacher level variable triggered
  ## reading the teacher data, which duplicates many student records
  # Recode teacher level variable (variable not included in example analysis)

  withr::with_options(list(digits = 7, scipen = 0), {
    kwt4.15B <- recode.sdf(kwt4.15,
      recode = list(atbm07a = list(
        from = "NO MATHEMATICS HOMEWORK",
        to = "NO MATH HOMEWORK"
      ))
    )
    # Run same analysis again, this time with different results and different N
    es1 <- edsurveyTable(mmat ~ itsex, kwt4.15, weightVar = "totwgt", jrrIMax = 1)
    es2 <- edsurveyTable(mmat ~ itsex, kwt4.15B, weightVar = "totwgt", jrrIMax = 1)
  })

  expect_equal(es1, es2)
})

context("TIMSS gap")
test_that("TIMSS gap", {
  withr::with_options(list(digits = 7, scipen = 0), {
    # varEstInputs
    g3d <- gap("mmat", data=fin4.11, groupA= asbg01 == "BOY", returnVarEstInputs = TRUE, achievementLevel = c("Intermediate International Benchmark"), achievementDiscrete = TRUE)
    # gap percentile
    g2p <- gap("mmat", data=fin4.11, groupA= asbg01 == "BOY", groupB=asbg01 == "GIRL", percentile = c(50, 90), pctMethod = "symmetric")
    # gap achievement levels, discrete
    g1al <- gap("mmat", data=fin4.11, groupA= asbg01 == "BOY", groupB=asbg01 == "GIRL", achievementLevel = "Low International Benchmark", achievementDiscrete = TRUE)
    # gap percentage with recode
    g1eq <- gap("asbg07d", data=fin4.11, groupA= asbg01 == "BOY", groupB= asbg01 == "GIRL", targetLevel = "ONCE OR TWICE A WEEK")

    co1 <- capture.output(print(g3d))
    co2 <- capture.output(print(g2p))
    co3 <- capture.output(print(g1al))
    co4 <- capture.output(print(g1eq))
  })

  expect_equal(co1, g3dREF)
  expect_equal(co2, g2pREF)
  expect_equal(co3, g1alREF)
  expect_equal(co4, g1eqREF)
})


context("TIMSS gap dynamic subsets")
test_that("TIMSS gap dynamic subsets", {
  skip_on_cran()

  # put this in .Global so gap finds has it in the search path
  assign(x = "levelLabels", value = c("GIRL", "BOY"), envir = globalenv())
  gapResult <- gap(
    variable = "mmat", data = multiESDFL,
    groupA = itsex %in% "GIRL"
  )
  gapResult2 <- gap(
    variable = "mmat", data = multiESDFL,
    groupA = itsex %in% levelLabels[1]
  )

  gapResult2$labels <- gapResult$labels
  gapResult2$call <- gapResult$call
  expect_equal(gapResult, gapResult2)

  gapResult <- gap(
    variable = "mmat", data = usa8.11,
    groupA = itsex %in% "GIRL"
  )

  gapResult2 <- gap(
    variable = "mmat", data = usa8.11,
    groupA = itsex %in% levelLabels[1]
  )
  gapResult2$labels <- gapResult$labels
  gapResult2$call <- gapResult$call
  expect_equal(gapResult, gapResult2)

  # does not work in testthat
  gd2 <- EdSurvey::getData(usa4.07, c("totwgt", "itsex", "mmat"), defaultConditions = FALSE, addAttributes = TRUE)
  gapResult <- gap(
    variable = "mmat", data = gd2,
    groupA = itsex %in% "GIRL"
  )

  gapResult2 <- gap(
    variable = "mmat", data = gd2,
    groupA = itsex %in% levelLabels[1]
  )

  gapResult2$labels <- gapResult$labels
  gapResult2$call <- gapResult$call
  expect_equal(gapResult, gapResult2)
  # now remove levelLabels
  rm("levelLabels", envir = globalenv())
})

context("mml.sdf")
test_that("mml.sdf", {
  # load data
  usa4.15 <- readTIMSS(file.path(edsurveyHome, "TIMSS", "2015"), countries = "usa", grade = c("4"), verbose = FALSE)

  pv <- c("mmat")
  wgt <- "totwgt"
  pvi <- getAllItems(usa4.15, pv)
  psustr <- c(getPSUVar(usa4.15, wgt), getStratumVar(usa4.15, wgt))
  otherVars <- c("ROWID", "asbg05a")

  withr::with_options(list(digits = 4, scipen = 0), {
    lesdf <- suppressWarnings(EdSurvey::getData(usa4.15, varnames = c(pv, pvi, psustr, wgt, otherVars), dropOmittedLevels = FALSE, addAttributes = TRUE, returnJKreplicates = TRUE))
    # run mml
    mml1 <- suppressWarnings(mml.sdf(mmat ~ 1, usa4.15, weightVar = "totwgt", verbose = TRUE))
    mml2 <- suppressWarnings(mml.sdf(mmat ~ 1, lesdf, weightVar = "totwgt", verbose = TRUE)) # test with light.edsurvey.data.frame

    mml3 <- suppressWarnings(mml.sdf(mmat ~ asbg05a, usa4.15, weightVar = "totwgt", dropOmittedLevels = FALSE, verbose = TRUE)) # 86 rows removed from analysis message
    mml4 <- suppressWarnings(mml.sdf(mmat ~ asbg05a, lesdf, weightVar = "totwgt", dropOmittedLevels = FALSE, verbose = TRUE))

    # intercept
    coInt1 <- capture.output(mml1)
    coInt2 <- capture.output(mml2)
    coInt3 <- capture.output(mml3)
    coInt4 <- capture.output(mml4)

    coSum1 <- capture.output(summary(mml1))
    coSum2 <- capture.output(summary(mml2))
  })

  expect_equal(coInt1, mmlIntREF)
  expect_equal(coInt1, coInt2)
  expect_equal(coInt3, coInt4)

  # remove 'data = xxx' and 'object = xxx' from summary output to test for equality between esdf and light esdf
  dataRegex <- "data = (\\w|[.])+"
  objRegex <- "[(]object = \\w+[)]"
  coSum1x <- sub(dataRegex, "", coSum1, ignore.case = FALSE)
  coSum1x <- sub(objRegex, "", coSum1x, ignore.case = FALSE)
  coSum2x <- sub(dataRegex, "", coSum2, ignore.case = FALSE)
  coSum2x <- sub(objRegex, "", coSum2x, ignore.case = FALSE)

  # don't compare the 'iterations = ##' value, remove that from the comparison
  expect_equal(dropIterations(coSum1), dropIterations(mmlSumREF))
  expect_equal(coSum1x, coSum2x)
})
