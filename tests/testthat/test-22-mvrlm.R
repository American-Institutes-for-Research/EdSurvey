skip_on_cran()
require(testthat)
context("Multivariate Regression")
require(EdSurvey)
options(width = 500)
options(useFancyQuotes = FALSE)

sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

context("Multivariate Regression: 2 DVs (2 non PV)")
test_that("mvrlm.sdf results align with lm.sdf", {
  mvrlm.fit <- mvrlm.sdf(mrps51 | mrps22 ~ dsex | m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  lm.fit.mrps51 <- lm.sdf(mrps51 ~ dsex + m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  lm.fit.mrps22 <- lm.sdf(mrps22 ~ dsex + m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  # compare coefficient tables
  expect_equal(mvrlm.fit$coefmat$mrps51, lm.fit.mrps51$coefmat)
  expect_equal(mvrlm.fit$coefmat$mrps22, lm.fit.mrps22$coefmat)

  # compare residuals
  expect_equal(as.vector(mvrlm.fit$residuals[, 1]), as.vector(lm.fit.mrps51$residuals))
  expect_equal(as.vector(mvrlm.fit$residuals[, 2]), as.vector(lm.fit.mrps22$residuals))

  # compare residual covariance
  mrps51 <- lm.fit.mrps51$residuals
  mrps22 <- lm.fit.mrps22$residuals
  ee <- cbind(mrps51, mrps22)
  lmResidCov <- t(ee) %*% ee
  expect_equivalent(mvrlm.fit$residCov, lmResidCov)
})

context("Multivariate Regression: 2 DVs (1 PV 1 non PV)")
test_that("mvrlm.sdf results align with lm.sdf", {
  mvrlm.fit <- mvrlm.sdf(composite | mrps22 ~ dsex | m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  lm.fit.comp <- lm.sdf(composite ~ dsex + m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE, verbose = FALSE)
  lm.fit.mrps22 <- lm.sdf(mrps22 ~ dsex + m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE, verbose = FALSE)

  # compare coefficient tables
  expect_equal(mvrlm.fit$coefmat$composite, lm.fit.comp$coefmat)
  expect_equal(mvrlm.fit$coefmat$mrps22, lm.fit.mrps22$coefmat)

  # compare residuals
  dimnames(lm.fit.comp$PV.residuals)[[2]] <- c("mrpcm1", "mrpcm2", "mrpcm3", "mrpcm4", "mrpcm5")
  mvrResid <- as.matrix(mvrlm.fit$residPV[[1]])
  lmResid <- lm.fit.comp$PV.residuals
  expect_equal(mvrResid, lmResid)
  mvrResid <- as.vector(mvrlm.fit$residPV[[2]][, 1])
  lmResid <- as.vector(lm.fit.mrps22$residuals)
  expect_equal(mvrResid, lmResid)

  # compare residual covariance
  composite <- rowMeans(lm.fit.comp$PV.residuals)
  mrps22 <- lm.fit.mrps22$residuals
  ee <- cbind(composite, mrps22)
  lmResidCov <- t(ee) %*% ee
  expect_equivalent(mvrlm.fit$residCov, lmResidCov)
})

context("Multivariate Regression: 2 DVs (both PVs)")
test_that("mvrlm.sdf results align with lm.sdf", {
  # compare coefficient tables
  mvrlm.fit <- mvrlm.sdf(algebra | geometry ~ dsex | m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  lm.fit.alg <- lm.sdf(algebra ~ dsex + m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  lm.fit.geom <- lm.sdf(geometry ~ dsex + m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  expect_equal(mvrlm.fit$coefmat$algebra, lm.fit.alg$coefmat)
  expect_equal(mvrlm.fit$coefmat$geometry, lm.fit.geom$coefmat)

  # compare residuals
  attr(lm.fit.alg$PV.residuals, "dimnames") <- NULL
  attr(mvrlm.fit$residPV[[1]], "dimnames") <- NULL
  expect_equal(mvrlm.fit$residPV[[1]], lm.fit.alg$PV.residuals)
  attr(lm.fit.geom$PV.residuals, "dimnames") <- NULL
  attr(mvrlm.fit$residPV[[2]], "dimnames") <- NULL
  expect_equal(mvrlm.fit$residPV[[2]], lm.fit.geom$PV.residuals)

  # compare residual covariance
  algebra <- rowMeans(lm.fit.alg$PV.residuals)
  geometry <- rowMeans(lm.fit.geom$PV.residuals)
  ee <- cbind(algebra, geometry)
  lmResidCov <- t(ee) %*% ee
  expect_equivalent(mvrlm.fit$residCov, lmResidCov)
})

context("Multivariate Regression: 3 DVs (all PVs)")
test_that("mvrlm.sdf results align with lm.sdf", {
  mvrlm.fit <- mvrlm.sdf(algebra | geometry | measurement ~ dsex | m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)

  ### compare coefficients
  lm.fit.alg <- lm.sdf(algebra ~ dsex + m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  lm.fit.geom <- lm.sdf(geometry ~ dsex + m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  lm.fit.meas <- lm.sdf(measurement ~ dsex + m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  expect_equal(mvrlm.fit$coefmat$algebra, lm.fit.alg$coefmat)
  expect_equal(mvrlm.fit$coefmat$geometry, lm.fit.geom$coefmat)
  expect_equal(mvrlm.fit$coefmat$measurement, lm.fit.meas$coefmat)

  # compare residuals
  attr(lm.fit.alg$PV.residuals, "dimnames") <- NULL
  attr(mvrlm.fit$residPV[[1]], "dimnames") <- NULL
  expect_equal(mvrlm.fit$residPV[[1]], lm.fit.alg$PV.residuals)
  attr(lm.fit.geom$PV.residuals, "dimnames") <- NULL
  attr(mvrlm.fit$residPV[[2]], "dimnames") <- NULL
  expect_equal(mvrlm.fit$residPV[[2]], lm.fit.geom$PV.residuals)
  attr(lm.fit.meas$PV.residuals, "dimnames") <- NULL
  attr(mvrlm.fit$residPV[[3]], "dimnames") <- NULL
  expect_equal(mvrlm.fit$residPV[[3]], lm.fit.meas$PV.residuals)

  # compare residual covariance
  algebra <- rowMeans(lm.fit.alg$PV.residuals)
  geometry <- rowMeans(lm.fit.geom$PV.residuals)
  measurement <- rowMeans(lm.fit.meas$PV.residuals)
  ee <- cbind(algebra, geometry, measurement)
  lmResidCov <- t(ee) %*% ee
  expect_equivalent(mvrlm.fit$residCov, lmResidCov)
})

# regression tests

context("mvrlm.sdf Regression tests")
test_that("mvrlm.sdf results remain the same", {
  skip_on_cran()

  source("REF-13-MVRLM.R")
  # no pv case
  mvrlm.fit <- mvrlm.sdf(mrps51 | mrps22 ~ dsex | m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  expect_equal(mvrlm.fit$coefmat, mvrlm_coef_ref1)
  expect_equal(mvrlm.fit$residPV, NULL)
  expect_equal(mvrlm.fit$residCov, mvrlm_resid_cov1)

  # mixed DV case
  mvrlm.fit <- mvrlm.sdf(composite | mrps22 ~ dsex | m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  expect_equal(mvrlm.fit$coefmat, mvrlm_coef_ref2)
  expect_equal(lapply(mvrlm.fit$residPV, head), mvrlm_residpv2)
  expect_equal(mvrlm.fit$residCov, mvrlm_resid_cov2)

  # 2 PV case
  mvrlm.fit <- mvrlm.sdf(algebra | geometry ~ dsex | m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  expect_equal(mvrlm.fit$coefmat, mvrlm_coef_ref3)
  expect_equal(lapply(mvrlm.fit$residPV, head), mvrlm_residpv3)
  expect_equal(mvrlm.fit$residCov, mvrlm_resid_cov3)
  # 3 PV case
  mvrlm.fit <- mvrlm.sdf(algebra | geometry | measurement ~ dsex | m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  expect_equal(mvrlm.fit$coefmat, mvrlm_coef_ref4)
  expect_equal(lapply(mvrlm.fit$residPV, head), mvrlm_residpv4)
  expect_equal(mvrlm.fit$residCov, mvrlm_resid_cov4)
})

context("Wald Test Coefficient restrictions")
test_that("wald test works", {
  options(digits = 2)
  mvr <- mvrlm.sdf(algebra | geometry ~ dsex | m072801, data = sdf, jrrIMax = 5, returnVarEstInputs = TRUE)
  hypothesis <- c("geometry_dsexFemale = 0", "algebra_dsexFemale = 0")

  test1 <- linearHypothesis(mvr, hypothesis = hypothesis, test = "F")
  test2 <- linearHypothesis(mvr, hypothesis = hypothesis, test = "Chisq")

  waldtestF <- c(
    "",
    "Linear hypothesis test:",
    "geometry_dsexFemale = 0",
    "algebra_dsexFemale = 0",
    "",
    "Model 1: restricted model",
    "Model 2: algebra | geometry ~ dsex | m072801",
    "",
    "  Res.Df Df    F Pr(>F)   ",
    "1   3307                  ",
    "2   3305  2 4.81 0.0082 **",
    "---",
    "Signif. codes:  ",
    "0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
  )
  waldtestChisq <- c(
    "",
    "Linear hypothesis test:",
    "geometry_dsexFemale = 0",
    "algebra_dsexFemale = 0",
    "",
    "Model 1: restricted model",
    "Model 2: algebra | geometry ~ dsex | m072801",
    "",
    "  Res.Df Df Chisq Pr(>Chisq)   ",
    "1   3307                       ",
    "2   3305  2  9.62     0.0081 **",
    "---", "Signif. codes:  ",
    "0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
  )
  # Regression tests for wald test output
  expect_equal(capture.output(test1)[1:12], waldtestF[1:12])
  expect_equal(capture.output(test2)[1:12], waldtestChisq[1:12])
})
