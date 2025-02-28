# This file tests that the light.edsurvey.data.frame works
# the same as the edsurvey.data.frame
require(testthat)

options(width = 500)
options(useFancyQuotes = FALSE)

test_that("read LESDF", {
  sdf <<- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
  lsdf <<- EdSurvey::getData(sdf, c(all.vars(composite ~ dsex + b017451), "origwt", "jkunit", "repgrp1"), addAttributes = TRUE)
  suppressWarnings(lsdf0 <<- EdSurvey::getData(sdf, colnames(sdf), addAttributes = TRUE, dropOmittedLevels = FALSE, defaultConditions = FALSE))
  expect_s3_class(lsdf, "light.edsurvey.data.frame")
})


test_that("LESDF cbind function", {
  sm1 <- EdSurvey::getData(sdf, c("composite", "dsex", "origwt"), dropUnusedLevels = FALSE, defaultConditions = FALSE, dropOmittedLevels = FALSE, addAttributes = TRUE)
  sm2 <- EdSurvey::getData(sdf, c("b017451"), dropUnusedLevels = FALSE, defaultConditions = FALSE, dropOmittedLevels = FALSE)
  sm3 <- cbind(b017451 = sm2$b017451, sm1)
  sm4 <- EdSurvey::getData(sdf, c("composite", "b017451", "dsex", "origwt"), dropUnusedLevels = FALSE, defaultConditions = FALSE, dropOmittedLevels = FALSE, addAttributes = TRUE)
  expect_equal(attributes(sm4)$names, attributes(sm3)$names) # test just attribute names
  expect_equal(sm4, sm3) # test everything

  # cbind a data.frame
  sm5 <- cbind(sm1, sm2) # in S3 the first class dominates, so must flop the columns here
  expect_equal(attributes(unname(sm5)), attributes(unname(sm3)))

  # cbind matrix
  expect_true(is.matrix(cbind(c(1, 2), c(1, 2))))

  # cbind vector with data.frame
  expect_equal(cbind(data.frame(V1 = c(1, 2)), c(3, 4)), base::cbind(data.frame(V1 = c(1, 2)), c(3, 4)))
})

