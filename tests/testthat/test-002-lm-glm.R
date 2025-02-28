options(width = 500)
options(useFancyQuotes = FALSE)
options(digits = 7)

sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

head2 <- function(obj, ...) {
  if(inherits(obj, "formula")) {
    obj <- as.character(obj)
  }
  if(inherits(obj, "call")) {
    obj <- substr(as.character(obj),0,100)
  }
  head(obj, ...)
}

test_that("lm.sdf returns", {
  expect_s3_class(
    sdf_taylor <- lm.sdf(composite ~ sdracem + dsex + pared,
      data = subset(sdf, pared == 1 | pared == 2, verbose = FALSE),
      weightVar = "origwt",
      varMethod = "Taylor",
      jrrIMax = Inf, verbose = FALSE
    ),
    "edsurveyLm"
  )
  expect_s3_class(
    sdf_taylor <- lm.sdf(composite ~ sdracem + dsex + pared,
      data = subset(sdf, pared == 1 | pared == 2, verbose = FALSE),
      weightVar = "origwt",
      varMethod = "Taylor",
      jrrIMax = Inf, verbose = FALSE
    ),
    "edsurveyLm"
  )
  sdf_taylorWV <- lm.sdf(composite ~ sdracem + dsex + pared,
    data = subset(sdf, pared == 1 | pared == 2, verbose = FALSE),
    weightVar = origwt,
    varMethod = "Taylor",
    jrrIMax = Inf, verbose = FALSE
  )
  sdf_taylorWV$call <- sdf_taylor$call <- NULL
  expect_equal(sdf_taylor, sdf_taylorWV)
  lm1t <- lm.sdf(composite ~ dsex + b017451, sdf, varMethod = "Taylor", verbose = FALSE)
  lm1t$data <- NULL
  expect_snapshot_value(lapply(lm1t, head2), style="json2", tol=1e-5, ignore_attr = TRUE)
  lm1jk <- lm.sdf(composite ~ dsex + b017451, sdf, varMethod = "jackknife", verbose = FALSE)
  expect_equal(coef(lm1t), coef(lm1jk))
  lm1jk$data <- NULL
  lm1jk <- summary(lm1jk, src = TRUE)
  lm1 <- lm.sdf(~ dsex + b017451, sdf)
  lm1$data <- NULL
  lm1$call <- NULL
  lm1jk.ref <- lm1
  lm1jk.ref$formula <- NULL
  lm1jk$call <- NULL
  lm1jk$formula <- NULL
  lm1jk$coefmat$stdCoef <- NULL
  lm1jk$coefmat$stdSE <- NULL
  expect_equal(lapply(lm1jk, head2), lapply(lm1jk.ref, head))
})

