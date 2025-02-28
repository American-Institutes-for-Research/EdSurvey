require(testthat)

options(width = 500)
options(useFancyQuotes = FALSE)

test_that("read ESDFL", {
  sdf <<- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
  sdfA <<- subset(sdf, scrpsu %in% c(5, 45, 56))
  sdfB <- subset(sdf, scrpsu %in% c(75, 76, 78))
  sdfC <- subset(sdf, scrpsu %in% 100:200)
  sdfD <<- subset(sdf, scrpsu %in% 201:300)

  sdfB$year <- 2002
  sdfC$year <- 2003

  sdfB <<- sdfB
  sdfC <<- sdfC
  # construct an edsurvey.data.frame.list from these four data sets
  sdfl <<- edsurvey.data.frame.list(list(sdfA, sdfB, sdfC, sdfD),
    labels = c(
      "A locations",
      "B locations",
      "C locations",
      "D locations"
    )
  )
  expect_s3_class(sdfl, "edsurvey.data.frame.list")
})


# for subset test
i <- "invalid level: outside"

test_that("ESDFL subset and scope", {
  yes <- "Yes"
  g1 <- subset(sdfl, ell3 == "Yes")
  g2 <- subset(sdfl, ell3 == yes)
  expect_equal(dim(g1), dim(g2))
  i <- "invalid level: inside"
  ssfun <- function(data) {
    i <- "Yes"
    subset(data, ell3 == i)
  }
  g3 <- ssfun(sdfl)
  expect_equal(dim(g1), dim(g3))
})

test_that("ESDFL achievementLevels", {
  expect_snapshot_value(test8 <- achievementLevels(data = sdfl), style = "json2")
})


