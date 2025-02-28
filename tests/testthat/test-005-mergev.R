require(EdSurvey)
require(testthat)
skip_if_not_installed("data.table")
skip_if_not_installed("tibble")
require(data.table)
require(tibble)

test_that("mergev agrees with merge, base", {
  dfa0 <<- data.frame(
    key = letters[1:10],
    key2 = LETTERS[1:10],
    cola = paste("a", letters[1:10], sep = ":")
  )
  lt <- c("a", "b", "b", "c", "c", "d", "d", "e", "f", "f")
  lt2 <- c("A", "B", "Z", "C", "Z", "A", "D", "L", "F", "Z")
  dfb0 <<- data.frame(
    key = lt,
    key2 = lt2,
    colb = paste("b", lt, lt2, sep = ":")
  )
  lt <- rep(letters[10], 10)
  lt2 <- rep(c(LETTERS[10], "A"), each = 5)
  dfq0 <<- data.frame(
    key = lt,
    key2 = lt2,
    colq = paste("q", lt, lt2, sep = ":")
  )
  dfa <- dfa0
  dfb <- dfb0
  dfq <- dfq0
  dfl <- list(dfa, dfb, dfq)
  byl <- list("key", 0, c("key", "key2"), c(0, 1, 2), c(1, 0, 2), c(1, 2, 0), "key2")

  dfi1 <- 1
  dfi2 <- 2
  while (!(dfi1 == (length(dfl) - 1) & dfi2 == length(dfl))) {
    dfi2 <- dfi2 + 2
    if (dfi2 > length(dfl)) {
      dfi1 <- dfi1 + 1
      dfi2 <- dfi1 + 1
    }
    df1 <- dfl[[dfi1]]
    df2 <- dfl[[dfi2]]
    for (byi in 1:length(byl)) {
      thisby <- byl[[byi]]
      mg <- merge(df1, df2, by = thisby, all = TRUE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all = TRUE, verbose = FALSE))
      mgv$merge.type <- NULL
      expect_equal(mg, mgv)

      mg <- merge(df1, df2, by = thisby, all = FALSE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all = FALSE, verbose = FALSE))
      mgv$merge.type <- NULL
      expect_equal(mg, mgv)

      mg <- merge(df1, df2, by = thisby, all.x = FALSE, all.y = TRUE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all.x = FALSE, all.y = TRUE, verbose = FALSE))
      mgv$merge.type <- NULL
      expect_equal(mg, mgv)

      mg <- merge(df1, df2, by = thisby, all.x = TRUE, all.y = FALSE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all.x = TRUE, all.y = FALSE, verbose = FALSE))
      mgv$merge.type <- NULL
      expect_equal(mg, mgv)
    }
  }
  m1co <<- c(
    "Variables in both  dfa  and  dfb :",
    "[1] \"key2\"",
    "Merge type is  one:many ",
    "",
    " x only  y only matched ",
    "      4       0      10 ",
    "                dfa dfb total",
    "Rows in          10  10    20",
    "Unique keys in   10   6    10",
    "Rows out         14  10    14",
    "Unique keys out  10   6    10"
  )

  withr::with_options(
    list(width=500),
    co <- capture.output(mgv <- mergev(dfa, dfb, by = "key", all = TRUE))
  )
  expect_equal(co, m1co)
  dfbu <- dfb[!duplicated(dfb$key), ]

  m2co <<- c(
    "Variables in both  dfa  and  dfbu :",
    "[1] \"key2\"",
    "Merge type is  one:one ",
    "",
    " x only  y only matched ",
    "      4       0       6 ",
    "                dfa dfbu total",
    "Rows in          10    6    16",
    "Unique keys in   10    6    10",
    "Rows out         10    6    10",
    "Unique keys out  10    6    10"
  )
  withr::with_options(
    list(width=500),
    co <- capture.output(mgv <- mergev(dfa, dfbu, by = "key", all = TRUE))
  )
  expect_equal(co, m2co)
  m3co <<- c(
    "Variables in both  dfas  and  dfb :",
    "[1] \"key2\"",
    "Merge type is  one:many ",
    "",
    " x only  y only matched ",
    "      3       2       8 ",
    "                dfas dfb total",
    "Rows in            8  10    18",
    "Unique keys in     8   6     9",
    "Rows out          11  10    13",
    "Unique keys out    8   6     9"
  )
  dfas <- dfa[c(1, 3:7, 9:10), ]
  withr::with_options(
    list(width=500),
    co <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all = TRUE))
  )
  expect_equal(co, m3co)

  m4co <<- c(
    "Variables in both  dfas  and  dfb :",
    "[1] \"key2\"",
    "Merge type is  one:many ",
    "",
    " x only  y only matched ",
    "      0       0       8 ",
    "                dfas dfb total",
    "Rows in            8  10    18",
    "Unique keys in     8   6     9",
    "Rows out           8   8     8",
    "Unique keys out    5   5     5"
  )
  withr::with_options(
    list(width=500),
    co <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all = FALSE))
  )
  expect_equal(co, m4co)

  m5co <<- c(
    "Variables in both  dfas  and  dfb :",
    "[1] \"key2\"",
    "Merge type is  one:many ",
    "",
    " x only  y only matched ",
    "      0       2       8 ",
    "                dfas dfb total",
    "Rows in            8  10    18",
    "Unique keys in     8   6     9",
    "Rows out           8  10    10",
    "Unique keys out    5   6     6"
  )
  withr::with_options(
    list(width=500),
    co <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all.x = FALSE, all.y = TRUE))
  )
  expect_equal(co, m5co)

  m6co <<- c(
    "Variables in both  dfas  and  dfb :",
    "[1] \"key2\"",
    "Merge type is  one:many ",
    "",
    " x only  y only matched ",
    "      3       0       8 ",
    "                dfas dfb total",
    "Rows in            8  10    18",
    "Unique keys in     8   6     9",
    "Rows out          11   8    11",
    "Unique keys out    8   5     8"
  )
  withr::with_options(
    list(width=500),
    co <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all.x = TRUE, all.y = FALSE))
  )
  expect_equal(co, m6co)
})

test_that("mergev agrees with merge, data.table", {
  dfa <- data.table(dfa0)
  dfb <- data.table(dfb0)
  dfq <- data.table(dfq0)

  dfl <- list(dfa, dfb, dfq)
  byl <- list("key", c("key", "key2"), "key2")
  # data.table is a bit more restrictive than data.frame, not all of the byl work
  # byl <- list("key", 0, c("key", "key2"), c(0, 1, 2), c(1, 0, 2), c(1, 2, 0), "key2")
  dfi1 <- 1
  dfi2 <- 2
  while (!(dfi1 == (length(dfl) - 1) & dfi2 == length(dfl))) {
    dfi2 <- dfi2 + 2
    if (dfi2 > length(dfl)) {
      dfi1 <- dfi1 + 1
      dfi2 <- dfi1 + 1
    }
    df1 <- dfl[[dfi1]]
    df2 <- dfl[[dfi2]]
    for (byi in 1:length(byl)) {
      thisby <- byl[[byi]]
      mg <- merge(df1, df2, by = thisby, all = TRUE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all = TRUE, verbose = FALSE))
      suppressWarnings(mgv$merge.type <- NULL)
      expect_equal(mg, mgv)

      mg <- merge(df1, df2, by = thisby, all = FALSE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all = FALSE, verbose = FALSE))
      suppressWarnings(mgv$merge.type <- NULL)
      expect_equal(mg, mgv)

      mg <- merge(df1, df2, by = thisby, all.x = FALSE, all.y = TRUE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all.x = FALSE, all.y = TRUE, verbose = FALSE))
      suppressWarnings(mgv$merge.type <- NULL)
      expect_equal(mg, mgv)

      mg <- merge(df1, df2, by = thisby, all.x = TRUE, all.y = FALSE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all.x = TRUE, all.y = FALSE, verbose = FALSE))
      suppressWarnings(mgv$merge.type <- NULL)
      expect_equal(mg, mgv)
    }
  }
  withr::with_options(
    list(width=500),
    co1 <- capture.output(mgv <- mergev(dfa, dfb, by = "key", all = TRUE))
  )
  expect_equal(co1, m1co)
  dfbu <- dfb[!duplicated(dfb$key), ]
  withr::with_options(
    list(width=500),
    co2 <- capture.output(mgv <- mergev(dfa, dfbu, by = "key", all = TRUE))
  )
  expect_equal(co2, m2co)
  dfas <- dfa[c(1, 3:7, 9:10), ]
  withr::with_options(
    list(width=500),
    co3 <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all = TRUE))
  )
  expect_equal(co3, m3co)
  withr::with_options(
    list(width=500),
    co4 <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all = FALSE))
  )
  expect_equal(co4, m4co)
  withr::with_options(
    list(width=500),
    co5 <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all.x = FALSE, all.y = TRUE))
  )
  expect_equal(co5, m5co)
  withr::with_options(
    list(width=500),
    co6 <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all.x = TRUE, all.y = FALSE))
  )
  expect_equal(co6, m6co)
})

test_that("mergev agrees with merge, tibble", {
  dfa <- as_tibble(dfa0)
  dfb <- as_tibble(dfb0)
  dfq <- as_tibble(dfq0)
  dfl <- list(dfa, dfb, dfq)
  byl <- list("key", c("key", "key2"), "key2")
  dfi1 <- 1
  dfi2 <- 2
  while (!(dfi1 == (length(dfl) - 1) & dfi2 == length(dfl))) {
    dfi2 <- dfi2 + 2
    if (dfi2 > length(dfl)) {
      dfi1 <- dfi1 + 1
      dfi2 <- dfi1 + 1
    }
    df1 <- dfl[[dfi1]]
    df2 <- dfl[[dfi2]]
    for (byi in 1:length(byl)) {
      thisby <- byl[[byi]]
      mg <- merge(df1, df2, by = thisby, all = TRUE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all = TRUE, verbose = FALSE))
      suppressWarnings(mgv$merge.type <- NULL)
      expect_equal(mg, mgv)

      mg <- merge(df1, df2, by = thisby, all = FALSE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all = FALSE, verbose = FALSE))
      suppressWarnings(mgv$merge.type <- NULL)
      expect_equal(mg, mgv)

      mg <- merge(df1, df2, by = thisby, all.x = FALSE, all.y = TRUE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all.x = FALSE, all.y = TRUE, verbose = FALSE))
      suppressWarnings(mgv$merge.type <- NULL)
      expect_equal(mg, mgv)

      mg <- merge(df1, df2, by = thisby, all.x = TRUE, all.y = FALSE)
      mgv <- suppressWarnings(mergev(df1, df2, by = thisby, all.x = TRUE, all.y = FALSE, verbose = FALSE))
      suppressWarnings(mgv$merge.type <- NULL)
      expect_equal(mg, mgv)
    }
  }
  withr::with_options(
    list(width=500),
    co1 <- capture.output(mgv <- mergev(dfa, dfb, by = "key", all = TRUE))
  )
  expect_equal(co1, m1co)
  dfbu <- dfb[!duplicated(dfb$key), ]
  withr::with_options(
    list(width=500),
    co2 <- capture.output(mgv <- mergev(dfa, dfbu, by = "key", all = TRUE))
  )
  expect_equal(co2, m2co)
  dfas <- dfa[c(1, 3:7, 9:10), ]
  withr::with_options(
    list(width=500),
    co3 <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all = TRUE))
  )
  expect_equal(co3, m3co)

  withr::with_options(
    list(width=500),
    co4 <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all = FALSE))
  )
  expect_equal(co4, m4co)

  withr::with_options(
    list(width=500),
    co5 <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all.x = FALSE, all.y = TRUE))
  )
  expect_equal(co5, m5co)

  withr::with_options(
    list(width=500),
    co6 <- capture.output(mgv <- mergev(dfas, dfb, by = "key", all.x = TRUE, all.y = FALSE))
  )
  expect_equal(co6, m6co)
})

test_that("column attributes are retained", {
  dfa <- dfa0
  dfb <- dfb0
  attr(dfa$cola, "testAttrib1") <- "I'm a test!"
  attr(dfb$colb, "testAttrib2") <- "I'm also a test!"

  mgv <- mergev(dfa, dfb, by = "key", all.x = TRUE, all.y = FALSE, verbose = FALSE)

  expect_equal(attributes(mgv$cola)$testAttrib1, "I'm a test!")
  expect_equal(attributes(mgv$colb)$testAttrib2, "I'm also a test!")
})
