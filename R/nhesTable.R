#' @author Paul Bailey
nhesTable <- function(data, cols = NULL, byVar = NULL, wtvar = "fpwt", nCol = TRUE, totalRow = TRUE) {
  if (is.null(byVar) & !totalRow) {
    stop(paste0("Either ", sQuote("byVar"), " must be defined or ", sQuote("totalRow"), " must be TRUE."))
  }
  if (is.null(cols) & !nCol) {
    stop(paste0("Either ", sQuote("cols"), " must be defined or ", sQuote("nCol"), " must be TRUE."))
  }

  weightedTable <- function(x, w = rep(1, length(x))) {
    tab <- table(x)
    for (i in 1:nrow(tab)) {
      tab[i] <- sum(w[x %in% dimnames(tab)[[1]][i]])
    }
    tab
  }

  if (!is.null(byVar)) {
    bylevels <- names(table(data[ , byVar]))
  } else {
    bylevels <- NULL
  }

  row_names <- bylevels
  if (totalRow) {
    row_names <- c(row_names, "Total")
  }

  col_names <- cols
  if (nCol) {
    col_names <- c("n", col_names)
  }
  res <- matrix(0, nrow = length(row_names), ncol = length(col_names), dimnames = list(row_names, col_names))
  for (i in 1:nrow(res)) {
    if (i <= length(bylevels)) { # subset to just this row
      datai <- data[data[ , byVar] %in% bylevels[i], ]
    } else { # this is the toal row
      datai <- data
    }

    # now itterate over columns
    for (j in 1:ncol(res)) {
      jj <- ifelse(nCol, j - 1, j)
      if (jj == 0) { # ncol
        res[i, j] <- round(sum(datai[ , wtvar]))
      } else { # all other columns
        tab <- weightedTable(datai[ , cols[jj]], datai[ , wtvar]) #
        res[i, j] <- round(tab[names(tab) == "Yes"] / sum(tab) * 100)
      }
    } # for each column
  } # for each row

  res
}
