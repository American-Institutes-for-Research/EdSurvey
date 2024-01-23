#' @title mergev
#'
#' @description More verbose merge function
#'
#' @param x first data.frame to merge, same as in \code{\link[base]{merge}}.
#' @param y second data.frame to merge, same as in \code{\link[base]{merge}}.
#' @param by character vector of column names to merge by. When \code{by} is used, the column names
#'           must be the same in \code{x} and \code{y}. Silently overrides \code{by.x}
#'           and \code{by.y}
#' @param by.x character vector of column names on \code{x} to merge by. The resulting file will have
#'             these names.
#' @param by.y character vector of column names on \code{y} to merge by.
#' @param all.x logical value indicating if unmerged rows from \code{x} should be included in the output.
#' @param all.y logical value indicating if unmerged rows from \code{y} should be included in the output.
#' @param all logical value indicating if unmerged rows from \code{x} and \code{y} should be included in the output.
#'            Silently overrides \code{all.x} and \code{all.y}.
#' @param order character string from "sort", "unsorted", "x", and "y".
#'              Specifies the order of the output. Setting this to "sort"
#'              gives the same result as \code{\link[base]{merge}} with sort=TRUE.
#'              unsorted gives the same result as sort=FALSE. "x" and "y" sort by the incoming
#'              sort order of \code{x} and \code{y}, respectively.
#' @param fast logical value indicating if \code{data.table} should be used to do the merge.
#' @param merge.type.colname character indicating the column name of the resulting merge type column.
#'                           See description.
#' @param return.list logical value indicating if the merged data.frame and verbose output should be
#'                    returned as elements of a list. Defaults to FALSE where the function
#'                    simply returns a data.frame.
#' @param verbose logical value indicating if output should be reported. Defaults to TRUE. Useful for testing.
#' @param showWarnings logical value to output warning messages (TRUE) or suppress (FALSE).  Defaults to TRUE.
#' @param \dots additional parameters passed to merge.
#'
#' @details
#' This is a wrapper for the base package merge function that prints out verbose information
#' about the merge, including the merge type (one/many to one/many), the overlapping column
#' names that will have suffixes applied, the number of rows and the number of unique
#' keys that are in each dataset and in the resulting dataset.
#'
#' Also gives more detailed errors when, e.g. the columns named in the \code{by} argument are
#' not on the \code{x} or \code{y} data.frames.
#'
#' @return
#' depends on the value of \code{return.list}.
#'
#' When \code{return.list} is \code{FALSE}, returns a \code{data.frame}.
#'
#' When \code{return.list} is \code{TRUE}, returns a list with two elements. The first is the same \code{data.frame} result. The second
#' is a list with the values that were printed out. Elements include merge.type with two elements, each "one" or "many" indicating the
#' merge type for \code{x} and \code{y}, respectively; inBoth, the list of column names in both merged data.frames; and merge.matrix
#' the matrix printed out by this function.
mergev <- function(x, y,
                   by = NULL, by.x = NULL, by.y = NULL,
                   all.x = NULL, all.y = NULL, all = FALSE,
                   order = c("sort", "unsorted", "x", "y"),
                   fast = FALSE,
                   merge.type.colname = "merge.type",
                   return.list = FALSE,
                   verbose = TRUE,
                   showWarnings = TRUE,
                   ...) {
  if (requireNamespace("data.table")) {
    o0 <- options(datatable.WhenJisSymbolThenCallingScope = TRUE)
    on.exit(options(o0))
  }

  order <- match.arg(order)
  params <- list(...)
  checkMergevVars(x=x, y=y,
                  by=by, by.x=by.x, by.y=by.y,
                  order=order,
                  fast=fast,
                  merge.type.colname = merge.type.colname,
                  showWarnings=showWarnings,
                  params=params)

  # get the names x and y were called with
  x.arg.name <- paste(deparse(substitute(x)), collapse = "")
  y.arg.name <- paste(deparse(substitute(y)), collapse = "")

  
  lst <- list()

  bys <- resolveMergevBy(x, y, by, by.x, by.y)
  x <- bys$x
  y <- bys$y
  by.x <- bys$by.x
  by.y <- bys$by.y

  if (is.null(all.x)) {
    all.x <- all
  }

  if (is.null(all.y)) {
    all.y <- all
  }

  checkBy(x, by.x, x.arg.name)
  checkBy(y, by.y, y.arg.name)

  for (i in 1:length(by.x)) {
    nx <- sum(is.na(x[ , by.x[i]]))
    ny <- sum(is.na(y[ , by.y[i]]))
    if (showWarnings && (nx >= 1 || ny >= 1)) {
      warning("For by variable, '", by.x[i], "' There are ", nx, " NA values on ", x.arg.name, " and ", ny, " NA values on ", y.arg.name, ".")
    }
  }

  if (order %in% c("x", "y")) {
    order.var <- "order"
    while (order.var %in% c(names(x), names(y))) {
      order.var <- paste0(order.var, sample(LETTERS, 1))
    }
    if (order == "x") {
      x[ , order.var] <- 1:nrow(x)
    } else {
      y[ , order.var] <- 1:nrow(y)
    }
  } else {
    if (order == "sort") {
      sort <- TRUE
    } else {
      sort <- FALSE
    }
  }

  tempx <- genVarNames("x", x)
  tempy <- genVarNames("y", y)

  x[ , tempx] <- c(1:nrow(x))
  y[ , tempy] <- c(1:nrow(y))

  v1 <- names(x)
  v2 <- names(y)

  # create the verbose output matrix
  lst$merge.summary <- buildMatTop(x=x, y=y, by.x = by.x, by.y= by.y, x.arg.name=x.arg.name, y.arg.name=y.arg.name)

  merge_type <- ""
  not.one.to.one <- FALSE
  if (lst$merge.summary[2, 1] < lst$merge.summary[1, 1]) {
    lst$merge.type <- c("many:")
    merge_type <- "many:"
    not.one.to.one <- TRUE
  } else {
    lst$merge.type <- c("one:")
    merge_type <- "one:"
  }

  if (lst$merge.summary[2, 2] < lst$merge.summary[1, 2]) {
    lst$merge.type <- c(lst$merge.type, "many")
    merge_type <- paste0(merge_type, "many")
    not.one.to.one <- TRUE
  } else {
    lst$merge.type <- c(lst$merge.type, "one")
    merge_type <- paste0(merge_type, "one")
  }

  # make output
  v1 <- v1[!v1 %in% by]
  col <- v1[v1 %in% v2]
  if (length(col) > 0) {
    if (verbose) {
      cat("Variables in both ", x.arg.name, " and ", y.arg.name, ":\n")
      print(col)
    }
    lst <- c(lst, list(inBoth = col))
  }

  if (verbose) {
    cat("Merge type is ", merge_type, "\n")
  }
  # clear off the rn.x and rn.y variables. merge will add the proper variables
  if (!is.null(bys$rn.x)) {
    x[ , bys$rn.x] <- NULL
    if (bys$num.x) {
      by.x <- bys$by.x.original
    }
  }
  if (!is.null(bys$rn.y)) {
    y[ , bys$rn.y] <- NULL
    if (bys$num.y) {
      by.y <- bys$by.y.original
    }
  }

  # do the merge
  if (fast & !inherits(x, "data.table")) {
    mg <- as.data.frame(merge(as.data.table(x),
      as.data.table(y),
      by.x = by.x,
      by.y = by.y,
      all.x = all.x,
      all.y = all.y,
      ...
    ))
  } else {
    mg <- merge(x, y, by.x = by.x, by.y = by.y, all.x = all.x, all.y = all.y, ...)
  }
  if (nrow(mg) == 0) {
    return(mergevReturnZeroes(mg, tempx, tempy, return.list, lst, showWarnings, verbose))
  }
  if (order %in% c("x", "y")) {
    mg[is.na(mg[ , order.var]), ] <- Inf
    mg <- mg[order(mg[ , order.var]), ]
    mg[ , order.var] <- NULL
  }

  flag <- FALSE
  if (is.null(merge.type.colname)) {
    flag <- TRUE
    merge.type.colname <- genVarNames("merge", mg)
  }

  bmb_ret <- buildMatBottom(mg, by.x, by.y, tempx, tempy, lst, flag, merge.type.colname, return.list, verbose)
  mg <- bmb_ret$mg
  lst <- bmb_ret$lst

  # remove temp columns
  mg[ , tempx] <- NULL
  mg[ , tempy] <- NULL


  if (verbose) {
    print(lst$merge.summary)
  }

  if (flag) {
    mg[ , merge.type.colname] <- NULL
  }

  # add back column attributes, X will take precedence over Y in the event of duplicate variables with attributes
  cols <- names(mg)
  for (i in 1:length(cols)) {
    coli <- cols[i]
    mgcoli <- mg[[coli]]
    if (coli %in% names(x) | coli %in% names(y)) {
      if (coli %in% names(x)) {
        ocoli <- x[[coli]]
      } else {
        ocoli <- y[[coli]]
      }
      newAtrs <- attributes(ocoli)
      oldAnames <- names(attributes(mgcoli))
      transname <- names(newAtrs)
      transname <- transname[!transname %in% oldAnames]
      for (tri in 1:length(transname)) {
        if ((!is.null(transname[tri])) && (!is.na(transname[tri])) && (length(transname[tri]) > 0)) {
          attr(mgcoli, transname[tri]) <- newAtrs[[transname[tri]]]
        }
      }
      mg[[coli]] <- mgcoli
    }
  }

  # return
  if (return.list) {
    return(list(data = mg, list = lst))
  }
  return(mg)
}

checkMergevVars <- function(x, y,
                            by, by.x, by.y,
                            order,
                            fast,
                            merge.type.colname,
                            showWarnings,
                            params) {
  if (!inherits(x, "data.frame")) {
    stop(paste0(sQuote("x"), " must be a data.frame or something that can be cast as a data.frame"))
  }

  if (!inherits(y, "data.frame")) {
    stop(paste0(sQuote("y"), " must be a data.frame or something that can be cast as a data.frame"))
  }

  if (nrow(x) == 0) {
    stop(paste0("No rows in ", x.arg.name, "."))
  }

  if (nrow(y) == 0) {
    stop(paste0("No rows in ", y.arg.name, "."))
  }

  # RFE: this will not deal with the situation where a suffix is the 0 length string
  if (showWarnings && sum(merge.type.colname == c(names(x), names(y))) == 1) {
    warning(paste0(sQuote("merge.type.colname"), " is on x or y. It will be overwritten."))
  }

  if (fast & is.null(by)) {
    stop(paste0("You must specify ", sQuote("by"), " if you set ", sQuote("fast"), " to ", sQuote("TRUE")))
  }
  if ("sort" %in% names(params)) {
    if (length(order) != 4) {
      if (params$sort == TRUE & order != "sort") {
        stop(paste0("You need to set order to ", sQuote("sort"), "if sort is set to ", sQuote(TRUE)))
      }
    }
  }

  if (is.null(by) & (is.null(by.x) & is.null(by.y))) {
    # note, merge will allow this and return the Cartesian product.
    # there is not so much verbose output for such a thing.
    stop(paste0("For mergev, you need to specify either ", sQuote("by"), " or both ", sQuote("by.x"), "and ", sQuote("by.y"), ". If you want the Cartesian product, try the merge function in the base package."))
  }

  if (!order %in% c("unsorted", "sort", "x", "y")) {
    stop(paste0(sQuote("order"), " must be one of ", sQuote("x"), ", ", sQuote("y"), ", ", sQuote("sort"), ", or ", sQuote("unsorted"), "."))
  }
}

resolveMergevBy <- function(x, y, by, by.x, by.y) {
  if (!is.null(by)) {
    by.x <- by
    by.y <- by
  }

  # fix logical by values. These are resolved seperately on x and y in merge
  if (inherits(by.x, "logical")) {
    by.x <- names(x)[by.x]
  }

  if (inherits(by.y, "logical")) {
    by.y <- names(x)[by.y]
  }

  # numeric by.x
  rn.x <- NULL
  num.x <- FALSE
  by.x.original <- by.x
  if (is.numeric(by.x)) {
    if (0 %in% by.x) {
      num.x <- TRUE
      rn.x <- genVarNames("rn", x)
      x[ , rn.x] <- rownames(x)
      by.x[!by.x %in% 0] <- by.x[!by.x %in% 0]
      by.x[by.x %in% 0] <- ncol(x)
    }
    by.x <- names(x)[by.x]
  }

  # numeric by.y
  rn.y <- NULL
  num.y <- FALSE
  by.y.original <- by.y
  if (is.numeric(by.y)) {
    if (0 %in% by.y) {
      num.y <- TRUE
      rn.y <- genVarNames("rny", y)
      y[ , rn.y] <- rownames(y)
      by.y[!by.y %in% 0] <- by.y[!by.y %in% 0]
      by.y[by.y %in% 0] <- ncol(y)
    }
    by.y <- names(y)[by.y]
  }

  if (length(by.x) != length(by.y)) {
    stop(paste0("You need to specify same number of ", sQuote("X"), " and ", sQuote("Y"), " variables"))
  }
  return(list(x=x, y=y, by.x=by.x, by.y=by.y, by.x.original=by.x.original, by.y.original = by.y.original, rn.x=rn.x, rn.y=rn.y, num.x=num.x,num.y=num.y))
}

mergevReturnZeroes <- function(mg, tempx, tempy, return.list, lst, showWarnings, verbose) {
  if (showWarnings) {
    warning("No rows on resulting data.frame.")
  }
  mat <- lst$merge.summary
  mat[3, 1] <- 0
  mat[3, 2] <- 0
  mat[3, 3] <- 0
  mat[4, 1] <- 0
  mat[4, 2] <- 0
  mat[4, 3] <- 0
  mg[ , tempx] <- NULL
  mg[ , tempy] <- NULL
  mg[ , ]
  if (verbose) {
    print(mat)
  }
  if (return.list) {
    lst$merge.summary <- mat
    return(list(data = mg, list = lst))
  }
  return(mg)
}

buildMatTop <- function(x, y, by.x, by.y, x.arg.name, y.arg.name) {
  rn <- c("Rows in", "Unique keys in", "Rows out", "Unique keys out")
  mat <- matrix(0, nrow = 4, ncol = 3, dimnames = list(rn, c(x.arg.name, y.arg.name, "total")))
  mat[1, 1] <- nrow(x)
  mat[1, 2] <- nrow(y)
  mat[1, 3] <- mat[1, 1] + mat[1, 2]
  if (inherits(x, "data.table") & inherits(y, "data.table")) {
    mat[2, 1] <- nrow(ux <- unique(as.data.frame(x[ , ..by.x])))
    mat[2, 2] <- nrow(uy <- unique(as.data.frame(y[ , ..by.y])))
  } else {
    mat[2, 1] <- nrow(ux <- unique(as.data.frame(x)[ , by.x, drop = FALSE]))
    mat[2, 2] <- nrow(uy <- unique(as.data.frame(y)[ , by.y, drop = FALSE]))
  }
  names(ux) <- by.x
  names(uy) <- by.x
  mat[2, 3] <- nrow(as.data.frame(unique(rbind(ux, uy))))
  return(mat)
}

buildMatBottom <- function(mg, by.x, by.y, tempx, tempy, lst, flag, merge.type.colname, return.list, verbose) {
  mat <- lst$merge.summary
  mat[3, 1] <- nrow(mg[!is.na(mg[[tempx]]), ])
  mat[3, 2] <- nrow(mg[!is.na(mg[[tempy]]), ])
  mat[3, 3] <- nrow(mg)

  if (nrow(mg) != 0) {
    mg[ , merge.type.colname] <- 0L
    types <- c("x only", "y only", "matched")
    if (inherits(mg, "data.table")) {
      mx <- mg[[tempx]]
      mg <- mg[is.na(mx), (merge.type.colname) := 2L]
      my <- mg[[tempy]]
      mg <- mg[is.na(my), (merge.type.colname) := 1L]
      mz <- mg[[merge.type.colname]]
      mg <- mg[mz == 0L, (merge.type.colname) := 3L]
      
      mg[[merge.type.colname]] <- lfactor(mg[[merge.type.colname]], levels = 1:3, labels = types)
    } else {
      mg[ , merge.type.colname][is.na(mg[ , tempx])] <- 2L
      mg[ , merge.type.colname][is.na(mg[ , tempy])] <- 1L
      mg[ , merge.type.colname][mg[ , merge.type.colname] == 0L] <- 3L
      mg[ , merge.type.colname] <- lfactor(mg[ , merge.type.colname], levels = 1:3, labels = types)
    }
    if (verbose) {
      print(table(mg[[merge.type.colname]]))
    }
    if (return.list) {
      lst$merge.type.table <- table(mg[[merge.type.colname]])
    }
  }

  # This is by.x because by.y might not be defined for mg
  if (inherits(mg, "data.table")) {
    mat[4, 1] <- nrow(na.omit(unique(mg[!is.na(mg[[tempx]]), ..by.x])))
    mat[4, 2] <- nrow(na.omit(unique(mg[!is.na(mg[[tempy]]), ..by.x])))
    mat[4, 3] <- nrow(na.omit(data.frame(unique(mg[ , ..by.x]))))
  } else {
    mat[4, 1] <- length(na.omit(unique(mg[!is.na(mg[ , tempx]), by.x])))
    mat[4, 2] <- length(na.omit(unique(mg[!is.na(mg[ , tempy]), by.x])))
    mat[4, 3] <- length(na.omit(unique(mg[ , by.x])))
  }
  lst$merge.summary <- mat
  return(list(mg=mg, lst=lst))
}

checkBy <- function(x, by, argname) {
  if (sum(by %in% names(x)) != length(by)) {
    stop(paste0("Not all by variables in ", argname, ", the following are missing:", paste(by[!by %in% names(x)], collapse = ", ")))
  }
}

genVarNames <- function(x, df) {
  while (TRUE) {
    prev <- substr(x, 2, nchar(x))
    y <- paste0(x, prev, sample(9, 1))
    if (!y %in% names(df)) {
      break
    }
  }
  y
}
