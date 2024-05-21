#' @title Recode Levels Within Variables
#'
#' @description Recodes variables in an \code{edsurvey.data.frame},
#'              a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#'
#' @param x an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'        or an \code{edsurvey.data.frame.list}
#' @param recode a list of recoding rules. See Examples for the format of recoding rules.
#'
#' @return an object of the same class as \code{x} with the \code{recode} added to it
#' @usage recode.sdf(x, recode)
#'
#' @author Trang Nguyen and Paul Bailey
#' @example man\examples\recode.sdf.R
#' @export recode.sdf
recode.sdf <- function(x, recode) {
  checkDataClass(x, c("edsurvey.data.frame.list", "edsurvey.data.frame", "light.edsurvey.data.frame"))

  if (inherits(x, "edsurvey.data.frame.list")) {
    for (i in seq_along(x$data)) {
      newUserConditions <- getAttributes(x$datalist[[i]], "userConditions", errorCheck = FALSE)
      if (is.null(newUserConditions)) {
        newUserConditions <- list()
      }
      newUserConditions <- append(as.list(newUserConditions), list(recode = recode))
      checkRecode(x$datalist[[i]], recode) # print out warnings if need be
      x$datalist[[i]] <- setAttributes(x$datalist[[i]], "userConditions", newUserConditions)
    }
  } else if (inherits(x, "edsurvey.data.frame")) {
    newUserConditions <- getAttributes(x, "userConditions", errorCheck = FALSE)
    if (is.null(newUserConditions)) {
      newUserConditions <- list()
    }
    newUserConditions <- append(as.list(newUserConditions), list(recode = recode))
    checkRecode(x, recode) # print out warnings if need be
    x <- setAttributes(x, "userConditions", newUserConditions)
  } else if (inherits(x, "light.edsurvey.data.frame")) {
    for (i in seq_along(recode)) {
      ni <- names(recode)[i]
      from <- recode[[i]]$from
      to <- recode[[i]]$to
      if (length(to) > 1) {
        stop(paste0("More than one 'To' value found in the ", sQuote(ni), " element of the 'recode' argument."))
      }

      badFrom <- c() # levels with incorrect recodes
      if (inherits(x[ , ni], "factor")) {
        newto <- to
        if (to %in% from) { # remove degenerate recode
          from <- from[!from %in% to]
        }
        labs <- levels(x[ , ni]) # used for both lfactors and factors
        if (newto %in% labs) { # this is not a new label
          newto <- NULL
        }
        tmp <- as.character(x[ , ni])
        if (inherits(x[ , ni], "lfactor")) { # it is an lfactor
          levs <- llevels(x[ , ni])
          # in case of lfactor:
          # + from can be numeric or character
          # + to can be numeric or character
          # To simplify the code, if to is a numeric, we will coerce it to character
          if (is.numeric(to)) {
            if (!to %in% levs) {
              labs <- c(labs, as.character(to)) # since there are no labels provided, we will use character format of levels
              levs <- c(levs, to)
            }
            toNum <- to
            to <- labs[levs == to]
          } else {
            if (!to %in% labs) {
              labs <- c(labs, to)
              toNum <- max(levs, na.rm = TRUE) + 1
              levs <- c(levs, toNum)
            } else {
              toNum <- levs[which(to %in% labs)]
            }
          }
          # after the code above, to is always a character label

          # from can be a vector of mixed numeric and character values
          # fromNum: numeric values in from
          # fromChar: character values in from
          suppressWarnings(fromNum <- as.numeric(from)) # numeric from variables
          fromChar <- from[is.na(fromNum)] # character from variables
          # numeric from variables
          fromNum <- fromNum[!is.na(fromNum)]

          # changing tmp according to numeric values of from
          if (length(fromNum) > 0) {
            tmp_numeric <- lfactors:::switchllevels(x[ , ni])
            tmp[tmp_numeric %in% fromNum] <- to
            if (any(!fromNum %in% levs)) {
              # add any missing levels to missing list
              badFrom <- fromNum[!fromNum %in% levs]
            }
            labs <- labs[!levs %in% setdiff(fromNum, toNum)]
            levs <- levs[!levs %in% setdiff(fromNum, toNum)]
          }
          # changing tmp according to character values of from
          if (length(fromChar) > 0) {
            tmp[tmp %in% fromChar] <- to
            if (any(!fromChar %in% labs)) {
              badFrom <- c(badFrom, fromChar[!fromChar %in% labs])
            }
            levs <- levs[!labs %in% setdiff(fromChar, to)]
            labs <- labs[!labs %in% setdiff(fromChar, to)]
          }
          # Now we need to call lfactors again to make sure levels are mapped correctly to modified character vectors
          x[ , ni] <- lfactor(tmp, levels = levs, labels = labs, exclude = NULL)
        } else { # end if(inherits(x[ ,ni],"lfactor"))
          # it is a base r factor so from and to have to be character
          tmp[tmp %in% from] <- to
          if (any(!from %in% labs)) {
            # add any missing levels to missing list
            badFrom <- c(badFrom, from[!from %in% labs])
          }
          if (!to %in% labs) {
            labs <- c(labs, to)
          }
          x[ , ni] <- factor(tmp, levels = labs)
        }
      } else { # end if(inherits(x[ ,ni], "factor"))
        # recode for non factors
        if (any(!from %in% x[ , ni])) {
          badFrom <- from[!from %in% x[ , ni]]
        }
        x[ , ni][x[ , ni] %in% from] <- to
      } # end else for if(inherits(x[ ,ni], "factor"))
      if (length(badFrom) > 0) {
        warning(paste0(
          "When recoding, could not find the level(s) ",
          pasteItems(dQuote(badFrom)),
          " in the variable ", dQuote(ni), "."
        ))
      }
    } # for (i in seq_along(recode))
  } # ends else if (inherits(x, "light.edsurvey.data.frame"))
  invisible(x)
} # end of fuction recode.sdf

checkRecode <- function(x, recode) {
  for (vi in seq_along(recode)) {
    v <- names(recode)[vi]
    from <- recode[[vi]]$from # can have multiple values
    to <- recode[[vi]]$to # can only have one value
    if (length(to) > 1) {
      stop(paste0("More than one 'To' value found in the ", sQuote(v), " element of the 'recode' argument."))
    }
    varlevels <- levelsSDF(v, x)[[1]]
    levs <- varlevels$level
    labs <- varlevels$labels
    if (any(!tolower(from) %in% c(tolower(labs), tolower(levs)))) {
      warning(paste0(
        "When recoding, could not find the level(s) ",
        pasteItems(dQuote(from[which(!tolower(from) %in% tolower(c(labs, levs)))])),
        " in the variable ", dQuote(v), "."
      ))
    }
    recodeNames <- names(recode[[vi]])
    badV <- recodeNames[!recodeNames %in% c("from", "to")]
    if (length(badV) > 0) {
      stop(paste0("Each recode should have a ", sQuote("from"), " and a ", sQuote("to"), " and no other elements. Found additional element(s) named ", pasteItems(dQuote(badV)), "."))
    }
  }
}
