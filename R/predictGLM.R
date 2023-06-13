predict.edsurveyGlm <- function(object,
                                newdata = NULL,
                                type = c("link", "response"),
                                se.fit = FALSE,...) {
  type <- match.arg(type)
  if (is.null(newdata)) {
    dat <- object$data
  } else {
    dat <- newdata
  }
  b <- object$coef
  frm <- object$formula
  # add missing elements of response
  if (length(frm) == 3) {
    av <- all.vars(frm[[2]])
    for (avi in av) {
      if (!avi %in% colnames(dat)) {
        dat[[avi]] <- 0
      }
    }
  }
  fam <- object$family
  if (inherits(dat, "edsurvey.data.frame")) {
    dat <- getData(dat, varnames = all.vars(frm))
  }
  X <- model.matrix(frm, dat)
  if (any(!colnames(X) %in% names(b))) {
    notinX <- colnames(X)[!colnames(X) %in% names(b)]
    stop("cannot find columns in X: ", pasteItems(notinX), ".")
  }
  if (any(!names(b) %in% colnames(X))) {
    notinb <- names(b)[!names(b) %in% colnames(X)]
    stop("cannot find coefficients in b: ", pasteItems(notinb), ".")
  }
  res <- X[ , names(b)] %*% b
  if (type %in% "response") {
    res <- fam$linkinv(res)
  }
  return(res)
}
