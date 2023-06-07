# not exported
# returns a list with call performed on each element of "list" where "list" is an edsurvey.data.frame.list
#' @author Paul Bailey
itterateESDFL <- function(call, list) {
  lapply(list$data, function(li) {
    call$data <- li
    tryCatch(eval(call),
      error = function(cond) {
        message(paste("An error occurred while working on a dataset. Excluding results from this dataset."))
        message(cond)
        # returning zero allows unlist and other operations that remove e.g. NULLs
        return(0)
      }
    )
  })
}
