#' Build a bit flag by checking that values are not NaN
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked for NaN values.
#'
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_nan <- function(x, test){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))

  out <- !is.nan(x[[test]])

  attr(out, which = "name") <- paste0("nan_", test)
  attr(out, which = "desc") <- c(paste0("the value in column '", test, "' is NaN."), paste0("the value in column '", test, "' is not NaN."))
  attr(out, which = "triple") <- paste0(test, "|is_value|NaN")

  return(out)

}
