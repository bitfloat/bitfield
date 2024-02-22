#' Build a bit flag by checking whether values are NaN
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked for NaN values.
#' @examples
#' bf_nan(x = example_data, test = "y")
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_nan <- function(x, test){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))

  out <- is.nan(x[[test]])

  attr(out, which = "name") <- paste0("nan_", test)
  attr(out, which = "desc") <- c(paste0("{FALSE} the value in column '", test, "' is not NaN."), paste0("{TRUE}  the value in column '", test, "' is NaN."))
  attr(out, which = "triple") <- paste0(test, "|value|NaN")

  return(out)

}
