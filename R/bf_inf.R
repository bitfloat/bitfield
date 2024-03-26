#' Build a bit flag by checking whether values are Inf
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked for Inf values.
#' @examples
#' bf_inf(x = bityield, test = "y")
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_inf <- function(x, test){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))

  out <- is.infinite(x[[test]])

  attr(out, which = "name") <- paste0("nan_", test)
  attr(out, which = "desc") <- c(paste0("{FALSE} the value in column '", test, "' is not Inf."), paste0("{TRUE}  the value in column '", test, "' is Inf."))
  attr(out, which = "triple") <- paste0(test, "|value|Inf")

  return(out)

}
