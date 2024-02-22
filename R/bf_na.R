#' Build a bit flag by checking whether values are NA
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked for NA values.
#' @examples
#' bf_na(x = example_data, test = "y")
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_na <- function(x, test){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))

  out <- is.na(x[[test]])

  attr(out, which = "name") <- paste0("na_", test)
  attr(out, which = "desc") <- c(paste0("{FALSE} the value in column '", test, "' is not NA."), paste0("{TRUE}  the value in column '", test, "' is NA."))
  attr(out, which = "triple") <- paste0(test, "|value|NA")

  return(out)
}
