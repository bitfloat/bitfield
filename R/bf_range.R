#' Build a bit flag by checking for a range
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which a
#'   range is checked.
#' @param min [`numeric(1)`][numeric]\cr the minimum allowed value.
#' @param max [`numeric(1)`][numeric]\cr the maximum allowed value.
#'
#' @importFrom checkmate assertDataFrame assertSubset assertNumeric
#' @export

bf_range <- function(x, test, min, max){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertNumeric(x = min, len = 1, finite = TRUE)
  assertNumeric(x = max, len = 1, finite = TRUE)

  out <- x[[test]] > min & x[[test]] < max

  attr(out, which = "name") <- paste0("range_", test)
  attr(out, which = "desc") <- paste0("the value in column '", test, "' ranges between [", min, ",", max, "].")

  return(out)

}
