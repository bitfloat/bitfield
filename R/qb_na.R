#' Build a bit by checking for NA values
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked for NA values.
#'
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

qb_na <- function(x, test){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))

  out <- is.na(x[[test]])

  return(out)

}
