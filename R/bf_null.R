#' Build a bit flag by checking whether values are NULL
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked for NULL values.
#' @examples
#' bf_null(x = tbl_bityield, test = "z")
#' @importFrom checkmate assertDataFrame
#' @export

bf_null <- function(x, test){

  assertDataFrame(x = x)

  out <- is.null(x[[test]])

  attr(out, which = "name") <- paste0("nan_", test)
  attr(out, which = "desc") <- c(paste0("{FALSE} the column '", test, "' is not NULL."), paste0("{TRUE}  the column '", test, "' is NULL."))
  attr(out, which = "triple") <- paste0(test, "|value|NULL")

  return(out)

}
