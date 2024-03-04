#' Build a bit flag by checking for a range
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which a
#'   range is checked.
#' @param min [`numeric(1)`][numeric]\cr the minimum allowed value.
#' @param max [`numeric(1)`][numeric]\cr the maximum allowed value.
#' @details when leaving either \code{min} or \code{max} at NULL they are set to
#'   the minimum or maximum value of \code{test}, respectively, thereby carrying
#'   out a "less than max" or "greater than min" operation.
#' @importFrom checkmate assertDataFrame assertSubset assertNumeric
#' @export

bf_range <- function(x, test, min = NULL, max = NULL){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertNumeric(x = min, finite = TRUE, null.ok = TRUE)
  assertNumeric(x = max, finite = TRUE, null.ok = TRUE)

  if(is.null(min)) min <- min(x[[test]], na.rm = TRUE)
  if(is.null(max)) max <- max(x[[test]], na.rm = TRUE)

  temp <- x[[test]] > min & x[[test]] < max

  name <- paste0("range_", test)
  desc <- c(paste0("the value in column '", test, "' ranges between [", min, ",", max, "]."),
            paste0("the value in column '", test, "' is outside the range [", min, ",", max, "]."))
  triple <- paste0(test, "|range|[", min, ",", max, "]")

  out <- temp

  attr(out, which = "name") <- name
  attr(out, which = "desc") <- desc
  attr(out, which = "triple") <- triple

  return(out)

}
