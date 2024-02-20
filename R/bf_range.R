#' Build a bit flag by checking for a range
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which a
#'   range is checked.
#' @param min [`numeric(1)`][numeric]\cr the minimum allowed value.
#' @param max [`numeric(1)`][numeric]\cr the maximum allowed value.
#' @details when leaving either \code{min} or \code{max} at NULL, a "less than
#'   max" or "greater than min" operation is carried out.
#' @importFrom checkmate assertDataFrame assertSubset assertNumeric
#' @export

bf_range <- function(x, test, min = NULL, max = NULL){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertNumeric(x = min, null.ok = TRUE)
  assertNumeric(x = max, null.ok = TRUE)

  if(is.null(min)){

    assertNumeric(x = max, len = 1, finite = TRUE)

    temp <- x[[test]] < max

    name <- paste0("range_", test)
    desc <- c(paste0("the value in column '", test, "' is less than [", max, "]."),
              paste0("the value in column '", test, "' is greater than [", max, "]."))
    triple <- paste0(test, "|less_than|[", max, "]")
  } else if(is.null(max)){

    assertNumeric(x = min, len = 1, finite = TRUE)

    temp <- x[[test]] > min

    name <- paste0("range_", test)
    desc <- c(paste0("the value in column '", test, "' is greater than [", min, "]."),
              paste0("the value in column '", test, "' is less than [", min, "]."))
    triple <- paste0(test, "|greater_than|[", min, "]")
  } else {

    assertNumeric(x = max, len = 1, finite = TRUE)
    assertNumeric(x = min, len = 1, finite = TRUE)

    temp <- x[[test]] > min & x[[test]] < max

    name <- paste0("range_", test)
    desc <- c(paste0("the value in column '", test, "' ranges between [", min, ",", max, "]."),
              paste0("the value in column '", test, "' is outside the range [", min, ",", max, "]."))
    triple <- paste0(test, "|has_range|[", min, ",", max, "]")
  }

  out <- temp

  attr(out, which = "name") <- name
  attr(out, which = "desc") <- desc
  attr(out, which = "triple") <- triple

  return(out)

}
