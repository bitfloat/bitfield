#' Build a bit flag by checking for a numeric value
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which a
#'   numeric value is identified.
#' @param coerce [`logical(1)`][logical]\cr whether or not to carry out a more
#'   aggressive test, where the values are coerced to the target type and those
#'   that end up not having NA-values are reported as \code{TRUE}.
#' @details When coercing values to numeric, this function returns \code{TRUE}
#'   for any value that can sensibly be interpreted as numeric, such as
#'   \code{1L} (an integer) or \code{"1"} (a character), but not \code{"1L"} (a
#'   character of an integer) or \code{"one"} (another character). It returns
#'   \code{FALSE} for NA and NaN-values, and also for the special case NA_real_,
#'   as this is not supported in many other programs that might provide data for
#'   or use them from this function.
#' @importFrom checkmate assertDataFrame assertSubset assertLogical
#' @importFrom purrr map_lgl
#' @export

bf_isNumeric <- function(x, test, coerce = FALSE){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertLogical(x = coerce, len = 1, any.missing = FALSE)

  if(coerce){
    out <- map_lgl(x[[test]], function(ix){
      temp <- as.numeric(ix)
      if(!is.na(temp) & !is.nan(temp)){
        is.numeric(temp)
      } else {
        FALSE
      }
    })
  } else {
    out <- map_lgl(x[[test]], is.numeric)
  }

  attr(out, which = "name") <- paste0("match_", test)
  attr(out, which = "description") <- c(paste0("the value in column '", test, "' has type 'numeric'."),
                                        paste0("the value in column '", test, "' does not have type 'numeric'."))
  attr(out, which = "triple") <- paste0(test, "|has_type|[numeric]")

}
