#' Build a bit flag by checking for a integer value
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which a
#'   integer value is identified.
#' @param coerce [`logical(1)`][logical]\cr whether or not to carry out a more
#'   aggressive test, where the values are coerced to the target type and those
#'   that end up not having NA-values are reported as \code{TRUE}.
#' @details When coercing values to integer, they are typically truncated to the
#'   non-decimal part of the value. This function compares the truncated value
#'   to the original value and thus returns \code{TRUE} only when the original
#'   value didn't have a decimals part or when that part was \code{*.0}. It,
#'   moreover, returns \code{FALSE} for NA and NaN-values, and also for the
#'   special case NA_integer_, as this is not supported in many other programs
#'   that might provide data for or use them from this function.
#' @importFrom checkmate assertDataFrame assertSubset
#' @importFrom purrr map_lgl
#' @export

bf_isInteger <- function(x, test, coerce = FALSE){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertLogical(x = coerce, len = 1, any.missing = FALSE)

  if(coerce){
    out <- map_lgl(x[[test]], function(ix){
      temp <- as.integer(ix)
      if(!is.na(temp) & !is.nan(temp) & temp == ix){
        is.integer(temp)
      } else {
        FALSE
      }
    })
  } else {
    out <- map_lgl(x[[test]], is.integer)
  }

  attr(out, which = "name") <- paste0("match_", test)
  attr(out, which = "description") <- c(paste0("the value in column '", test, "' has type 'integer'."),
                                        paste0("the value in column '", test, "' does not have type 'integer'."))
  attr(out, which = "triple") <- paste0(test, "|has_type|[integer]")


}
