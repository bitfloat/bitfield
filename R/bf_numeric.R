#' Build a bit flag from a numeric value
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{source}.
#' @param source [`character(1)`][character]\cr the column in \code{x} from
#'   which to take the numeric value.
#' @param digits [`integerish(1)`][integer]\cr the number of decimals to which
#'   to round the numeric value.
#' @details The length of the bitfield depends on the floating point precision
#'   of the numeric values returned with this function...
#'
#' @examples
#' bf_numeric(x = example_data, source = "y", digits = 0)
#' @importFrom checkmate assertDataFrame assertSubset assertNumeric
#' @export

bf_numeric <- function(x, source, digits = 0){

  assertDataFrame(x = x)
  assertSubset(x = source, choices = names(x))
  assertNumeric(x = digits, len = 1, lower = 0, finite = TRUE)

  out <- round(x[[source]], digits)

  attr(out, which = "name") <- paste0("numeric_", source)
  attr(out, which = "desc") <- paste0("the bit-representaiton decodes to the numeric value [...] in column '", source, "'.")
  attr(out, which = "triple") <- paste0("BIT|encodes|", source)

  return(out)
}
