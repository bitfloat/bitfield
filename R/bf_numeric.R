#' Build a bit flag from a numeric value
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{source}.
#' @param source [`character(1)`][character]\cr the column in \code{x} from
#'   which to take the numeric value.
#' @param exponent description
#' @param significand description
#' @param bias description
#' @details The length of the bitfield depends on the floating point precision
#'   of the numeric values returned with this function...
#'
#' @examples
#' bf_numeric(x = example_data, source = "y")
#' @importFrom checkmate assertDataFrame assertSubset assertNumeric
#' @export

bf_numeric <- function(x, source, exponent = 8, significand = 7, bias = 127,
                       provenance = NULL){

  assertDataFrame(x = x)
  assertSubset(x = source, choices = names(x))
  assertIntegerish(x = exponent, len = 1, any.missing = FALSE)
  assertIntegerish(x = significand, len = 1, any.missing = FALSE)
  assertIntegerish(x = bias, len = 1, any.missing = FALSE)

  # https://en.wikipedia.org/wiki/Minifloat
  # https://en.wikipedia.org/wiki/Bfloat16_floating-point_format
  # https://www.youtube.com/watch?v=D-9SQMWo6kI
  #
  # for use-case
  # https://en.wikipedia.org/wiki/Decimal_degrees

  out <- x[[source]]

  attr(out, which = "name") <- paste0("numeric_", source)
  attr(out, which = "desc") <- paste0("the bits encode the numeric value in column '", source, "' (with {E} exponent bits, {S} significand bits and bias {B}, base-2).")
  attr(out, which = "triple") <- paste0(source, "|encoded|{E}.{S}.{B}")
  attr(out, which = "prov") <- provenance

  return(out)
}
