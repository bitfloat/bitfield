#' Build a bit flag from a (probability density) distribution
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{source}.
#' @param ... description
#' @param family [`character(1)`][character]\cr the family of the density
#'   distribution.
#' @importFrom checkmate assertDataFrame assertCharacter

bf_distribution <- function(x, ..., familiy = NULL){

  # https://en.wikipedia.org/wiki/Probability_density_function

  assertDataFrame(x = x)
  assertCharacter(x = family, len = 1, any.missing = FALSE)
  # assertChoice(x = family, choices = c(""))





}
