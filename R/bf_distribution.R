#' Build a bit flag from a (probability density) distribution
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains columns
#'   with the distribution parameters.
#' @param ... bla
#' @param family [`character(1)`][character]\cr the family of the density
#'   distribution.
#' @importFrom checkmate assertDataFrame assertCharacter

bf_distribution <- function(x, ..., family = NULL){

  # https://en.wikipedia.org/wiki/Probability_density_function

  assertDataFrame(x = x)
  assertCharacter(x = family, len = 1, any.missing = FALSE)
  # assertChoice(x = family, choices = c(""))



  # newRegistry <- newRegistry %>%
  #   bf_grow(flags = bf_numeric(x = input, source = x, digits = 0),
  #           name = "normal_distribution.mean",
  #           desc = "the bit-representation decodes to a numeric value in column 'soandso' that represents the mean of a normal distribution",
  #           pos = 1:8, registry = .) %>%
  #   bf_grow(flags = bf_numeric(x = input, source = x, digits = 0),
  #           name = "normal_distribution.standard_deviation",
  #           desc = "the bit-representation decodes to a numeric value in column 'thisandthat' that represents the standard deviation of a normal distribution",
  #           pos = 9:16, registry = .)


}
