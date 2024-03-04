#' Build a bit flag from the summary of ...
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains a column
#'   with the residuals.
#' @param sum [`character(1)`][character]\cr the column in \code{x} that ...
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_summarise <- function(x, sum){

  assertDataFrame(x = x)
  assertSubset(x = sum, choices = names(x))



}
