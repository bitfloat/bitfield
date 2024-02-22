#' Build a bit flag from the residuals of a model
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains a column
#'   with the residuals.
#' @param resid [`character(1)`][character]\cr the column in \code{x} that
#'   contains the residuals.
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_residuals <- function(x, resid){

  assertDataFrame(x = x)
  assertSubset(x = resid, choices = names(x))



}
