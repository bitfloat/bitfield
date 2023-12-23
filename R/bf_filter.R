#' Build a bit by checking for a match with an external vector (not working yet)
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that
#'   contains \code{test}.
#' @param test [`character(1)`][character]\cr the column in
#'   \code{x} for which a match is checked.
#' @param condition \cr any statement that results in a logical return value
#'
#' @importFrom checkmate assertDataFrame assertSubset assertClass
#' @export

bf_filter <- function(x, test, condition){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))


}
