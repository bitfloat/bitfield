#' Build a bit flag from a histogram
#'
#' A histogram a frequency distribution where the count of values is binned into
#' a small set of values so that it provides a simple overview over the values.
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains...
#' @importFrom checkmate assertDataFrame
#' @export

bf_histogram <- function(x){

  assertDataFrame(x = x)


}
