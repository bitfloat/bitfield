#' Build a bit by checking
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param ... \cr any set of (mutually exclusive) statements that results in a
#'   logical return value
#' @param exclusive [`logical(1)`][logical]\cr whether the function should check
#'   that the cases are mutually exclusive, or whether it would allow that cases
#'   defined later in the sequence overwrite cases earlier in the sequence.
#'
#' @importFrom checkmate assertDataFrame assertLogical
#' @export

qb_case <- function(x, ..., exclusive = TRUE){

  assertDataFrame(x = x)
  assertLogical(x = exclusive, len = 1)



}
