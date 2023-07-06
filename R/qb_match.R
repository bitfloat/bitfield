#' Build a bit by checking for a match with an external vector
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that
#'   contains \code{test}.
#' @param test [`character(1)`][character]\cr the column in
#'   \code{x} for which a match is checked.
#' @param against [`numeric(.)`][numeric] | [`character(.)`][character] |
#'   [`logical(1)`][logical]\cr a vector of the same class as in \code{test}
#'   against which a match is performed.
#'
#' @importFrom checkmate assertDataFrame assertSubset assertClass
#' @export

qb_match <- function(x, test, against){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertClass(x = against, classes = class(x[[test]]))

  out <- x[[test]] %in% against

  return(out)

}
