#' Build a bit flag by checking for a match with an external vector
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

bf_match <- function(x, test, against){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertClass(x = against, classes = class(x[[test]]))

  out <- x[[test]] %in% against

  attr(out, which = "name") <- paste0("match_", test)
  attr(out, which = "desc") <- paste0("the values in column '", test, "' are contained in the values (", paste0(against, collapse = "|"), ")")

  return(out)

}
