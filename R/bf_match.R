#' Build a bit flag by comparing a columns with a set
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which a
#'   match is checked.
#' @param set [`numeric(.)`][numeric] | [`character(.)`][character]\cr a vector
#'   of the same class as in \code{test} against which a set operation is
#'   performed.
#' @param negate [`logical(1)`][logical]\cr whether or not to determine a subset
#'   or a disjoint match.
#'
#' @importFrom checkmate assertDataFrame assertSubset assertClass
#' @export

bf_match <- function(x, test, set, negate = FALSE){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertClass(x = set, classes = class(x[[test]]))

  if(negate){
    out <- !x[[test]] %in% set
    type_label <- "disjoint_of"
  } else {
    out <- x[[test]] %in% set
    type_label <- "subset_of"
  }

  attr(out, which = "name") <- paste0("match_", test)
  attr(out, which = "desc") <- c(paste0("the value in column '", test, "' is contained in the set [", paste0(set, collapse = "|"), "]."),
                                 paste0("the value in column '", test, "' is not contained in the set [", paste0(set, collapse = "|"), "]."))
  attr(out, which = "triple") <- paste0(test, "|", type_label,"|[", paste0(set, collapse = "|"), "]")

  return(out)

}
