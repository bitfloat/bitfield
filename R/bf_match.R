#' Build a bit flag by comparing a column with a set
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which a
#'   match is checked.
#' @param set a vector of the same class as in \code{test} against which a set
#'   operation is performed.
#' @param negate [`logical(1)`][logical]\cr whether or not to determine a subset
#'   or a disjoint match.
#' @examples
#' bf_match(x = tbl_bityield, test = "commodity", set = c("soybean", "maize"))
#'
#' @importFrom checkmate assertDataFrame assertSubset assertClass
#' @export

bf_match <- function(x, test, set, negate = FALSE){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertClass(x = set, classes = class(x[[test]]))

  if(negate){
    out <- !x[[test]] %in% set
    type_label <- "disjoint"
    theDesc <- c(paste0("{FALSE} the value in column '", test, "' is not disjoint from the set [", paste0(set, collapse = "|"), "]."),
                 paste0("{TRUE}  the value in column '", test, "' is disjoint from the set [", paste0(set, collapse = "|"), "]."))
  } else {
    out <- x[[test]] %in% set
    type_label <- "included"
    theDesc <- c(paste0("{FALSE} the value in column '", test, "' is not included in the set [", paste0(set, collapse = "|"), "]."),
                 paste0("{TRUE}  the value in column '", test, "' is included in the set [", paste0(set, collapse = "|"), "]."))
  }

  attr(out, which = "name") <- paste0("match_", test)
  attr(out, which = "desc") <- theDesc
  attr(out, which = "triple") <- paste0(test, "|", type_label,"|[", paste0(set, collapse = "|"), "]")

  return(out)

}
