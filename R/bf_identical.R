#' Build a bit flag by checking whether two columns are identical
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test} and \code{against}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked against \code{against}.
#' @param against [`character(1)`][character]\cr the column in \code{x} that is
#'   checked against \code{test}.
#' @details This function compares the values of two columns element-wise and
#'   returns \code{TRUE} when they are identical.
#' @examples
#' bf_identical(x = tbl_bityield, test = "x", against = "y")
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_identical <- function(x, test, against){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertSubset(x = against, choices = names(x))

  out <- x[[test]] == x[[against]]

  attr(out, which = "name") <- paste0("identical_", test, "_", against)
  attr(out, which = "desc") <- c(paste0("{FALSE} the value in '", test, "' is distinct from the value in '", against, "'."),
                                 paste0("{TRUE}  the value in '", test, "' is identical to the value in '", against, "'."))
  attr(out, which = "triple") <- paste0(test, "|identical|", against)

  return(out)

}
