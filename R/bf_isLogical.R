#' Build a bit flag by checking for a logical value
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which a
#'   logical value is identified.
#' @param coerce [`logical(1)`][logical]\cr whether or not to carry out a more
#'   aggressive test, where the values are coerced to the target type and those
#'   that end up not having NA-values are reported as \code{TRUE}.
#' @details When coercing values to logical, this function returns \code{TRUE}
#'   only for the integer (!) values \code{0} and \code{1}. For all other
#'   values, including NA, this function returns \code{FALSE}.
#' @importFrom checkmate assertDataFrame assertSubset assertLogical
#' @importFrom purrr map_lgl
#' @export

bf_isLogical <- function(x, test, coerce = FALSE){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertLogical(x = coerce, len = 1, any.missing = FALSE)

  if(coerce){
    allBin <- all(x[[test]] %in% c(0L, 1L))

    out <- map_lgl(x[[test]], function(ix){
      if(is.integer(ix) & allBin){
        temp <- as.logical(ix)
      } else {
        temp <- NA
      }
      if(!is.na(temp)){
        is.logical(temp)
      } else {
        FALSE
      }
    })
  } else {
    out <- map_lgl(x[[test]], is.logical)
  }

  attr(out, which = "name") <- paste0("match_", test)
  attr(out, which = "description") <- c(paste0("the value in column '", test, "' has type 'logical'."),
                                        paste0("the value in column '", test, "' does not have type 'logical'."))
  attr(out, which = "triple") <- paste0(test, "|has_type|[logical]")


}
