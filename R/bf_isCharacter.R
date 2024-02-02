#' Build a bit flag by checking for a character value
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which a
#'   character value is identified.
#' @param coerce [`logical(1)`][logical]\cr whether or not to carry out a more
#'   aggressive test, where the values are coerced to the target type and those
#'   that end up not having NA-values are reported as \code{TRUE}.
#' @details When coercing values to character, this function returns \code{TRUE}
#'   for any value other than NA or Inf, and also for the all special cases of
#'   NA (such as NA_character_, NA_real_, etc), as this is not supported in many
#'   other programs that might provide data for or use them from this function.
#' @importFrom checkmate assertDataFrame assertSubset assertLogical
#' @importFrom purrr map_lgl
#' @export

bf_isCharacter <- function(x, test, coerce = FALSE){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertLogical(x = coerce, len = 1, any.missing = FALSE)

  if(coerce){
    out <- map_lgl(x[[test]], function(ix){
      if(!is.infinite(ix)){
        temp <- as.character(ix)
      } else {
        temp <- NA
      }
      if(!is.na(temp)){
        is.character(temp)
      } else {
        FALSE
      }
    })
  } else {
    out <- map_lgl(x[[test]], is.character)
  }

  attr(out, which = "name") <- paste0("match_", test)
  attr(out, which = "description") <- c(paste0("the value in column '", test, "' has type 'character'."),
                                        paste0("the value in column '", test, "' does not have type 'character'."))
  attr(out, which = "triple") <- paste0(test, "|has_type|[character]")


}
