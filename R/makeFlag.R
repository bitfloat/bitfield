#' Make a bit flag from an integer
#'
#' @param x [`integerish(1)`][integer]\cr the integer for which to derive the
#'   binary representation (bit flags).
#' @param len [`integerish(1)`][integer]\cr the number of bit flags.
#' @param rev [`logical(1)`][logical]\cr whether or not to revert the direction
#'   of how the bit flag is returned.
#' @details Wrapper around \code{\link{intToBits}}.
#'
#'
#' @importFrom checkmate assertIntegerish assertCharacter assertLogical
#' @importFrom stringr str_split_i
#' @importFrom dplyr slice
#' @export

.makeFlag <- function(x, len = NULL, rev = FALSE){

  assertIntegerish(x = x, len = 1, any.missing = FALSE)
  assertIntegerish(x = len, len = 1, lower = 1, null.ok = TRUE, any.missing = FALSE)
  assertLogical(x = rev, len = 1, any.missing = FALSE)

  if(is.null(len)){
    len <- 32
  }

  temp <- intToBits(x)
  temp <- as.character(temp)
  temp <- str_split_i(string = temp, pattern = "", 2)

  if(rev){
    temp <- rev(temp)
  }

  out <- paste0(head(x = temp, n = len), collapse = "")

  return(out)
}
