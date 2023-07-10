#' Extract bit values from integers
#'
#' @param x [`integerish(1)`][integer]\cr the integer from which to extract a
#'   bit.
#' @param len [`integerish(1)`][integer]\cr the length of the returned bit.
#' @param rev [`logical(1)`][logical]\cr whether or not to revert the direction
#'   of how the bit is returned.
#'
#' @importFrom checkmate assertIntegerish assertCharacter assertLogical
#' @importFrom stringr str_split_i
#' @importFrom dplyr slice
#' @export

.getBit <- function(x, len = NULL, rev = FALSE){

  assertIntegerish(x = x, len = 1, any.missing = FALSE)
  assertIntegerish(x = len, len = 1, lower = 1, null.ok = TRUE, any.missing = FALSE)
  assertLogical(x = rev, len = 1, any.missing = FALSE)

  if(is.null(len)){
    len <- 32
  }

  temp <- x %>%
    intToBits() %>%
    as.character() %>%
    str_split_i(pattern = "", 2)

  if(rev){
    temp <- rev(temp)
  }

  out <- temp %>%
    head(len) %>%
    paste0(collapse = "")

  return(out)
}
