#' Add new bits to a bitfield
#'
#' @param bitfield [`bitfield(1)`][bitfield]\cr the bitfield with which the bits
#'   should be combined.
#' @param inspect [`logical(1)`][logical]\cr whether or not to inspect the bit.
#' @export

qb_combine <- function(bitfield, inspect = FALSE){


  # inspect = TRUE should return somehow information on the single bits for debugging purposes

  # https://stackoverflow.com/questions/62333478/from-integer-to-bits-and-back-again
  # 'intToBase2' <- function(x){
  #   x %>%
  #     intToBits() %>%
  #     rev %>%
  #     as.character() %>%
  #     {sapply(strsplit(., "", fixed = TRUE), '[', 2)} %>%
  #     paste0(.,collapse = '')
  # }
  #
  # base2ToInt <- function(string){
  #   sapply(strsplit(string, ""), function(x) sum(rev(+(x == "1")) * 2^(seq(x)-1)))
  # }
  #
}
