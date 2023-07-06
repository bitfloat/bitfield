#'
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
