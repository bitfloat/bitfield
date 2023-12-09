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

  temp <- intToBits(x)
  temp <- as.character(temp)
  temp <- str_split_i(string = temp, pattern = "", 2)

  if(rev){
    temp <- rev(temp)
  }

  out <- paste0(head(x = temp, n = len), collapse = "")

  return(out)
}

#' Build a bit by checking
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked for NA values.
#'
#' @export

.getPrecision <- function(x, test){


  # assertDataFrame(x = input, min.cols = 2)
  # assertNames(x = names(input), must.include = c("x", "y"))
  #
  # areaLUT <- tibble(digits = c(0, 1, 2, 3, 4, 5),
  #                   meters = c(111000, 11100, 1110, 111, 11.1, 1.11))
  #
  # temp <- input %>%
  #   separate(col = y, into = c(NA, "precision"), sep = "[.]", remove = FALSE, fill = "right") %>%
  #   rowwise() %>%
  #   mutate(digits = str_split(precision, ""),
  #          rleRounded = if_else(is.na(precision), "0",
  #                               if_else(tail(rle(unlist(digits))$lengths, 1) == 1,
  #                                       paste0(digits, collapse = ""),
  #                                       paste0(rle(unlist(digits))$values, collapse = ""))
  #          ),
  #          precision = 1 / 10 ^ nchar(rleRounded)) %>%
  #   select(-digits)
  #
  # if(area){
  #   temp <- temp %>%
  #     mutate(derivArea = if_else(nchar(rleRounded) <= 5, areaLUT$meters[nchar(rleRounded)], areaLUT$meters[6]),
  #            derivArea = derivArea ** 2 * pi,
  #            area = if_else(is.na(area), derivArea, as.numeric(area)))
  # }
  #
  # return(temp)

}
