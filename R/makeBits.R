#' Make a bit flag from an integer
#'
#' @param x the integer for which to derive the
#'   binary representation (bit flags).
#' @param encoding the number of bit flags.
#' @importFrom checkmate assertList
#' @importFrom purrr map_chr
#' @importFrom stringr str_split_i

.makeBits <- function(x, encoding = NULL){

  assertList(x = encoding, types = "integer", any.missing = FALSE, len = 4)

  len <- encoding$sign + encoding$exponent + encoding$significand

  if(encoding$exponent == 0){

    out <- map_chr(.x = x, .f = function(ix){
      intToBits(ix) |>
        as.character() |>
        str_split_i(pattern = "", 2) |>
        head(n = len) |>
        rev()  |>
        paste0(collapse = "")
    })

  } else {



  }



  # dec <- str_length(str_extract(theValues, paste0("(?<=\\.)\\d+")))
  # dec[is.na(dec)] <- 0L
  #
  # https://stackoverflow.com/questions/872544/what-range-of-numbers-can-be-represented-in-a-16-32-and-64-bit-ieee-754-syste
  # https://en.wikipedia.org/wiki/Half-precision_floating-point_format

  # https://en.wikipedia.org/wiki/Minifloat
  # https://en.wikipedia.org/wiki/Bfloat16_floating-point_format
  # https://www.youtube.com/watch?v=D-9SQMWo6kI
  #
  # https://youtu.be/D-9SQMWo6kI?t=35 -> explanation of the data model of a bitfield, I should write this into a short description to explain how this function works
  #
  # for use-case
  # https://en.wikipedia.org/wiki/Decimal_degrees
  #
  # https://stackoverflow.com/questions/872544/what-range-of-numbers-can-be-represented-in-a-16-32-and-64-bit-ieee-754-syste
  # https://en.wikipedia.org/wiki/Half-precision_floating-point_format

  return(out)
}
