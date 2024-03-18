#' Make a bit flag from an integer
#'
#' @param x [`numeric(1)`][numeric]\cr the numeric value for which to derive the
#'   binary value.
#' @param len [`integerish(1)`][integer]\cr the number of bits used to capture
#'   the value.
#' @details Additional details...
#'
#' @examples
#' # example code
#'
#' @importFrom checkmate assertIntegerish assertNumeric
#' @importFrom stringr str_pad
#' @export

.intToBin <- function(x, len = NULL){

  x <- as.integer(x)
  assertIntegerish(x = x, any.missing = FALSE)

  # ensure that 'len' is enough to capture the value
  # assertNumeric(x = len, len = 1, lower = ceiling(log2(val)), any.missing = FALSE, finite = TRUE, null.ok = TRUE)

  temp <- map(.x = x, .f = function(ix){
    val <- ix
    bin <- NULL
    i <- 1
    if(val == 0){
      bin <- "0"
    } else {
      while(val > 0){

        bin[i] <- val %% 2
        val <- val %/% 2
        i <- i + 1

      }
    }
    bin <- rev(bin)
    bin <- paste0(bin, collapse = "")


    return(bin)
  }) |> unlist()

  # temp <- str_pad(string = temp, width = max(nchar(temp)), side = "left", pad = "0")

  # fill with 0s
  if(!is.null(len)){
    bin <- bin[1:len]
    bin[is.na(bin)] <- 0
  }

  return(temp)
}

.decToBin <- function(x, len){

  x <- as.numeric(paste0(0, ".", str_split(x, "[.]", simplify = T)[,2]))

  temp <- map(.x = x, .f = function(ix){
    val <- ix
    bin <- NULL
    i <- 0
    while(val > 0 & i < len){
      val <- val * 2
      bin <- c(bin, val %/% 1)
      val <- val - val %/% 1
      i <-  i + 1
    }
    bin <- paste0(bin, collapse = "")

    return(bin)
  }) |> unlist()

  return(temp)
}


# good explanation
# https://www.cs.cornell.edu/~tomf/notes/cps104/floating
#
# calculator
# https://float.exposed/0x3e458b82
#
# https://en.wikipedia.org/wiki/Minifloat
# https://en.wikipedia.org/wiki/Bfloat16_floating-point_format
# https://www.youtube.com/watch?v=D-9SQMWo6kI
#
# https://youtu.be/D-9SQMWo6kI?t=35 -> explanation of the data model of a bitfield, I should write this into a short description to explain how this function works
#
# for use-case
# https://en.wikipedia.org/wiki/Decimal_degrees
