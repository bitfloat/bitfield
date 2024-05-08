#' Make a binary value from an integer
#'
#' @param x [`numeric(1)`][numeric]\cr the numeric value for which to derive the
#'   binary value.
#' @param len [`integerish(1)`][integer]\cr the number of bits used to capture
#'   the value.
#' @param dec [`logical(1)`][logical]\cr whether to transform the decimal part
#'   to bits, or the integer part.
#' @details Additional details...
#'
#' @examples
#' # example code
#'
#' @importFrom checkmate assertIntegerish assertNumeric
#' @importFrom stringr str_pad

.toBin <- function(x, len, dec = FALSE){

  assertNumeric(x = x)
  assertIntegerish(x = len, len = 1, any.missing = FALSE)
  assertLogical(x = dec, len = 1, any.missing = FALSE)

  if(dec){
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
  } else {
    x <- as.integer(x)

    temp <- map(.x = x, .f = function(ix){
      val <- ix
      bin <- NULL
      i <- len
      if(val == 0){
        bin <- rep("0", len)
      } else {
        while(i > 0){
          bin[i] <- val %% 2
          val <- val %/% 2
          i <- i - 1
        }
      }
      bin <- paste0(bin, collapse = "")

      return(bin)
    }) |> unlist()
  }

  return(temp)
}


#' Make an integer from a binary value
#'
#' @param x [`character(1)`][character]\cr the binary value (character of 0s and
#'   1s) for which to derive the integer.
#' @details Additional details...
#'
#' @examples
#' # example code
#'
#' @importFrom checkmate assertCharacter assertNames
#' @importFrom stringr str_split

.toInt <- function(x){

  assertCharacter(x = x)

  temp <- str_split(x, "")

  map(seq_along(temp), function(y){
    assertNames(x = temp[[y]], subset.of = c("1", "0"))

    sum(+(rev(temp[[y]]) == "1") * 2^(seq(temp[[y]])-1)) |>
      as.integer()
  }) |>
    unlist()
}

#' Make encoding for floating point values
#'
#' @param x description
#' @param precision [`integer(1)`][integer]\cr the number of digits that should
#'   be reliably represented, see Details.
#' @param range [`(1)`][]\cr the ratio between the smallest and largest possible value to be
#'   reliably represented, see Details.
#' @param opts [`list(.)`][list]\cr options to control the encoding of the
#'   binary representation of the numeric values, see Details.
#' @details \code{precision} modifies the significand \code{range} modifies the
#' exponent
#'
#' @examples
#' # example code
#'
#' @importFrom checkmate assertIntegerish assertLogical

.toEncoding <- function(x, precision = NULL, range = NULL, opts = NULL){

  assertIntegerish(x = precision, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = range, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertList(x = opts, null.ok = TRUE)

  sign <- exp <- signif <- bias <- NULL
  # implement also support for NaNs and Infs
  # study unum values that may allow more accurate and/or more highly compressed storage of information (https://de.wikipedia.org/wiki/Unum_(Zahlenformat))

  if(!is.null(opts)){
    assertNames(x = names(opts), subset.of = c("type", "significand", "exponent", "sign"))

    if(!is.null(opts$type)){
      assertChoice(x = opts$type, choices = c("half", "bfloat16", "tensor19", "fp24", "pxr24", "single", "double"))

      sign <- 1
      exp <- case_when(opts$type == "half" ~ 5,
                       opts$type == "bfloat16" ~ 8,
                       opts$type == "tensor19" ~ 8,
                       opts$type == "fp24" ~ 7,
                       opts$type == "pxr24" ~ 8,
                       opts$type == "single" ~ 8,
                       opts$type == "double" ~ 11
      )

      signif <- case_when(opts$type == "half" ~ 10,
                          opts$type == "bfloat16" ~ 7,
                          opts$type == "tensor19" ~ 10,
                          opts$type == "fp24" ~ 16,
                          opts$type == "pxr24" ~ 15,
                          opts$type == "single" ~ 23,
                          opts$type == "double" ~ 52
      )

      bias <- 2**(exp-1)-1

    } else {

      # implement here tests for whether the parameters make sense, and give errors if not
      if(!is.null(opts$sign)){
        assertChoice(x = opts$sign, choices = c(0L, 1L))
        sign <- opts$sign
      }

      if(!is.null(opts$exponent)){

        exp <- opts$exponent
      }

      if(!is.null(opts$significand)){

        signif <- opts$significand
      }

      bias <- 2**(exp-1)-1

    }

  }

  # https://blog.demofox.org/2017/11/21/floating-point-precision/

  # assess how likely precision values of (much) larger than 7 digits would be and discuss this in the paper. 7 is the boundary of float32 values
  # how about "machine precision" instead? https://fncbook.github.io/v1.0/intro/floating-point.html, i.e., the smallest values that can be mapped within a certain range

  if(is.null(sign)){
    sign <- ifelse(any(x < 0), 1L, 0L)
  }

  if(is.null(exp)){
    # range comes in here

  }
  if(is.null(signif)){
    # precision comes in here

    # x <- 329.390625
    # xInt <- as.integer(x)
    # signif <- ceiling(log2(max(xInt)))

  }

  if(is.null(bias)){
    bias <- 2**(exp-1)-1
  }

  out <- list(sign = as.integer(sign),
              exponent = as.integer(exp),
              significand = as.integer(signif),
              bias = as.integer(bias))

}
