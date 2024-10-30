#' Make a binary value from an integer
#'
#' @param x [`numeric(1)`][numeric]\cr the numeric value for which to derive the
#'   binary value.
#' @param len [`integerish(1)`][integer]\cr the number of bits used to capture
#'   the value.
#' @param dec [`logical(1)`][logical]\cr whether to transform the decimal part
#'   to bits, or the integer part.
#' @param pad [`logical(1)`][logical]\cr whether to pad the binary value with 0
#'   values.
#' @details Additional details...
#'
#' @examples
#' # example code
#'
#' @importFrom checkmate assertIntegerish assertNumeric
#' @importFrom stringr str_pad

.toBin <- function(x, len = NULL, pad = TRUE, dec = FALSE){

  assertNumeric(x = x)
  assertIntegerish(x = len, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = dec, len = 1, any.missing = FALSE)
  assertLogical(x = pad, len = 1, any.missing = FALSE)

  # if(!testIntegerish(x = x)){
  #   dec <- as.numeric(paste0(0, ".", str_split(x, "[.]", simplify = T)[,2]))
  # } else {
  #   dec <- NULL
  # }
  # int <- as.integer(x)
  #
  # out <- map(.x = int, .f = function(ix){
  #   val <- ix
  #   bin <- NULL
  #   # if(!is.null(len)){
  #   #
  #   #   while(len > 0){
  #   #     bin[len] <- val %% 2
  #   #     val <- val %/% 2
  #   #     len <- len - 1
  #   #   }
  #   #
  #   # } else {
  #
  #     while(val > 0){
  #       bin <- c(val %% 2, bin)
  #       val <- val %/% 2
  #     }
  #
  #     if(is.null(bin)) bin <- 0
  #
  #   # }
  #
  #   bin <- paste0(bin, collapse = "")
  #
  #   return(bin)
  # }) |> unlist()
  #
  # if(pad){
  #   out <- str_pad(out, width = max(nchar(out)), pad = "0")
  # }
  #
  # if(!is.null(dec)){
  #
  #   temp <- map(.x = dec, .f = function(ix){
  #     val <- ix
  #     bin <- NULL
  #
  #     if(is.null(len)){
  #
  #       while(val > 0){
  #         val <- val * 2
  #         bin <- c(bin, val %/% 1)
  #         val <- val - val %/% 1
  #       }
  #
  #     } else {
  #
  #       i <- 0
  #       while(val > 0 & i < len){
  #         val <- val * 2
  #         bin <- c(bin, val %/% 1)
  #         val <- val - val %/% 1
  #         i <-  i + 1
  #       }
  #
  #     }
  #     bin <- paste0(bin, collapse = "")
  #
  #     return(bin)
  #   }) |> unlist()
  #
  #   out <- paste0(out, ".", temp)
  #
  # }

  if(dec){
    x <- as.numeric(paste0(0, ".", str_split(x, "[.]", simplify = T)[,2]))

    temp <- map(.x = x, .f = function(ix){
      val <- ix
      bin <- NULL

      if(is.null(len)){

        while(val > 0){
          val <- val * 2
          bin <- c(bin, val %/% 1)
          val <- val - val %/% 1
        }

      } else {

        i <- 0
        while(val > 0 & i < len){
          val <- val * 2
          bin <- c(bin, val %/% 1)
          val <- val - val %/% 1
          i <-  i + 1
        }

      }
      bin <- paste0(bin, collapse = "")

      return(bin)
    }) |> unlist()

  } else {
    x <- as.integer(x)

    temp <- map(.x = x, .f = function(ix){
      val <- ix
      bin <- NULL
      if(!is.null(len)){

        while(len > 0){
          bin[len] <- val %% 2
          val <- val %/% 2
          len <- len - 1
        }

      } else {

        while(val > 0){
          bin <- c(val %% 2, bin)
          val <- val %/% 2
        }

        if(is.null(bin)) bin <- 0

      }

      bin <- paste0(bin, collapse = "")

      return(bin)
    }) |> unlist()

    if(pad){
      temp <- str_pad(temp, width = max(nchar(temp)), pad = "0")
    }
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
#' @param decimals [`integer(1)`][integer]\cr the number of decimal digits that should
#'   be reliably represented, see Details.
#' @param range [`(1)`][]\cr the ratio between the smallest and largest possible value to be
#'   reliably represented, see Details.
#' @param opts [`list(.)`][list]\cr options to control the encoding of the
#'   binary representation of the numeric values, see Details.
#' @details \code{digits} modifies the significand \code{range} modifies the
#' exponent
#'
#' @examples
#' # example code
#'
#' @importFrom checkmate assertIntegerish assertLogical
#' @importFrom dplyr case_when

.setEncoding <- function(x, decimals = NULL, range = NULL, opts = NULL){

  assertIntegerish(x = decimals, len = 1, any.missing = FALSE, null.ok = TRUE)
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

  # assess how likely digits values of (much) larger than 7 digits would be and
  # discuss this in the paper. 7 is the boundary of float32 values
  # how about "machine digits" instead? https://fncbook.github.io/v1.0/intro/floating-point.html, i.e., the smallest values that can be mapped within a certain range

  # https://trekhleb.dev/static/131599cc123194bea250477696211e3a/78ed4/02-half-precision-floating-point-number-explained.png
  # how to encode -27.15625 in half-precision and also in a least-bits usage form

  # x <- rnorm(10, 1)
  # x <- c(x[4], x[10], 329.390625)
  # x[2] <- x[2] * -1
  if(is.null(sign)){
    sign <- ifelse(any(x < 0), 1L, 0L)
  }
  if(is.null(exp)){
    minBits <- max(ceiling(log2(max(min(abs(x)), 1))), 1) # if the smallest value is smaller than 1, take 1 as value
    maxBits <- ceiling(log2(max(x)))
    expRange <- maxBits - minBits + 1
    # how many different exponents (n) do I have? If I have more exponents than
    # 0:n, for instance only from 3:n (because there are no values with exponent
    # 2^0, 2^1 and 2^2), I do need to adapt the bias to offset the bit-encoded
    # exponent values so they start at 0 and thus only take n-3 bit combinations,
    # which can be drastically less than for 0:n. A practical example would be if
    # I have numbers that are several magnitudes larger than 0 but don't include 0,
    # let's say from 7'000'000 to 10'000'000. They would have the exponents 10^6
    # and 10^7 (i.e., 7.0 * 10^6 and 1.0 * 10^7). If I encode the range from 10^0
    # to 10^7, I need three bits, whereas if I encode the range from 10^6 to 10^7,
    # I need only two bits. This obviously compounds with larger ranges, and even
    # more so in base2, in which I have to consider the ranges actually.

    exp <- expRange
  }

  if(is.null(signif)){
    xInt <- x * 10^decimals
    xInt <- as.integer(round(xInt))
    signif <- ceiling(log2(max(xInt)))

    # signif <- 10
  }

  if(is.null(bias)){

    if(minBits != 0){

      bias <- minBits - expRange
      # bias <- bias needs to be adapted so that when the minimum 'exp' is not 0, it becomes 0 when combined with the bias. This allows me to store large values, as described above with a small range (i.e., without 0 or other small values in x)

      # bla <- tibble(bit = c("00", "01", "11", "000", "001", "010", "011", "100", "101", "110", "111"),
      #               biased = c(-1, 0, 1, -3, -2, -1, 0, 1, 2, 3, 4),
      #               needed = c(0, 1, 2, 0, 1, 2, 3, 4, 5, 6, 7))

    } else {
      bias <- 2**(exp-1)-1
    }

  }

  out <- list(sign = as.integer(sign),
              exponent = as.integer(exp),
              significand = as.integer(signif),
              bias = as.integer(bias))

}
