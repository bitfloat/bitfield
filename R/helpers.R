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
#' @importFrom checkmate assertCharacter assertNames testIntegerish
#' @importFrom stringr str_split

.toDec <- function(x){

  assertCharacter(x = x, any.missing = FALSE)

  out <- map(.x = x, .f = function(ix){

    temp <- str_split(ix, "", simplify = TRUE)
    radix <- which(temp == ".")
    if(length(radix) == 0){
      radix <- length(temp)+1
      bits <- as.integer(temp)
    } else {
      assertIntegerish(x = radix, any.missing = FALSE, len = 1)
      bits <- as.integer(temp[-radix])
    }
    assertSubset(x = bits, choices = c(0, 1))

    sum(bits * 2^((seq(bits) * -1) + radix-1))
  }) |> unlist()

  # if it's integerish, convert it to integer
  if(testIntegerish(out)){
    out <- as.integer(out)
  }

  return(out)
}

#' Determine encoding for floating point values
#'
#' @param x [`numeric(.)`][numeric]\cr a set of numeric values for which to
#'   determine the floating point encoding.
#' @param precision [`character(1)`][character]\cr option that determines the
#'   configuration of the floating point encoding. Possible values are
#'   \code{"half"} \[1.5.10\], \code{"bfloat16"} \[1.8.7\], \code{"tensor19"}
#'   \[1.8.10\], \code{"fp24"} \[1.7.16\], \code{"pxr24"} \[1.8.15\],
#'   \code{"single"} \[1.8.23\], \code{"double"} \[1.11.52\] and \code{"auto"}
#'   (where the positions are determined based on the provided numeric values in
#'   \code{x}; not supported yet).
#' @param decimals [`integer(1)`][integer]\cr the number of decimal digits that
#'   should be reliably represented, not supported yet.
#' @param range [`numeric(2)`][numeric]\cr the ratio between the smallest and
#'   largest possible value to be reliably represented, not supported yet.
#' @param fields [`list(3)`][list]\cr list that controls how many bits are
#'   allocated to \code{sign}, \code{exponent} and \code{mantissa} for encoding
#'   the numeric values.
#' @details For background information study for instance
#'   \href{https://www.cs.cornell.edu/~tomf/notes/cps104/floating}{'Floating
#'   Point' by Thomas Finley} and check out
#'   \href{https://float.exposed/}{https://float.exposed/} to play around with
#'   floating point encoding.
#' @importFrom checkmate assertNumeric assertIntegerish assertList assert
#'   testNull testIntegerish assertNames assertChoice
#' @importFrom dplyr case_when

.determineEncoding <- function(x, precision = "single", decimals = NULL,
                               range = NULL, fields = NULL){

  assertNumeric(x = x)
  assertIntegerish(x = decimals, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = range, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = precision, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertList(x = fields, null.ok = TRUE)
  assert(!testNull(x = precision), !testNull(x = fields))

  # - study unum values that may allow more accurate and/or more highly
  #   compressed storage of information (https://de.wikipedia.org/wiki/Unum_(Zahlenformat))
  # - https://blog.demofox.org/2017/11/21/floating-point-precision/
  # - assess how likely digits values of (much) larger than 7 digits would be and
  #   discuss this in the paper. 7 is the boundary of float32 values
  # - how about "machine digits" instead? https://fncbook.github.io/v1.0/intro/floating-point.html,
  #   i.e., the smallest values that can be mapped within a certain range
  # - https://trekhleb.dev/static/131599cc123194bea250477696211e3a/78ed4/02-half-precision-floating-point-number-explained.png
  # - how to encode -27.15625 in half-precision and also in a least-bits usage form

  if(is.null(decimals)){
    if(!testIntegerish(x)){
      decimals <- max(nchar(x) - nchar(as.integer(x))-1)
    } else {
      decimals <- 0
    }
  }

  if(is.null(range)){
    range <- 0L
  }

  if(!is.null(fields)){
    assertNames(x = names(fields), must.include = c("sign", "exponent", "mantissa"))

    assertChoice(x = fields$sign, choices = c(0L, 1L))
    sign <- fields$sign
    exp <- fields$exponent
    mant <- fields$mantissa

    precision <- NULL
  }

  if(!is.null(precision)){
    assertChoice(x = precision, choices = c("half", "bfloat16", "tensor19", "fp24", "pxr24", "single", "double", "auto"))

    if(precision == "auto"){
      stop("work in process, please check in later to see whether this has been implemented!")
      # sign <- ifelse(any(x < 0), 1L, 0L)
      #
      # minBits <- max(ceiling(log2(max(min(abs(x)), 1))), 1) # if the smallest value is smaller than 1, take 1 as value
      # maxBits <- ceiling(log2(max(x)))
      # expRange <- maxBits - minBits + 1
      # # how many different exponents (n) do I have? If I have more exponents than
      # # 0:n, for instance only from 3:n (because there are no values with exponent
      # # 2^0, 2^1 and 2^2), I do need to adapt the bias to offset the bit-encoded
      # # exponent values so they start at 0 and thus only take n-3 bit combinations,
      # # which can be drastically less than for 0:n. A practical example would be if
      # # I have numbers that are several magnitudes larger than 0 but don't include 0,
      # # let's say from 7'000'000 to 10'000'000. They would have the exponents 10^6
      # # and 10^7 (i.e., 7.0 * 10^6 and 1.0 * 10^7). If I encode the range from 10^0
      # # to 10^7, I need three bits, whereas if I encode the range from 10^6 to 10^7,
      # # I need only two bits. This obviously compounds with larger ranges, and even
      # # more so in base2, in which I have to consider the ranges actually.
      # exp <- expRange
      #
      # xInt <- x * 10^decimals
      # xInt <- as.integer(round(xInt))
      # mant <- ceiling(log2(max(xInt)))

    } else {

      sign <- 1
      exp <- case_when(precision == "half" ~ 5,
                       precision == "bfloat16" ~ 8,
                       precision == "tensor19" ~ 8,
                       precision == "fp24" ~ 7,
                       precision == "pxr24" ~ 8,
                       precision == "single" ~ 8,
                       precision == "double" ~ 11)

      mant <- case_when(precision == "half" ~ 10,
                        precision == "bfloat16" ~ 7,
                        precision == "tensor19" ~ 10,
                        precision == "fp24" ~ 16,
                        precision == "pxr24" ~ 15,
                        precision == "single" ~ 23,
                        precision == "double" ~ 52)
    }
  }

  # implement here some tests that ensure the selected values don't fully cripple the input

  out <- list(sign = as.integer(sign),
              exponent = as.integer(exp),
              mantissa = as.integer(mant),
              bias = as.integer(2**(exp-1)-1))

  return(out)
}
