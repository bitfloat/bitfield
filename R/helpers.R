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

  if(is.null(decimals)){
    if(!testIntegerish(x)){
      decimals <- suppressWarnings(max(nchar(x) - nchar(as.integer(x))-1, na.rm = TRUE))
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
      #
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

#' Determine and write MD5 sum
#'
#' @param x [`registry(1)`][registry]\cr registry for which to determine the MD5
#'   checksum.
#' @details This function follows the following algorithm: \itemize{
#'   \item set the current MD5 checksum to NA_character_,
#'   \item write the registry into the temporary directory,
#'   \item calculate the checksum of this file and finally
#'   \item store the checksum  in the md5 slot of the registry.}
#' This means that when comparing the MD5 checksum in this slot, one first has to set that value also to NULL, otherwise the two values won't coincide.
#' @return this function is called for its side-effect of storing the MD5 checksum in the md5 slot of the registry.
#' @importFrom checkmate assertClass
#' @importFrom tools md5sum
#' @export

.updateMD5 <- function(x){

  assertClass(x = x, classes = "registry")

  tempReg <- paste0(tempdir(), "/tempReg.rds")

  temp <- out <- x
  temp@md5 <- NA_character_
  saveRDS(object = temp, file = tempReg)

  tempSum <- md5sum(files = tempReg)
  names(tempSum) <- NULL

  unlink(x = tempReg)

  out@md5 <- tempSum

  return(out)
}


#' Extract values and metadata from terra::SpatRaster
#'
#' @param x [SpatRaster(1)][terra::SpatRaster]\cr the SpatRaster object.
#' @details This function simply extracts the values from \code{x} and appends
#'   the raster metadata as attributes.
#' @return the function that extracts values and metadata.
#' @importFrom checkmate assertClass
#' @importFrom terra values nrow ncol res ext crs
#' @export

.rast <- function(x){

  assertClass(x = x, classes = "SpatRaster")

  # Create the accessor function that will extract values when called
  accessor <- function() {
    values <- terra::values(x)

    # Return values along with metadata about the raster
    attr(values, "rast_meta") <- list(
      nrow = nrow(x),
      ncol = ncol(x),
      res = res(x),
      ext = ext(x),
      crs = crs(x)
    )

    return(values)
  }

  # Set class so bitfield operators can recognize this is a raster accessor
  class(accessor) <- c("bf_rast", "function")

  return(accessor)

}
