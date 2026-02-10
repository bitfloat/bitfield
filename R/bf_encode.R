#' Encode bit flags into a bitfield
#'
#' This function picks up the flags mentioned in a registry and encodes them as
#' integer values.
#' @param registry [`registry(1)`][registry]\cr the registry that should be
#'   encoded into a bitfield.
#' @return Depending on the registry template type: a \code{data.frame} with
#'   integer columns (one per 32-bit chunk) if template is a table, or a
#'   \code{SpatRaster} with integer layers if template is a raster.
#' @examples
#' reg <- bf_registry(name = "testBF", description = "test bitfield",
#'                    template = bf_tbl)
#' reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
#'
#' field <- bf_encode(registry = reg)
#'
#' # with raster data
#' library(terra)
#' bf_rst <- rast(nrows = 3, ncols = 3, vals = bf_tbl$commodity, names = "commodity")
#' bf_rst$yield <- rast(nrows = 3, ncols = 3, vals = bf_tbl$yield)
#'
#' reg <- bf_registry(name = "testBF", description = "raster bitfield",
#'                    template = bf_rst)
#' reg <- bf_map(protocol = "na", data = bf_rst, registry = reg, x = commodity)
#'
#' field <- bf_encode(registry = reg)  # returns a SpatRaster
#' @importFrom checkmate assertClass assertList
#' @importFrom tibble tibble
#' @importFrom purrr map map_int map2_chr
#' @importFrom dplyr bind_rows arrange bind_cols select across mutate
#' @importFrom stringr str_split str_split_i str_sub str_pad str_remove
#' @importFrom tidyr separate_wider_position unite
#' @importFrom rlang  `:=`
#' @importFrom terra rast ext crs
#' @export

bf_encode <- function(registry){

  assertClass(x = registry, classes = "registry")
  assertList(x = registry@flags, min.len = 1)

  # open the registry
  theBitfield <- tibble(.rows = registry@template$length)

  # arrange them by position of the bit ...
  theFlags <- bind_rows(map(seq_along(registry@flags), function(ix){
    tibble(pos = min(registry@flags[[ix]]$wasGeneratedBy$assignPosition),
           name = names(registry@flags)[ix])
  }))
  theFlags <- arrange(theFlags, pos)

  # ... and write into the registry
  for(i in seq_along(theFlags$name)){

    theName <- theFlags$name[i]
    theFlag <- registry@flags[[theName]]
    thisLen <- theFlag$wasGeneratedBy$encodeAsBinary$sign + theFlag$wasGeneratedBy$encodeAsBinary$exponent + theFlag$wasGeneratedBy$encodeAsBinary$significand
    theVals <- bf_flag(registry = registry, flag = theName)

    if(!is.logical(theVals)){

      # track NA positions before any conversion (for float types)
      naIdx <- is.na(theVals)

      # replace NAs with dummy value for .toBin processing
      # (these positions will be overwritten with NA pattern later for floats)
      theValsForBin <- theVals
      theValsForBin[naIdx] <- 1

      # get the integer part of the binary value
      intBits <- .toBin(x = as.integer(theValsForBin), pad = TRUE)

      if(!is.integer(theVals)){
        # good explanation: https://www.cs.cornell.edu/~tomf/notes/cps104/floating

        if(grepl(x = theFlag$wasGeneratedBy$useTest, pattern = "integer|case|category")) stop("you are trying to apply an integer protocol when the values are numeric.")

        # get encoding parameters
        signBit <- theFlag$wasGeneratedBy$encodeAsBinary$sign
        expBits <- theFlag$wasGeneratedBy$encodeAsBinary$exponent
        sigBits <- theFlag$wasGeneratedBy$encodeAsBinary$significand
        bias <- theFlag$wasGeneratedBy$encodeAsBinary$bias
        maxExp <- 2^expBits - 1

        # vectorized float encoding using math (not strings)
        n <- length(theValsForBin)
        absVals <- abs(theValsForBin)
        isZero <- absVals == 0

        # compute exponent via log2 for non-zero values
        actualExp <- rep(0L, n)
        actualExp[!isZero] <- floor(log2(absVals[!isZero]))

        # biased exponent
        biasedExp <- actualExp + bias

        # zero encodes as all-zero bits (exponent = 0, significand = 0)
        biasedExp[isZero] <- 0L

        # detect underflow and overflow
        underflow <- !isZero & biasedExp < 0
        overflow <- !isZero & biasedExp > maxExp

        # clamp exponents
        biasedExp[underflow] <- 0L
        biasedExp[overflow] <- maxExp

        # compute significand as integer value
        # normalize to [1, 2) then extract fractional part
        normalized <- rep(1, n)
        normalized[!isZero & !underflow] <- absVals[!isZero & !underflow] / 2^actualExp[!isZero & !underflow]
        fracPart <- normalized - 1
        sigVal <- floor(fracPart * 2^sigBits)

        # underflow: significand is 0
        sigVal[underflow] <- 0L
        # overflow: max significand but not all 1s (reserved for NA)
        sigVal[overflow] <- 2^sigBits - 2

        # convert exponent and significand integers to binary strings
        expBin <- .toBin(biasedExp, len = expBits)
        sigBin <- .toBin(sigVal, len = sigBits)

        theBits <- paste0(expBin, sigBin)

        # NA pattern: all 1s (like IEEE NaN)
        naPattern <- paste0(rep("1", expBits + sigBits), collapse = "")
        theBits[naIdx] <- naPattern

        # prepend sign bit if needed
        if (signBit == 1) {
          signBin <- as.integer(theValsForBin < 0)
          theBits <- paste0(signBin, theBits)
          # for NA values, sign bit is 0
          theBits[naIdx] <- paste0("0", naPattern)
        }

      } else {
        theBits <- intBits
      }

    } else {
      theBits <- as.integer(theVals)
    }

    # internal test that should never trigger
    if(any(nchar(theBits) > thisLen)) stop("the bit sequence is longer than it is allowed to be.")

    # add trailing 0s
    theBits <- str_pad(string = theBits, width = thisLen, side = "right", pad = "0")

    theBitfield <- bind_cols(theBitfield, tibble(!!paste0("flag", i) := theBits), .name_repair = "minimal")
  }

  tempBits <- unite(theBitfield, col = "bf_int", 1:ncol(theBitfield), sep = "")
  bitLen <- nchar(tempBits[[1]][1])
  intLen <- ceiling(bitLen / 32)
  widths <- NULL
  while(bitLen > 0){
    if(bitLen > 32){
      widths <- c(widths, 32)
    } else {
      widths <- c(widths, bitLen)
    }
    bitLen <- bitLen - 32
  }
  names(widths) <- paste0("bf_int", 1:intLen)

  tempBits <- separate_wider_position(data = tempBits, cols = "bf_int", widths = widths)

  out <- tempBits |>
    mutate(across(1:ncol(tempBits), .toDec))

  # convert to raster if template is a raster
 if(registry@template$type == "SpatRaster"){
    tmpl <- registry@template

    # determine the appropriate GeoTIFF datatype for each chunk
    maxWidth <- max(widths)
    if(maxWidth <= 8){
      dtype <- "INT1U"
    } else if(maxWidth <= 16){
      dtype <- "INT2U"
    } else {
      dtype <- "INT4U"
    }

    out <- rast(nrows = tmpl$nrows, ncols = tmpl$ncols,
                extent = ext(tmpl$extent), crs = tmpl$crs,
                nlyrs = ncol(out), vals = as.matrix(out),
                names = names(out))

    message("use `writeRaster(x, filename, datatype = '", dtype, "', gdal = 'COMPRESS=DEFLATE')` to preserve bitfield values when saving to disk")
  }

  return(out)

}
