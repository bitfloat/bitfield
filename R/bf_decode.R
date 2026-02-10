#' Decode (unpack) a bitfield
#'
#' This function takes an integer bitfield and the registry used to build it
#' upstream to decode it into bit representation and thereby unpack the data
#' stored in the bitfield.
#' @param x integer table or raster of the bitfield. For registries with a
#'   \code{SpatRaster} template, \code{x} should be a \code{SpatRaster}. For
#'   registries with a \code{data.frame} template, \code{x} should be a
#'   \code{data.frame}.
#' @param registry [`registry(1)`][registry]\cr the registry that should be used
#'   to decode the bitfield.
#' @param flags [`character(.)`][character]\cr the name(s) of flags to extract
#'   from this bitfield; leave at \code{NULL} to extract the full bitfield.
#' @param envir [`environment(1)`][environment]\cr optional environment to store
#'   decoded flags as individual objects. If \code{NULL} (default), returns
#'   results as a list or SpatRaster. Use \code{.GlobalEnv} to store flags
#'   directly in the workspace.
#' @param verbose [`logical(1)`][logical]\cr whether or not to print the
#'   registry legend.
#' @return Depending on the registry template type and \code{envir} parameter:
#'   If \code{envir} is \code{NULL}, returns a named \code{list} with decoded
#'   values for table templates, or a multi-layer \code{SpatRaster} for raster
#'   templates. If \code{envir} is specified, stores decoded flags as individual
#'   objects in that environment and returns \code{invisible(NULL)}.
#' @examples
#' # build registry
#' reg <- bf_registry(name = "testBF", description = "test bitfield",
#'                    template = bf_tbl)
#' reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = commodity)
#' reg <- bf_map(protocol = "matches", data = bf_tbl, registry = reg,
#'               x = commodity, set = c("soybean", "maize"), na.val = FALSE)
#' reg
#'
#' # encode the flags into a bitfield
#' field <- bf_encode(registry = reg)
#' field
#'
#' # decode (somewhere downstream) - returns a named list
#' decoded <- bf_decode(x = field, registry = reg)
#' decoded$na_commodity
#' decoded$matches_commodity
#'
#' # alternatively, store directly in global environment
#' bf_decode(x = field, registry = reg, envir = .GlobalEnv, verbose = FALSE)
#' na_commodity
#' matches_commodity
#'
#' # with raster data
#' library(terra)
#' bf_rst <- rast(nrows = 3, ncols = 3, vals = bf_tbl$commodity, names = "commodity")
#' bf_rst$yield <- rast(nrows = 3, ncols = 3, vals = bf_tbl$yield)
#'
#' reg <- bf_registry(name = "testBF", description = "raster bitfield",
#'                    template = bf_rst)
#' reg <- bf_map(protocol = "na", data = bf_rst, registry = reg, x = commodity)
#' field <- bf_encode(registry = reg)
#'
#' # decode back to multi-layer raster
#' decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)
#' decoded  # SpatRaster with one layer per flag
#' @importFrom checkmate assertDataFrame assertNames assertClass assertCharacter
#'   assertLogical assertSubset
#' @importFrom purrr map map_dbl map_chr map_int
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows arrange group_by ungroup summarise rowwise mutate
#'   left_join n first row_number if_else
#' @importFrom tidyr separate unite separate_longer_delim
#' @importFrom rlang env_bind `:=`
#' @importFrom stringr str_sub_all str_replace
#' @importFrom terra rast ext crs values nlyr readStart readStop
#' @export

bf_decode <- function(x, registry, flags = NULL, envir = NULL, verbose = TRUE){

  assertClass(x = registry, classes = "registry")
  assertClass(x = envir, classes = "environment", null.ok = TRUE)
  assertLogical(x = verbose, len = 1, any.missing = FALSE)

  # handle raster input
  isRaster <- inherits(x, "SpatRaster")
  if(registry@template$type == "SpatRaster"){
    tmpl <- registry@template
    if(!isRaster){
      stop("registry template is 'SpatRaster' but 'x' is not a SpatRaster")
    }
    # extract values as doubles via the C++ pointer to avoid Terra's
    # as.integer() coercion, which destroys INT4U values > 2^31-1
    readStart(x)
    v <- x@pntr$readValues(0, nrow(x), 0, ncol(x))
    readStop(x)
    x <- data.frame(matrix(v, ncol = nlyr(x)))
    names(x) <- paste0("bf_int", seq_len(ncol(x)))
  } else {
    if(isRaster){
      stop("registry template is 'data.frame' but 'x' is a SpatRaster")
    }
  }

  # allow numeric types since large bitfields (>31 bits) may exceed signed 32-bit integer range
  assertDataFrame(x = x, types = c("integer", "numeric"), any.missing = FALSE)

  # get the bits ...
  theBits <- map(.x = seq_along(registry@flags), .f = function(ix){
    tibble(pos = registry@flags[[ix]]$wasGeneratedBy$assignPosition[1],
           length = registry@flags[[ix]]$wasGeneratedBy$assignPosition[2] - registry@flags[[ix]]$wasGeneratedBy$assignPosition[1],
           name = names(registry@flags)[ix],
           desc = paste0(registry@flags[[ix]]$comment, collapse = " | "))
  }) |>
    bind_rows() |>
    arrange(pos) |>
    mutate(rn = row_number())

  # create look-up table for what the bits stand for
  lut <- separate_longer_delim(data = theBits, cols = desc, delim = " | ") |>
    select(pos, name, desc)

  # calculate bit widths for each integer column (same logic as bf_encode)
  bitLen <- registry@template$width
  intLen <- ncol(x)
  widths <- NULL
  tempLen <- bitLen
  while(tempLen > 0){
    if(tempLen > 32){
      widths <- c(widths, 32)
    } else {
      widths <- c(widths, tempLen)
    }
    tempLen <- tempLen - 32
  }

  tempBits <- NULL
  for(i in seq_along(x)){
    tempBits <- bind_cols(tempBits, tibble(!!paste0("bin", i) := .toBin(x[[i]], len = widths[i])))
  }
  tempOut <- tempBits |>
    unite(col = "bin", 1:ncol(tempBits), sep = "") |>
    separate(col = bin, into = paste0("flag", theBits$rn), sep = c(theBits$pos + theBits$length))

  if(!is.null(flags)){
    assertSubset(x = flags, choices = names(registry@flags))
    theFlags <- flags
    tempOut <- tempOut[which(names(registry@flags) %in% flags)]
  } else {
    theFlags <- names(registry@flags)
  }

  # collect decoded values
  out <- list()

  for(i in seq_along(theFlags)){

    flagName <- theFlags[i]
    theFlag <- registry@flags[[flagName]]
    flagEnc <- theFlag$wasGeneratedBy$encodeAsBinary

    # get NA substitute value
    naVal <- theFlag$wasGeneratedBy$substituteValue

    if(flagName == "cases"){
      temp <- .toDec(x = tempOut[[i]]) + 1
    } else if(flagEnc$bias == 0){
      temp <- .toDec(x = tempOut[[i]])

      # reverse auto-scaling if scale parameters exist in provenance
      flagScale <- flagEnc$scale
      if (!is.null(flagScale)) {
        maxInt <- 2L^flagEnc$significand - 1L
        temp <- temp / maxInt * (flagScale$max - flagScale$min) + flagScale$min
      }
    } else {
      flagPos <- unlist(flagEnc)[1:3]
      flagSplit <- str_sub_all(tempOut[[i]], start = cumsum(c(1, flagPos[-length(flagPos)])), end = cumsum(flagPos))

      # NA pattern: all 1s in exponent and significand
      naPattern <- paste0(rep("1", flagPos[2] + flagPos[3]), collapse = "")

      temp <- map_dbl(seq_along(flagSplit), function(ix){
        parts <- flagSplit[[ix]]
        expBits <- parts[2]
        sigBits <- parts[3]

        # check for NA: all exponent and significand bits are 1
        if (!grepl("0", paste0(expBits, sigBits))) {
          return(NA_real_)
        }

        # check for zero: all exponent and significand bits are 0
        if (!grepl("1", paste0(expBits, sigBits))) {
          return(0)
        }

        # sign
        if (flagPos[1] == 0) {
          sgn <- 1
        } else {
          sgn <- (-1)^as.integer(parts[1])
        }

        # exponent
        exp <- .toDec(x = expBits) - flagEnc$bias

        # significand with implicit leading 1
        sig <- paste0("1", sigBits)

        # handle radix point placement based on exponent
        if (exp + 1 > 0) {
          # positive radix position: split the significand
          sigWithRadix <- str_replace(sig, pattern = paste0("^(.{", exp + 1, "})(.*)$"), replacement = "\\1.\\2")
        } else {
          # negative radix position: prepend zeros after radix
          sigWithRadix <- paste0("0.", paste0(rep("0", -(exp + 1)), collapse = ""), sig)
        }

        sgn * .toDec(x = sigWithRadix)
      })
    }

    if(registry@template$type == "SpatRaster"){
      out[[flagName]] <- rast(nrows = tmpl$nrows, ncols = tmpl$ncols,
                              extent = ext(tmpl$extent), crs = tmpl$crs,
                              vals = temp,
                              names = flagName)
      # restore levels/attribute table for categorical data
      flagProtocol <- strsplit(theFlag$wasGeneratedBy$useTest, "_")[[1]][1]
      if(flagProtocol %in% c("category", "case")){
        flagLevels <- theFlag$wasGeneratedBy$extractLevels
        if(!is.null(flagLevels) && nrow(flagLevels) > 0){
          # adjust levels to match 0-indexed decoded values
          flagLevels$id <- flagLevels$id - min(flagLevels$id)
          levels(out[[flagName]]) <- flagLevels
        }
      }
    } else {
      out[[flagName]] <- temp
    }
  }

  if(verbose) print(lut)

  # combine raster layers into single SpatRaster
  if(registry@template$type == "SpatRaster"){
    out <- rast(out)
  }

  if(!is.null(envir)){
    for(name in names(out)){
      env_bind(.env = envir, !!name := out[[name]])
    }
    env_bind(.env = envir, bf_legend = lut)
    return(invisible(NULL))
  }

  attr(out, "legend") <- lut
  return(out)
}
