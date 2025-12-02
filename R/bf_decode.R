#' Decode (unpack) a bitfield
#'
#' This function takes an integer bitfield and the registry used to build it
#' upstream to decode it into bit representation and thereby unpack the data
#' stored in the bitfield.
#' @param x [`integerish(1)`][integer]\cr table of the integer representation of
#'   the bitfield.
#' @param registry [`registry(1)`][registry]\cr the registry that should be used
#'   to decode the bitfield.
#' @param flags [`character(.)`][character]\cr the name(s) of flags to extract
#'   from this bitfield; leave at \code{NULL} to extract the full bitfield.
#' @param sep [`character(1)`][character]\cr a symbol with which, if given, the
#'   distinct fields shall be separated.
#' @param verbose [`logical(1)`][logical]\cr whether or not to return the
#'   registry legend.
#' @return data.frame with the binary values of flags in the registry in columns.
#' @examples
#' # build registry
#' reg <- bf_registry(name = "testBF", description = "test bitfield")
#' reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = commodity)
#' reg <- bf_map(protocol = "matches", data = bf_tbl, registry = reg,
#'               x = commodity, set = c("soybean", "maize"))
#' reg
#'
#' # encode the flags into a bitfield
#' field <- bf_encode(registry = reg)
#' field
#'
#' # decode (somewhere downstream)
#' flags <- bf_decode(x = field, registry = reg, sep = "-")
#' flags
#'
#' # more reader friendly
#' cbind(bf_tbl, bf_decode(x = field, registry = reg, verbose = FALSE))
#' @importFrom checkmate assertDataFrame assertNames assertClass assertCharacter
#'   assertLogical assertSubset
#' @importFrom purrr map map_dbl map_chr
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows arrange group_by ungroup summarise rowwise mutate
#'   left_join n first row_number if_else
#' @importFrom tidyr separate unite separate_longer_delim
#' @importFrom rlang env_bind `:=`
#' @importFrom stringr str_sub_all str_replace
#' @export

bf_decode <- function(x, registry, flags = NULL, sep = NULL, verbose = TRUE){

  assertDataFrame(x = x, types = "integer", any.missing = FALSE)
  assertClass(x = registry, classes = "registry")
  assertCharacter(x = sep, len = 1, null.ok = TRUE)
  assertLogical(x = verbose, len = 1, any.missing = FALSE)

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

  tempBits <- NULL
  for(i in seq_along(x)){
    tempBits <- tibble(!!paste0("bin", i) := .toBin(x[[i]], len = registry@width)) |>
      bind_cols(tempBits)
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

  for(i in seq_along(theFlags)){

    flagName <- theFlags[i]
    theFlag <- registry@flags[[flagName]]
    flagEnc <- theFlag$wasGeneratedBy$encodeAsBinary

    if(flagName == "cases"){
      temp <- .toDec(x = tempOut[[i]]) + 1
    } else if(flagEnc$bias == 0){
      temp <- .toDec(x = tempOut[[i]])
    } else {
      flagPos <- unlist(flagEnc)[1:3]
      flagSplit <- str_sub_all(tempOut[[i]], start = cumsum(c(1, flagPos[-length(flagPos)])), end = cumsum(flagPos))
      sign <- map_dbl(flagSplit, function(ix){
        (-1)^as.integer(ix[1])
      })
      exponent <- map_int(flagSplit, function(ix){
        .toDec(x = ix[2]) - flagEnc$bias
      })
      significand <- map_chr(seq_along(flagSplit), function(ix){
        paste0("1", flagSplit[[ix]][3])
      })

      temp <- sign * .toDec(x = str_replace(significand, pattern = paste0("^(.{", exponent + 1, "})(.*)$"), replacement = "\\1.\\2"))
    }

    env_bind(.env = .GlobalEnv, !!flagName := temp)
  }

  if(!is.null(sep)){
    out <- unite(tempOut, col = "bf_bin", paste0("flag", theBits$rn), sep = sep)
  } else {
    colnames(tempOut) <- theFlags
    out <- tempOut
  }

  # assign look-up table to the environment as well
  env_bind(.env = .GlobalEnv, bf_legend = lut)

  if(verbose) print(lut)

  return(out)
}
