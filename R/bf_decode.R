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
#' @examples
#' # build registry
#' reg <- bf_map(protocol = "na", data = bf_tbl, x = commodity)
#' reg <- bf_map(protocol = "matches", data = bf_tbl, x = commodity, set = c("soybean", "maize"),
#'               registry = reg)
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
#' @importFrom tidyselect everything
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
    tibble(pos = registry@flags[[ix]]$position,
           name = names(registry@flags)[ix],
           flags = length(registry@flags[[ix]]$values),
           bits = length(registry@flags[[ix]]$position),
           desc = paste0(registry@flags[[ix]]$description, collapse = " | "))
  }) |>
    bind_rows() |>
    arrange(pos)

  # ... and the positions where they should be split
  theBits <- theBits |>
    group_by(name) |>
    summarise(split = max(pos),
              pos = if_else(n() == 1, as.character(split), paste0(min(pos), ":", max(pos))),
              flags = max(flags),
              bits = max(bits),
              desc = first(desc)) |>
    arrange(split)

  # create look-up table for what the bits stand for
  lut <- separate_longer_delim(data = theBits, cols = desc, delim = " | ") |>
    group_by(name) |>
    mutate(flags = row_number()-1,
           grp = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(flag = if_else(grp == 1, paste0(rep("x", bits), collapse = ""), .toBin(x = flags, len = bits))) |>
    select(pos, name, flag, desc)

  # process bits
  tempBits <- NULL
  for(i in seq_along(x)){
    tempBits <- tibble(!!paste0("bin", i) := .toBin(x[[i]], len = registry@width)) |>
      bind_cols(tempBits)
  }
  tempOut <- tempBits |>
    unite(col = "bin", everything(), sep = "") |>
    separate(col = bin, into = paste0("flag", theBits$split), sep = theBits$split)

  # decode also the bit values
  if(!is.null(flags)){
    assertSubset(x = flags, choices = names(registry@flags))
    theFlags <- flags
  } else {
    theFlags <- names(registry@flags)
  }

  for(i in seq_along(theFlags)){

    flagName <- theFlags[i]
    theFlag <- registry@flags[[flagName]]
    flagEnc <- theFlag$encoding

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
      mantissa <- map_chr(seq_along(flagSplit), function(ix){
        paste0("1", flagSplit[[ix]][3])
      })

      temp <- .toDec(x = str_replace(mantissa, pattern = paste0("^(.{", exponent + 1, "})(.*)$"), replacement = "\\1.\\2"))
    }

    env_bind(.env = bf_env, !!flagName := temp)
  }

  if(!is.null(sep)){
    out <- unite(tempOut, col = "bf_bin", paste0("flag", theBits$split), sep = sep)
  } else {
    colnames(tempOut) <- theFlags
    out <- tempOut
  }

  # assign look-up table to the environment as well
  env_bind(.env = bf_env, legend = lut)

  if(verbose) print(lut)

  return(out)
}
