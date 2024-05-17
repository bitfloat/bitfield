#' Decode (unpack) a bitfield
#'
#' @param x [`integerish(1)`][integer]\cr table of the integer representation of
#'   the bitfield.
#' @param registry  [`registry(1)`][registry]\cr the registry that should be
#'   used to decode the bitfield into a binary representation.
#' @param positions [`integerish(.)`][integer]\cr extract flags that are covered
#'   by those values into explicit data items.
#' @param sep [`character(1)`][character]\cr a symbol with which, if given, the
#'   distinct fields shall be separated.
#'
#' @importFrom checkmate assertDataFrame assertNames assertClass assertCharacter
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows arrange group_by ungroup summarise rowwise mutate
#'   left_join n first row_number
#' @importFrom tidyr separate unite separate_longer_delim
#' @importFrom rlang env_bind
#' @export

bf_decode <- function(x, registry, positions = NULL, sep = NULL){

  assertDataFrame(x = x, types = "integer", any.missing = FALSE)
  assertClass(x = registry, classes = "registry")
  assertCharacter(x = sep, len = 1, null.ok = TRUE)

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
    mutate(flags = row_number()-1) |>
    ungroup() |>
    rowwise() |>
    mutate(flag = .toBin(x = flags, len = bits)) |>
    select(bits = pos, name, flag, desc)

  # process bits
  tempBits <- NULL
  for(i in seq_along(x)){
    tempBits <- tibble(!!paste0("bin", i) := .toBin(x[[i]], len = registry@width)) |>
      bind_cols(tempBits)
  }
  out <- tempBits |> #identify how to combine several columns
    unite(col = "bin", everything(), sep = "") |>
    separate(col = bin, into = paste0("flag", theBits$split), sep = theBits$split)

  if(!is.null(sep)){
    out <- unite(out, col = "bf_bin", paste0("flag", theBits$split), sep = sep)
  } else {
    colnames(out)[-1] <- theBits$name
  }

  # assign look-up table to the environment as well
  env_bind(.env = bf_env, legend = lut)
  # make sure that also all the other items are placed in bf_env, if they are not there yet.

  print(lut)

  return(out)

}
