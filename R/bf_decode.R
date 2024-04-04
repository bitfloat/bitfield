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

  assertIntegerish(x = x, any.missing = FALSE, min.len = 1)
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
  tempTab <- theBits |>
    group_by(name) |>
    summarise(split = max(pos),
              pos = if_else(n() == 1, as.character(split), paste0(min(pos), ":", max(pos))),
              flags = max(flags),
              bits = max(bits),
              desc = first(desc)) |>
    arrange(split)

  # process bits
  out <- .toBin(x = x) |>
    separate(col = bit, into = paste0("b", tempTab$split), sep = tempTab$split)

    # rowwise(tibble(bf_int = x)) |>
    # mutate(bit = .toBin(x = bf_int)) |> # fix here

  if(!is.null(sep)){
    out <- unite(out, col = "bf_binary", paste0("b", tempTab$split), sep = sep)
  } else {
    colnames(out)[-1] <- tempTab$name
  }

  # create look-up table for what the bits stand for
  lut <- separate_longer_delim(data = tempTab, cols = desc, delim = " | ")
  lut <- group_by(lut, name)
  lut <- mutate(lut, flags = row_number()-1)
  lut <- ungroup(lut)
  lut <- rowwise(lut)
  # lut <- mutate(lut, flag = .makeFlag(x = flags, len = bits, rev = TRUE))
  lut <- ungroup(lut)
  lut <- select(lut, bits = pos, name, flag, desc)

  # assign look-up table to the environment as well
  env_bind(.env = bf_env, legend = lut)

  print(lut)

  return(out)

}
