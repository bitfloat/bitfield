#' Unpack a bitfield
#'
#' @param x [`integerish(1)`][integer]\cr table of the integer representation of
#'   the bitfield.
#' @param registry  [`registry(1)`][registry]\cr the registry that should be
#'   used to unpack the bitfield into a binary representation.
#' @param sep the value by which the distinct fields shall be separated
#'
#' @importFrom checkmate assertDataFrame assertNames assertClass assertCharacter
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows arrange group_by summarise rowwise mutate
#'   left_join n
#' @importFrom tidyr separate unite
#' @export

bf_unpack <- function(x, registry, sep = "."){

  assertIntegerish(x = x, any.missing = FALSE, min.len = 1)
  assertClass(x = registry, classes = "registry")
  assertCharacter(x = sep, len = 1, any.missing = FALSE)

  .insertSep <- function(x, pos, insert){
    gsub(paste0("^(.{", pos, "})(.*)$"),
         paste0("\\1", insert, "\\2"),
         x)
  }

  # get the bits ...
  theBits <- bind_rows(map(seq_along(registry@flags), function(ix){
    tibble(pos = registry@flags[[ix]]$position,
           name = names(registry@flags)[ix],
           flags = length(registry@flags[[ix]]$values))
  }))
  theBits <- arrange(theBits, pos)

  # ... and the positions where they should be split
  tempTab <- theBits
  tempTab <- group_by(theBits, name)
  tempTab <- summarise(tempTab,
                       flags = max(flags),
                       split = max(pos),
                       pos = if_else(n() == 1, as.character(split), paste0(min(pos), ":", max(pos)))
  )
  tempTab <- arrange(tempTab, split)

  # process bits
  out <- rowwise(tibble(bf_int = x))
  out <- mutate(out, bit = .makeFlag(x = bf_int, len = registry@width))
  out <- separate(out, col = bit, into = paste0("b", tempTab$split), sep = tempTab$split)
  out <- unite(out, col = "bf_binary", paste0("b", tempTab$split), sep = sep)

  # create look-up table for what the bits stand for
  lut <- mutate(tempTab, split = as.character(split))
  lut <- left_join(lut, registry@desc, c("split" = "pos"))
  lut <- select(lut, -split)

  # assign look-up table to the environment as well
  env_bind(.env = bf_env, legend = lut)

  print(lut)

  return(out)

}
