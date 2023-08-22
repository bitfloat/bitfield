#' Unpack the integer representation of QB
#'
#' @param x description
#' @param bitfield  [`bitfield(1)`][bitfield]\cr the bitfield that should be
#'   unpacked into a quality bit (QB).
#' @param sep the value by which the distinct fields shall be separated
#'
#' @importFrom checkmate assertDataFrame assertNames assertClass assertCharacter
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows arrange group_by summarise rowwise mutate
#'   left_join n
#' @importFrom tidyr separate unite
#' @export

qb_unpack <- function(x, bitfield, sep = "."){

  assertDataFrame(x = x, ncols = 1)
  assertNames(x = names(x), identical.to = "QB")
  assertClass(x = bitfield, classes = "bitfield")
  assertCharacter(x = sep, len = 1, any.missing = FALSE)

  .insertSep <- function(x, pos, insert){
    gsub(paste0("^(.{", pos, "})(.*)$"),
         paste0("\\1", insert, "\\2"),
         x)
  }

  # get the bits ...
  theBits <- map(seq_along(bitfield@bits), function(ix){
    tibble(pos = bitfield@bits[[ix]]$position,
           name = names(bitfield@bits)[ix],
           flags = length(bitfield@bits[[ix]]$values))
  }) %>%
    bind_rows() %>%
    arrange(pos)

  # ... and the positions where they should be split
  tempTab <- theBits %>%
    group_by(name) %>%
    summarise(flags = max(flags),
              split = max(pos),
              pos = if_else(n() == 1, as.character(split), paste0(min(pos), ":", max(pos)))
      ) %>%
    arrange(split)

  # process bits
  out <- x %>%
    rowwise() %>%
    mutate(bit = .getBit(x = QB, len = bitfield@width)) %>%
    separate(col = bit, into = paste0("b", tempTab$split), sep = tempTab$split) %>%
    unite(col = "QB_chr", paste0("b", tempTab$split), sep = sep)

  # create look-up table for what the bits stand for
  lut <- tempTab %>%
    mutate(split = as.character(split)) %>%
    left_join(bitfield@desc, c("split" = "pos")) %>%
    select(-split)

  # assign look-up table to the environment as well
  env_bind(.env = qb_env, legend = lut)

  print(lut)

  return(out)

}
