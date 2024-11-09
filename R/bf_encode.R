#' Build a bitfield from a bit registry
#'
#' @param registry [`registry(1)`][registry]\cr the bit registry that should be
#'   encoded into a bitfield.
#' @return description
#' @examples
#' myRegistry <- bf_na(tbl_bityield, "y")
#'
#' myBitfield <- bf_encode(registry = myRegistry)
#' @importFrom checkmate assertClass assertCharacter assertLogical
#' @importFrom tibble tibble
#' @importFrom purrr map map_int
#' @importFrom dplyr bind_rows arrange bind_cols select across
#' @importFrom stringr str_split str_split_i str_sub str_pad str_remove
#' @importFrom tidyselect everything
#' @importFrom tidyr separate_wider_position
#' @export

bf_encode <- function(registry){

  assertClass(x = registry, classes = "registry")

  # open the registry
  theBitfield <- tibble(.rows = registry@length)

  # arrange them by position of the bit ...
  theFlags <- bind_rows(map(seq_along(registry@flags), function(ix){
    tibble(pos = min(registry@flags[[ix]]$position),
           name = names(registry@flags)[ix])
  }))
  theFlags <- arrange(theFlags, pos)

  # ... and write into the registry
  for(i in seq_along(theFlags$name)){

    theName <- theFlags$name[i]
    theFlag <- registry@flags[[theName]]
    thisLen <- theFlag$encoding$sign + theFlag$encoding$exponent + theFlag$encoding$mantissa
    theVals <- bf_env[[theName]]

    if(!is.logical(theVals)){

      # get the integer part of the binary value
      # theVals <- c(theVals[4], theVals[10], 329.390625)
      # theVals <- c(theVals, 329.390625)
      intBits <- .toBin(x = as.integer(theVals), pad = TRUE)

      if(!is.integer(theVals)){
        # good explanation: https://www.cs.cornell.edu/~tomf/notes/cps104/floating
        # bin<->dec       : https://www.rapidtables.com/convert/number/binary-to-decimal.html

        # get the decimal part of the binary value and ...
        decBits <- .toBin(x = theVals, dec = TRUE)

        # 1. transform to scientific notation, then ...
        temp <- paste0(intBits, decBits)
        # the first value must be 101001001.011001 -> remove "." after finishing
        temp <- gsub("^(.{1})(.*)$", "\\1.\\2", str_remove(temp, "^0+"))

        # 2. encode as bit sequence
        sign <- as.integer(0 > theVals)

        # 3. bias exponent and encode as binary
        exponent <- .toBin(x = nchar(str_remove(intBits, "^0+"))-1 + theFlag$encoding$bias)

        # 4. extract mantissa
        mantissa <- map(.x = temp,
                        .f = \(x) str_sub(str_split(string = x, pattern = "[.]", simplify = TRUE)[2], end = theFlag$encoding$mantissa))  |>
          unlist()

        # 5. store as bit sequence
        theBits <- paste0(sign, exponent, mantissa)

      } else {
        theBits <- intBits
      }

    } else {
      theBits <- as.integer(theVals)
    }

    # add trailing 0s
    theBits <- str_pad(string = theBits, width = thisLen, side = "right", pad = "0")

    theBitfield <- bind_cols(theBitfield, tibble(!!paste0("flag", i) := theBits), .name_repair = "minimal")
  }

  tempBits <- unite(theBitfield, col = "bf_int", everything(), sep = "")
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
    mutate(across(everything(), .toDec))

  return(out)

}
