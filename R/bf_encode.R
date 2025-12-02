#' Encode bit flags into a bitfield
#'
#' This function picks up the flags mentioned in a registry and encodes them as
#' integer values.
#' @param registry [`registry(1)`][registry]\cr the registry that should be
#'   encoded into a bitfield.
#' @return data.frame of the same length as the input data. Depending on type
#'   and amount of bit flags, this can be a table with any number of columns, each
#'   of which encodes a sequence of 32 bits into an integer.
#' @examples
#' reg <- bf_registry(name = "testBF", description = "test bitfield")
#' reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
#'
#' field <- bf_encode(registry = reg)
#' @importFrom checkmate assertClass assertList
#' @importFrom tibble tibble
#' @importFrom purrr map map_int
#' @importFrom dplyr bind_rows arrange bind_cols select across mutate
#' @importFrom stringr str_split str_split_i str_sub str_pad str_remove
#' @importFrom tidyr separate_wider_position unite
#' @importFrom rlang  `:=`
#' @export

bf_encode <- function(registry){

  assertClass(x = registry, classes = "registry")
  assertList(x = registry@flags, min.len = 1)

  # open the registry
  theBitfield <- tibble(.rows = registry@length)

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

      # get the integer part of the binary value
      intBits <- .toBin(x = as.integer(theVals), pad = TRUE)

      if(!is.integer(theVals)){
        # good explanation: https://www.cs.cornell.edu/~tomf/notes/cps104/floating

        # get the decimal part of the binary value and ...
        decBits <- .toBin(x = theVals, dec = TRUE)

        # 1. transform to scientific notation...
        temp <- gsub("^(.{1})(.*)$", "\\1.\\2", str_remove(paste0(intBits, decBits), "^0+"))
        temp <- ifelse(temp == "", paste0(c(".", rep(0, theFlag$wasGeneratedBy$encodeAsBinary$significand)), collapse = ""), temp)

        # 2. ... then encode as bit sequence
        sign <- as.integer(0 > theVals)

        # 3. bias exponent and encode as binary
        exponent <- .toBin(x = nchar(str_remove(intBits, "^0+"))-1 + theFlag$wasGeneratedBy$encodeAsBinary$bias)

        # 4. extract significand
        significand  <- map(.x = temp,
                        .f = \(x) str_pad(str_sub(str_split(string = x, pattern = "[.]", simplify = TRUE)[2], end = theFlag$wasGeneratedBy$encodeAsBinary$significand), width = theFlag$wasGeneratedBy$encodeAsBinary$significand, pad = "0", side = "right"))  |>
          unlist()

        # 5. store as bit sequence
        theBits <- paste0(sign, exponent, significand)

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

  return(out)

}
