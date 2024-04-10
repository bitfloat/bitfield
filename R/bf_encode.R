#' Build a bitfield from a bit registry
#'
#' @param registry [`registry(1)`][registry]\cr the bit registry that should be
#'   combined into a bitfield.
#'
#' @importFrom checkmate assertClass assertCharacter assertLogical
#' @importFrom tibble tibble
#' @importFrom purrr map map_int
#' @importFrom dplyr bind_rows arrange bind_cols select across
#' @importFrom stringr str_split str_split_i str_sub
#' @importFrom tidyselect everything
#' @importFrom tidyr separate_wider_position
#' @export

bf_encode <- function(registry){

  assertClass(x = registry, classes = "registry")

  # open the registry
  theBitfield <- tibble(.rows = registry@length)

  # arrange them by position of the bit ...
  theFlags <- bind_rows(map(seq_along(registry@flags), function(ix){
    tibble(pos = registry@flags[[ix]]$position,
           name = names(registry@flags)[ix])
  }))
  theFlags <- arrange(theFlags, pos)

  # ... and write into the registry
  for(i in seq_along(theFlags$name)){

    if(i != 1){
      if(theFlags$name[i] == theName){
        next
      }
    }

    theName <- theFlags$name[i]
    theFlag <- registry@flags[[theName]]
    theVals <- bf_env[[theName]]

    if(!is.logical(theVals)){

      # get the integer part of the binary value
      intBits <- .toBin(x = theVals)

      if(!is.integer(theVals)){
        # get the decimal part of the binary value and ...
        decBits <- .toBin(x = theVals, len = 23, dec = TRUE)

        # transform to scientific notation, then ...
        temp <- paste0(intBits, decBits)
        temp <- str_pad(string = temp, width = max(nchar(temp)), side = "right", pad = "0")
        temp <- gsub("^(.{1})(.*)$", "\\1.\\2", temp)

        # encode as bit sequence
        sign <- as.integer(0 > theVals)
        exponent <- .toBin(x = nchar(intBits)-1 + 127, len = 8)# replace this with the correct dynamic bias
        mantissa <- map(.x = temp, .f = \(x) str_split(string = x, pattern = "[.]", simplify = TRUE)[2]) |>
          unlist()

        theBits <- paste0(sign, exponent, mantissa)
      } else {
        # pad with 0s to have the same specs for all values of this flag
        theBits <- intBits |>
          str_pad(width = max(nchar(intBits)), side = "left", pad = "0")
      }

    } else {
      theBits <- as.integer(theVals)
    }

    theBitfield <- bind_cols(theBitfield, theBits, .name_repair = "unique")
  }

  tempBits <- unite(theBitfield, col = "int", everything(), sep = "")
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
  names(widths) <- paste0("int", 1:intLen)

  tempBits <- separate_wider_position(data = tempBits, cols = "int", widths = widths)

  .toInt <- function(x){
    temp <- str_split(x, "")
    map(seq_along(temp), function(y){
      sum(+(rev(temp[[y]]) == "1") * 2^(seq(temp[[y]])-1))
    }) |> unlist()
  }

  out <- tempBits |>
    mutate(across(everything(), .toInt))

  return(out)

}
