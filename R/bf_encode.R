#' Build a bitfield from a bit registry
#'
#' @param registry [`registry(1)`][registry]\cr the bit registry that should be
#'   combined into a bitfield.
#'
#' @importFrom checkmate assertClass assertCharacter assertLogical
#' @importFrom tibble tibble
#' @importFrom purrr map map_int
#' @importFrom dplyr bind_rows arrange bind_cols select
#' @importFrom stringr str_split str_split_i str_sub
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
      # .intToBin(x = theVals, len = theFlag$encoding$significand)
      intBits <- .toBin(x = theVals)

      if(!is.integer(theVals)){
        # if(theFlag$encoding$exponent != 0L){
        # optionally get the decimal part of the binary value and ...
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

    theBitfield <- bind_cols(theBitfield, theBits, .name_repair = "minimal")
  }

  # build the integer representation
  out <- map(1:dim(theBitfield)[1], function(ix){

    temp <- paste0(theBitfield[ix, ], collapse = "") implement it so that it splits this up into chunks of 32 bits, if it's longer
    int <- rev(str_split(temp, "")[[1]])
    sum(+(int == "1") * 2^(seq(int)-1))

  }) |> unlist()

  return(out)

}
