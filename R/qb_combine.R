#' Combine the bitfield into a quality bit (QB)
#'
#' @param bitfield [`bitfield(1)`][bitfield]\cr the bitfield that should be
#'   combined into a quality bit (QB).
#'
#' @importFrom checkmate assertClass assertCharacter assertLogical
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr bind_rows arrange bind_cols select
#' @importFrom stringr str_split str_split_i
#' @export

qb_combine <- function(bitfield){

  assertClass(x = bitfield, classes = "bitfield")

  # open the bitfield
  theBitfield <- tibble(.rows = bitfield@length)

  # arrange them by position of the bit ...
  theBits <- map(seq_along(bitfield@bits), function(ix){
    tibble(pos = bitfield@bits[[ix]]$position,
           name = names(bitfield@bits)[ix])
  }) %>%
    bind_rows() %>%
    arrange(pos)

  # ... and write into the bitfield
  for(i in seq_along(unique(theBits$name))){

    theName <- theBits$name[i]
    theBit <- bitfield@bits[[theName]]
    theVals <- qb_env[[theName]]

    if(is.logical(theBit$values)){
      # insert logical values as is

      theBitfield <- theBitfield %>%
        bind_cols(as.character(as.integer(theVals)), .name_repair = "minimal")

    } else {
      # build new bit representations from integer values

      bitVals <- map(seq_along(theVals), function(ix){
        temp <- .getBit(theVals[ix])

        paste0(temp[1:length(theBit$position)], collapse = "")
      }) %>%
        unlist()

      theBitfield <- theBitfield %>%
        bind_cols(bitVals, .name_repair = "minimal")

    }

  }

  out <- map(1:dim(theBitfield)[1], function(ix){

    temp <- paste0(theBitfield[ix, ], collapse = "")
    int <- str_split(temp, "")[[1]]
    int <- sum(+(int == "1") * 2^(seq(int)-1))

    tibble(QB = as.integer(int))

  }) %>%
    bind_rows()

  return(out)

}
