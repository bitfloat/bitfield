#' Combine the bitfield into a quality bit (QB)
#'
#' @param bitfield [`bitfield(1)`][bitfield]\cr the bitfield that should be
#'   combined into a quality bit (QB).
#' @param sep [`character(1)`][character]\cr when \code{inspect = TRUE}, this is
#'   used as separator for the bit-groups.
#' @param inspect [`logical(1)`][logical]\cr whether or not to inspect the
#'   bitfield. This will return not only the quality bit in integer
#'   representation, but also in bit representation.
#'
#' @importFrom checkmate assertClass assertCharacter assertLogical
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr bind_rows arrange bind_cols select
#' @importFrom stringr str_split str_split_i
#' @export

qb_combine <- function(bitfield, sep = "", inspect = FALSE){

  assertClass(x = bitfield, classes = "bitfield")
  assertCharacter(x = sep, len = 1, any.missing = FALSE)
  assertLogical(x = inspect, any.missing = FALSE, len = 1)

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
        temp <- theVals[ix] %>%
          intToBits() %>%
          as.character() %>%
          str_split_i(pattern = "", 2)
        paste0(temp[1:length(theBit$position)], collapse = "")
      }) %>%
        unlist()

      theBitfield <- theBitfield %>%
        bind_cols(bitVals, .name_repair = "minimal")

    }

  }

  out <- map(1:dim(theBitfield)[1], function(ix){

    temp <- paste0(theBitfield[ix, ], collapse = "")
    temp_debug <- paste0(theBitfield[ix, ], collapse = sep)

    int <- str_split(temp, "")[[1]]
    int <- sum(+(int == "1") * 2^(seq(int)-1))

    tibble(QB_insepct = temp_debug, QB = as.integer(int))

  }) %>%
    bind_rows()

  if(!inspect){
    out <- out %>%
      select(-QB_insepct)
  }

  return(out)


  # https://stackoverflow.com/questions/62333478/from-integer-to-bits-and-back-again
  # intToBase2 <- function(x){
  #   x %>%
  #     intToBits() %>%
  #     rev %>%
  #     as.character() %>%
  #     {sapply(strsplit(., "", fixed = TRUE), '[', 2)} %>%
  #     paste0(.,collapse = '')
  # }
  #
  # base2ToInt <- function(string){
  #   sapply(strsplit(string, ""), function(x) )
  # }
  #
}
