#' Add bits to a bitfield
#'
#' @param bit [`numeric(.)`][numeric] | [`character(.)`][character] |
#'   [`logical(1)`][logical]\cr a function that returns a vector that shall be
#'   stored as bit.
#' @param name [`character(.)`][character]\cr the internal name of the bit(s).
#' @param desc [`character(.)`][character]\cr the description of the bit(s).
#' @param na_val [`numeric(.)`][numeric] | [`character(.)`][character] |
#'   [`logical(1)`][logical]\cr which value NAs in the output of .bit should
#'   have in the bitfield.
#' @param pos [`integerish(1)`][integer]\cr the position in the bitfield that
#'   should be set.
#' @param bitfield [`bitfield(1)`][bitfield]\cr the bitfield in which the bits
#'   should be set.
#'
#' @importFrom checkmate assertCharacter assertClass assertIntegerish
#'   assertTRUE
#' @importFrom rlang env_bind
#' @importFrom dplyr arrange distinct bind_rows
#' @export

qb_grow <- function(bit, name, desc = NULL, na_val = NULL, pos, bitfield){

  # assertions
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertCharacter(x = desc, null.ok = TRUE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE)
  assertClass(x = bitfield, classes = "bitfield")

  # test whether the number of flags corresponds to the number of positions provided
  theValues <- bit

  # replace NA values in theValues
  if(any(is.na(theValues))){
    assertClass(x = na_val, classes = class(theValues), .var.name = "na_val")
    theValues[is.na(theValues)] <- na_val
  }

  if(is.logical(theValues)){
    # this is needed because not always both TRUE and FALSE are in the vector
    nFlags <- 2
    outValues <- c(TRUE, FALSE)
  } else if(is.numeric(theValues) | is.integer(theValues)) {
    nFlags <- max(theValues)
    outValues <- 1:nFlags
  } else {
    nFlags <- length(unique(theValues))
    outValues <- seq_along(unique(theValues))
  }
  nBits <- ceiling(log2(nFlags))

  assertTRUE(x = nBits == length(pos))

  # handle descriptions
  theDesc <- bitfield@desc
  if(is.null(desc)){
    message(paste0("please provide a description for ", name))
  } else {
    outDesc <- tibble(pos = as.character(pos), description = desc) %>%
      bind_rows(theDesc, .) %>%
      distinct() %>%
      arrange(pos)
  }

  # assign tentative bit values into the current environment
  env_bind(.env = qb_env, !!name := theValues)

  # and store everything in the bitfield
  temp <- list(values = outValues,
               position = pos)

  bitfield@bits[[name]] <- temp
  bitfield@desc <- outDesc

  return(bitfield)

}
