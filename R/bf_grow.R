#' Add flags to a registry
#'
#' @param flags [`function(.)`][function]\cr a function that returns a vector
#'   that shall be stored as bit.
#' @param name [`character(.)`][character]\cr the internal name of the bit
#'   flag(s).
#' @param desc [`character(.)`][character]\cr the description of the bit
#'   flag(s).
#' @param na_val [`numeric(.)`][numeric] | [`character(.)`][character] |
#'   [`logical(1)`][logical]\cr which value NAs in the output of \code{flags}
#'   should have in the registry.
#' @param pos [`integerish(1)`][integer]\cr the position in the bitfield that
#'   should be set.
#' @param registry [`registry(1)`][registry]\cr the registry in which the bit
#'   flag(s) should be stored.
#'
#' @importFrom checkmate assertCharacter assertClass assertIntegerish assertTRUE
#' @importFrom rlang env_bind
#' @importFrom dplyr arrange distinct bind_rows
#' @export

bf_grow <- function(flags, name = NULL, desc = NULL, na_val = NULL, pos = NULL, registry){

  # assertions
  assertCharacter(x = name, len = 1, null.ok = TRUE)
  assertCharacter(x = desc, len = 1, null.ok = TRUE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertClass(x = registry, classes = "registry")

  # test whether the number of flags corresponds to the number of positions provided
  theValues <- flags

  if(is.null(name)) name <- attr(flags, "name")
  if(is.null(desc)) desc <- attr(flags, "desc")

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
    nFlags <- max(c(max(theValues), length(unique(theValues))))
    outValues <- 1:nFlags
  } else {
    nFlags <- length(unique(theValues))
    outValues <- seq_along(unique(theValues))
  }
  nBits <- ceiling(log2(nFlags))

  assertTRUE(x = nBits <= length(pos))

  # handle descriptions
  if(is.null(desc)){
    message(paste0("please provide a description for ", name))
  }

  # assign tentative flags values into the current environment
  env_bind(.env = bf_env, !!name := theValues)

  # and store everything in the registry
  temp <- list(values = outValues,
               position = pos,
               description = desc)

  registry@flags[[name]] <- temp
  # registry@desc <- outDesc

  return(registry)

}
