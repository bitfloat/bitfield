#' Add flags to a registry
#'
#' @param flags [`function(.)`][function]\cr a function that returns a vector
#'   that shall be stored as bit.
#' @param name [`character(1)`][character]\cr the internal name of the bit
#'   flag(s).
#' @param desc [`character(1)`][character]\cr the description of the bit
#'   flag(s).
#' @param na_val [`numeric(1)`][numeric] | [`character(1)`][character] |
#'   [`logical(1)`][logical]\cr which value NAs in the output of \code{flags}
#'   should have in the registry.
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param registry [`registry(1)`][registry]\cr the registry in which the bit
#'   flag(s) should be stored.
#' @details The following set of flags can be built with \code{bitfield}
#' operators: \itemize{
#' \item flags where a column is tested against some condition, such as
#'   \code{\link{bf_na}}, \code{\link{bf_nan}}, \code{\link{bf_null}} and
#'   \code{\link{bf_inf}} (output length 1).
#' \item flags where an attribute is identified from a small set of possible
#'   choices, such as \code{\link{bf_decimals}}, \code{\link{bf_length}} or
#'   \code{\link{bf_type}} (output length 1).
#' \item flags where a column is compared with another column (of the same
#'   length) or a set of values (of another length), such as
#'   \code{\link{bf_identical}}, \code{\link{bf_match}} and
#'   \code{\link{bf_range}} (output length 1).
#' \item flags where a test that results in a small set of categorical values
#'   that are combined into a compound flag, such as \code{\link{bf_case}}
#'   (output length > 1)
#' \item flags where relation between objects is recorded, including the output
#'   of other bitfield operators.
#' }
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
  triple <- attr(flags, "triple")

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
    outValues <- sort(unique(theValues))
  } # else {
  #   nFlags <- length(unique(theValues))
  #   outValues <- seq_along(unique(theValues))
  # }
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
               description = desc,
               triple = triple)

  registry@flags[[name]] <- temp

  return(registry)

}
