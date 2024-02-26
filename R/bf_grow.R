#' Add flags to a registry
#'
#' @param flags [`function(.)`][function]\cr a function that returns a vector of
#'   flags.
#' @param name [`character(1)`][character]\cr the internal name of the bit
#'   flag(s).
#' @param desc [`character(1)`][character]\cr the description of the bit
#'   flag(s).
#' @param na [`numeric(1)`][numeric] | [`character(1)`][character] |
#'   [`logical(1)`][logical]\cr which value NAs in the output of \code{flags}
#'   should have in the registry.
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param registry [`registry(1)`][registry]\cr the registry in which the bit
#'   flag(s) should be stored.
#' @details In case you don't use a native function to build flags, to end up
#'   with useful triples, the \code{name} should be formulated in one of three
#' ways: \enumerate{
#' \item in case the function has a logical return value, the name will be
#'   paired with \code{'...|value|[TRUE,FALSE]'}
#' \item in case the function has a integer return value, the name will be
#'   paired with \code{'...|case|[case1:caseN]'} and
#' \item in case the function has a numeric return value, the name will be
#'   paired with \code{'BIT|encodes|...'}
#' }
#'
#' The following set of flags can be built with \code{bitfield} operators:
#' \itemize{
#' \item flags where a column is tested against some condition, such as
#'   \code{\link{bf_na}}, \code{\link{bf_nan}}, \code{\link{bf_null}} and
#'   \code{\link{bf_inf}} (output length 1).
#' \item flags where an attribute is identified from a small set of possible
#'   choices, such as \code{\link{bf_length}} or \code{\link{bf_type}} (output
#'   length 1).
#' \item flags where a column is compared with another column (of the same
#'   length) or a set of values (of another length), such as
#'   \code{\link{bf_identical}}, \code{\link{bf_match}} and
#'   \code{\link{bf_range}} (output length 1).
#' \item flags where the categorical values of a test are combined into a
#'   compound flag. These are functions that are derived from
#'   \code{\link{bf_case}} (output length > 1)
#' \item flags where a numeric value is encoded as bit value. These are
#'   functions that are derived from \code{\link{bf_numeric}}, such as
#'   \code{\link{bf_distribution}}, \code{\link{bf_histogram}},
#'   \code{\link{bf_residuals}} and \code{\link{summarise}}. (output length
#'   depending on floating-point precision)
#' \item flags where relation between objects is recorded, including the output
#'   of other bitfield operators.
#' }
#' @examples
#' flags1 <- bf_na(x = example_data, test = "x")  # build flag for NAs
#' flags2 <- bf_range(x = example_data, test = "x",
#'                   min = -180, max = 180)       # build flag for range
#'
#' registry <- bf_grow(flags = flags1, pos = 1)   # start registry ...
#' registry <- bf_grow(flags = flags1, pos = 2,
#'                     registry = registry)       # ... and update it
#' @importFrom checkmate assertCharacter assertClass assertIntegerish assertTRUE
#' @importFrom rlang env_bind
#' @importFrom dplyr arrange distinct bind_rows
#' @export

bf_grow <- function(flags, name = NULL, desc = NULL, na = NULL, pos = NULL,
                    registry = NULL){

  # assertions
  assertCharacter(x = name, len = 1, null.ok = TRUE)
  assertCharacter(x = desc, len = 1, null.ok = TRUE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertClass(x = registry, classes = "registry", null.ok = TRUE)

  # test whether the number of flags corresponds to the number of positions provided
  theValues <- flags

  if(is.null(name)) name <- attr(flags, "name")
  if(is.null(desc)) desc <- attr(flags, "desc")
  triple <- attr(flags, "triple")
  # fix triple and description creation when flags are constructed manually

  # replace NA values in theValues
  if(any(is.na(theValues))){
    assertClass(x = na, classes = class(theValues), .var.name = "na")
    theValues[is.na(theValues)] <- na
  }

  # this is needed because not always both TRUE and FALSE are in the vector
  if(is.logical(theValues)){
    nFlags <- 2
    outValues <- c(TRUE, FALSE)

    if(is.null(triple)){
      triple <- paste0(name, "|is|[TRUE,FALSE]")
    }
  } else if(is.integer(theValues)) {
    nFlags <- max(c(max(theValues), length(unique(theValues))))
    outValues <- sort(unique(theValues))

    if(is.null(triple)){
      triple <- paste0(name, "|case|[", paste0("case", seq_along(outValues), collapse = ","), "]")
    }
  } else if(is.numeric(theValues)) {

    # https://stackoverflow.com/questions/872544/what-range-of-numbers-can-be-represented-in-a-16-32-and-64-bit-ieee-754-syste
    # https://en.wikipedia.org/wiki/Half-precision_floating-point_format
    #
    # nFlags <- length(unique(theValues))
    # outValues <- unique(theValues)
    #
    # dec <- str_length(str_extract(theValues, paste0("(?<=\\.)\\d+")))
    # dec[is.na(dec)] <- 0L
    #
    # pos <-

    if(is.null(triple)){
      triple <- paste0("BIT|encodes|[", name, "]")
    }
  }
  nBits <- as.integer(ceiling(log2(nFlags)))

  assertTRUE(x = nBits <= length(pos))

  # handle descriptions
  if(is.null(desc)){
    message(paste0("please provide a description for ", name))
  } else if(length(desc) != length(outValues)){
    desc <- rep(x = desc, times = length(outValues))
  }

  if(is.null(registry)){
    registry <- bf_create()
  }

  # assign tentative flags values into the current environment
  env_bind(.env = bf_env, !!name := theValues)

  registry@width <- registry@width + nBits
  if(registry@length == 0){
    registry@length <- length(theValues)
  } else {
    if(registry@length != length(theValues)){
      stop(paste0("the flag '", name, "' doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # and store everything in the registry
  temp <- list(values = outValues,
               position = pos,
               description = desc,
               triple = triple)

  registry@flags[[name]] <- temp

  return(registry)

}
