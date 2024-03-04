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
#' @details In case you don't use a native function to build flags, the name
#'   should be of the form \code{activity_entity}, where 'activity' succinctly
#'   describes the function and 'entity' the object(s) or column(s) used by the
#'   function (if it's more than one object/column, they need to be concatenated
#'   with using a "_", as this is used internally to determine the provenance of
#'   the function). Both terms are from the
#'   \href{https://www.w3.org/TR/2013/REC-prov-dm-20130430/}{PROV Data Model}.
#'
#'   There are two basic types of flags distinguished by their output value. A
#'   flag can consist of one bit or of more than on bit. Flags that consists of
#'   only one bit are from tests that return FALSE/TRUE values: \itemize{
#'     \item where a column is tested against an individual value, such as
#'       \code{\link{bf_na}}, \code{\link{bf_nan}}, \code{\link{bf_null}},
#'       \code{\link{bf_inf}} or \code{\link{bf_type}}.
#'     \item where a column is tested against a set of values, such as
#'       \code{\link{bf_match}} or \code{\link{bf_range}}.
#'     \item where a column is compared with another column, such as
#'       \code{\link{bf_identical}}.
#'   }
#'   Flags that consist of a sequence of bits are either from tests that return
#'   a (small) number of cases, or from tests that return a (set of) numeric
#'   values: \itemize{
#'     \item where a categorical value is returned, such as \code{\link{bf_case}}
#'       (where the cases can be represented by integers from 1 to X).
#'     \item where a count value is returned, such as \code{\link{bf_length}}
#'       (that may not include 0 and that may be quite large).
#'     \item where a numeric value (floating-point number) of any kind is
#'       returned, such as \code{\link{bf_numeric}}.
#'     \item where a set of numeric values is returned, such as
#'       \code{\link{bf_distribution}}, \code{\link{bf_histogram}},
#'       \code{\link{bf_residuals}} or \code{\link{bf_summarise}} (which are
#'       basically compound flags that are stored in the registry individually)
#'   }
#' @examples
#' flags1 <- bf_na(x = example_data, test = "x")  # build flag for NAs
#' flags2 <- bf_range(x = example_data, test = "x",
#'                    min = -180, max = 180)       # build flag for range
#'
#' registry <- bf_grow(flags = flags1, pos = 1)   # start registry ...
#' registry <- bf_grow(flags = flags2, pos = 2,
#'                     registry = registry)       # ... and update it
#' @importFrom checkmate assertCharacter assertClass assertIntegerish assertTRUE
#' @importFrom rlang env_bind
#' @importFrom dplyr arrange distinct bind_rows
#' @export

bf_grow <- function(flags, name = NULL, desc = NULL, na = NULL, pos = NULL,
                    registry = NULL){

  # flags = bf_na(x = example_data, test = "y")
  # flags = bf_length(x = example_data, test = "y", dec = "\\.")
  # flags = bf_case(x = input, exclusive = FALSE, yield >= 11, yield < 11 & yield > 9, yield < 9 & commodity == "maize")
  # flags = bf_numeric(x = example_data, source = "y", digits = 1)
  # name = NULL; desc = NULL; na = NULL; pos = 1; registry = bf_create(name = "yield_errors", description = "this bitfield documents errors in a table of yield data.")

  # assertions
  assertCharacter(x = name, len = 1, null.ok = TRUE)
  assertCharacter(x = desc, len = 1, null.ok = TRUE)
  assertCharacter(x = na, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertClass(x = registry, classes = "registry", null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_create(name = "nameless_registry",
                          description = "descriptionless_registry")
  }

  # test whether the number of flags corresponds to the number of positions provided
  theValues <- flags

  if(is.null(name)) name <- attr(flags, "name")
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  if(is.null(desc)) desc <- attr(flags, "desc")
  assertCharacter(x = desc, any.missing = FALSE)
  triple <- attr(flags, "triple")
  assertCharacter(x = triple, len = 1, null.ok = TRUE)

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
  } else if(is.integer(theValues)){
    nFlags <- max(c(max(theValues), length(unique(theValues))))
    outValues <- sort(unique(theValues))

    # need something that distinguishes between cases (from bf_case) and counts (such as from bf_length), if they are not from these functions

    if(is.null(triple)){
      triple <- paste0(name, "|encoded|0.{E}")
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
      triple <- paste0("|encoded|[", name, "]")
    }
  }
  nBits <- as.integer(ceiling(log2(nFlags)))

  assertTRUE(x = nBits <= length(pos))

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
