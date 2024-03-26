#' Build a bit flag by checking whether values are NA
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked for NA values.
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param prov description
#' @param registry description
#' @examples
#' bf_na(x = bityield, test = "y")
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_na <- function(x, test, pos = NULL, na.val = NULL, prov = NULL,
                  registry = NULL){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)
  assertList(x = prov, types = "character", any.missing = FALSE, null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "nameless_registry", description = "descriptionless_registry")
  }

  thisName <- paste0("na_", test)

  out <- is.na(x[[test]])

  # replace NA values
  if(any(is.na(out))){
    out[is.na(out)] <- na.val
  }

  # update position if it's not set
  if(is.null(pos)){
    pos <- registry@width + 1L
  }

  # assign tentative flags values into the current environment
  env_bind(.env = bf_env, !!thisName := out)

  # update the registry
  registry@width <- registry@width + 1L
  if(registry@length == 0L){
    registry@length <- length(out)
  } else {
    if(registry@length != length(out)){
      stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # store encoding metadata
  enc <- list(sign = 0L,
              exponent = 0L,
              significand = 1L,
              bias = 0L)

  # and store everything in the registry
  temp <- list(description = c(paste0("{FALSE} the value in column '", test, "' is not NA."), paste0("{TRUE}  the value in column '", test, "' is NA.")),
               position = pos,
               encoding = enc,
               provenance = prov,
               triple = paste0(test, "|value|NA"))

  registry@flags[[thisName]] <- temp

  return(registry)
}
