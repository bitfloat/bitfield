#' Build a bit flag from a numeric value
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{source}.
#' @param source [`character(1)`][character]\cr the column in \code{x} from
#'   which to take the numeric value.
#' @param exponent description
#' @param significand description
#' @param bias description
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param prov description
#' @param registry description
#' @details The length of the bitfield depends on the floating point precision
#'   of the numeric values returned with this function...
#'
#' @examples
#' bf_numeric(x = example_data, source = "yield")
#' @importFrom checkmate assertDataFrame assertSubset assertIntegerish assertList
#' @export

bf_numeric <- function(x, source, exponent = 8, significand = 7, bias = 127,
                       pos = NULL, na.val = NULL, prov = NULL, registry = NULL){

  assertDataFrame(x = x)
  assertSubset(x = source, choices = names(x))
  assertIntegerish(x = exponent, len = 1, any.missing = FALSE)
  assertIntegerish(x = significand, len = 1, any.missing = FALSE)
  assertIntegerish(x = bias, len = 1, any.missing = FALSE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertNumeric(x = na.val, len = 1, null.ok = TRUE)
  assertList(x = prov, types = "character", any.missing = FALSE, null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "nameless_registry", description = "descriptionless_registry")
  }

  exponent <- as.integer(exponent)
  significand <- as.integer(significand)

  thisName <- paste0("numeric_", source)

  out <- x[[source]]

  # replace NA values
  if(any(is.na(out))){
    out[is.na(out)] <- na.val
  }

  # update position if it's not set
  if(is.null(pos)){
    pos <- (registry@width + 1L):(registry@width + 1L + exponent + significand)
  }

  # assign tentative flags values into the current environment
  env_bind(.env = bf_env, !!thisName := out)

  # update the registry
  registry@width <- registry@width + 1L + exponent + significand
  if(registry@length == 0L){
    registry@length <- length(out)
  } else {
    if(registry@length != length(out)){
      stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # store encoding metadata
  enc <- list(sign = 1L,
              exponent = as.integer(exponent),
              significand = as.integer(significand),
              bias = as.integer(bias))

  # and store everything in the registry
  temp <- list(description = paste0("the bits encode the numeric value in column '", source, "'."),
               position = pos,
               encoding = enc,
               provenance = prov,
               triple = paste0(source, "|encoded|1.", exponent, ".", significand, "/", bias, ""))

  registry@flags[[thisName]] <- temp

  return(registry)
}
