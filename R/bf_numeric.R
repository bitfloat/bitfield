#' Build a bit flag from a numeric value
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{source}.
#' @param source [`character(1)`][character]\cr the column in \code{x} from
#'   which to take the numeric value.
#' @param precision [`integerish(1)`][integer]\cr description
#' @param range [`integerish(1)`][integer]\cr description
#' @param options [`list(.)`][list]\cr description
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param description description
#' @param prov description
#' @param registry description
#' @details The length of the bitfield depends on the floating point precision
#'   of the numeric values returned with this function...
#'
#' @examples
#' bf_numeric(x = bityield, precision = 3, source = "yield")
#' @importFrom checkmate assertDataFrame assertSubset assertIntegerish assertList
#' @export

bf_numeric <- function(x, source, precision = NULL, range = NULL, options = NULL,
                       pos = NULL, na.val = NULL, description = NULL, prov = NULL,
                       registry = NULL){

  assertDataFrame(x = x)
  assertSubset(x = source, choices = names(x))
  assertIntegerish(x = precision, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = range, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertNumeric(x = na.val, len = 1, null.ok = TRUE)
  assertList(x = prov, types = "character", any.missing = FALSE, null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "nameless_registry", description = "descriptionless_registry")
  }

  codings <- .toEncoding(x = x, precision = precision, range = range, opts = options)

  thisName <- paste0("numeric_", source)

  out <- x[[source]]

  # replace NA values
  if(any(is.na(out))){
    out[is.na(out)] <- na.val
  }

  # update position if it's not set
  if(is.null(pos)){
    pos <- (registry@width + 1L):(registry@width + codings$sign + codings$exponent + codings$significand)
  } else {
    # include test that checks whether sufficient positions are set, and give an error if not
  }

  # update the registry
  registry@width <- registry@width + codings$sign + codings$exponent + codings$significand
  if(registry@length == 0L){
    registry@length <- length(out)
  } else {
    if(registry@length != length(out)){
      stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # update flag metadata ...
  if(is.null(description)){
    description <- paste0("the bits encode the numeric value in column '", source, "'.")
  }

  enc <- list(sign = codings$sign,
              exponent = codings$exponent,
              significand = codings$significand,
              bias = codings$bias)

  if(is.null(prov)){
    prov <- source
  }

  provenance <- list(wasDerivedFrom = prov,
                     wasGeneratedBy = paste0("encodingAsBinary: ", codings$sign, ".", codings$exponent, ".", codings$significand, "/", codings$bias))

  # ... and store everything in the registry
  temp <- list(description = description,
               position = pos,
               encoding = enc,
               provenance = provenance)

  registry@flags[[thisName]] <- temp

  # assign tentative flags values into the current environment
  env_bind(.env = bf_env, !!thisName := out)

  return(registry)
}
