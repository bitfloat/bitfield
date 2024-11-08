#' Build a bit flag from a numeric value
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{source}.
#' @param source [`character(1)`][character]\cr the column in \code{x} from
#'   which to take the numeric value.
#' @param ... description
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param description description
#' @param registry description
#' @details The length of the bitfield depends on the floating point digits of
#'   the numeric values returned with this function...
#'
#' @examples
#' # bf_numeric(x = tbl_bityield, digits = 3, source = "yield")
#' @importFrom checkmate assert assertDataFrame assertSubset assertIntegerish
#'   assertList testNull
#' @export

bf_numeric <- function(x, source, ..., pos = NULL, na.val = NULL,
                       description = NULL, registry = NULL){

  # library(checkmate); library(rlang); library(tidyverse); type = "half"; options = NULL; pos = NULL; na.val = NULL; description = NULL; prov = NULL

  assertDataFrame(x = x)
  assertSubset(x = source, choices = names(x))
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)
  assertCharacter(x = description, len = 1, null.ok = TRUE)
  assertClass(x = registry, classes = "registry", null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "new_registry")
  }

  thisName <- paste0("numeric_", source)

  out <- x[[source]]

  # determine floating point encoding
  enc <- .determineEncoding(x = out, ...)

  # replace NA values
  if(any(is.na(out))){
    if(is.null(na.val)) stop("there are NA values in the bit representation, please define 'na.val'.")
    out[is.na(out)] <- na.val
    naProv <- paste0("substituteValue: NA->", na.val)
  } else {
    naProv <- NULL
  }

  # update position if it's not set
  if(is.null(pos)){
    pos <- (registry@width + 1L):(registry@width + enc$sign + enc$exponent + enc$mantissa)
  } else {
    # include test that checks whether sufficient positions are set, and give an error if not
  }

  # update the registry
  registry@width <- registry@width + enc$sign + enc$exponent + enc$mantissa
  if(registry@length == 0L){
    registry@length <- length(out)
  } else {
    if(registry@length != length(out)){
      stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # update flag metadata ...
  if(is.null(description)){
    # barcode <- paste0(c(rep("-", sign), "|", rep("-", exponent), "|", rep("-", mantissa)), collapse = "")
    description <- paste0("the bits encode the numeric value in column '", source, "' [", enc$sign, ".", enc$exponent, ".", enc$mantissa, "].")
  }

  prov <- list(wasDerivedFrom = source,
               wasGeneratedBy = c(naProv, paste0("encodingAsBinary: ", enc$sign, ".", enc$exponent, ".", enc$mantissa, "/", enc$bias)))

  # ... and store everything in the registry
  temp <- list(description = description,
               position = pos,
               encoding = enc,
               provenance = prov)

  registry@flags[[thisName]] <- temp

  # assign tentative flags values into the current environment
  env_bind(.env = bf_env, !!thisName := out)

  return(registry)
}
