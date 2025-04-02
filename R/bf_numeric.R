#' Build a bit flag from a numeric value
#'
#' @param x the object to build bit flags for.
#' @param source [`character(1)`][character]\cr the column or layer in \code{x}
#'   from which to take the numeric value.
#' @param ... description
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val [`logical(1)`][logical]\cr value that needs to be given, if the
#'   test for this flag results in \code{NA}s.
#' @param description [`character(.)`][character]\cr optional description that
#'   should be used instead of the default function-specific description. This
#'   description is used in the registry legend, so it should have as many
#'   entries as there will be entries per the respective flag in the legend (two
#'   for a binary flag, as many as there are cases for a cases flag and one for
#'   count or numeric flags).
#' @param registry [`registry(1)`][registry]\cr a bitfield registry that has
#'   been defined with \code{\link{bf_registry}}; if it's undefined, an empty
#'   registry will be defined on-the-fly.
#' @details The length of the bitfield depends on the floating point precision
#'   of the numeric values returned with this function. This is given as a
#'   sequence of 3 integers of the form \[1.5.10\]. The first position encodes
#'   how many bit are used (0 or 1), the second position encodes how many
#'   exponent bits are used and the third position encodes how many significant
#'   fields are used. See \code{\link{.determineEncoding}} for details.
#' @return an (updated) object of class 'registry' with the additional flag
#'   defined here.
#' @examples
#' bf_numeric(x = bf_tbl, source = "yield")
#' @importFrom checkmate assert assertDataFrame assertSubset assertIntegerish
#'   assertList testNull
#' @importFrom rlang env_bind
#' @export

bf_numeric <- function(x, source, ..., pos = NULL, na.val = NULL,
                       description = NULL, registry = NULL){

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
               wasGeneratedBy = c(naProv, paste0("encodeAsBinary: ", enc$sign, ".", enc$exponent, ".", enc$mantissa, "/", enc$bias)))

  # ... and store everything in the registry
  temp <- list(description = description,
               position = pos,
               encoding = enc,
               provenance = prov)

  registry@flags[[thisName]] <- temp
  registry <- .updateMD5(registry)

  # assign tentative flags values into the current environment
  env_bind(.env = bf_env, !!thisName := out)

  return(registry)
}
