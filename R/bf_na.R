#' Build a bit flag by checking whether values are NA
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked for NA values.
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param description description
#' @param prov description
#' @param registry description
#' @examples
#' bf_na(x = tbl_bityield, test = "y")
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_na <- function(x, test,
                  pos = NULL, na.val = NULL, description = NULL, prov = NULL,
                  registry = NULL){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)
  assertList(x = prov, types = "character", any.missing = FALSE, null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "new_registry")
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
  } else {
    # include test that checks whether sufficient positions are set, and give an error if not
  }

  # update the registry
  registry@width <- registry@width + 1L
  if(registry@length == 0L){
    registry@length <- length(out)
  } else {
    if(registry@length != length(out)){
      stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # update flag metadata ...
  if(is.null(description)){
    description <- c(paste0("{FALSE} the value in column '", test, "' is not NA."), paste0("{TRUE}  the value in column '", test, "' is NA."))
  }

  enc <- list(sign = 0L,
              exponent = 0L,
              significand = 1L,
              bias = 0L)

  prov <- list(wasDerivedFrom = test,
               wasGeneratedBy = c(paste0(test, "|value|NA"), paste0("encodingAsBinary: 0.0.1/0")))

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
