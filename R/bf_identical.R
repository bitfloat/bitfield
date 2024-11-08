#' Build a bit flag by checking whether two columns are identical
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test} and \code{against}.
#' @param test [`character(1)`][character]\cr the column in \code{x} that is
#'   checked \code{against}.
#' @param against [`character(1)`][character]\cr the column in \code{x} that is
#'   checked against \code{test}.
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param description description
#' @param registry description
#' @details This function compares the values of two columns element-wise and
#'   returns \code{TRUE} when they are identical.
#' @examples
#' bf_identical(x = tbl_bityield, test = "x", against = "y", na.val = 0)
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_identical <- function(x, test, against, pos = NULL, na.val = NULL,
                         description = NULL, registry = NULL){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertSubset(x = against, choices = names(x))
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)
  assertCharacter(x = description, len = 2, null.ok = TRUE)
  assertClass(x = registry, classes = "registry", null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "new_registry")
  }

  thisName <- paste0("identical_", test, "_", against)

  out <- x[[test]] == x[[against]]

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
    description <- c(paste0("{FALSE} the value in '", test, "' is distinct from the value in '", against, "'."),
                     paste0("{TRUE}  the value in '", test, "' is identical to the value in '", against, "'."))
  }

  enc <- list(sign = 0L,
              exponent = 0L,
              mantissa = 1L,
              bias = 0L)

  prov <- list(wasDerivedFrom = paste0(test, ", ", against),
               wasGeneratedBy = c(paste0("testValue: ", test, "==", against), naProv, "encodeAsBinary: 0.0.1/0"))

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
