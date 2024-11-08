#' Build a bit flag by comparing a column with a set
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which a
#'   match is checked.
#' @param set a vector of the same class as in \code{test} against which a set
#'   operation is performed.
#' @param negate [`logical(1)`][logical]\cr whether or not to determine a subset
#'   or a disjoint match.
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param description description
#' @param registry description
#' @examples
#' bf_match(x = tbl_bityield, test = "commodity", set = c("soybean", "maize"))
#'
#' @importFrom checkmate assertDataFrame assertSubset assertClass
#' @export

bf_match <- function(x, test, set, negate = FALSE, pos = NULL, na.val = NULL,
                     description = NULL, registry = NULL){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertClass(x = set, classes = class(x[[test]]))
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)
  assertCharacter(x = description, len = 2, null.ok = TRUE)
  assertClass(x = registry, classes = "registry", null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "new_registry")
  }

  thisName <- paste0("match_", test)

  if(negate){
    out <- !x[[test]] %in% set
    typeProv <- "!"
    theDesc <- c(paste0("{FALSE} the value in column '", test, "' is not disjoint from the set [", paste0(set, collapse = "|"), "]."),
                 paste0("{TRUE}  the value in column '", test, "' is disjoint from the set [", paste0(set, collapse = "|"), "]."))
  } else {
    out <- x[[test]] %in% set
    typeProv <- NULL
    theDesc <- c(paste0("{FALSE} the value in column '", test, "' is not included in the set [", paste0(set, collapse = "|"), "]."),
                 paste0("{TRUE}  the value in column '", test, "' is included in the set [", paste0(set, collapse = "|"), "]."))
  }
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
    description <- theDesc
  }

  enc <- list(sign = 0L,
              exponent = 0L,
              mantissa = 1L,
              bias = 0L)

  prov <- list(wasDerivedFrom = test,
               wasGeneratedBy = c(paste0("testValue: ", typeProv, test, "%in%", paste0(set, collapse = "|")), naProv, "encodeAsBinary: 0.0.1/0"))

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
