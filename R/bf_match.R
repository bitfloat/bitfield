#' Build a bit flag by comparing a column with a set
#'
#' @param x the object to build bit flags for.
#' @param test [`character(1)`][character]\cr the column or layer in \code{x}
#'   for which a match is checked.
#' @param set a vector of the same class as in \code{test} against which a set
#'   operation is performed.
#' @param negate [`logical(1)`][logical]\cr whether or not to determine a subset
#'   or a disjoint match.
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
#' @return an (updated) object of class 'registry' with the additional flag
#'   defined here.
#' @examples
#' bf_match(x = bf_tbl, test = "commodity", set = c("soybean", "maize"))
#' @importFrom checkmate assertDataFrame assertSubset assertClass
#' @importFrom rlang env_bind
#' @export

bf_match <- function(x, test, set, negate = FALSE, pos = NULL, na.val = NULL,
                     description = NULL, registry = NULL){

  assertLogical(x = negate, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertLogical(x = na.val, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = description, len = 2, null.ok = TRUE)
  assertClass(x = registry, classes = "registry", null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "new_registry")
  }

  thisName <- paste0("match_", test)

  # extract values in x
  if(inherits(x, "bf_rast")){
    assertSubset(x = test, choices = colnames(x()))
    tempOut <- x()[,test]
    where <- "layer"
  } else {
    assertDataFrame(x = x)
    assertSubset(x = test, choices = names(x))
    tempOut <- x[[test]]
    where <- "column"
  }

  # determine flag values
  if(negate){
    out <- !tempOut %in% set
    type <- "disjoint"
    typeProv <- "!"
  } else {
    out <- tempOut %in% set
    type <- "included"
    typeProv <- NULL
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
    description <- c(paste0("{FALSE} the value in ", where, " '", test, "' is not ", type, " in the set [", paste0(set, collapse = "|"), "]."),
                     paste0("{TRUE}  the value in ", where, " '", test, "' is ", type, " in the set [", paste0(set, collapse = "|"), "]."))
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
  registry <- .updateMD5(registry)

  # reconstruct out if it comes from a raster
  if(inherits(x, "bf_rast")){
    md <- attr(x(), "rast_meta")
    out <- rast(vals = out, ncols = md$ncol, nrows = md$nrow, res = md$res, extent = md$ext, names = test, crs = md$crs)
  }

  # assign tentative flags values into the current environment
  env_bind(.env = bf_env, !!thisName := out)

  return(registry)
}
