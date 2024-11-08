#' Build a bit flag by counting aspects of an entity
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which
#'   the length is determined.
#' @param dec [`character(1)`][character]\cr regex-compatible symbol that
#'   separates the decimals from the numeric value (such as \code{\\.} in case
#'   the decimal symbol is a ".").
#' @param fill [`logical(1)`][logical]\cr whether the function should consider
#'   decimals lengths from 0 through the maximum length in `x[[test]]`, or
#'   whether it should only document existing decimal lengths.
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param description description
#' @param registry description
#' @details The show method of various classes shows decimals that may not
#'   really be present, and that even includes ordinary numeric vectors. In
#'   these cases, what is printed in the R console may be misleading. Double
#'   check with \code{\link{str}}.
#'
#'   Determine the number of decimals of coordinates to determine their
#'   precision:
#'   \href{https://en.wikipedia.org/wiki/Decimal_degrees}{https://en.wikipedia.org/wiki/Decimal_degrees}
#' @examples
#' bf_length(x = tbl_bityield, test = "y")
#' bf_length(x = tbl_bityield, test = "y", dec = "\\.")
#' @importFrom checkmate assertDataFrame assertSubset
#' @importFrom stringr str_length str_extract
#' @importFrom rlang expr
#' @importFrom dplyr bind_cols
#' @export

bf_length <- function(x, test, dec = NULL, fill = TRUE, pos = NULL, na.val = NULL,
                      description = NULL, registry = NULL){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertCharacter(x = dec, len = 1, null.ok = TRUE)
  assertLogical(x = fill, len = 1, any.missing = FALSE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)
  assertCharacter(x = description, len = 1, null.ok = TRUE)
  assertClass(x = registry, classes = "registry", null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "new_registry")
  }

  if(!is.null(dec)){
    testVals <- str_length(str_extract(x[[test]], paste0("(?<=", dec, ")\\d+")))
    testVals[is.na(testVals)] <- 0L
    testVals[is.nan(testVals)] <- 0L
    testVals[is.infinite(testVals)] <- 0L

    thisName <- paste0("decimals_", test)
  } else {
    testVals <- str_length(x[[test]])
    testVals[is.na(x[[test]])] <- 0L
    testVals[is.nan(x[[test]])] <- 0L
    testVals[is.infinite(x[[test]])] <- 0L

    thisName <- paste0("length_", test)
  }

  temp <- bind_cols(x, test = testVals)

  if(fill){
    tempVals <- 0:max(testVals)
  } else {
    tempVals <- unique(testVals)
  }

  temp <- bind_cols(
    map(seq_along(tempVals), function(ix){
      blubb <- eval_tidy(expr = parse_expr(expr(!!paste0("test == ", tempVals[ix]))), data = temp)
      blubb <- as_tibble(blubb)
      rename(blubb, !!paste0("d", ix) := value)
    })
  )

  status <- bind_cols(
    map(seq_along(temp), function(ix){
      temp[[ix]][temp[[ix]]] <- ix
      as_tibble(temp[ix])
    })
  )

  out <- reduce(status, function(first, second){
    if_else(second != 0, second, first)
  })

  out <- tempVals[out]

  len <- as.integer(ceiling(log2(length(unique(tempVals)))))

  if(!is.null(dec)){
    mantissa <- 0L
    exponent <- len
    theDesc <- paste0("the bits encode the number of decimals in column '", test, "'.")
  } else {
    mantissa <- len
    exponent <- 0L
    theDesc <- paste0("the bits encode the value length in column '", test, "'.")
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
    pos <- (registry@width + 1L):(registry@width + len)
  } else {
    # include test that checks whether sufficient positions are set, and give an error if not
  }

  # update the registry
  registry@width <- registry@width + len
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
              exponent = exponent,
              mantissa = mantissa,
              bias = 0L)

  prov <- list(wasDerivedFrom = test,
               wasGeneratedBy = c(naProv, paste0("encodingAsBinary: 0.", exponent, ".", mantissa, "/0")))

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
