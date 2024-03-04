#' Build a bit flag by counting aspects of an entity
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which
#'   the length is determined.
#' @param dec [`character(1)`][character]\cr regex-compatible symbol that
#'   separates the decimals from the numeric value (such as \code{\\.} if the
#'   decimal symbol is a "."). If this is
#' @param fill [`logical(1)`][logical]\cr whether the function should consider
#'   decimals lengths from 0 through the maximum length in `x[[test]]`, or
#'   whether it should only document existing decimal lengths.
#' @details The show method of various classes shows decimals that may not
#'   really be present, and that even includes ordinary numeric vectors. In
#'   these cases, what is printed in the R console may be misleading. Double
#'   check with \code{\link{str}}.
#'
#'   Determine the number of decimals of coordinates to determine their
#'   precision:
#'   \href{https://en.wikipedia.org/wiki/Decimal_degrees}{https://en.wikipedia.org/wiki/Decimal_degrees}
#' @examples
#' bf_length(x = example_data, test = "y")
#' bf_length(x = example_data, test = "y", dec = "\\.")
#' @importFrom checkmate assertDataFrame assertSubset
#' @importFrom stringr str_length str_extract
#' @importFrom rlang expr
#' @importFrom dplyr bind_cols
#' @export

bf_length <- function(x, test, dec = NULL, fill = TRUE){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertCharacter(x = dec, len = 1, null.ok = TRUE)
  assertLogical(x = fill, len = 1, any.missing = FALSE)

  if(!is.null(dec)){
    testVals <- str_length(str_extract(x[[test]], paste0("(?<=", dec, ")\\d+")))
    testVals[is.na(testVals)] <- 0L

    theName <- paste0("decimals_", test)
  } else {
    testVals <- str_length(x[[test]])
    testVals[is.na(x[[test]])] <- 0L

    theName <- paste0("length_", test)
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

  if(!is.null(dec)){
    significand <- "0"
    exponent <- "{E}"
    theDesc <- paste0("the bits encode the number of decimals in column '", test, "'.")
  } else {
    significand <- "{S}"
    exponent <- "0"
    theDesc <- paste0("the bits encode the value length in column '", test, "'.")
  }

  attr(out, which = "name") <- theName
  attr(out, which = "desc") <- theDesc
  attr(out, which = "triple") <- paste0(test, "|encoded|", significand, ".", exponent)

  return(out)
}
