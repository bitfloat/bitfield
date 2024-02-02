#' Build a bit flag by counting the number of decimals
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which
#'   decimals are checked.
#' @param sep [`character(1)`][character]\cr regex-compatible symbol that
#'   separates the decimals from the numeric value.
#' @param fill [`logical(1)`][logical]\cr whether the function should consider
#'   decimals lengths from 0 through the maximum length in `x[[test]]`, or
#'   whether it should only document existing decimal lengths.
#' @details The show method of various classes shows decimals that may not
#'   really be present, and that even includes ordinary numeric vectors. In
#'   these cases, what is printed in the R console may be misleading.
#'   Doublecheck with \code{\link{str}}.
#'
#'   Determine the number of decimals of coordinates to determine their
#'   precision:
#'   \href{https://en.wikipedia.org/wiki/Decimal_degrees}{https://en.wikipedia.org/wiki/Decimal_degrees}
#'
#' @importFrom checkmate assertDataFrame assertSubset
#' @importFrom stringr str_length str_extract
#' @importFrom rlang expr
#' @importFrom dplyr bind_cols
#' @export

bf_decimals <- function(x, test, sep = "\\.", fill = TRUE){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))

  dec <- str_length(str_extract(x[[test]], paste0("(?<=", sep, ")\\d+")))
  dec[is.na(dec)] <- 0L

  temp <- bind_cols(x, dec = dec)

  if(fill){
    tempDec <- 0:max(dec)
  } else {
    tempDec <- unique(dec)
  }

  temp <- bind_cols(
    map(seq_along(tempDec), function(ix){
      blubb <- eval_tidy(expr = parse_expr(expr(!!paste0("dec == ", tempDec[ix]))), data = x)
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

  out <- tempDec[out]

  attr(out, which = "name") <- paste0("decimals_", test)
  attr(out, which = "desc") <- paste0("the value in '", test, "' has [", paste0(tempDec, collapse = ", "), "] decimals.")

  return(out)
}
