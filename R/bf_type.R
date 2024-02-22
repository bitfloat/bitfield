#' Build a bit flag by checking for the type
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param test [`character(1)`][character]\cr the column in \code{x} for which
#'   the type is identified.
#' @param type [`character(1)`][character]\cr the for which to check, possible
#'   values are \code{"integer"}, \code{"numeric"}, \code{"character"} or
#'   \code{"logical"}.
#' @param coerce [`logical(1)`][logical]\cr whether or not to carry out a more
#'   aggressive test, where the values are coerced to the target type and those
#'   that end up not having NA-values are reported as \code{TRUE}.
#' @details When coercing values to \itemize{ \item integer, they are typically
#'   truncated to the non-decimal part of the value. This function compares the
#'   truncated value to the original value and thus returns \code{TRUE} only
#'   when the original value didn't have a decimals part or when that part was
#'   \code{.0}. It, moreover, returns \code{FALSE} for \code{NA} and
#'   \code{NaN}-values, and also for the special case \code{NA_integer_}, as
#'   this is not supported in many other programs that might provide data for or
#'   use them from this function. \item numeric, this function returns
#'   \code{TRUE} for any value that can sensibly be interpreted as numeric, such
#'   as \code{1L} (an integer) or \code{"1"} (a character), but not \code{"1L"}
#'   (a character of an integer) or \code{"one"} (another character). It returns
#'   \code{FALSE} for \code{NA} and \code{NaN}-values, and also for the special
#'   case \code{NA_real_}, as this is not supported in many other programs that
#'   might provide data for or use them from this function. \item character,
#'   this function returns \code{FALSE} for Inf and \code{NA}, and also for the
#'   special cases of \code{NA_character_}, as this is not supported in many
#'   other programs that might provide data for or use them from this function.
#'   \item logical, this function returns \code{TRUE} only for the integer(!)
#'   values \code{0L} and \code{1L}. For all other values, including \code{NA},
#'   this function returns \code{FALSE}. }
#' @return a logical vector of the same length as \code{test} with attributes
#'   \code{name}, \code{description} and \code{triple}.
#' @examples
#' bf_type(x = example_data, test = "y", type = "character", coerce = TRUE)
#' @importFrom checkmate assertDataFrame assertSubset assertCharacter
#'   assertChoice assertLogical
#' @importFrom purrr map_lgl
#' @export

bf_type <- function(x, test, type = NULL, coerce = FALSE){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertCharacter(x = type, len = 1, any.missing = FALSE, ignore.case = TRUE)
  assertChoice(x = type, choices = c("integer", "numeric", "character", "logical"))
  assertLogical(x = coerce, len = 1, any.missing = FALSE)

  if(!coerce){

    if(type == "integer"){
      out <- map_lgl(x[[test]], is.integer)
    } else if(type == "numeric"){
      out <- map_lgl(x[[test]], is.numeric)
    } else if(type == "character"){
      out <- map_lgl(x[[test]], is.character)
    } else {
      out <- map_lgl(x[[test]], is.logical)
    }

  } else {

    if(type == "integer"){

      out <- map_lgl(x[[test]], function(ix){
        temp <- as.integer(ix)
        if(!is.na(temp) & !is.nan(temp) & temp == ix){
          is.integer(temp)
        } else {
          FALSE
        }
      })

    } else if(type == "numeric"){

      out <- map_lgl(x[[test]], function(ix){
        temp <- as.numeric(ix)
        if(!is.na(temp) & !is.nan(temp)){
          is.numeric(temp)
        } else {
          FALSE
        }
      })

    } else if(type == "character"){

      out <- map_lgl(x[[test]], function(ix){
        if(!is.infinite(ix)){
          temp <- as.character(ix)
        } else {
          temp <- NA
        }
        if(!is.na(temp)){
          is.character(temp)
        } else {
          FALSE
        }
      })

    } else {

      allBin <- all(x[[test]] %in% c(0L, 1L))
      out <- map_lgl(x[[test]], function(ix){
        if(is.integer(ix) & allBin){
          temp <- as.logical(ix)
        } else {
          temp <- NA
        }
        if(!is.na(temp)){
          is.logical(temp)
        } else {
          FALSE
        }
      })

    }

  }

  attr(out, which = "name") <- paste0("type_", test)
  attr(out, which = "description") <- c(paste0("the value in column '", test, "' has type '", type, "'."),
                                        paste0("the value in column '", test, "' does not have type '", type, "'."))
  attr(out, which = "triple") <- paste0(test, "|type|'", type, "'")

  return(out)
}
