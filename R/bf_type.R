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
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param description description
#' @param registry description
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
#' bf_type(x = tbl_bityield, test = "y", type = "character", coerce = TRUE)
#' @importFrom checkmate assertDataFrame assertSubset assertCharacter
#'   assertChoice assertLogical
#' @importFrom purrr map_lgl
#' @export

bf_type <- function(x, test, type = NULL, coerce = FALSE, pos = NULL,
                    na.val = NULL, description = NULL, registry = NULL){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertCharacter(x = type, len = 1, any.missing = FALSE, ignore.case = TRUE)
  assertChoice(x = type, choices = c("integer", "numeric", "character", "logical"))
  assertLogical(x = coerce, len = 1, any.missing = FALSE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)
  assertCharacter(x = description, len = 2, null.ok = TRUE)
  assertClass(x = registry, classes = "registry", null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "new_registry")
  }

  thisName <- paste0("type_", test)

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
    description <- c(paste0("{FALSE} the value in column '", test, "' does not have type '", type, "'."),
                     paste0("{TRUE}  the value in column '", test, "' has type '", type, "'."))
  }

  enc <- list(sign = 0L,
              exponent = 0L,
              mantissa = 1L,
              bias = 0L)

  prov <- list(wasDerivedFrom = test,
               wasGeneratedBy = c(paste0("testValue: is.", type, "(", test, ")"), naProv, "encodeAsBinary: 0.0.1/0"))

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
