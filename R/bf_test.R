#' Build a bit flag
#'
#' @param operator [`character(1)`][character]\cr the operator based on which
#'   the flag should be built, see Details.
#' @param data the object to build bit flags for.
#' @param ... the operator-specific arguments for building a test, see Details.
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param name [`character(1)`][character]\cr optional flag-name.
#' @param na.val value, of the same encoding type as the flag, that needs to be
#'   given, if the test for this flag results in \code{NA}s.
#' @param description [`character(.)`][character]\cr optional description that
#'   should be used instead of the default operator-specific description. This
#'   description is used in the registry legend, so it should have as many
#'   entries as there will be flags (two for a binary flag, as many as there are
#'   cases for a enumeration flag and one for integer or numeric flags).
#' @param registry [`registry(1)`][registry]\cr a bitfield registry that has
#'   been defined with \code{\link{bf_registry}}; if it's undefined, an empty
#'   registry will be defined on-the-fly.
#' @details \code{operator} can either be the name of an internal encoding, a
#'   newly built local encoding or one that has been imported  from the bitfield
#'   community standards repo on github. Any \code{operator} has specific
#'   arguments, typically at least the name of the column containing the
#'   variable values (\code{x}). To make this function as general as possible,
#'   all of these arguments are specified via the \code{...} argument of
#'   \code{bf_test}. Internal
#'   operators are: \itemize{
#'     \item \code{na} (x): test whether a variable contains \code{NA}-values
#'           (\emph{boolean}).
#'     \item \code{nan} (x): test whether a variable contains \code{NaN}-values
#'           (\emph{boolean}).
#'     \item \code{inf} (x): test whether a variable contains \code{Inf}-values
#'           (\emph{boolean}).
#'     \item \code{identical} (x, y): element-wise test whether values are
#'           identical across two variables (\emph{boolean}).
#'     \item \code{range} (x, min, max): test whether the values are within a
#'           given range (\emph{boolean}).
#'     \item \code{matches} (x, set): test whether the values match a given set
#'           (\emph{boolean}).
#'     \item \code{grepl} (x, pattern): test whether the values match a given
#'           pattern (\emph{boolean}).
#'     \item \code{case} (...): test whether values are part of given cases
#'           (\emph{enumeration}).
#'     \item \code{nChar} (x): count the number of characters of the values
#'           (\emph{unsigned integer}).
#'     \item \code{nInt} (x): count the number of integer digits of the values
#'           (\emph{unsigned integer}).
#'     \item \code{nDec} (x): count the decimal digits of the variable values
#'           (\emph{unsigned integer}).
#'     \item \code{integer} (x, ...): encode the integer values as bit-sequence
#'           (\emph{signed integer}).
#'     \item \code{numeric} (x, ...): encode the numeric value as floating-point
#'           bit-sequence (with an adapted precision) (\emph{floating-point}).
#'   }
#'
#' @section Notes: The console output of various classes (such as tibble) shows
#'   decimals that are not present or rounds decimals that are present, even for
#'   ordinary numeric vectors. R stores numeric values internally as
#'   double-precision floating-point values (with 64 bits, where 52 bits encode
#'   the mantissa), which corresponds to a decimal precision of ~16 digits
#'   (\code{log10(2^52)}). Hence, if a bit flag doesn't seem to coincide with
#'   the values you see in the console, double check the values with
#'   \code{sprintf("%16f", values)}. If you use a larger decimal precision,
#'   you'll see more digits, but those are not meaningful, as they result merely
#'   from the binary-to-decimal conversion (check out
#'   \code{\link{.makeEncoding}} for additional information.
#'
#'   When testing for cases, they are evaluate in the order they have been
#'   defined in. If an observation is part of two cases, it will thus have the
#'   value of the last case it matches.
#' @return an (updated) object of class 'registry' with the additional flag
#'   defined here.
#' @examples
#' opr <- "identical"
#'
#' # identify which arguments need to be given to call a test ...
#' formalArgs(bf_internal[[opr]]$test)
#'
#' # put the test together
#' bf_test(operator = opr, data = bf_tbl, x = x, y = y, na.val = FALSE)
#'
#' # some other examples of ...
#' # boolean encoding
#' bf_test(operator = "matches", data = bf_tbl, x = commodity, set = c("soybean", "honey"))
#' bf_test(operator = "range", data = bf_tbl, x = yield, min = 10.4, max = 11)
#'
#' # enumeration encoding
#' bf_test(operator = "case", data = bf_tbl,
#'         yield >= 11, yield < 11 & yield > 9, yield < 9 & commodity == "maize")
#'
#' # integer encoding
#' bf_test(operator = "integer", data = bf_tbl, x = as.integer(year), na.val = 0L)
#'
#' # floating-point encoding
#' bf_test(operator = "numeric", data = bf_tbl, x = yield, decimals = 2)
#'
#' @importFrom rlang enquos eval_tidy exec env_bind get_expr `:=`
#' @importFrom purrr map safely list_rbind
#' @importFrom stringr str_split str_replace
#' @importFrom checkmate assertCharacter assertIntegerish assertClass
#'   assertNames assertDisjunct
#' @importFrom glue glue
#' @importFrom terra rast
#' @importFrom methods formalArgs
#' @export

bf_test <- function(operator, data, ..., name = NULL, pos = NULL, na.val = NULL,
                    description = NULL, registry = NULL){

  assertCharacter(x = operator, len = 1, any.missing = FALSE)
  assertCharacter(x = name, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertClass(x = registry, classes = "registry", null.ok = TRUE)

  args <- enquos(..., .named = TRUE)
  # return(args)

  # load registry ----
  if(is.null(registry)){
    registry <- bf_registry(name = "new_registry")
  }

  # extract values in data ----
  if(inherits(data, "bf_rast")){
    tempData <- as.data.frame(data())
  } else {
    tempData <- data
  }

  # determine intermediate flag values ----
  if(operator %in% c("na", "nan", "inf", "identical", "range", "type", "matches", "grepl", "case", "nChar", "nInt", "nDec", "integer", "numeric")){
    attrib <- bf_internal[[operator]]
  } else {
    assertNames(x = operator, subset.of = names(bf_opr))
    attrib <- bf_opr[[operator]]
  }

  # evaluate arguments of the operator ----
  tidyArgs <- map(args, eval_tidy, data = tempData)

  # call operator ----
  if(!is.null(attrib$require)) map(attrib$require, safely(~require(.x, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
  out <- exec(attrib$test, !!!tidyArgs)

  # determine encoding ----
  enc <- .makeEncoding(var = out, type = attrib$encoding_type, tidyArgs)
  encProv <- paste0("encodeAsBinary: ", enc$sign, ".", enc$exp, ".", enc$mant, "/", enc$bias)

  # replace NA values ----
  if(any(is.na(out))){
    if(is.null(na.val)) stop("there are NA values in the bit representation, please define 'na.val'.")
    assertClass(x = na.val, classes = class(out))
    out[is.na(out)] <- na.val
    naProv <- paste0("substituteValue: NA->", na.val)
  } else {
    naProv <- NULL
  }

  # update position ----
  if(is.null(pos)){
    pos <- (registry@width + 1L):(registry@width + enc$sign + enc$exponent + enc$mantissa)
  } else {
    # include test that checks whether sufficient positions are set, and give an error if not
  }

  # update registry ----
  registry@width <- registry@width + enc$sign + enc$exponent + enc$mantissa
  if(registry@length == 0L){
    registry@length <- length(out)
  } else {
    if(registry@length != length(out)){
      stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # make meta-data ----
  testProv <- paste0("useTest: ", paste0(operator, "_", attrib$version))
  tempArgs <- map(.x = seq_along(args), .f = function(x){
    temp <- get_expr(args[[x]])
    if(is.character(temp)) temp <- paste0("'", temp, "'")
    data.frame(name = names(args)[x], val = as.character(temp), expr = paste0(names(args)[x], "=", temp))
  }) |> list_rbind()
  formalNames <- tempArgs[tempArgs$name %in% formalArgs(attrib$test),]
  result <- ""

  if(attrib$encoding_type %in% c("bool", "int", "float")){

    thisName <- paste0(c(operator, paste0(formalNames$val, collapse = "-")), collapse = "_")
    argsProv <- paste0("withArguments: ", paste0(tempArgs$expr, collapse = ", "))

    for(i in seq_along(formalNames$name)){
      assign(formalNames$name[i], paste0(c("'", formalNames$val[i], "'"), collapse = ""), envir = environment())
    }

  } else if(attrib$encoding_type == "enum"){

    inputNames <- unlist(str_split(tempArgs$name, " "))
    inputNames <- unique(inputNames[inputNames %in% colnames(tempData)])
    if(is.null(names(registry@flags))){
      iter <- 1
    } else {
      if(grepl(pattern = operator, x = names(registry@flags))){
        iter <- length(grep(pattern = operator, x = names(registry@flags))) + 1
      } else {
        iter <- 1
      }
    }
    thisName <- paste0(operator, iter, "_", paste0(inputNames, collapse = "-"))
    argsProv <- paste0("withArguments: ", paste0(names(tidyArgs), collapse = ", "))

    attrib$description <- str_replace(string = attrib$description, pattern = "\\{...\\}", replacement = "\\{cases\\}")
    cases <- c("NULL", paste0("'", names(tidyArgs), "'"))
  }
  desc <- glue(attrib$description)

  # customise name ----
  if(is.null(name)){
    name <- thisName
  } else {
    assertDisjunct(x = name, y = names(registry@flags))
  }

  # store in registry ----
  registry@flags[[name]] <- list(description = desc,
                                 position = pos,
                                 encoding = enc,
                                 provenance = list(wasDerivedFrom = deparse(substitute(data)),
                                                   wasGeneratedBy = c(testProv, argsProv, naProv, encProv),
                                                   wasAssociatedWith = paste0(Sys.info()[["nodename"]], "_", Sys.info()[["user"]])))

  # update MD5 checksum ----
  registry <- .updateMD5(registry)

  # assign tentative flags to environment ----
  env_bind(.env = bf_env, !!name := out)

  return(registry)

}
