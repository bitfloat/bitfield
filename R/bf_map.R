#' Map variables to a bitflag
#'
#' This function maps values from a dataset to bit flags that can be encoded
#' into a bitfield.
#' @param protocol [`character(1)`][character]\cr the protocol based on which
#'   the flag should be determined, see Details.
#' @param data the object to build bit flags for.
#' @param ... the protocol-specific arguments for building a bit flag, see
#'   Details.
#' @param pos [`integerish(.)`][integer]\cr optional position(s) in the bitfield
#'   that should be set.
#' @param name [`character(1)`][character]\cr optional flag-name.
#' @param na.val value, of the same encoding type as the flag, that needs to be
#'   given, if the test for this flag results in \code{NA}s.
#' @param registry [`registry(1)`][registry]\cr an already defined bitfield
#'   registry.
#' @details \code{protocol} can either be the name of an internal item (see
#'   \code{\link{bf_pcl}}), a newly built local protocol
#'   (\code{\link{bf_protocol}}) or one that has been imported from the bitfield
#'   community standards repo on github (\code{\link{bf_standards}}). Any
#'   \code{protocol} has specific arguments, typically at least the name of the
#'   column containing the values to test (\code{x}). To make this function as
#'   general as possible, all of these arguments are specified via the
#'   \code{...} argument of \code{bf_map}. Internal
#'   protocols are: \itemize{
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
#'     \item \code{category} (x): test whether the values are part of a set of
#'           given categories. (\emph{enumeration}).
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
#'           bit-sequence (see \code{\link{.makeEncoding}} for details on the
#'           ... argument) (\emph{floating-point}).
#'   }
#'
#' @section Notes: The console output of various classes (such as tibble) shows
#'   decimals that are not present or rounds decimals that are present, even for
#'   ordinary numeric vectors. R stores numeric values internally as
#'   double-precision floating-point values (with 64 bits, where 52 bits encode
#'   the significand), which corresponds to a decimal precision of ~16 digits
#'   (\code{log10(2^52)}). Hence, if a bit flag doesn't seem to coincide with
#'   the values you see in the console, double check the values with
#'   \code{sprintf("%16f", values)}. If you use a larger value than 16 for
#'   precision, you'll see more digits, but those are not meaningful, as they
#'   result merely from the binary-to-decimal conversion (check out
#'   \code{\link{.makeEncoding}} for an additional details.
#' @return an (updated) object of class 'registry' with the additional flag
#'   defined here.
#' @examples
#' # first, set up the registry
#' reg <- bf_registry(name = "testBF", description = "test bitfield")
#'
#' # then, put the test for NA values together
#' reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg,
#'               x = year)
#'
#' # all the other protocols...
#' # boolean encoding
#' reg <- bf_map(protocol = "nan", data = bf_tbl, registry = reg,
#'               x = y)
#' reg <- bf_map(protocol = "inf", data = bf_tbl, registry = reg,
#'               x = y)
#' reg <- bf_map(protocol = "identical", data = bf_tbl, registry = reg,
#'               x = x, y = y, na.val = FALSE)
#' reg <- bf_map(protocol = "range", data = bf_tbl, registry = reg,
#'               x = yield, min = 10.4, max = 11)
#' reg <- bf_map(protocol = "matches", data = bf_tbl, registry = reg,
#'               x = commodity, set = c("soybean", "honey"))
#' reg <- bf_map(protocol = "grepl", data = bf_tbl, registry = reg,
#'               x = year, pattern = ".*r")
#'
#' # enumeration encoding
#' reg <- bf_map(protocol = "category", data = bf_tbl, registry = reg,
#'               x = commodity, na.val = 0)
#' reg <- bf_map(protocol = "case", data = bf_tbl, registry = reg, na.val = 0,
#'               yield >= 11, yield < 11 & yield > 9, yield < 9 & commodity == "maize")
#'
#' # integer encoding
#' reg <- bf_map(protocol = "nChar", data = bf_tbl, registry = reg,
#'               x = commodity, na.val = 0)
#' reg <- bf_map(protocol = "nInt", data = bf_tbl, registry = reg,
#'               x = yield)
#' reg <- bf_map(protocol = "nDec", data = bf_tbl, registry = reg,
#'               x = yield)
#' reg <- bf_map(protocol = "integer", data = bf_tbl, registry = reg,
#'               x = as.integer(year), na.val = 0L)
#'
#' # floating-point encoding
#' reg <- bf_map(protocol = "numeric", data = bf_tbl, registry = reg,
#'               x = yield, decimals = 2)
#'
#' # finally, take a look at the registry
#' reg
#'
#' # alternatively, a raster
#' library(terra)
#' bf_rst <- rast(nrows = 3, ncols = 3, vals = bf_tbl$commodity, names = "commodity")
#' bf_rst$yield <- rast(nrows = 3, ncols = 3, vals = bf_tbl$yield)
#'
#' reg <- bf_registry(name = "testBF", description = "raster bitfield")
#'
#' reg <- bf_map(protocol = "na", data = bf_rst, registry = reg,
#'               x = commodity)
#'
#' reg <- bf_map(protocol = "range", data = bf_rst, registry = reg,
#'               x = yield, min = 5, max = 11)
#'
#' reg <- bf_map(protocol = "category", data = bf_rst, registry = reg,
#'               x = commodity, na.val = 0)
#' reg
#'
#' @importFrom checkmate assertCharacter assertIntegerish assertClass
#'   assertNames assertDisjunct
#' @importFrom rlang enquos eval_tidy exec env_bind get_expr `:=` expr_text
#' @importFrom purrr map safely list_rbind imap_chr
#' @importFrom stringr str_split str_replace
#' @importFrom glue glue
#' @importFrom terra global has.colors ncell levels is.factor
#' @importFrom methods formalArgs
#' @importFrom stats quantile
#' @export

bf_map <- function(protocol, data, registry, ..., name = NULL, pos = NULL,
                   na.val = NULL){

  assertCharacter(x = protocol, len = 1, any.missing = FALSE)
  if(grepl(x = protocol, pattern = "_")) stop("protocol name ('", protocol, "'), must not contain '_' symbols.")
  assertClass(x = registry, classes = "registry")
  assertCharacter(x = name, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)

  args <- enquos(..., .named = TRUE)
  # return(args)

  # load protocol ----
  if(protocol %in% names(bf_pcl)){
    pcl <- bf_pcl[[protocol]]
    # internal protocols already have string tests, convert to function for execution
    if (is.character(pcl$test)) {
      pcl$test <- eval(parse(text = pcl$test))
    }
  } else {
    pcl <- get(protocol)
    pcl <- .validateProtocol(pcl)
    # custom protocols need string->function conversion
    if (is.character(pcl$test)) {
      pcl$test <- eval(parse(text = pcl$test))
    }
  }

  # load potentially missing packages ----
  if(!is.null(pcl$requires)){
    map(pcl$requires, safely(~require(.x, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
  }

  # determine so far anonymous arguments ----
  tempArgs <- map(.x = seq_along(args), .f = function(ix){
    temp <- get_expr(args[[ix]])
    if(length(temp) != 1 & exists(as.character(temp)[1], mode = "function")) temp <- temp[-1]
    if(is.character(temp)) temp <- paste0("'", temp, "'")
    data.frame(name = names(args)[ix], val = as.character(temp))
  }) |> list_rbind()
  formalNames <- tempArgs[tempArgs$name %in% formalArgs(pcl$test),]

  levelsProv <- NULL
  if(pcl$encoding_type == "bool") {

    sample_vals <- c(FALSE, TRUE)
    levels_data <- data.frame(
      id = 0:1,
      label = sample_vals
    )

  } else if(pcl$encoding_type == "enum") {

    if(protocol == "case"){

      sample_vals <- 0:length(args) # add 0 as "no match" case
      levels_data <- data.frame(
        id = 0:length(args),
        label = c("no match", names(args)))

    } else {

      if(inherits(data, "SpatRaster")) {
        if(is.factor(data[formalNames$val])){
          levels_data <- levels(data)[[1]]
          names(levels_data)[1] <- "id"
        } else {
          stop("to encode the values as enumeration, please provide a raster attribute table.")
        }

      } else {
        lvls <- levels(data[[formalNames$val]])
        levels_data <- data.frame(
          id = 0:length(lvls),
          label = c("no category", lvls))
      }
      sample_vals <- levels_data$id
    }

  } else {

    if(protocol %in% c("integer", "numeric") & "x" %in% names(args)){

      if(inherits(data, "SpatRaster")) {
        qq <- global(data, quantile, na.rm = TRUE)
        minmax <- global(abs(data), range, na.rm = TRUE)
      } else {
        temp_col <- eval_tidy(args[[1]], data)
        qq <- quantile(temp_col, na.rm = TRUE)
        minmax <- range(abs(temp_col), na.rm = TRUE)
      }

      levels_data <- data.frame(
        id = 0:4,
        label = unlist(qq, use.names = FALSE)
      )
      sample_vals <- c(qq[[1]], qq[[5]], minmax[[1]], minmax[[2]])

    } else {
      levels_data <- data.frame(
        id = 0,
        label = paste0(protocol, " output (computed)"))
      sample_vals <- c(0, 2^pcl$bits - 1)
    }

  }

  enc <- .makeEncoding(var = sample_vals, type = pcl$encoding_type, ...)
  # return(enc)

  # update position ----
  if(is.null(pos)){
    pos <- (registry@width + 1L):(registry@width + enc$sign + enc$exponent + enc$significand)
  } else {
    # include test that checks whether sufficient positions are set, and give an error if not
  }

  # update registry ----
  registry@width <- registry@width + enc$sign + enc$exponent + enc$significand
  len <- if(inherits(data, "SpatRaster")){
    as.integer(ncell(data))
  } else {
    as.integer(nrow(data))
  }
  if(registry@length == 0L){
    registry@length <- len
  } else {
    if(registry@length != len){
      stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # if the protocol is 'case', ensure that it has an iterator and uses argument
  # names as argument text, otherwise combine them from name and value
  if(protocol %in% c("range", "matches", "case", "grepl")){
    if(is.null(names(registry@flags))){
      iter <- NULL
    } else {
      if(any(grepl(pattern = protocol, x = names(registry@flags)))){
        iter <- length(grep(pattern = protocol, x = names(registry@flags))) + 1
      } else {
        iter <- NULL
      }
    }
  } else {
    iter <- NULL
  }

  if(protocol == "case"){
    args_text <- names(args)
    pcl$description <- str_replace(string = pcl$description, pattern = "\\{...\\}", replacement = "\\{cases\\}")
    cases <- c("NULL", paste0("'", names(args), "'"))
    inputNames <- unlist(str_split(tempArgs$name, " "))
  } else {
    args_text <- imap_chr(args, ~ paste0(.y, " = ", expr_text(get_expr(.x))))
    inputNames <- unlist(str_split(tempArgs$val, " "))

    # define descriptive variables
    for(i in seq_along(formalNames$name)){
      assign(formalNames$name[i], paste0("'", paste0(formalNames$val[which(formalNames$name == formalNames$name[i])], collapse = ", "), "'"), envir = environment())
    }
  }
  inputNames <- unique(inputNames[inputNames %in% names(data)])
  desc <- glue(pcl$description)

  # customise name ----
  if(!is.null(name)){
    thisName <- name
  } else {
    thisName <- paste0(protocol, iter, "_", paste0(inputNames, collapse = "-"))
    assertDisjunct(x = thisName, y = names(registry@flags))
  }

  # create the provenance items ----
  testProv <- list(useTest = paste0(protocol, "_", pcl$version))
  argsProv <- list(withArguments = args_text)
  naProv <- list(substituteValue = na.val)
  levelsProv <- list(extractLevels = levels_data)
  encProv <- list(encodeAsBinary = list(sign = enc$sign, exponent = enc$exponent, significand = enc$significand, bias = enc$bias))
  posProv <- list(assignPosition = c(min(pos), max(pos)))

  # store in registry ----
  registry@flags[[thisName]] <- list(comment = desc,
                                     wasDerivedFrom = deparse(substitute(data)),
                                     wasGeneratedBy = c(testProv, argsProv, naProv, levelsProv, encProv, posProv),
                                     wasAssociatedWith = paste0(Sys.info()[["nodename"]], "_", Sys.info()[["user"]]))

  # update MD5 checksum ----
  registry <- .updateMD5(registry)

  return(registry)

}
