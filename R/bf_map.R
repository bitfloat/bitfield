#' Map variables to a bitflag
#'
#' This function maps values from a dataset to bit flags that can be encoded
#' into a bitfield.
#' @param protocol [`character(1)`][character]\cr the protocol based on which
#'   the flag should be determined, see Details.
#' @param data the object to build bit flags for.
#' @param ... the protocol-specific arguments for building a bit flag, see
#'   Details.
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
#'     \item \code{integer} (x, ...): encode values as integer bit-sequence.
#'           Accepts raw integer data directly, or numeric data with
#'           auto-scaling when \code{range}, \code{fields}, or \code{decimals}
#'           are provided. With \code{range = c(min, max)} and
#'           \code{fields = list(significand = n)}, values are linearly mapped
#'           from \code{[min, max]} to \code{[0, 2^n - 1]} during encoding and
#'           back during decoding. The scaling parameters are stored in
#'           provenance for transparent round-trips (\emph{signed integer}).
#'     \item \code{numeric} (x, ...): encode the numeric value as floating-point
#'           bit-sequence (see \code{\link{.makeEncoding}} for details on the
#'           ... argument) (\emph{floating-point}).
#'   }
#'
#' @section Notes: Console output from R classes (such as tibble) often rounds
#'   or truncates decimal places, even for ordinary numeric vectors. Internally,
#'   R stores numeric values as double-precision floating-point numbers (64
#'   bits, with 52 bits for the significand), providing approximately 16
#'   significant decimal digits (\eqn{log10(2^52) = 15.65}). If a bit flag
#'   appears inconsistent with the displayed values, verify the full precision
#'   using \code{sprintf("%.16f", values)}. Using more than 16 digits will show
#'   additional figures, but these are artifacts of binary-to-decimal conversion
#'   and carry no meaningful information.
#' @return an (updated) object of class 'registry' with the additional flag
#'   defined here.
#' @examples
#' # first, set up the registry
#' reg <- bf_registry(name = "testBF", description = "test bitfield",
#'                    template = bf_tbl)
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
#'               x = commodity, set = c("soybean", "honey"), na.val = FALSE)
#' reg <- bf_map(protocol = "grepl", data = bf_tbl, registry = reg,
#'               x = year, pattern = ".*r", na.val = FALSE)
#'
#' # enumeration encoding
#' reg <- bf_map(protocol = "category", data = bf_tbl, registry = reg,
#'               x = commodity, na.val = 0)
#' reg <- bf_map(protocol = "case", data = bf_tbl, registry = reg, na.val = 4,
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
#' # integer encoding with auto-scaling (numeric data mapped to integer range)
#' dat <- data.frame(density = c(0.5, 1.2, 2.8, 0.0, 3.1))
#' reg2 <- bf_registry(name = "scaledBF", description = "auto-scaled",
#'                     template = dat)
#' reg2 <- bf_map(protocol = "integer", data = dat, registry = reg2,
#'                x = density, range = c(0, 3.1),
#'                fields = list(significand = 5), na.val = 0L)
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
#' reg <- bf_registry(name = "testBF", description = "raster bitfield",
#'                    template = bf_rst)
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

bf_map <- function(protocol, data, registry, ..., name = NULL, na.val = NULL){

  assertCharacter(x = protocol, len = 1, any.missing = FALSE)
  if(grepl(x = protocol, pattern = "_")) stop("protocol name ('", protocol, "'), must not contain '_' symbols.")
  assertClass(x = registry, classes = "registry")
  assertCharacter(x = name, len = 1, any.missing = FALSE, null.ok = TRUE)

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

  # extract test values for validation (if 'x' argument is provided)
  testVals <- NULL
  if ("x" %in% names(args)) {
    if (inherits(data, "SpatRaster")) {
      colName <- as.character(get_expr(args[["x"]]))
      testVals <- values(data[[colName]])[, 1]
    } else {
      testVals <- eval_tidy(args[["x"]], data)
    }
  }

  levelsProv <- NULL
  if(pcl$encoding_type == "bool") {

    # protocols like na/nan/inf always output TRUE/FALSE, so no NA check needed
    if (!is.null(testVals) && !protocol %in% c("na", "nan", "inf")) {
      if (any(is.na(testVals)) && is.null(na.val)) {
        stop("Data contains NA values but no 'na.val' was specified. ",
             "For 'bool' encoding, you must provide 'na.val' to substitute NAs.")
      }
    }

    sample_vals <- c(FALSE, TRUE)
    levels_data <- data.frame(
      id = 0:1,
      label = sample_vals
    )

  } else if(pcl$encoding_type == "enum") {

    # validate: NA values need na.val
    if (!is.null(testVals) && any(is.na(testVals)) && is.null(na.val)) {
      stop("Data contains NA values but no 'na.val' was specified. ",
           "For 'enum' encoding, you must provide 'na.val' to substitute NAs.")
    }

    if(protocol == "case"){

      n_cases <- length(args)
      # case values are: 0 (no match), 1 to n_cases (the cases)
      # na.val must be distinct from these to avoid confusion
      if (!is.null(na.val)) {
        if (!is.numeric(na.val) || na.val < 0 || na.val != as.integer(na.val)) {
          stop("'na.val' must be a non-negative integer for 'case' protocol.")
        }
        if (na.val <= n_cases) {
          stop("'na.val' (", na.val, ") conflicts with existing case values (0 = no match, 1-", n_cases, " = cases). ",
               "Please use a value > ", n_cases, ", e.g., na.val = ", n_cases + 1, ".")
        }
      }
      # expand sample_vals to include na.val so encoding allocates enough bits
      max_val <- if (!is.null(na.val)) max(n_cases, na.val) else n_cases
      sample_vals <- 0:max_val
      levels_data <- data.frame(
        id = 0:length(args),
        label = c("no match", names(args)))

    } else {

      # validate: category protocol expects factor/character data
      if (protocol == "category" && !is.null(testVals)) {
        if (!inherits(data, "SpatRaster") && !is.factor(testVals) && !is.character(testVals)) {
          stop("Protocol 'category' expects factor or character data, but got '",
               class(testVals)[1], "'. Convert your data or use a different protocol.")
        }
      }

      if(inherits(data, "SpatRaster")) {
        if(is.factor(data[formalNames$val])){
          levels_data <- levels(data)[[1]]
          names(levels_data)[1] <- "id"

          # validate that actual raster values match expected level IDs
          actualVals <- unique(testVals[!is.na(testVals)])
          expectedIds <- levels_data$id
          invalidVals <- setdiff(actualVals, expectedIds)
          if (length(invalidVals) > 0) {
            stop("Raster values do not match attribute table levels.\n",
                 "  Found values: ", paste(sort(actualVals), collapse = ", "), "\n",
                 "  Expected IDs: ", paste(expectedIds, collapse = ", "), "\n",
                 "  Invalid values: ", paste(sort(invalidVals), collapse = ", "), "\n",
                 "Ensure your raster values are 0-indexed integers matching the attribute table.")
          }
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

      # include na.val so encoding allocates enough bits
      if (!is.null(na.val)) {
        sample_vals <- c(sample_vals, na.val)
      }
    }

  } else {
    # int or float encoding types

    # validate: integer protocol expects integer data (unless auto-scaling args are provided)
    if (protocol == "integer" && !is.null(testVals)) {
      nonNaVals <- testVals[!is.na(testVals)]
      hasScalingArgs <- any(c("range", "decimals", "fields") %in% names(args))
      if (length(nonNaVals) > 0 && is.numeric(nonNaVals) && !is.integer(nonNaVals)
          && !all(nonNaVals == as.integer(nonNaVals)) && !hasScalingArgs) {
        stop("Data contains non-integer numeric values but protocol 'integer' ",
             "expects integer values. Use protocol 'numeric' for floating-point data, ",
             "or provide 'range', 'decimals', or 'fields' for auto-scaling.")
      }
      if (any(is.na(testVals)) && is.null(na.val)) {
        stop("Data contains NA values but no 'na.val' was specified. ",
             "For 'int' encoding, you must provide 'na.val' to substitute NAs.")
      }
    }
    # float encoding handles NAs internally via IEEE-style all-1s pattern, no na.val needed

    if(protocol %in% c("integer", "numeric") & "x" %in% names(args)){

      if(inherits(data, "SpatRaster")) {
        qq <- global(data[[formalNames$val]], quantile, na.rm = TRUE)
        minmax <- global(abs(data[[formalNames$val]]), range, na.rm = TRUE)
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

  # detect if original data is non-integer for auto-scaling decisions
  dataIsNonInteger <- FALSE
  if (protocol == "integer" && !is.null(testVals)) {
    nonNaVals <- testVals[!is.na(testVals)]
    if (length(nonNaVals) > 0 && is.numeric(nonNaVals) && !is.integer(nonNaVals)
        && !all(nonNaVals == as.integer(nonNaVals))) {
      dataIsNonInteger <- TRUE
    }
  }
  enc <- .makeEncoding(var = sample_vals, type = pcl$encoding_type,
                       .data_is_non_integer = dataIsNonInteger, ...)
  # return(enc)

  # validate data type matches template ----
  dataIsRaster <- inherits(data, "SpatRaster")
  templateIsRaster <- registry@template$type == "SpatRaster"
  if(dataIsRaster != templateIsRaster){
    stop("data type does not match registry template: expected '",
         registry@template$type, "' but got '",
         if(dataIsRaster) "SpatRaster" else "data.frame", "'")
  }

  # validate data length matches template ----
  len <- if(dataIsRaster) as.integer(ncell(data)) else as.integer(nrow(data))
  if(registry@template$length != len){
    stop("data length (", len, ") does not match registry template length (",
         registry@template$length, ")")
  }

  # update position ----
  pos <- (registry@template$width + 1L):(registry@template$width + enc$sign + enc$exponent + enc$significand)

  # update registry ----
  registry@template$width <- registry@template$width + enc$sign + enc$exponent + enc$significand

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
  encProv <- list(encodeAsBinary = list(sign = enc$sign, exponent = enc$exponent, significand = enc$significand, bias = enc$bias, scale = enc$scale))
  posProv <- list(assignPosition = c(min(pos), max(pos)))

  # assess encoding quality ----
  # for int/float encoding, raw non-numeric input (e.g., factor from nChar) is
  # not representative of the encoded output, so skip quality assessment
  qualityVals <- testVals
  if (pcl$encoding_type %in% c("int", "float") && !is.numeric(qualityVals)) {
    qualityVals <- NULL
  }
  qualityMetrics <- .assessEncodingQuality(
    values = qualityVals,
    type = pcl$encoding_type,
    enc = enc,
    isRaster = dataIsRaster,
    rasterLevels = if (pcl$encoding_type == "enum" && dataIsRaster) levels_data else NULL
  )
  qd <- qualityMetrics$data
  qualityProv <- list(assessQuality = list(
    type = qd$type,
    n = qd$n,
    n_valid = qd$n_valid,
    n_na = qd$n_na,
    underflow = qd$underflow,
    overflow = qd$overflow,
    precision_loss = qd$precision_loss,
    rmse = qd$rmse,
    max_error = qd$max_error,
    min_resolution = qd$min_resolution,
    max_resolution = qd$max_resolution
  ))

  # store in registry ----
  registry@flags[[thisName]] <- list(comment = desc,
                                     wasDerivedFrom = deparse(substitute(data)),
                                     wasGeneratedBy = c(testProv, argsProv, naProv, levelsProv, encProv, posProv, qualityProv),
                                     wasAssociatedWith = paste0(Sys.info()[["nodename"]], "_", Sys.info()[["user"]]))

  # update MD5 checksum ----
  registry <- .updateMD5(registry)

  return(registry)

}
