#' Build a bit flag from a numeric value
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{source}.
#' @param source [`character(1)`][character]\cr the column in \code{x} from
#'   which to take the numeric value.
#' @param decimals [`integerish(1)`][integer]\cr description
#' @param type [`character(1)`][integer]\cr description
#' @param options [`list(.)`][list]\cr description
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param description description
#' @param prov description
#' @param registry description
#' @details The length of the bitfield depends on the floating point digits of
#'   the numeric values returned with this function...
#'
#' @examples
#' # bf_numeric(x = tbl_bityield, digits = 3, source = "yield")
#' @importFrom checkmate assert assertDataFrame assertSubset assertIntegerish
#'   assertList testNull
#' @export

bf_numeric <- function(x, source, decimals = NULL, type = NULL, options = NULL,
                       pos = NULL, na.val = NULL, description = NULL, prov = NULL,
                       registry = NULL){

  # library(checkmate); library(rlang); library(tidyverse); type = "half"; options = NULL; pos = NULL; na.val = NULL; description = NULL; prov = NULL

  assertDataFrame(x = x)
  assertSubset(x = source, choices = names(x))
  assertIntegerish(x = decimals, len = 1, any.missing = FALSE, null.ok = TRUE)
  assert(!testNull(x = type), !testNull(x = options))
  if(!is.null(type)){
    assertChoice(x = type, choices = c("half", "bfloat16", "tensor19", "fp24", "pxr24", "single", "double", "auto"))
  }
  if(!is.null(options)){
    assertList(x = options)
    assertNames(x = names(options), subset.of = c("sign", "exponent", "mantissa", "range"))
  }
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertNumeric(x = na.val, len = 1, null.ok = TRUE)
  assertList(x = prov, types = "character", any.missing = FALSE, null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "new_registry")
  }

  thisName <- paste0("numeric_", source)

  out <- x[[source]]

  # determine floating point encoding
  sign <- exponent <- mantissa <- bias <- NULL

  if(!is.null(type)){

    if(type == "auto"){

      stop("work in process, please check in later to see whether this has been implemented!")
      # sign <- ifelse(any(out < 0), 1L, 0L)
      #
      # minBits <- max(ceiling(log2(max(min(abs(out)), 1))), 1) # if the smallest value is smaller than 1, take 1 as value
      # maxBits <- ceiling(log2(max(out)))
      # expRange <- maxBits - minBits + 1
      # exponent <- as.integer(expRange)
      #
      # xInt <- out * 10^decimals
      # xInt <- as.integer(round(xInt))
      # mantissa <- ceiling(log2(max(xInt)))
      # mantissa <- as.integer(mantissa)
      #
      # if(minBits != 0){
      #   bias <- minBits - expRange
      # } else {
      #   bias <- 2**(exponent-1)-1
      # }
      # bias <- as.integer(bias)

    } else {

      sign <- 1L

      exponent <- case_when(type == "half" ~ 5L,
                            type == "bfloat16" ~ 8L,
                            type == "tensor19" ~ 8L,
                            type == "fp24" ~ 7L,
                            type == "pxr24" ~ 8L,
                            type == "single" ~ 8L,
                            type == "double" ~ 11L)

      mantissa <- case_when(type == "half" ~ 10L,
                            type == "bfloat16" ~ 7L,
                            type == "tensor19" ~ 10L,
                            type == "fp24" ~ 16L,
                            type == "pxr24" ~ 15L,
                            type == "single" ~ 23L,
                            type == "double" ~ 52L)

      bias <- 2**(exponent-1)-1

    }

  }

  if(!is.null(options)){

    stop("work in process, please check in later to see whether this has been implemented!")
    # # todo: implement here tests for whether the parameters make sense, and give errors if not
    # assertChoice(x = options$sign, choices = c(0L, 1L))
    # sign <- options$sign
    # exponent <- options$exponent
    # mantissa <- options$mantissa
    # bias <- 2**(exponent-1)-1

  }


  # replace NA values
  if(any(is.na(out))){
    out[is.na(out)] <- na.val
  }

  # update position if it's not set
  if(is.null(pos)){
    pos <- (registry@width + 1L):(registry@width + sign + exponent + mantissa)
  } else {
    # include test that checks whether sufficient positions are set, and give an error if not
  }

  # update the registry
  registry@width <- registry@width + sign + exponent + mantissa
  if(registry@length == 0L){
    registry@length <- length(out)
  } else {
    if(registry@length != length(out)){
      stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # update flag metadata ...
  if(is.null(description)){
    description <- paste0("the bits encode the numeric value in column '", source, "'.")
  }

  enc <- list(sign = sign,
              exponent = exponent,
              mantissa = mantissa,
              bias = bias)

  prov <- list(wasDerivedFrom = source,
               wasGeneratedBy = paste0("encodingAsBinary: ", sign, ".", exponent, ".", mantissa, "/", bias))

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
