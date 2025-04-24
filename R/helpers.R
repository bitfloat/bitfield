#' Make a binary value from an integer
#'
#' @param x [`numeric(1)`][numeric]\cr the numeric value for which to derive the
#'   binary value.
#' @param len [`integerish(1)`][integer]\cr the number of bits used to capture
#'   the value.
#' @param dec [`logical(1)`][logical]\cr whether to transform the decimal part
#'   to bits, or the integer part.
#' @param pad [`logical(1)`][logical]\cr whether to pad the binary value with 0
#'   values.
#' @importFrom checkmate assertIntegerish assertNumeric
#' @importFrom stringr str_pad

.toBin <- function(x, len = NULL, pad = TRUE, dec = FALSE){

  assertNumeric(x = x)
  assertIntegerish(x = len, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = dec, len = 1, any.missing = FALSE)
  assertLogical(x = pad, len = 1, any.missing = FALSE)

  if(dec){
    x <- as.numeric(paste0(0, ".", str_split(x, "[.]", simplify = T)[,2]))

    temp <- map(.x = x, .f = function(ix){
      val <- ix
      bin <- NULL

      if(is.null(len)){

        while(val > 0){
          val <- val * 2
          bin <- c(bin, val %/% 1)
          val <- val - val %/% 1
        }

      } else {

        i <- 0
        while(val > 0 & i < len){
          val <- val * 2
          bin <- c(bin, val %/% 1)
          val <- val - val %/% 1
          i <-  i + 1
        }

      }
      bin <- paste0(bin, collapse = "")

      return(bin)
    }) |> unlist()

  } else {
    x <- as.integer(x)

    temp <- map(.x = x, .f = function(ix){
      val <- ix
      bin <- NULL
      if(!is.null(len)){

        while(len > 0){
          bin[len] <- val %% 2
          val <- val %/% 2
          len <- len - 1
        }

      } else {

        while(val > 0){
          bin <- c(val %% 2, bin)
          val <- val %/% 2
        }

        if(is.null(bin)) bin <- 0

      }

      bin <- paste0(bin, collapse = "")

      return(bin)
    }) |> unlist()

    if(pad){
      temp <- str_pad(temp, width = max(nchar(temp)), pad = "0")
    }
  }

  return(temp)
}


#' Make an integer from a binary value
#'
#' @param x [`character(1)`][character]\cr the binary value (character of 0s and
#'   1s) for which to derive the integer.
#' @importFrom checkmate assertCharacter assertNames testIntegerish assertSubset
#' @importFrom stringr str_split

.toDec <- function(x){

  assertCharacter(x = x, any.missing = FALSE)

  out <- map(.x = x, .f = function(ix){

    temp <- str_split(ix, "", simplify = TRUE)
    radix <- which(temp == ".")
    if(length(radix) == 0){
      radix <- length(temp)+1
      bits <- as.integer(temp)
    } else {
      assertIntegerish(x = radix, any.missing = FALSE, len = 1)
      bits <- as.integer(temp[-radix])
    }
    assertSubset(x = bits, choices = c(0, 1))

    sum(bits * 2^((seq(bits) * -1) + radix-1))
  }) |> unlist()

  # if it's integerish, convert it to integer
  if(testIntegerish(out)){
    out <- as.integer(out)
  }

  return(out)
}

#' Determine encoding
#'
#' @param var the variable for which to determine encoding.
#' @param type the encoding type for which to determine encoding.
#' @param ... [`list(.)`][list]\cr named list of options to determine encoding,
#'   see Details.
#' @details Floating point values are encoded with three fields that can be
#'   readily stored as bit sequence. Any numeric value can be represented in
#'   scientific notation, for example, the decimal 923.52 can be represented as
#'   9.2352 * 10^2. These decimal values can be transformed to binary values,
#'   which can then likewise be represented in scientific notation. Here, the 10
#'   is replaced by a 2 (because we go from decimal to binary), for example the
#'   binary value 101011.101 can be represented as 1.01011101 * 2^5. This
#'   scientific notation can now be broken down into the three previously
#'   mentioned fields, one for the sign (positive or negative), one for the
#'   exponent and one for the remaining part, the mantissa (or significand). For
#'   background information on how these fields are processed, study for
#'   instance
#'   \href{https://www.cs.cornell.edu/~tomf/notes/cps104/floating}{'Floating
#'   Point' by Thomas Finley} and check out
#'   \href{https://float.exposed/}{https://float.exposed/} to play around with
#'   floating point encoding. Depending on the encoding needs, these three
#'   values can be adapted, for example increase the exponent to provide a wider
#'   range (i.e., smaller small and larger large values) or increase the
#'   mantissa to provide more precision (i.e., more decimal digits). In the
#'   scope of this package, these three values are documented with a tag of the
#'   form \[x.y.z\], with x = number of sign bits (either 0 or 1), y = number of
#'   exponent bits, and z number of mantissa bits.
#'
#'   When handling values that are not numeric, this package makes use of the
#'   same system, only that sign and exponent are set to 0, while the mantissa
#'   bits are set to either 1 (for binary responses \[0.0.1\]), or to whatever
#'   number of cases are required (i.e., for 8 cases with 3 required bits,
#'   resulting in the tag \[0.0.3\]).
#'
#'   Possible options (\code{...}) of this function are \itemize{
#'     \item \code{precision}: switch that determines the configuration of the
#'           \href{https://en.wikipedia.org/wiki/Bfloat16_floating-point_format}{floating point encoding}.
#'           Possible values are \code{"half"} \[1.5.10\], \code{"bfloat16"}
#'           \[1.8.7\], \code{"tensor19"} \[1.8.10\], \code{"fp24"} \[1.7.16\],
#'           \code{"pxr24"} \[1.8.15\], \code{"single"} \[1.8.23\] and
#'           \code{"double"} \[1.11.52\],
#'     \item \code{fields}: list of custom values that control how many bits are
#'           allocated to \code{sign}, \code{exponent} and \code{mantissa} for
#'           encoding the numeric values,
#'     \item \code{range}: the ratio between the smallest and largest possible
#'           value to be reliably represented (modifies the exponent),
#'     \item \code{decimals}: the number of decimal digits that should be
#'           represented reliably (modifies the mantissa).
#'   }
#' @return list of the encoding values for sign, exponent and mantissa, and an
#'   additional provenance term.
#' @importFrom purrr map
#' @importFrom checkmate assertIntegerish assertList testNull testIntegerish
#'   assertNames assertChoice assertCharacter
#' @importFrom dplyr case_when

.makeEncoding <- function(var, type, ...){

  assertChoice(x = type, choices = c("bool", "enum", "int", "float"))

  # set dots to arguments
  encArgs <- map(unlist(list(...), recursive = FALSE), eval_tidy)
  # return(encArgs)

  for(name in c("format", "decimals", "range", "fields")){
    if(name %in% names(encArgs)){
      assign(name, encArgs[[name]], envir = environment())
    } else {
      assign(name, NULL, envir = environment())
    }
  }
  assertList(x = fields, null.ok = TRUE)
  if(!is.null(fields))
    assertNames(x = names(fields), permutation.of = c("sign", "exponent", "mantissa", "bias"))
  assertIntegerish(x = decimals, len = 1, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(decimals))
    assertIntegerish(x = decimals, len = 1, any.missing = FALSE)
  assertIntegerish(x = range, len = 1, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(range))
    assertIntegerish(x = range, len = 1, any.missing = FALSE)
  assertCharacter(x = format, len = 1, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(format))
    assertChoice(x = format, choices = c("half", "bfloat16", "tensor19", "fp24", "pxr24", "single", "double"))

  sign <- exp <- mant <- bias <- NULL

  # determine variable sign
  autoSign <- ifelse(any(var < 0, na.rm = TRUE), 1, 0)

  # determine variable mantissa
  xInt <- var * 10^max(decimals, 0)
  xInt <- as.integer(xInt)
  autoMant <- ceiling(log2(max(xInt, 2, na.rm = TRUE) + 1))

  # determine variable exponent
  expRange <- floor(log10(abs(var)))
  bitSize <- max(max(expRange) - min(expRange), range)
  autoExp <- ceiling(log2(bitSize + 1))

  if(type == "bool"){

    sign <- 0
    exp <- 0
    mant <- 1

  } else if(type == "enum"){

    sign <- 0
    exp <- 0
    mant <- autoMant

  } else if(type == "int"){

    sign <- autoSign
    exp <- 0
    mant <- autoMant

  } else {

    if(!is.null(format)){

      sign <- 1
      exp <- case_when(format == "half" ~ 5,
                       format == "bfloat16" ~ 8,
                       format == "tensor19" ~ 8,
                       format == "fp24" ~ 7,
                       format == "pxr24" ~ 8,
                       format == "single" ~ 8,
                       format == "double" ~ 11)

      mant <- case_when(format == "half" ~ 10,
                        format == "bfloat16" ~ 7,
                        format == "tensor19" ~ 10,
                        format == "fp24" ~ 16,
                        format == "pxr24" ~ 15,
                        format == "single" ~ 23,
                        format == "double" ~ 52)

    } else {

      sign <- autoSign
      exp <- autoExp
      mant <- autoMant

    }

  }

  # possibly update the specific fields
  if(!is.null(fields) & !is.null(fields$sign)){
    assertChoice(x = fields$sign, choices = c(0, 1))
    if(fields$sign < sign){
      stop("It is not possible to set less than ", sign, " 'sign' bits.")
    } else {
      sign <- fields$sign
    }
  }

  if(!is.null(fields) & !is.null(fields$exponent)){
    assertIntegerish(x = fields$exponent, lower = 0, len = 1, any.missing = FALSE)
    if(fields$exponent < exp){
      stop("It is not possible to set less than ", exp, " 'exponent' bits.")
    } else {
      exp <- fields$exponent
    }
  }

  if(!is.null(fields) & !is.null(fields$mantissa)){
    assertIntegerish(x = fields$mantissa, lower = 0, len = 1, any.missing = FALSE)
    if(fields$mantissa < mant){
      stop("It is not possible to set less than ", mant, " 'mantissa' bits.")
    } else {
      mant <- fields$mantissa
    }
  }

  enc <- list(sign = as.integer(sign),
              exponent = as.integer(exp),
              mantissa = as.integer(mant),
              bias = as.integer(2**(exp-1)-1))

  return(enc)
}

#' Determine and write MD5 sum
#'
#' @param x [`registry(1)`][registry]\cr registry for which to determine the MD5
#'   checksum.
#' @details This function follows this algorithm: \itemize{ \item set the
#'   current MD5 checksum to NA_character_, \item write the registry into the
#'   temporary directory, \item calculate the checksum of this file and finally
#'   \item store the checksum in the md5 slot of the registry.} This means that
#'   when comparing the MD5 checksum in this slot, one first has to set that
#'   value also to NA_character_, otherwise the two values won't coincide.
#' @return this function is called for its side-effect of storing the MD5
#'   checksum in the md5 slot of the registry.
#' @importFrom checkmate assertClass
#' @importFrom tools md5sum

.updateMD5 <- function(x){

  assertClass(x = x, classes = "registry")

  tempReg <- paste0(tempdir(), "/tempReg.rds")

  temp <- out <- x
  temp@md5 <- NA_character_
  saveRDS(object = temp, file = tempReg)

  tempSum <- md5sum(files = tempReg)
  names(tempSum) <- NULL

  unlink(x = tempReg)

  out@md5 <- tempSum

  return(out)
}


#' Extract values and metadata from terra::SpatRaster
#'
#' @param x [SpatRaster(1)][terra::SpatRaster]\cr the SpatRaster object.
#' @details This function simply extracts the values from \code{x} and appends
#'   the raster metadata as attributes.
#' @return the function that extracts values and metadata.
#' @importFrom checkmate assertClass
#' @importFrom terra values nrow ncol res ext crs
#' @export

.rast <- function(x){

  assertClass(x = x, classes = "SpatRaster")

  # Create the accessor function that will extract values when called
  accessor <- function() {
    values <- terra::values(x)

    # Return values along with metadata about the raster
    attr(values, "rast_meta") <- list(
      nrow = nrow(x),
      ncol = ncol(x),
      res = res(x),
      ext = ext(x),
      crs = crs(x)
    )

    return(values)
  }

  # Set class so bitfield operators can recognize this is a raster accessor
  class(accessor) <- c("bf_rast", "function")

  return(accessor)

}

#' Identify packages to custom functions
#'
#' @param fun [`function(...)`][function]\cr the custom function in which to
#'   identify dependencies.
#' @return vector of packages that are required to run the function.
#' @importFrom checkmate assertFunction
#' @importFrom codetools findGlobals

.getDependencies <- function(fun){

  assertFunction(x = fun)

  # Get all function calls from the function
  funCalls <- findGlobals(fun, merge = FALSE)$functions

  # Base R packages to exclude
  basePkgs <- c("base", "stats", "graphics", "grDevices", "utils", "datasets", "methods")
  myPkgs <- setdiff(loadedNamespaces(), basePkgs)
  unknownFuns <- temp <- NULL

  for(i in seq_along(funCalls)){

    knownFun <- FALSE
    thisFun <- funCalls[i]
    if (exists(thisFun, envir = environment(fun), inherits = FALSE)) {
      next
    }

    for(j in seq_along(basePkgs)){
      inBasePkgs <- tryCatch({
        get(thisFun, envir = asNamespace(basePkgs[j]), mode = "function", inherits = FALSE)
        TRUE
      }, error = function(e) FALSE)

      if (inBasePkgs) {
        knownFun <- TRUE
        break
      }
    }

    if(!knownFun){

      for (k in seq_along(myPkgs)) {
        inMyPkgs <- tryCatch({
          getNamespaceName(environment(get(thisFun)))
        }, error = function(e) NULL)

        if (!is.null(inMyPkgs)) {
          temp <- c(temp, inMyPkgs)
          knownFun <- TRUE
          break
        }
      }
    }

    if(!knownFun){
      unknownFuns <- c(unknownFuns, thisFun)
    }

  }

  if(!is.null(unknownFuns)){
    stop(paste0("please load all packages that are required for this bitflag, the following function(s) are not available currently: '", paste0(unique(unknownFuns), collapse = ", "), "'"))
  } else {
    out <- unique(temp)
    return(out)
  }

}

#' Validate a github token
#'
#' This function checks whether the user-provided token is valid for use with
#' this package.
#' @param token [`character(1)`][character]\cr github PAT (personal access
#'   token).
#' @return the validated user token
#' @importFrom checkmate assertCharacter
#' @importFrom gitcreds gitcreds_get
#' @importFrom httr GET add_headers status_code

.validateToken <- function(token) {

  assertCharacter(x = token, len = 1, any.missing = FALSE, null.ok = TRUE)

  if (is.null(token) || token == "") {
    token <- tryCatch({
      gitcreds_get()$password
    }, error = function(e) {
      NULL
    })
  }

  if (is.null(token) || token == "") {
    token <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN", ""))
  }

  if (is.null(token) || token == "") {
    stop("No GitHub token found. Please either:
         1. Set up a GitHub PAT with `usethis::create_github_token()` and save it with `gitcreds::gitcreds_set()`
         2. Set the GITHUB_PAT environment variable in your .Renviron file

         For more information, run `usethis::gh_token_help()`", call. = FALSE)
  }

  # Validate token with a lightweight GitHub API call
  response <- GET(
    "https://api.github.com/user",
    add_headers(Authorization = paste("token", token)),
    add_headers("User-Agent" = "bitfield-r-package")
  )

  if (status_code(response) != 200) {
    warning("Invalid GitHub token")
    return(NULL)
  }

  return(token)
}

#' Validate a bit-flag protocol
#'
#' @param protocol the protocol to validate
#' @return the validated protocol
#' @importFrom checkmate assertList assertNames assertFunction assertCharacter
#'   assertIntegerish assertChoice testCharacter
#' @importFrom rlang exec
#' @importFrom stringr str_match

.validateProtocol <- function(protocol){

  # ensure all slots are properly set
  assertList(x = protocol)
  assertNames(x = names(protocol),
              subset.of = c("name", "version", "version_history", "extends", "extends_note", "description", "encoding_type", "bits", "requires", "test", "data", "reference"))
  assertCharacter(x = protocol$name, len = 1, any.missing = FALSE)
  assertCharacter(x = protocol$version, len = 1, any.missing = FALSE, pattern = "(\\d+\\.\\d+\\.\\d+)")
  assertCharacter(x = protocol$extends, len = 1, null.ok = TRUE)
  assertCharacter(x = protocol$extends_note, len = 1, null.ok = TRUE)
  assertCharacter(x = protocol$description, len = 1, any.missing = FALSE)
  assertCharacter(x = protocol$encoding_type, len = 1, any.missing = FALSE)
  assertIntegerish(x = protocol$bits, len = 1, lower = 1)
  assertFunction(x = protocol$test)
  assertList(x = protocol$data)

  # ensure that glue statements have only names that are also in the data

  # ensure extensions are provided correctly
  if(!is.null(protocol$extends)){
    assertCharacter(x = protocol$extends, pattern = "^([a-zA-Z]+)_(\\d+\\.\\d+\\.\\d+)$")
    matches <- as.character(str_match(protocol$extends, "^([a-zA-Z]+)_(\\d+)\\.(\\d+)\\.(\\d+)$"))
    assertChoice(x = matches[2], choices = names(bf_pcl))
    if(!testCharacter(x = protocol$extends_note, any.missing = FALSE, min.len = 1)) stop("please provide a short note about what this extension changes.")
  }

  # does the test run with the provided data
  testResult <- tryCatch({
    exec(protocol$test, !!!protocol$data)
  }, error = NULL)

  if(!is.null(testResult)){
    testTyp <- case_when(
      protocol$encoding_type == "bool" ~ is.logical(testResult),
      protocol$encoding_type == "enum" ~ is.factor(testResult) || is.integer(testResult) || all(testResult == as.integer(testResult), na.rm = TRUE),
      protocol$encoding_type == "int" ~ is.integer(testResult) || all(testResult == as.integer(testResult), na.rm = TRUE),
      protocol$encoding_type == "float" ~ is.numeric(testResult),
    )

    if(!testTyp) stop(paste0("'test' and 'data' result in output that doesn't fit the encoding '", protocol$encoding_type, "'."))
  } else {
    stop("'test' and 'data' don't result in a valid call.")
  }

  return(protocol)

}
