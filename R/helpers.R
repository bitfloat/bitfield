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
#' @details Additional details...
#'
#' @examples
#' # example code
#'
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
#' @details Additional details...
#'
#' @examples
#' # example code
#'
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
  if(!is.null(fields)) assertNames(x = names(fields), permutation.of = c("sign", "exponent", "mantissa", "bias"))
  assertIntegerish(x = decimals, len = 1, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(decimals)) assertIntegerish(x = decimals, len = 1, any.missing = FALSE)
  assertIntegerish(x = range, len = 1, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(range)) assertIntegerish(x = range, len = 1, any.missing = FALSE)
  assertCharacter(x = format, len = 1, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(format)) assertChoice(x = format, choices = c("half", "bfloat16", "tensor19", "fp24", "pxr24", "single", "double"))

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
#' @details This function follows the following algorithm: \itemize{ \item set
#'   the current MD5 checksum to NA_character_, \item write the registry into
#'   the temporary directory, \item calculate the checksum of this file and
#'   finally \item store the checksum  in the md5 slot of the registry.} This
#'   means that when comparing the MD5 checksum in this slot, one first has to
#'   set that value also to NULL, otherwise the two values won't coincide.
#' @return this function is called for its side-effect of storing the MD5
#'   checksum in the md5 slot of the registry.
#' @importFrom checkmate assertClass
#' @importFrom tools md5sum
#' @export

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
#' @export

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

      if(!knownFun){
        for (k in seq_along(myPkgs)) {
          inMyPkgs <- tryCatch({
            get(thisFun, envir = asNamespace(myPkgs[k]), mode = "function", inherits = FALSE)
            TRUE
          }, error = function(e) FALSE)

          if (inMyPkgs) {
            temp <- c(temp, myPkgs[k])
            knownFun <- TRUE
            break
          }
        }
      }

      if(!knownFun){
        unknownFuns <- c(unknownFuns, thisFun)
      }

    }

  }

  if(!is.null(unknownFuns)){
    stop(paste0("please load all packages that are required for this bitflag, the following function(s) are not available currently: '", paste0(unique(unknownFuns), collapse = ", "), "'"))
  } else {
    out <- unique(temp)
    return(out)
  }

}

#' Express a bit-flag protocol as yaml file
#'
#' @param pcl description
#' @param file description
#' @return called for the side effect of storing a yml file.
#' @importFrom yaml as.yaml
#' @export

.toYaml <- function(pcl, file){

  write

  # pcl <- bf_protocol(name = "na",
  #                    description = "{x} contains NA-values{result}.",
  #                    test = function(x) is.na(x = x),
  #                    example = list(x = bf_tbl$commodity))
  # file = paste0(getwd(), "/example.yml")

  assertClass(x = pcl, classes = "protocol")

  temp <- list()
  temp$meta <- pcl@meta
  temp$test <- pcl@test
  temp$specs <- pcl@specs
  temp$extends <- pcl@extends

  temp$test <- format(temp$test)

  out <- as.yaml(temp)

  writeLines(out, file)
}

.fromYaml <- function(file){

  write

  # if (!file.exists(file)) {
  #   stop("File not found: ", file)
  # }
  #
  # # Read YAML content
  # yaml_content <- yaml::read_yaml(file)
  #
  # # Convert test function from string to function
  # if (!is.null(yaml_content$test) && is.character(yaml_content$test)) {
  #   tryCatch({
  #     yaml_content$test <- eval(parse(text = yaml_content$test))
  #   }, error = function(e) {
  #     warning("Could not parse test function: ", e$message)
  #   })
  # }
  #
  # # Add the class back
  # class(yaml_content) <- c("bitfield_operator", class(yaml_content))
  #
  # return(yaml_content)

}



.pushGithub <- function(){


  # Push a bitfield operator to GitHub
  #
  # @param operator The bitfield operator to push
  # @param repo The GitHub repository in format "username/repo"
  # @param path Path in the repository where the file should be stored
  # @param branch Branch to push to (default: "main")
  # @param message Commit message
  # @param token GitHub personal access token (or use GITHUB_PAT environment variable)
  # @return Invisibly returns TRUE if successful
  # push_operator_to_github <- function(
  #   operator,
  #   repo,
  #   path = "encodings",
  #   branch = "main",
  #   message = paste("Add", operator$name, "operator"),
  #   token = Sys.getenv("GITHUB_PAT")
  # ) {
  #   # Check for required packages
  #   if (!requireNamespace("gh", quietly = TRUE) || !requireNamespace("base64enc", quietly = TRUE)) {
  #     stop("Packages 'gh' and 'base64enc' are required to push to GitHub. Please install them first.")
  #   }
  #
  #   if (token == "") {
  #     stop("GitHub token is required. Set the GITHUB_PAT environment variable or provide token parameter.")
  #   }
  #
  #   # Ensure operator has a name for the filename
  #   if (is.null(operator$name)) {
  #     stop("Operator must have a name to be pushed to GitHub")
  #   }
  #
  #   # Generate YAML content
  #   yaml_content <- export_operator_to_yaml(operator)
  #
  #   # Create filename from operator name
  #   filename <- paste0(operator$name, "_", operator$version, ".yml")
  #   full_path <- file.path(path, filename)
  #
  #   # Check if file already exists
  #   exists <- tryCatch({
  #     gh::gh(
  #       "GET /repos/{owner}/{repo}/contents/{path}",
  #       owner = strsplit(repo, "/")[[1]][1],
  #       repo = strsplit(repo, "/")[[1]][2],
  #       path = full_path,
  #       ref = branch,
  #       .token = token
  #     )
  #     TRUE
  #   }, error = function(e) {
  #     if (grepl("404", e$message)) {
  #       return(FALSE)
  #     } else {
  #       stop("GitHub API error: ", e$message)
  #     }
  #   })
  #
  #   # Encode content
  #   content_encoded <- base64enc::base64encode(charToRaw(yaml_content))
  #
  #   if (exists) {
  #     # Get the file sha for updating
  #     file_info <- gh::gh(
  #       "GET /repos/{owner}/{repo}/contents/{path}",
  #       owner = strsplit(repo, "/")[[1]][1],
  #       repo = strsplit(repo, "/")[[1]][2],
  #       path = full_path,
  #       ref = branch,
  #       .token = token
  #     )
  #
  #     # Update file
  #     result <- gh::gh(
  #       "PUT /repos/{owner}/{repo}/contents/{path}",
  #       owner = strsplit(repo, "/")[[1]][1],
  #       repo = strsplit(repo, "/")[[1]][2],
  #       path = full_path,
  #       message = message,
  #       content = content_encoded,
  #       sha = file_info$sha,
  #       branch = branch,
  #       .token = token
  #     )
  #
  #     message("Updated operator in GitHub repository: ", full_path)
  #   } else {
  #     # Create new file
  #     result <- gh::gh(
  #       "PUT /repos/{owner}/{repo}/contents/{path}",
  #       owner = strsplit(repo, "/")[[1]][1],
  #       repo = strsplit(repo, "/")[[1]][2],
  #       path = full_path,
  #       message = message,
  #       content = content_encoded,
  #       branch = branch,
  #       .token = token
  #     )
  #
  #     message("Added new operator to GitHub repository: ", full_path)
  #   }
  #
  #   invisible(TRUE)
  # }

}


.pullGithub <- function(){


  # # Download the standard definition file
  # temp_file <- tempfile(fileext = ".yml")
  # download.file(url, temp_file, mode = "wb", quiet = TRUE)
  #
  # # Parse the file (detect format from extension)
  # if (grepl("\\.json$", url, ignore.case = TRUE)) {
  #   if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #     stop("The 'jsonlite' package is required to load JSON standards. Please install it.")
  #   }
  #   standard_def <- jsonlite::fromJSON(temp_file)
  # } else {
  #   if (!requireNamespace("yaml", quietly = TRUE)) {
  #     stop("The 'yaml' package is required to load YAML standards. Please install it.")
  #   }
  #   standard_def <- yaml::read_yaml(temp_file)
  # }
  #
  # # Validate the standard format
  # required_fields <- c("name", "description", "bits_required", "encoding_type", "values", "bit_values")
  # missing_fields <- required_fields[!required_fields %in% names(standard_def)]
  # if (length(missing_fields) > 0) {
  #   stop("The standard definition is missing required fields: ",
  #        paste(missing_fields, collapse = ", "))
  # }
  #
  # # Register the standard (if requested)
  # name <- standard_def$name
  # if (register) {
  #   bf_encodings[[name]] <- list(
  #     bits_required = standard_def$bits_required,
  #     encoding_type = standard_def$encoding_type,
  #     description = standard_def$description,
  #     values = standard_def$values,
  #     bit_values = standard_def$bit_values,
  #     reference = standard_def$reference
  #   )
  #   message("Standard '", name, "' has been loaded and is ready to use with bf_standard().")
  # }
  #
  # # Check if this standard extends another
  # if (!is.null(standard_def$extends)) {
  #   parent_name <- standard_def$extends
  #   parent_version <- standard_def$extends_version
  #
  #   # Check if parent is available
  #   if (!parent_name %in% names(bf_encodings)) {
  #     warning("This standard extends '", parent_name, "' which is not loaded. ",
  #             "Loading only the derived standard.")
  #   } else {
  #     parent <- bf_encodings[[parent_name]]
  #
  #     # Version check if specified
  #     if (!is.null(parent_version) && !is.null(parent$version)) {
  #       if (parent$version != parent_version) {
  #         warning("Parent standard '", parent_name, "' is version ", parent$version,
  #                 " but this extension was created for version ", parent_version, ".")
  #       }
  #     }
  #
  #     # Validate structural compatibility
  #     if (parent$bits_required != standard_def$bits_required) {
  #       warning("Bit structure mismatch: parent requires ", parent$bits_required,
  #               " bits but derivative requires ", standard_def$bits_required, " bits.")
  #     }
  #
  #     # Add inheritance information to standard
  #     standard_def$parent <- parent_name
  #     standard_def$parent_version <- parent$version
  #   }
  # }
  #
  # return(name)



  # Pull a bitfield operator from GitHub
  #
  # @param url Direct URL to the raw YAML file on GitHub
  # @param repo The GitHub repository in format "username/repo"
  # @param path Path to the operator file in the repository
  # @param branch Branch to pull from (default: "main")
  # @param token GitHub personal access token (for private repositories)
  # @return The imported bitfield operator
  # pull_operator_from_github <- function(
  #   url = NULL,
  #   repo = NULL,
  #   path = NULL,
  #   branch = "main",
  #   token = Sys.getenv("GITHUB_PAT")
  # ) {
  #   # Check for required packages
  #   if (!requireNamespace("httr", quietly = TRUE)) {
  #     stop("Package 'httr' is required to pull from GitHub. Please install it first.")
  #   }
  #
  #   if (is.null(url) && (is.null(repo) || is.null(path))) {
  #     stop("Either direct 'url' or both 'repo' and 'path' must be provided")
  #   }
  #
  #   # Construct URL if not provided directly
  #   if (is.null(url)) {
  #     # Format: https://raw.githubusercontent.com/username/repo/branch/path
  #     url <- paste0(
  #       "https://raw.githubusercontent.com/",
  #       repo, "/", branch, "/", path
  #     )
  #   }
  #
  #   # Set headers for authentication if token is provided
  #   headers <- list()
  #   if (token != "" && !is.null(token)) {
  #     headers <- c(headers, Authorization = paste("token", token))
  #   }
  #
  #   # Fetch the YAML content
  #   response <- httr::GET(url, httr::add_headers(.headers = headers))
  #
  #   if (httr::status_code(response) != 200) {
  #     stop("Failed to fetch operator from GitHub: ", httr::http_status(response)$message)
  #   }
  #
  #   # Get content and write to temporary file
  #   content <- httr::content(response, "text", encoding = "UTF-8")
  #   temp_file <- tempfile(fileext = ".yml")
  #   writeLines(content, temp_file)
  #
  #   # Import from the temporary file
  #   operator <- import_operator_from_yaml(temp_file)
  #
  #   # Clean up
  #   unlink(temp_file)
  #
  #   message("Successfully imported operator from ", url)
  #   return(operator)
  # }

}

.listGithub <- function(){

  # List available bitfield operators in a GitHub repository
  #
  # @param repo The GitHub repository in format "username/repo"
  # @param path Path in the repository to search (default: "encodings")
  # @param pattern Pattern to filter filenames (default: "*.yml")
  # @param branch Branch to search (default: "main")
  # @param token GitHub personal access token (for private repositories)
  # @return Data frame of available operators
  # list_github_operators <- function(
  #   repo,
  #   path = "encodings",
  #   pattern = "\\.yml$",
  #   branch = "main",
  #   token = Sys.getenv("GITHUB_PAT")
  # ) {
  #   # Check for required packages
  #   if (!requireNamespace("gh", quietly = TRUE)) {
  #     stop("Package 'gh' is required to list GitHub operators. Please install it first.")
  #   }
  #
  #   # Set up authentication
  #   gh_auth <- list()
  #   if (token != "") {
  #     gh_auth <- list(.token = token)
  #   }
  #
  #   # Get repository contents at the specified path
  #   contents <- tryCatch({
  #     gh::gh(
  #       "GET /repos/{owner}/{repo}/contents/{path}",
  #       owner = strsplit(repo, "/")[[1]][1],
  #       repo = strsplit(repo, "/")[[1]][2],
  #       path = path,
  #       ref = branch,
  #       .token = token
  #     )
  #   }, error = function(e) {
  #     stop("Failed to list repository contents: ", e$message)
  #   })
  #
  #   # Filter for YAML files
  #   if (length(contents) == 0) {
  #     message("No files found in ", path)
  #     return(data.frame())
  #   }
  #
  #   # Process contents
  #   files <- do.call(rbind, lapply(contents, function(item) {
  #     if (item$type == "file" && grepl(pattern, item$name)) {
  #       data.frame(
  #         name = item$name,
  #         path = item$path,
  #         download_url = item$download_url,
  #         size = item$size,
  #         stringsAsFactors = FALSE
  #       )
  #     } else {
  #       NULL
  #     }
  #   }))
  #
  #   if (nrow(files) == 0) {
  #     message("No matching operator files found in ", path)
  #     return(data.frame())
  #   }
  #
  #   return(files)
  # }


}

# bf_citation <- function(registry){

  # Creates formatted citations in multiple styles
  # Supports academic attribution
  # Includes version information


  # format <- match.arg(format)
  #
  # if (!standard_name %in% names(bf_encodings)) {
  #   stop("Standard '", standard_name, "' is not loaded.")
  # }
  #
  # standard <- bf_encodings[[standard_name]]
  #
  # if (is.null(standard$citation)) {
  #   warning("Standard '", standard_name, "' does not have citation information.")
  #   return(paste0("Bitfield standard: ", standard_name,
  #                 ", version ", standard$version %||% "unknown"))
  # }
  #
  # cit <- standard$citation
  #
  # if (format == "text") {
  #   # Format authors
  #   authors <- sapply(cit$authors, function(a) a$name)
  #   if (length(authors) == 1) {
  #     author_str <- authors[1]
  #   } else if (length(authors) == 2) {
  #     author_str <- paste(authors, collapse = " and ")
  #   } else {
  #     author_str <- paste0(
  #       paste(authors[-length(authors)], collapse = ", "),
  #       ", and ", authors[length(authors)]
  #     )
  #   }
  #
  #   # Create text citation
  #   return(paste0(
  #     author_str, " (", cit$year, "). ",
  #     cit$title, ". ",
  #     if (!is.null(cit$publication)) paste0(cit$publication, ". ") else "",
  #     if (!is.null(cit$doi)) paste0("doi:", cit$doi, ". ") else "",
  #     "Bitfield standard: ", standard_name, ", version ", standard$version %||% "unknown", ". ",
  #     if (!is.null(cit$url)) paste0("Available at: ", cit$url) else ""
  #   ))
  # } else if (format == "bibtex") {
  #   # Create BibTeX citation
  #   authors <- sapply(cit$authors, function(a) a$name)
  #   author_str <- paste(authors, collapse = " and ")
  #
  #   return(paste0(
  #     "@misc{", gsub("[^a-zA-Z0-9]", "", standard_name), cit$year, ",\n",
  #     "  author = {", author_str, "},\n",
  #     "  title = {", cit$title, "},\n",
  #     "  year = {", cit$year, "},\n",
  #     if (!is.null(cit$publication)) paste0("  journal = {", cit$publication, "},\n") else "",
  #     if (!is.null(cit$doi)) paste0("  doi = {", cit$doi, "},\n") else "",
  #     "  note = {Bitfield standard: ", standard_name, ", version ", standard$version %||% "unknown", "},\n",
  #     if (!is.null(cit$url)) paste0("  url = {", cit$url, "},\n") else "",
  #     "}"
  #   ))
  # } else if (format == "ris") {
  #   # Create RIS citation
  #   ris <- c(
  #     "TY  - DATA",
  #     paste0("T1  - ", cit$title)
  #   )
  #
  #   for (author in cit$authors) {
  #     ris <- c(ris, paste0("AU  - ", author$name))
  #   }
  #
  #   ris <- c(
  #     ris,
  #     paste0("PY  - ", cit$year),
  #     if (!is.null(cit$publication)) paste0("JO  - ", cit$publication) else NULL,
  #     if (!is.null(cit$doi)) paste0("DO  - ", cit$doi) else NULL,
  #     paste0("N1  - Bitfield standard: ", standard_name, ", version ", standard$version %||% "unknown"),
  #     if (!is.null(cit$url)) paste0("UR  - ", cit$url) else NULL,
  #     "ER  - "
  #   )
  #
  #   return(paste(ris, collapse = "\n"))
  # }


# }
