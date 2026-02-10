#' Build a flag
#'
#' Convert a flag specification into actual flag values
#' @param registry [`registry(1)`][registry]\cr an already defined bitfield
#'   registry.
#' @param flag [`character(1)`][character]\cr name of the flag to build.
#' @details This function extracts the flag specification, including its test to
#' call it on the data from which the flag shall be created.
#' @return vector of the flag values.
#' @examples
#' reg <- bf_registry(name = "testBF", description = "test bitfield",
#'                    template = bf_tbl)
#' reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg,
#'               x = year)
#' str(reg@flags)
#'
#' bf_flag(registry = reg, flag = "na_year")
#' @importFrom checkmate assertClass assertSubset
#' @importFrom stringr str_split
#' @importFrom terra values
#' @importFrom purrr map
#' @importFrom rlang exec
#' @export

bf_flag <- function(registry, flag = NULL) {

  assertClass(x = registry, classes = "registry")
  assertSubset(x = flag, choices = names(registry@flags))

  theFlag <- registry@flags[[flag]]

  # parse attributes
  protocol <- str_split(theFlag$wasGeneratedBy$useTest, "_")[[1]][1]
  if(protocol %in% names(bf_pcl)){
    pcl <- bf_pcl[[protocol]]
  } else {
    pcl <- get(protocol)
  }
  pcl$test <- paste0(deparse(pcl$test), collapse = "")
  pcl <- .validateProtocol(pcl)

  # get NA value
  naVal <- theFlag$wasGeneratedBy$substituteValue

  # get data
  tryCatch({
    theData <- eval(parse(text = theFlag$wasDerivedFrom), envir = parent.frame(2))
  }, error = function(e) {
    stop("Data object '", theFlag$wasDerivedFrom, "' not found. ",
         "Ensure data remains in scope between bf_map() and bf_encode(), ",
         "or encode immediately after building registry.")
  })

  # call function to build flag
  isRasterCategory <- FALSE
  if(inherits(theData, "SpatRaster")) {
    tempData <- as.data.frame(values(theData))
    # check if this is a category protocol with attribute table
    # in this case, values are already 0-indexed integers and don't need conversion
    if(protocol == "category" && any(is.factor(theData))) {
      isRasterCategory <- TRUE
    }
  } else {
    tempData <- theData
  }
  usefulArgs <- theFlag$wasGeneratedBy$withArguments
  # Only filter by names if the vector has names (case protocol stores unnamed expressions)
  if (!is.null(names(usefulArgs))) {
    usefulArgs <- usefulArgs[!names(usefulArgs) %in% c("...", "fields", "precision", "decimals", "range", ".data_is_non_integer")]
  }
  tidyArgs <- map(as.list(parse(text = usefulArgs)), eval_tidy, tempData)

  # for SpatRaster with attribute table and category protocol,
  # values are already 0-indexed integers - return directly without conversion
  if(isRasterCategory) {
    out <- tidyArgs[[1]]
  } else {
    out <- exec(pcl$test, !!!tidyArgs)
  }

  # apply auto-scaling for integer protocol with scale parameters
  flagScale <- theFlag$wasGeneratedBy$encodeAsBinary$scale
  if (!is.null(flagScale) && pcl$encoding_type == "int") {
    maxInt <- 2L^theFlag$wasGeneratedBy$encodeAsBinary$significand - 1L
    naIdx <- is.na(out)
    scaled <- round((out - flagScale$min) /
                    (flagScale$max - flagScale$min) * maxInt)
    scaled <- pmin(pmax(scaled, 0L), maxInt)
    scaled[naIdx] <- NA
    out <- as.integer(scaled)
  }

  # handle NA values
  # for float encoding, NAs are handled by bf_encode using IEEE-style all-1s pattern
  # for other types, use the substitute value
  if(any(is.na(out)) && pcl$encoding_type != "float") {
    if(is.null(naVal)) {
      stop(paste0("there are NA values in the bit representation of '", flag, "', please define 'na.val'."))
    }
    out[is.na(out)] <- naVal
  }

  if(pcl$encoding_type %in% c("int", "enum")){
    if(all(as.integer(out) == out, na.rm = TRUE)) out <- as.integer(out)
  }

  return(out)
}
