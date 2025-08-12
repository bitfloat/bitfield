#' Define a new bit-flag protocol
#'
#' @param name [`character(1)`][character]\cr simple name of this protocol.
#' @param description [`character(1)`][character]\cr formalised description of
#'   the operation in this protocol. It will be parsed with
#'   \code{\link[glue]{glue}} and used in the bitfield legend, so can include
#'   the test arguments as enbraced expressions.
#' @param test [`function(...)`][function]\cr the function used to compute the bit
#'   flag.
#' @param example [`list(.)`][list]\cr named list that contains all arguments in
#'   \code{test} as name with values of the correct type.
#' @param type [`character(1)`][character]\cr the encoding type according to
#'   which the bit flag is determined. Possible values are \code{bool} (for
#'   binary flags), \code{enum} (for cases), \code{int} (for integers)  and
#'   \code{num} (for floating-point encoding).
#' @param bits [`integer(1)`][integer]\cr in case the flag requires more bits
#'   than the data in example indicate, provide this here.
#' @param version [`character(1)`][character]\cr the version of this protocol
#'   according to the \emph{semantic versioning
#'   specification}, i.e., of the form \code{X.Y.Z}, where \code{X} is a major
#'   version, \code{Y} is a minor version and \code{Z} is a bugfix. For
#'   additional details on when to increase which number, study
#'   \href{https://semver.org/}{this} website.
#' @param author [`person(.)`][person]\cr to attach a reference to this
#'   protocol, please provide here the relevant information about the author(s).
#'   If this is not provided, the author \code{"unknown"} will be used.
#' @param extends [`character(1)`][character]\cr optional protocol name and
#'   version that is extended by this protocol.
#' @param note [`character(1)`][character]\cr note on what the extension
#'   adds/modifies.
#' @return list containing bit-flag protocol
#' @examples
#' newFlag <- bf_protocol(name = "na",
#'                        description = "{x} contains NA-values{result}.",
#'                        test = function(x) is.na(x = x),
#'                        example = list(x = bf_tbl$commodity),
#'                        type = "bool")
#' @importFrom checkmate assertCharacter assertFunction assertChoice
#'   assertIntegerish
#' @importFrom rlang exec
#' @importFrom dplyr case_match case_when
#' @importFrom utils bibentry
#' @export

bf_protocol <- function(name, description, test, example, type, bits = NULL,
                        version = NULL, extends = NULL, note = NULL,
                        author = NULL){

  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertCharacter(x = description, len = 1, any.missing = FALSE)
  assertFunction(x = test)
  assertList(x = example)
  assertChoice(x = type, choices = c("bool", "enum", "int", "float"))
  assertIntegerish(x = bits, len = 1, null.ok = TRUE)
  assertClass(x = author, classes = "person", null.ok = TRUE)

  if(is.null(version)){
    version <- "1.0.0"
  }

  # determine number of bits, if not given ----
  if (is.null(bits)) {

    out <- exec(test, !!!example)

    bits <- case_when(
      type == "bool" ~ 1,
      type == "enum" & is.integer(out) ~ ceiling(log2(length(unique(out)) + 1)),
      type == "enum" & is.factor(out) ~ ceiling(log2(length(levels(out)) + 1)),
      type == "int" ~ ceiling(log2(max(abs(range(out, na.rm = TRUE))) + 1)) + as.integer(any(out < 0, na.rm = TRUE)),
      type == "float" ~ NA,# <insert code here>
    )
  }

  # identify non-base packages required ----
  requiredPkgs <- .getDependencies(fun = test)

  # reconstruct reference, if none is provided ----
  if(is.null(author)){
    author <- "unknown"
  }
  reference <- bibentry(bibtype = "Manual",
                        title = paste0("bit-flag protocol '", name, "' version ", version),
                        author = author,
                        year = format(Sys.Date(), "%Y"),
                        note = paste0("implemented with bitfield version ", packageVersion("bitfield"), " in ", R.version$version.string, "."))

  # put together the protocol ----
  out <- list(name = name,
              version = version,
              extends = extends,
              extends_note = note,
              description = description,
              encoding_type = type,
              bits = bits, # when the test is general and could result in any number of bits, use NA here
              requires = requiredPkgs,
              test = paste0(deparse(test), collapse = ""),
              data = example,
              reference = reference)

  out <- .validateProtocol(out)

  return(out)

}
