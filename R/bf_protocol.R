#' Define a new bit-flag protocol
#'
#' @param name description
#' @param description description
#' @param test [`function(...)`][function]\cr the function used to build the bit
#'   flag.
#' @param example [`list(.)`][list]\cr named list that contains all arguments in
#'   \code{test} as name with values of the correct type.
#' @param type [`cahracter(1)`][character]\cr the encoding typ according to
#'   which the bit flag is determined. Possible values are \code{bool} (for
#'   binary flags), \code{enum} (for cases), \code{int} (for integers)  and
#'   \code{num} (for floating-point encoding).
#' @param bits description
#' @param version description
#' @param reference description
#' @param extends description
#' @param note description
#' @details
#' @return
#' @examples
#' newFlag <- bf_protocol(name = "na",
#'                        description = "{x} contains NA-values{result}.",
#'                        test = function(x) is.na(x = x),
#'                        example = list(x = bf_tbl$commodity))
#' @importFrom checkmate assertCharacter assertFunction assertChoice
#'   assertIntegerish
#' @importFrom rlang exec
#' @importFrom dplyr case_match case_when
#' @export

bf_protocol <- function(name, description, test, example, type, bits = NULL,
                        extends = NULL, note = NULL, version = NULL,
                        reference = NULL){

  check whether code below needs to be incorporated

  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertCharacter(x = description, len = 1, any.missing = FALSE)
  assertFunction(x = test)
  if(missing(example)){
    stop("please provide example data to run the test on.")
  }
  assertChoice(x = type, choices = c("bool", "enum", "int", "float"))
  assertIntegerish(x = bits, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = version, len = 1, any.missing = FALSE, null.ok = TRUE)

  if(is.null(version)){
    version <- "1.0.0"
  }

  if(is.null(reference)){
    reference <- ""
  }

  out <- exec(test, !!!example)


  # max_val <- max(abs(out), na.rm = TRUE)
  # min_val <- min(abs(out[out != 0]), na.rm = TRUE)
  #
  # if (is.infinite(max_val) || is.infinite(min_val)) {
  #   float_bits <- 32  # Default to single precision for extreme values
  # } else {
  #   range_exponent <- ceiling(log10(max_val)) - floor(log10(min_val))
  #
  #   # Determine precision based on range
  #   if (range_exponent > 10) {
  #     float_bits <- 32  # Single precision
  #   } else if (range_exponent > 5) {
  #     float_bits <- 24  # Half precision plus
  #   } else {
  #     float_bits <- 16  # Half precision
  #   }
  # }

  # determine number of bits, if not given ----
  if (is.null(bits)) {
    bits <- case_when(
      type == "bool" ~ 1,
      type == "enum" & is.integer(out) ~ ceiling(log2(length(unique(out)) + 1)),
      type == "enum" & is.factor(out) ~ ceiling(log2(length(levels(out)) + 1)),
      type == "int" ~ ceiling(log2(max(abs(range(out, na.rm = TRUE))) + 1)) + as.integer(any(out < 0, na.rm = TRUE)),
      type == "float" ~ NA,
    )
  } else {
    # include test that there are not too few bits
  }

  # # If validation is requested, perform more thorough checks
  # if (validate && !is.null(test) && !is.null(example)) {
  #   tryCatch({
  #     test_output <- test(example)
  #
  #     # Check if output is of expected dimensions
  #     if (length(test_output) != length(example) && !is.list(example)) {
  #       warning("Test function output length (", length(test_output),
  #               ") doesn't match example length (", length(example), ")")
  #     }
  #
  #     # Check if output fits within the specified encoding
  #     if (!is.null(encoding_type) && !is.null(bits)) {
  #       encoding_valid <- TRUE
  #
  #       if (encoding_type == "bool" && !all(test_output %in% c(TRUE, FALSE, NA))) {
  #         encoding_valid <- FALSE
  #         warning("Boolean encoding specified but test output contains non-boolean values")
  #       } else if (encoding_type == "enum") {
  #         unique_values <- length(unique(test_output))
  #         max_values <- 2^bits
  #         if (unique_values > max_values) {
  #           encoding_valid <- FALSE
  #           warning("Enum encoding with ", bits, " bits can only represent ",
  #                   max_values, " unique values, but output has ", unique_values)
  #         }
  #       } else if (encoding_type == "int") {
  #         # Check if all values are integers and within range
  #         if (!all(test_output == floor(test_output), na.rm = TRUE)) {
  #           warning("Integer encoding specified but output contains non-integer values")
  #         }
  #
  #         # Check if within representable range
  #         max_int <- 2^(bits - 1) - 1  # Allowing for sign bit
  #         min_int <- -2^(bits - 1)
  #         if (any(test_output > max_int | test_output < min_int, na.rm = TRUE)) {
  #           encoding_valid <- FALSE
  #           warning("Integer values outside representable range with ", bits,
  #                   " bits. Range: [", min_int, ", ", max_int, "]")
  #         }
  #       } else if (encoding_type == "float") {
  #         # Very basic check for float encoding
  #         float_precision <- switch(as.character(bits),
  #                                   "16" = "half",
  #                                   "24" = "single-reduced",
  #                                   "32" = "single",
  #                                   "64" = "double",
  #                                   "custom")
  #
  #         # This is a placeholder for more sophisticated checks
  #         message("Using ", float_precision, " precision floating-point encoding")
  #       }
  #
  #       if (encoding_valid) {
  #         message("Encoding validation passed")
  #       }
  #     }
  #
  #     # Display sample output
  #     if (length(test_output) > 0) {
  #       cat("Test function sample output:\n")
  #       print(head(test_output, 3))
  #       if (length(test_output) > 3) cat("...\n")
  #     }
  #   }, error = function(e) {
  #     warning("Test function validation error: ", e$message)
  #   })
  # }


  if(!is.null(extends)){
    # do some tests here
    # ensure "note" also exists
  }

  # identify non-base packages required ----
  requiredPkgs <- .getDependencies(fun = test)

  # put together the protocol ----
  out <- new(Class = "protocol",
             meta = list(name = name,
                         description = description,
                         version = version,
                         reference = reference),
             test = test,
             specs = list(encoding = type,
                          bits = bits,
                          requires = requiredPkgs,
                          data = example),
             extends = list(protocol = extends,
                            notes = note)
  )

  return(out)

}


create_bitfield_operator <- function(
    name,
    description,
    encoding_type = NULL,
    bits = NULL,
    test = NULL,
    example = NULL,
    requires = NULL,
    extends = NULL,
    extends_note = NULL,
    version = "1.0.0",
    reference = "bitfield R-package",
    validate = TRUE,
    auto_serialize = TRUE
) {

  # Track derivation process for documentation
  derivation_notes <- list()

  # If example is provided but encoding_type or bits are not, try to derive them
  # if (!is.null(example) && (is.null(encoding_type) || is.null(bits))) {
  #   # Test the example with the provided test function
  #   if (!is.null(test) && is.function(test)) {
      test_result <- tryCatch(
        test(example),
        error = function(e) {
          warning(sprintf("Test function failed on example data: %s", e$message))
          return(NULL)
        }
      )

      if (!is.null(test_result)) {

        # Derive encoding_type based on result type
        # if (is.null(encoding_type)) {
          # if (is.logical(test_result)) {
          #   encoding_type <- "bool"
          #   derivation_notes$encoding_type <- "Derived as 'bool' from logical test results"
          # } else if (is.factor(test_result) ||
          #            (is.character(test_result) && length(unique(test_result)) <= 32) ||
          #            (is.integer(test_result) && all(test_result >= 0) && max(test_result, na.rm = TRUE) < 32)) {
          #   encoding_type <- "enum"
          #   derivation_notes$encoding_type <- "Derived as 'enum' from categorical test results"
          # } else if (is.integer(test_result) ||
          #            (is.numeric(test_result) && all(test_result == floor(test_result)))) {
          #   encoding_type <- "int"
          #   derivation_notes$encoding_type <- "Derived as 'int' from integer-like test results"
          # } else if (is.numeric(test_result)) {
          #   encoding_type <- "float"
          #   derivation_notes$encoding_type <- "Derived as 'float' from numeric test results"
          # } else {
          #   warning("Cannot automatically determine encoding_type from test result of class: ",
          #           paste(class(test_result), collapse = ", "))
          # }
        # }

        # Derive bits if needed
        # if (is.null(bits) && !is.null(encoding_type)) {
          # bits <- switch(
            # encoding_type,
            # "bool" = 1,
            # "enum" = {
            #   n_categories <- if(is.factor(test_result)) {
            #     length(levels(test_result))
            #   } else {
            #     length(unique(test_result))
            #   }
            #   needed_bits <- ceiling(log2(max(2, n_categories)))
            #   derivation_notes$bits <- sprintf("Allocated %d bits to represent %d categories",
            #                                    needed_bits, n_categories)
            #   needed_bits
            # },
            # "int" = {
            #   max_value <-
            #   # Add 1 for sign bit if there are negative values
            #   sign_bit <- as.integer(any(test_result < 0, na.rm = TRUE))
            #   needed_bits <-
            #   derivation_notes$bits <- sprintf("Allocated %d bits (%d for value, %d for sign) to represent range [%s, %s]",
            #                                    needed_bits, needed_bits - sign_bit, sign_bit,
            #                                    min(test_result, na.rm = TRUE), max(test_result, na.rm = TRUE))
            #   needed_bits
            # },
            # "float" = {
              # Simple heuristic for float precision
              # # Calculate number of significant digits needed
              # max_val <- max(abs(out), na.rm = TRUE)
              # min_val <- min(abs(out[out != 0]), na.rm = TRUE)
              #
              # if (is.infinite(max_val) || is.infinite(min_val)) {
              #   float_bits <- 32  # Default to single precision for extreme values
              # } else {
              #   range_exponent <- ceiling(log10(max_val)) - floor(log10(min_val))
              #
              #   # Determine precision based on range
              #   if (range_exponent > 10) {
              #     float_bits <- 32  # Single precision
              #   } else if (range_exponent > 5) {
              #     float_bits <- 24  # Half precision plus
              #   } else {
              #     float_bits <- 16  # Half precision
              #   }
              # }
#
#               derivation_notes$bits <- sprintf("Allocated %d bits for floating-point encoding based on data range", float_bits)
#               float_bits
#             },
            # {
            #   warning("Cannot determine bits for encoding_type: ", encoding_type)
            #   NULL
            # }
          # )
        # }
      # }
  #   } else if (is.null(test)) {
  #     warning("Test function is required to derive encoding_type and bits from example")
  #   }
  # }


  # Create the base operator definition
  operator <- list(
    # name = name,
    # description = description,
    encoding_type = encoding_type,
    bits = bits,
    test = test,
    require = requires,
    # extends = extends,
    # extends_note = extends_note,
    # version = version,
    # reference = reference
  )

  # If auto-serialization is enabled and we have a test function
  if (auto_serialize && is.function(test)) {
    # Convert function to string representation for YAML serialization
    operator$serialized_test <- format(test)
  }
}


create_bitfield_operator <- function(
    name,
    description,
    encoding_type = NULL,
    bits = NULL,
    test = NULL,
    example = NULL,
    requires = NULL,
    extends = NULL,
    extends_note = NULL,
    version = "1.0.0",
    reference = "bitfield R-package",
    validate = TRUE,
    auto_serialize = TRUE,
    github_push = FALSE,
    github_repo = NULL,
    github_path = "encodings",
    github_branch = "main",
    github_token = Sys.getenv("GITHUB_PAT")
) {


  # Push to GitHub if requested
  if (github_push) {
    if (is.null(github_repo)) {
      warning("GitHub push requested but no repository specified. Skipping push.")
    } else {
      tryCatch({
        push_operator_to_github(
          operator = operator,
          repo = github_repo,
          path = github_path,
          branch = github_branch,
          token = github_token
        )
      }, error = function(e) {
        warning("Failed to push to GitHub: ", e$message)
      })
    }
  }

  return(operator)
}


