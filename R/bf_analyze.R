#' Analyze encoding options for data
#'
#' This function helps you choose appropriate bit allocations for encoding data.
#' It auto-detects the data type and provides relevant analysis:
#' \itemize{
#'   \item Numeric with decimals: trade-offs for floating point encoding, which
#'         exponent/significand combinations are adequate for your range and
#'         precision requirements.
#'   \item Integer: (signed) integer encoding, how many bits are required.
#'   \item Factor/character: category/enumeration encoding, which levels are in
#'         the data and how many bits are required
#'   \item Logical: boolean encoding, do NA values require a second bit.
#' }
#'
#' All of this can be applied both to columns in a table or layers in a
#' SpatRaster. Use this before \code{\link{bf_map}} to understand your encoding
#' options.
#'
#' @section Float analysis output:
#' For numeric (float) data, the output table shows Pareto-optimal
#' exponent/significand configurations. The columns are:
#' \describe{
#'   \item{Exp, Sig, Total}{Number of exponent bits, significand bits, and
#'     their sum. More exponent bits extend the representable range (at the cost
#'     of coarser resolution), while more significand bits improve resolution
#'     within each exponent band.}
#'   \item{Underflow}{Percentage of data values that fall below the smallest
#'     representable positive value. These values are rounded to zero.}
#'   \item{Overflow}{Percentage of data values that exceed the largest
#'     representable value. These values are clipped to the maximum.}
#'   \item{Changed}{Percentage of data values that change when encoded and
#'     decoded (i.e., that do not survive the round-trip exactly).}
#'   \item{Min Res, Max Res}{Smallest and largest step size between adjacent
#'     representable values. In minifloat encoding, resolution varies across the
#'     range: small values near zero have fine resolution (small steps), while
#'     large values have coarse resolution (large steps). A Max Res of 1.0 means
#'     that in the coarsest region, only integer values can be represented --
#'     continuous input will be rounded to whole numbers.}
#'   \item{RMSE}{Root mean squared error between original and decoded values,
#'     computed over all non-NA data points.}
#'   \item{Max Err}{Largest absolute difference between any original value and
#'     its decoded counterpart.}
#' }
#'
#' @section Choosing a configuration:
#' The table only shows Pareto-optimal configurations, i.e., those where no
#' other configuration is strictly better on all quality metrics for the same
#' or fewer total bits. To choose between them:
#' \itemize{
#'   \item Check \strong{Underflow} and \strong{Overflow} first. Non-zero values
#'     indicate data loss at the extremes of your range. Adding exponent bits
#'     or using the \code{range} argument to widen the target range can help.
#'   \item Compare \strong{RMSE} and \strong{Max Err} to your acceptable
#'     precision. If you specified \code{decimals}, look for configurations
#'     where Max Res is at most \code{10^(-decimals)}.
#'   \item If \strong{Max Res} is >= 1, decoded values in the upper range will
#'     appear as integers even if the input was continuous. This may or may not
#'     be acceptable depending on your application.
#' }
#'
#' @section Specifying configurations with \code{fields}:
#' By default, all combinations up to \code{max_bits} are evaluated and only
#' the Pareto front is shown. Use the \code{fields} argument to instead compare
#' specific configurations:
#' \itemize{
#'   \item \code{fields = list(exponent = 4)} shows all significand values
#'     paired with 4 exponent bits.
#'   \item \code{fields = list(exponent = c(3, 4), significand = c(5, 4))}
#'     compares exp=3/sig=5 and exp=4/sig=4.
#' }
#'
#' @param x A numeric, integer, logical, factor, character vector, or single
#'   layer SpatRaster to analyze. The type is auto-detected.
#' @param range [`numeric(2)`][numeric]\cr optional target range \code{c(min,
#'   max)} to design for (float analysis only). Defaults to the actual data
#'   range.
#' @param decimals [`integer(1)`][integer]\cr optional decimal places of
#'   precision required (float analysis only).
#' @param min_bits [`integer(1)`][integer]\cr minimum total bits to display in
#'   the Pareto table output. Configurations with fewer bits are hidden. Default
#'   is \code{NULL} (show all).
#' @param max_bits [`integer(1)`][integer]\cr maximum total bits to consider.
#'   Defaults to 16.
#' @param fields [`list`][list]\cr optional list specifying which configurations
#'   to analyze (float analysis only). See Details.
#' @param plot [`logical(1)`][logical]\cr whether to generate a plot (float
#'   analysis only). Default is \code{FALSE}.
#'
#' @return An object of class \code{bf_analysis} with analysis results.
#'
#' @examples
#' # float analysis (numeric with decimals)
#' bf_analyze(bf_tbl$yield)
#'
#' # with specific decimal precision requirement
#' bf_analyze(bf_tbl$yield, decimals = 2)
#'
#' # design for a larger range than current data
#' bf_analyze(bf_tbl$yield, range = c(0, 20))
#'
#' # with visualization
#' bf_analyze(bf_tbl$yield, decimals = 2, plot = TRUE)
#'
#' # compare specific configurations
#' bf_analyze(bf_tbl$yield, fields = list(exponent = c(2, 3, 4), significand = c(5, 4, 3)))
#'
#' # show all combinations for a specific exponent
#' bf_analyze(bf_tbl$yield, fields = list(exponent = 4))
#'
#' # integer analysis
#' bf_analyze(as.integer(c(0, 5, 10, 100)))
#'
#' # category/enum analysis
#' bf_analyze(bf_tbl$commodity)
#'
#' # boolean analysis
#' bf_analyze(c(TRUE, FALSE, TRUE, NA))
#'
#' # raster with attribute table
#' library(terra)
#' r <- rast(nrows = 3, ncols = 3, vals = c(0, 1, 2, 0, 1, 2, 0, 1, 2))
#' levels(r) <- data.frame(id = 0:2, label = c("low", "medium", "high"))
#' bf_analyze(r)
#'
#' @importFrom checkmate assertNumeric assertInt assertLogical assertList
#' @importFrom terra values levels is.factor
#' @importFrom graphics axis legend lines mtext par layout rect segments
#' @importFrom grDevices rgb
#' @export

bf_analyze <- function(x, range = NULL, decimals = NULL, min_bits = NULL, max_bits = 16L, fields = NULL, plot = FALSE) {

  assertInt(x = max_bits, lower = 3, upper = 32)
  assertLogical(x = plot, len = 1, any.missing = FALSE)
  if (!is.null(min_bits)) assertInt(x = min_bits, lower = 1)

  # detect data type and extract values
  isRaster <- inherits(x, "SpatRaster")
  rasterLevels <- NULL

  if (isRaster) {
    if (dim(x)[3] > 1) {
      stop("Please provide a single layer SpatRaster, not multiple layers.")
    }
    if (is.factor(x)) {
      rasterLevels <- levels(x)[[1]]
    }
    xVals <- values(x)[, 1]
  } else {
    xVals <- x
  }

  # determine encoding type
  if (!is.null(rasterLevels) || is.factor(xVals) || is.character(xVals)) {
    type <- "enum"
  } else if (is.logical(xVals)) {
    type <- "bool"
  } else if (is.integer(xVals)) {
    type <- "int"
  } else if (is.numeric(xVals)) {
    xClean <- xVals[!is.na(xVals)]
    if (length(xClean) > 0 && all(xClean == floor(xClean))) {
      type <- "int"
      xVals <- as.integer(xVals)
    } else {
      type <- "float"
    }
  } else {
    stop("Unsupported data type: ", class(xVals)[1])
  }

  # call unified analysis function
  out <- .assessEncodingQuality(
    values = xVals,
    type = type,
    isRaster = isRaster,
    rasterLevels = rasterLevels,
    range = range,
    decimals = decimals,
    max_bits = max_bits,
    fields = fields,
    errorOnAllNA = TRUE
  )

  # plot if requested (float only)
  if (plot && out$data$type == "float" && !is.null(out$results)) {
    plotData <- out$results[out$data$pareto, ]
    fieldsMode <- out$data$fields_mode

    oldPar <- par(mar = c(5, 4, 3, 4))
    on.exit(par(oldPar), add = TRUE)

    ymax <- max(c(plotData$underflow, plotData$overflow, plotData$changed), na.rm = TRUE)
    ymax <- max(ymax, 1)

    if (fieldsMode) {
      xVals <- seq_len(nrow(plotData))
      xLabels <- paste0(plotData$exp, "/", plotData$sig)
      plotTitle <- "Encoding Comparison"
      xLabel <- "Configuration (Exp/Sig)"
    } else {
      xVals <- plotData$total
      xLabels <- plotData$total
      plotTitle <- "Encoding Trade-offs (Pareto Front)"
      xLabel <- "Total Bits"
    }

    plot(xVals, plotData$underflow,
         type = "b", pch = 16, col = "red",
         main = plotTitle,
         xlab = xLabel, ylab = "Percentage (%)",
         ylim = c(0, ymax * 1.1), xaxt = "n")
    axis(1, at = xVals, labels = xLabels, las = if (fieldsMode) 2 else 1)

    lines(xVals, plotData$overflow, type = "b", pch = 17, col = "orange")
    lines(xVals, plotData$changed, type = "b", pch = 15, col = "blue")

    par(new = TRUE)
    errMax <- max(c(plotData$rmse, plotData$max_err), na.rm = TRUE)
    errMax <- max(errMax, 0.001)

    plot(xVals, sqrt(plotData$rmse),
         type = "b", pch = 18, col = "gray40", lty = 2,
         xlab = "", ylab = "", ylim = c(0, sqrt(errMax) * 1.1),
         xaxt = "n", yaxt = "n")
    lines(xVals, sqrt(plotData$max_err), type = "b", pch = 4, col = "gray60", lty = 2)
    axis(4, col = "gray40", col.axis = "gray40")
    mtext("Error (sqrt)", side = 4, line = 2.5, col = "gray40")

    legend("topright",
           legend = c("Underflow", "Overflow", "Changed", "RMSE", "Max Err"),
           pch = c(16, 17, 15, 18, 4),
           col = c("red", "orange", "blue", "gray40", "gray60"),
           lty = c(1, 1, 1, 2, 2), cex = 0.8)
  }

  out$data$min_bits <- min_bits
  return(out)
}


#' Print method for bf_analysis
#' @param x bf_analysis object
#' @param min_bits minimum total bits to display (overrides value from
#'   bf_analyze if provided)
#' @param ... additional arguments (ignored)
#' @export
print.bf_analysis <- function(x, min_bits = NULL, ...) {

  # use min_bits from bf_analyze() unless overridden in print()
  if (is.null(min_bits)) min_bits <- x$data$min_bits

  analysisType <- x$data$type
  if (is.null(analysisType)) analysisType <- "float"

  # header
  title <- switch(analysisType,
    bool  = "Boolean",
    int   = "Integer",
    enum  = "Category/Enum",
    float = "Float"
  )
  cat(title, " Analysis\n", sep = "")
  cat(strrep("=", nchar(title) + 9), "\n\n", sep = "")

  # unified summary block
  nNA <- x$data$n_na
  if (is.null(nNA)) nNA <- x$data$n - x$data$n_valid

  rangeStr <- switch(analysisType,
    bool  = "-",
    int   = paste0("[", x$data$min, ", ", x$data$max, "]"),
    enum  = "-",
    float = paste0("[", sprintf("%.6g", x$data$data_range["min"]), ", ",
                   sprintf("%.6g", x$data$data_range["max"]), "]")
  )

  levelsStr <- switch(analysisType,
    enum  = as.character(x$data$n_levels),
    "-"
  )

  signStr <- switch(analysisType,
    int   = if (x$data$needs_sign) "yes" else "no",
    float = if (x$data$needs_sign) "yes" else "no",
    "-"
  )

  bitsStr <- switch(analysisType,
    float = "select from the table below",
    as.character(x$data$bits_required)
  )

  naValStr <- switch(analysisType,
    float = "automatic",
    "-"
  )
  if (nNA > 0) {
    naVal <- x$data$suggested_na_val
    if (!is.null(naVal)) naValStr <- as.character(naVal)
  }

  cat("  Observations      ", x$data$n, "\n", sep = "")
  cat("  NA values         ", nNA, "\n", sep = "")
  cat("  Range             ", rangeStr, "\n", sep = "")
  cat("  Levels            ", levelsStr, "\n", sep = "")
  cat("  Sign required     ", signStr, "\n", sep = "")
  cat("  Bits required     ", bitsStr, "\n", sep = "")
  cat("  Suggested na.val ", naValStr, "\n", sep = " ")

  # type-specific extras
  if (analysisType == "bool") {

    cat("\n  TRUE            ", x$data$n_true, "\n", sep = "")
    cat("  FALSE           ", x$data$n_false, "\n", sep = "")

  } else if (analysisType == "enum") {

    if (x$data$has_gaps) {
      cat("\n  WARNING: Gaps in level IDs at: ", paste(x$data$gaps, collapse = ", "), "\n", sep = "")
    }

    cat("\n")
    cat(sprintf("  %4s  %-20s  %s\n", "ID", "Label", "Count"))
    cat(sprintf("  %4s  %-20s  %s\n", "----", "--------------------", "-----"))
    for (i in seq_len(nrow(x$levels))) {
      r <- x$levels[i, ]
      label <- if (ncol(x$levels) > 1) as.character(r[[2]]) else "-"
      if (nchar(label) > 20) label <- paste0(substr(label, 1, 17), "...")
      cat(sprintf("  %4d  %-20s  %d\n", r$id, label, r$count))
    }

  } else if (analysisType == "float") {

    if (!identical(x$data$data_range, x$data$target_range)) {
      cat("  Target range      [", sprintf("%.6g", x$data$target_range["min"]), ", ",
          sprintf("%.6g", x$data$target_range["max"]), "]\n", sep = "")
    }
    if (!is.null(x$data$decimals)) {
      cat("  Decimals          ", x$data$decimals, "\n", sep = "")
    }
    cat("\n")

    pareto <- x$results[x$data$pareto, ]
    if (!is.null(min_bits)) {
      pareto <- pareto[pareto$total >= min_bits, , drop = FALSE]
    }

    cat(sprintf("%3s  %3s  %5s  %9s  %8s  %7s  %10s  %10s  %10s  %10s\n",
                "Exp", "Sig", "Total", "Underflow", "Overflow", "Changed", "Min Res", "Max Res", "RMSE", "Max Err"))
    cat(sprintf("%3s  %3s  %5s  %9s  %8s  %7s  %10s  %10s  %10s  %10s\n",
                "---", "---", "-----", "---------", "--------", "-------", "----------", "----------", "----------", "----------"))
    for (i in seq_len(nrow(pareto))) {
      r <- pareto[i, ]
      minResFmt <- if (r$min_res < 0.001) sprintf("%10.2e", r$min_res) else sprintf("%10.4f", r$min_res)
      maxResFmt <- if (r$max_res < 0.001) sprintf("%10.2e", r$max_res) else sprintf("%10.4f", r$max_res)
      # show "<0.1%" for non-zero values that round to 0.0%
      underflowFmt <- if (r$underflow > 0 && r$underflow < 0.05) "   <0.1%" else sprintf("%7.1f%%", r$underflow)
      overflowFmt <- if (r$overflow > 0 && r$overflow < 0.05) "  <0.1%" else sprintf("%6.1f%%", r$overflow)

      cat(sprintf("%3d  %3d  %5d  %s  %s  %6.1f%%  %s  %s  %10.4f  %10.4f\n",
                  r$exp, r$sig, r$total, underflowFmt, overflowFmt, r$changed, minResFmt, maxResFmt, r$rmse, r$max_err))
    }
  }

  # usage
  cat("\nUsage:\n")
  switch(analysisType,
    bool = {
      if (nNA > 0 && !is.null(x$data$suggested_na_val)) {
        cat("  bf_map(protocol = \"na\", ..., na.val = ", x$data$suggested_na_val, ")\n", sep = "")
      } else {
        cat("  bf_map(protocol = \"na\", ...)\n")
      }
    },
    int  = {
      if (nNA > 0 && !is.null(x$data$suggested_na_val)) {
        cat("  bf_map(protocol = \"integer\", ..., na.val = ", x$data$suggested_na_val, ")\n", sep = "")
      } else {
        cat("  bf_map(protocol = \"integer\", ...)\n")
      }
    },
    enum = {
      if (nNA > 0 && !is.null(x$data$suggested_na_val)) {
        cat("  bf_map(protocol = \"category\", ..., na.val = ", x$data$suggested_na_val, ")\n", sep = "")
      } else {
        cat("  bf_map(protocol = \"category\", ...)\n")
      }
    },
    float = {
      cat("  bf_map(protocol = \"numeric\", ...,\n")
      cat("         fields = list(exponent = <exp>, significand = <sig>))\n")
    }
  )

  invisible(x)
}
