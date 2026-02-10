#' Make a binary value from an integer
#'
#' @param x [`numeric(.)`][numeric]\cr numeric vector for which to derive the
#'   binary values.
#' @param len [`integerish(1)`][integer]\cr the number of bits used to capture
#'   each value. If NULL, computed from the maximum value.
#' @param pad [`logical(1)`][logical]\cr whether to pad the binary values with
#'   leading zeros to equal width.
#' @importFrom checkmate assertIntegerish assertNumeric assertLogical
#' @importFrom stringr str_pad

.toBin <- function(x, len = NULL, pad = TRUE){

  assertNumeric(x = x)
  assertIntegerish(x = len, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = pad, len = 1, any.missing = FALSE)

  # use floor() to support unsigned 32-bit values (up to 2^32-1)
  x <- floor(x)

  # if len not provided, compute from max value
  if (is.null(len)) {
    maxVal <- max(x, 1)
    len <- max(ceiling(log2(maxVal + 1)), 1)
  }

  temp <- vapply(x, function(val) {
    bits <- character(len)
    for (j in len:1) {
      bits[j] <- as.character(val %% 2)
      val <- val %/% 2
    }
    paste0(bits, collapse = "")
  }, character(1))

  # pad to same width if requested (only matters if len was auto-computed per value)
  if (pad) {
    temp <- str_pad(temp, width = max(nchar(temp)), pad = "0")
  }

  return(temp)
}


#' Make an integer from a binary value
#'
#' @param x [`character(1)`][character]\cr a binary string (sequence of 0s and
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

#' Assess encoding quality
#'
#' Internal function to analyze data and compute quality metrics for encoding.
#' This function is used both by \code{bf_analyze} for user-facing analysis and by
#' \code{bf_map} to store quality metrics in the provenance.
#'
#' @param values vector of values to assess (will be cleaned of NAs internally)
#' @param type encoding type ("bool", "enum", "int", "float")
#' @param enc list with encoding parameters (sign, exponent, significand, bias).
#'   If NULL (for bf_analyze), type-appropriate defaults or multiple configs are tested.
#' @param isRaster logical, whether data comes from a SpatRaster (enum only)
#' @param rasterLevels data.frame with raster attribute table (enum only)
#' @param range optional target range c(min, max) for float analysis
#' @param decimals optional decimal precision for float comparison
#' @param max_bits maximum total bits to consider (default 16)
#' @param fields optional list specifying which exp/sig configurations to analyze (float only)
#' @return list of quality metrics appropriate for the encoding type. For float
#'   without specific enc, returns a list with data summary and results data.frame.
#' @keywords internal

.assessEncodingQuality <- function(values, type, enc = NULL, isRaster = FALSE,
                                   rasterLevels = NULL, range = NULL,
                                   decimals = NULL, max_bits = 16L, fields = NULL,
                                   errorOnAllNA = FALSE) {

  # common stats
  nTotal <- length(values)
  nNA <- sum(is.na(values))
  nValid <- nTotal - nNA
  validVals <- values[!is.na(values)]

  # For user-facing analysis (bf_analyze), error if all values are NA
  if (errorOnAllNA && nValid == 0 && type %in% c("int", "float")) {
    stop("No non-NA values to analyze.")
  }

  # initialize output - will be populated in type-specific blocks
  out <- list(
    data = list(
      type = type,
      n = nTotal,
      n_valid = nValid,
      n_na = nNA
    )
  )

  # ============================================================================
  # BOOL ANALYSIS
  # ============================================================================
  if (type == "bool") {
    nTrue <- sum(validVals == TRUE, na.rm = TRUE)
    nFalse <- sum(validVals == FALSE, na.rm = TRUE)

    bitsRequired <- if (nNA > 0) 2L else 1L
    suggestedNaVal <- if (nNA > 0) 2L else NULL

    out$data$n_true <- nTrue
    out$data$n_false <- nFalse
    out$data$suggested_na_val <- suggestedNaVal
    out$data$bits_required <- bitsRequired

    # if enc provided (from bf_map), add quality assessment
    if (!is.null(enc)) {
      out$data$bits_used <- enc$significand

      out$data$underflow <- 0L
      out$data$overflow <- 0L
      out$data$precision_loss <- 0L
      out$data$rmse <- 0
      out$data$max_error <- 0
      out$data$min_resolution <- 1
      out$data$max_resolution <- 1
    }
  }

  # ============================================================================
  # ENUM ANALYSIS
  # ============================================================================
  if (type == "enum") {
    levelsData <- NULL
    actualVals <- NULL

    # Handle case where values is NULL (some protocols don't provide testVals)
    if (is.null(values) || length(values) == 0) {
      # Return minimal quality info when no values available
      out$data$n_levels <- NA_integer_
      out$data$max_id <- NA_integer_
      out$data$suggested_na_val <- NULL
      out$data$bits_required <- NA_integer_
      out$data$has_gaps <- FALSE
      out$data$gaps <- NULL
      out$data$is_raster <- isRaster

      if (!is.null(enc)) {
        out$data$bits_used <- enc$significand

        out$data$underflow <- 0L
        out$data$overflow <- 0L
        out$data$precision_loss <- 0L
        out$data$rmse <- 0
        out$data$max_error <- 0
        out$data$min_resolution <- 1
        out$data$max_resolution <- 1
      }

      class(out) <- c("bf_analysis", "list")
      return(out)
    }

    if (isRaster && !is.null(rasterLevels)) {
      levelsData <- rasterLevels
      if (!"id" %in% names(levelsData)) {
        names(levelsData)[1] <- "id"
      }
      actualVals <- validVals

      # validate values match levels
      uniqueVals <- unique(actualVals[!is.na(actualVals)])
      expectedIds <- levelsData$id
      invalidVals <- setdiff(uniqueVals, expectedIds)

      if (length(invalidVals) > 0) {
        warning("Raster contains values not in attribute table: ",
                paste(sort(invalidVals), collapse = ", "), "\n",
                "Expected IDs: ", paste(expectedIds, collapse = ", "))
      }

    } else if (is.factor(values)) {
      lvls <- levels(values)
      levelsData <- data.frame(
        id = seq_along(lvls) - 1L,
        label = lvls
      )
      actualVals <- as.integer(values) - 1L

    } else if (is.character(values)) {
      uniqueLabels <- sort(unique(validVals))
      levelsData <- data.frame(
        id = seq_along(uniqueLabels) - 1L,
        label = uniqueLabels
      )
      actualVals <- match(values, uniqueLabels) - 1L

    } else {
      # numeric categorical values
      uniqueVals <- sort(unique(validVals))
      if (length(uniqueVals) == 0) {
        # All NA values - return minimal info
        out$data$n_levels <- 0L
        out$data$max_id <- NA_integer_
        out$data$suggested_na_val <- NULL
        out$data$bits_required <- NA_integer_
        out$data$has_gaps <- FALSE
        out$data$gaps <- NULL
        out$data$is_raster <- isRaster

        if (!is.null(enc)) {
          out$data$bits_used <- enc$significand

          out$data$underflow <- 0L
          out$data$overflow <- 0L
          out$data$precision_loss <- 0L
          out$data$rmse <- 0
          out$data$max_error <- 0
          out$data$min_resolution <- 1
          out$data$max_resolution <- 1
        }

        class(out) <- c("bf_analysis", "list")
        return(out)
      }
      levelsData <- data.frame(
        id = as.integer(uniqueVals),
        label = as.character(uniqueVals)
      )
      actualVals <- validVals
    }

    nLevels <- nrow(levelsData)

    # check for gaps in IDs (only if we have levels)
    if (nLevels > 0) {
      expectedSeq <- seq(min(levelsData$id), max(levelsData$id))
      gaps <- setdiff(expectedSeq, levelsData$id)
      hasGaps <- length(gaps) > 0
    } else {
      gaps <- NULL
      hasGaps <- FALSE
    }

    # calculate bits needed
    maxId <- if (nLevels > 0) max(levelsData$id) else 0L

    if (nNA > 0) {
      maxIdWithNa <- maxId + 1L
      bitsRequired <- ceiling(log2(maxIdWithNa + 1))
    } else {
      bitsRequired <- if (maxId == 0) 1L else ceiling(log2(maxId + 1))
    }

    # count occurrences of each level
    valCounts <- table(actualVals)
    levelsData$count <- 0L
    for (i in seq_len(nrow(levelsData))) {
      idChar <- as.character(levelsData$id[i])
      if (idChar %in% names(valCounts)) {
        levelsData$count[i] <- as.integer(valCounts[[idChar]])
      }
    }

    out$data$n_levels <- nLevels
    out$data$max_id <- maxId
    out$data$suggested_na_val <- if (nNA > 0) maxId + 1L else NULL
    out$data$bits_required <- bitsRequired
    out$data$has_gaps <- hasGaps
    out$data$gaps <- if (hasGaps) gaps else NULL
    out$data$is_raster <- isRaster
    out$levels <- levelsData

    # if enc provided (from bf_map), add quality assessment
    if (!is.null(enc)) {
      out$data$bits_used <- enc$significand

      out$data$underflow <- 0L
      out$data$overflow <- 0L
      out$data$precision_loss <- 0L
      out$data$rmse <- 0
      out$data$max_error <- 0
      out$data$min_resolution <- 1
      out$data$max_resolution <- 1
    }
  }

  # ============================================================================
  # INT ANALYSIS
  # ============================================================================
  if (type == "int") {
    if (nValid == 0) {
      out$data$bits_required <- NA_integer_
      if (!is.null(enc)) {
        out$data$bits_used <- enc$sign + enc$significand

        out$data$underflow <- 0L
        out$data$overflow <- 0L
        out$data$precision_loss <- 0L
        out$data$rmse <- 0
        out$data$max_error <- 0
        out$data$min_resolution <- 1
        out$data$max_resolution <- 1
      }
    } else {
      minVal <- min(validVals)
      maxVal <- max(validVals)
      needsSign <- minVal < 0

      # calculate bits needed (without NA consideration first)
      if (needsSign) {
        maxAbs <- max(abs(minVal), abs(maxVal))
        bitsForMagnitude <- if (maxAbs == 0) 1L else ceiling(log2(maxAbs + 1))
        bitsBase <- bitsForMagnitude + 1L
      } else {
        bitsBase <- if (maxVal == 0) 1L else ceiling(log2(maxVal + 1))
      }

      # if there are NAs, suggest na.val and recalculate bits
      suggestedNaVal <- NULL
      if (nNA > 0) {
        suggestedNaVal <- maxVal + 1L
        if (needsSign) {
          maxAbsWithNa <- max(abs(minVal), abs(maxVal), abs(suggestedNaVal))
          bitsForMagnitude <- ceiling(log2(maxAbsWithNa + 1))
          bitsRequired <- bitsForMagnitude + 1L
        } else {
          bitsRequired <- ceiling(log2(suggestedNaVal + 1))
        }
      } else {
        bitsRequired <- bitsBase
      }

      out$data$min <- minVal
      out$data$max <- maxVal
      out$data$needs_sign <- needsSign
      out$data$suggested_na_val <- suggestedNaVal
      out$data$bits_required <- bitsRequired

      # if enc provided (from bf_map), add quality assessment
      if (!is.null(enc)) {
        out$data$bits_used <- enc$sign + enc$significand

        if (!is.null(enc$scale)) {
          # auto-scaled integer: round-trip quality assessment
          domainMin <- enc$scale$min
          domainMax <- enc$scale$max
          maxInt <- 2L^enc$significand - 1L

          # forward: scale to integer domain
          scaled <- round((validVals - domainMin) / (domainMax - domainMin) * maxInt)
          clamped <- pmin(pmax(scaled, 0), maxInt)

          # reverse: back to original domain
          reconstructed <- clamped / maxInt * (domainMax - domainMin) + domainMin

          errors <- reconstructed - validVals
          out$data$underflow <- sum(validVals < domainMin)
          out$data$overflow <- sum(validVals > domainMax)
          out$data$precision_loss <- sum(abs(errors) > .Machine$double.eps * 100)
          out$data$rmse <- sqrt(mean(errors^2))
          out$data$max_error <- max(abs(errors))
          resolution <- (domainMax - domainMin) / maxInt
          out$data$min_resolution <- resolution
          out$data$max_resolution <- resolution
        } else {
          # standard integer quality assessment
          if (enc$sign == 1) {
            minRepresentable <- -(2^(enc$significand))
            maxRepresentable <- 2^(enc$significand) - 1
          } else {
            minRepresentable <- 0
            maxRepresentable <- 2^enc$significand - 1
          }

          nUnderflow <- sum(validVals < minRepresentable)
          nOverflow <- sum(validVals > maxRepresentable)
          out$data$underflow <- nUnderflow
          out$data$overflow <- nOverflow

          # compute errors from clamping
          clamped <- pmin(pmax(validVals, minRepresentable), maxRepresentable)
          errors <- clamped - validVals
          out$data$precision_loss <- sum(errors != 0)
          out$data$rmse <- sqrt(mean(errors^2))
          out$data$max_error <- max(abs(errors))
          out$data$min_resolution <- 1
          out$data$max_resolution <- 1
        }
      }
    }
  }

  # ============================================================================
  # FLOAT ANALYSIS
  # ============================================================================
  if (type == "float") {
    if (nValid == 0) {
      out$data$bits_required <- NA_integer_
      if (!is.null(enc)) {
        out$data$bits_used <- enc$sign + enc$exponent + enc$significand
        out$data$underflow <- 0L
        out$data$overflow <- 0L
        out$data$precision_loss <- 0L
        out$data$rmse <- 0
        out$data$max_error <- 0
        out$data$min_resolution <- NA_real_
        out$data$max_resolution <- NA_real_
      }
    } else {
      # determine sign requirement
      needsSign <- any(validVals < 0)
      signBits <- if (needsSign) 1L else 0L

      # determine range
      absX <- abs(validVals)
      nonZeroX <- absX[absX != 0]

      if (length(nonZeroX) == 0) {
        stop("All non-NA values are zero. Cannot determine encoding parameters.")
      }

      dataMin <- min(nonZeroX)
      dataMax <- max(absX)

      if (!is.null(range)) {
        targetMin <- if (range[1] == 0) dataMin else range[1]
        targetMax <- range[2]
      } else {
        targetMin <- dataMin
        targetMax <- dataMax
      }

      out$data$data_range <- c(min = dataMin, max = dataMax)
      out$data$target_range <- c(min = targetMin, max = targetMax)
      out$data$needs_sign <- needsSign
      out$data$decimals <- decimals

      # If enc provided (single config from bf_map), assess that specific encoding
      if (!is.null(enc)) {
        expBits <- enc$exponent
        sigBits <- enc$significand
        bias <- enc$bias

        # calculate representable range
        minActualExp <- -bias
        maxStoredExp <- 2^expBits - 2
        maxActualExp <- maxStoredExp - bias
        minRepresentable <- 2^minActualExp
        maxRepresentable <- 2^(maxActualExp + 1)

        # simulate encoding
        n <- nValid
        maxExpVal <- 2^expBits - 1
        isZero <- validVals == 0
        isNeg <- validVals < 0
        absVals <- abs(validVals)

        actualExp <- rep(0, n)
        actualExp[!isZero] <- floor(log2(absVals[!isZero]))
        biasedExp <- actualExp + bias

        underflowMask <- !isZero & biasedExp < 0
        overflowMask <- !isZero & biasedExp > maxExpVal

        biasedExpClamped <- biasedExp
        biasedExpClamped[underflowMask] <- 0
        biasedExpClamped[overflowMask] <- maxExpVal

        normalized <- rep(1, n)
        normalized[!isZero & !underflowMask] <- absVals[!isZero & !underflowMask] / 2^actualExp[!isZero & !underflowMask]
        fracPart <- normalized - 1
        sigVal <- floor(fracPart * 2^sigBits)
        sigVal[overflowMask] <- 2^sigBits - 2

        decodedActualExp <- biasedExpClamped - bias
        significand <- 1 + sigVal / 2^sigBits
        decoded <- significand * 2^decodedActualExp
        decoded[isNeg & signBits == 1] <- -decoded[isNeg & signBits == 1]
        decoded[underflowMask] <- 0
        decoded[isZero] <- 0

        # compare with optional decimal rounding
        orig <- validVals
        dec <- decoded
        if (!is.null(decimals)) {
          orig <- round(orig, decimals)
          dec <- round(dec, decimals)
        }

        errors <- dec - orig
        absErrors <- abs(errors)

        # calculate resolution at data extremes
        effectiveMin <- max(targetMin, minRepresentable)
        effectiveMax <- min(targetMax, maxRepresentable)
        minRes <- 2^floor(log2(effectiveMin)) / 2^sigBits
        maxRes <- 2^floor(log2(effectiveMax)) / 2^sigBits

        out$data$bits_used <- signBits + expBits + sigBits
        out$data$underflow <- sum(underflowMask)
        out$data$overflow <- sum(overflowMask)
        out$data$precision_loss <- sum(orig != dec & !underflowMask & !overflowMask)
        out$data$rmse <- sqrt(mean(errors^2))
        out$data$max_error <- max(absErrors)
        out$data$min_resolution <- minRes
        out$data$max_resolution <- maxRes

      } else {
        # No specific enc - analyze multiple configurations (for bf_analyze)
        # Validate and process fields parameter
        fieldsExp <- NULL
        fieldsSig <- NULL
        if (!is.null(fields)) {
          if (!is.null(fields$exponent)) {
            fieldsExp <- as.integer(fields$exponent)
            if (any(fieldsExp < 1)) {
              stop("exponent in fields must be positive integers.")
            }
          }
          if (!is.null(fields$significand)) {
            fieldsSig <- as.integer(fields$significand)
            if (any(fieldsSig < 1)) {
              stop("significand in fields must be positive integers.")
            }
          }
          if (!is.null(fieldsExp) && !is.null(fieldsSig) && length(fieldsExp) != length(fieldsSig)) {
            stop("When both exponent and significand are specified in fields, they must have the same length.")
          }
        }

        # determine which exp/sig combinations to test
        if (!is.null(fields)) {
          if (!is.null(fieldsExp) && !is.null(fieldsSig)) {
            combos <- data.frame(exp = fieldsExp, sig = fieldsSig)
          } else if (!is.null(fieldsExp)) {
            combos <- do.call(rbind, lapply(fieldsExp, function(e) {
              maxSig <- max_bits - e - signBits
              if (maxSig < 1) return(NULL)
              data.frame(exp = e, sig = 1:maxSig)
            }))
          } else {
            combos <- do.call(rbind, lapply(fieldsSig, function(s) {
              maxExp <- max_bits - s - signBits
              if (maxExp < 1) return(NULL)
              data.frame(exp = 1:maxExp, sig = s)
            }))
          }
        } else {
          combos <- expand.grid(exp = 2:min(8, max_bits - signBits - 1),
                                sig = 1:min(16, max_bits - signBits - 2))
          combos <- combos[combos$exp + combos$sig + signBits <= max_bits, ]
        }

        # test all combinations
        results <- list()

        for (i in seq_len(nrow(combos))) {
          expBits <- combos$exp[i]
          sigBits <- combos$sig[i]
          totalBits <- signBits + expBits + sigBits

          bias <- 2^(expBits - 1) - 1
          minActualExp <- -bias
          maxStoredExp <- 2^expBits - 2
          maxActualExp <- maxStoredExp - bias
          minRepresentable <- 2^minActualExp
          maxRepresentable <- 2^(maxActualExp + 1)

          # simulate encoding
          n <- length(validVals)
          maxExpVal <- 2^expBits - 1
          isZero <- validVals == 0
          isNeg <- validVals < 0
          absVals <- abs(validVals)

          actualExp <- rep(0, n)
          actualExp[!isZero] <- floor(log2(absVals[!isZero]))
          biasedExp <- actualExp + bias

          underflow <- !isZero & biasedExp < 0
          overflow <- !isZero & biasedExp > maxExpVal

          biasedExp[underflow] <- 0
          biasedExp[overflow] <- maxExpVal

          normalized <- rep(1, n)
          normalized[!isZero & !underflow] <- absVals[!isZero & !underflow] / 2^actualExp[!isZero & !underflow]
          fracPart <- normalized - 1
          sigVal <- floor(fracPart * 2^sigBits)
          sigVal[overflow] <- 2^sigBits - 2

          decodedActualExp <- biasedExp - bias
          significand <- 1 + sigVal / 2^sigBits
          decoded <- significand * 2^decodedActualExp
          decoded[isNeg & signBits == 1] <- -decoded[isNeg & signBits == 1]
          decoded[underflow] <- 0
          decoded[isZero] <- 0

          orig <- validVals
          dec <- decoded
          if (!is.null(decimals)) {
            orig <- round(orig, decimals)
            dec <- round(dec, decimals)
          }

          errors <- dec - orig
          absErrors <- abs(errors)
          pctUnderflow <- 100 * sum(underflow) / n
          pctOverflow <- 100 * sum(overflow) / n
          pctChanged <- round(100 * sum(orig != dec) / n, 2)
          rmse <- sqrt(mean(errors^2))
          maxErr <- max(absErrors)

          effectiveMin <- max(targetMin, minRepresentable)
          effectiveMax <- min(targetMax, maxRepresentable)
          minRes <- 2^floor(log2(effectiveMin)) / 2^sigBits
          maxRes <- 2^floor(log2(effectiveMax)) / 2^sigBits

          results[[length(results) + 1]] <- data.frame(
            exp = expBits, sig = sigBits, total = totalBits,
            underflow = pctUnderflow, overflow = pctOverflow, changed = pctChanged,
            min_res = minRes, max_res = maxRes, rmse = rmse, max_err = maxErr,
            stringsAsFactors = FALSE
          )
        }

        resultsDF <- do.call(rbind, results)
        rownames(resultsDF) <- NULL

        # determine Pareto front
        if (!is.null(fields)) {
          displayIdx <- order(resultsDF$total, resultsDF$exp)
        } else {
          displayIdx <- unlist(lapply(split(seq_len(nrow(resultsDF)), resultsDF$total), function(idx) {
            if (length(idx) == 1) return(idx)

            subset <- resultsDF[idx, ]
            paretoIdx <- c()

            for (i in seq_along(idx)) {
              dominated <- FALSE
              for (j in seq_along(idx)) {
                if (i == j) next
                jBetterOrEqual <- subset$underflow[j] <= subset$underflow[i] &&
                                 subset$overflow[j] <= subset$overflow[i] &&
                                 subset$changed[j] <= subset$changed[i] &&
                                 subset$rmse[j] <= subset$rmse[i] &&
                                 subset$max_err[j] <= subset$max_err[i]
                jStrictlyBetter <- subset$underflow[j] < subset$underflow[i] ||
                                  subset$overflow[j] < subset$overflow[i] ||
                                  subset$changed[j] < subset$changed[i] ||
                                  subset$rmse[j] < subset$rmse[i] ||
                                  subset$max_err[j] < subset$max_err[i]
                if (jBetterOrEqual && jStrictlyBetter) {
                  dominated <- TRUE
                  break
                }
              }
              if (!dominated) paretoIdx <- c(paretoIdx, idx[i])
            }
            paretoIdx[order(resultsDF$exp[paretoIdx])]
          }))
          displayIdx <- displayIdx[order(resultsDF$total[displayIdx], resultsDF$exp[displayIdx])]
        }
        names(displayIdx) <- NULL

        out$data$pareto <- displayIdx
        out$data$fields_mode <- !is.null(fields)
        out$results <- resultsDF
      }
    }
  }

  # Set class for bf_analyze output
  class(out) <- c("bf_analysis", "list")
  return(out)
}


#' Determine encoding
#'
#' @param var the variable for which to determine encoding.
#' @param type the encoding type for which to determine encoding.
#' @param ... [`list(.)`][list]\cr named list of options to determine encoding,
#'   see Details.
#' @details Floating-point values are encoded using three fields that map
#'   directly to bit sequences. Any numeric value can be written in scientific
#'   notation. For example, the decimal 923.52 becomes 9.2352 × 10². The same
#'   principle applies in binary: the value 101011.101₂ becomes 1.01011101 × 2⁵.
#'   This binary scientific notation directly yields the three encoding fields:
#'   \itemize{
#'     \item Sign: whether the value is positive or negative (here: positive → 0)
#'     \item Exponent: the power of 2 (here: 5)
#'     \item Significand: the fractional part after the leading 1 (here: 01011101)
#'   }
#'   For background on floating-point representation, see \href{https://www.cs.cornell.edu/~tomf/notes/cps104/floating}{'Floating
#'   Point'} by Thomas Finley, or explore encodings interactively at
#'   \href{https://float.exposed/}{https://float.exposed/}.
#'
#'   The allocation of bits across these fields can be adjusted to suit
#'   different needs: more exponent bits provide a wider range (smaller minimums
#'   and larger maximums), while more significand bits provide finer precision.
#'   This package documents bit allocation using the notation \[s.e.m\], where
#'   s = sign bits (0 or 1), e = exponent bits, and m = significand bits.
#'
#'   For non-numeric data (boolean or categorical), the same notation applies
#'   with sign and exponent set to 0. A binary flag uses \[0.0.1\], while a
#'   categorical variable with 8 levels requires 3 bits, yielding \[0.0.3\].
#'
#'   Possible options (\code{...}) of this function are \itemize{
#'     \item \code{format}: switch that determines the configuration of the
#'           \href{https://en.wikipedia.org/wiki/Bfloat16_floating-point_format}{floating point encoding}.
#'           Possible values are \code{"half"} \[1.5.10\], \code{"bfloat16"}
#'           \[1.8.7\], \code{"tensor19"} \[1.8.10\], \code{"fp24"} \[1.7.16\],
#'           \code{"pxr24"} \[1.8.15\], \code{"single"} \[1.8.23\] and
#'           \code{"double"} \[1.11.52\],
#'     \item \code{fields}: list of custom values that control how many bits are
#'           allocated to \code{sign}, \code{exponent} and \code{significand} for
#'           encoding the numeric values,
#'     \item \code{range}: the ratio between the smallest and largest possible
#'           value to be reliably represented (modifies the exponent),
#'     \item \code{decimals}: the number of decimal digits that should be
#'           represented reliably (modifies the significand).
#'   }
#'   In a future version, it should also be possible to modify the bias to focus
#'   number coverage to where it's most useful for the data.
#' @return list of the encoding values for sign, exponent and significand, and
#'   an additional provenance term.
#' @importFrom purrr map
#' @importFrom checkmate assertIntegerish assertList testNull testIntegerish
#'   assertNames assertChoice assertCharacter
#' @importFrom dplyr case_when

.makeEncoding <- function(var, type, ...){

  assertChoice(x = type, choices = c("bool", "enum", "int", "float"))

  # set dots to arguments
  dots <- enquos(...)
  named_dots <- dots[names(dots) != ""]
  encArgs <- map(named_dots[names(named_dots) %in% c("format", "fields", "range", "decimals", ".data_is_non_integer")], eval_tidy)
  # return(encArgs)
  dataIsNonInteger <- isTRUE(encArgs[[".data_is_non_integer"]])

  for(name in c("format", "decimals", "range", "fields")){
    if(name %in% names(encArgs)){
      assign(name, encArgs[[name]], envir = environment())
    } else {
      assign(name, NULL, envir = environment())
    }
  }
  assertList(x = fields, null.ok = TRUE)
  if(!is.null(fields)){
    assertNames(x = names(fields), subset.of = c("sign", "exponent", "significand", "bias"))
    if(!is.null(fields$significand))
      names(fields)[which(names(fields) == "significand")] <- "mant" # needs to be named differently than "significand", otherwise it also matches with "sign" below
  }

  assertCharacter(x = format, len = 1, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(format))
    assertChoice(x = format, choices = c("half", "bfloat16", "tensor19", "fp24", "pxr24", "single", "double"))

  assertIntegerish(x = decimals, len = 1, any.missing = FALSE, null.ok = TRUE)
  # range: length-1 integer for float exponent range, or length-2 numeric for int domain range
  if (!is.null(range)) {
    if (type == "int" && length(range) == 2) {
      assertNumeric(x = range, len = 2, any.missing = FALSE)
      if (range[1] >= range[2]) stop("'range' must be c(min, max) with min < max.")
    } else {
      assertIntegerish(x = range, len = 1, any.missing = FALSE)
    }
  }

  sign <- exp <- sig <- bias <- NULL
  intScale <- NULL  # auto-scaling parameters for integer protocol

  # determine variable sign
  autoSign <- ifelse(any(var < 0, na.rm = TRUE), 1, 0)

  # determine variable significand
  # for int/enum types: bits needed to store the max integer value
  # for float type: bits needed for decimal precision
  if (type == "float") {
    if (!is.null(decimals)) {
      # decimals places needs about log2(10^decimals) bits
      autoSig <- ceiling(decimals * 3.32)  # log2(10) ≈ 3.32
    } else {
      # default: 8 bits gives ~0.4% relative precision
      autoSig <- 8L
    }
  } else if (type == "int" && !is.null(range) && length(range) == 2) {
    # auto-scaling: explicit domain range provided
    domainMin <- range[1]
    domainMax <- range[2]
    if (!is.null(fields) && !is.null(fields$mant)) {
      maxInt <- 2L^fields$mant - 1L
    } else if (!is.null(decimals)) {
      maxInt <- as.integer(ceiling((domainMax - domainMin) * 10^decimals))
    } else {
      # auto-compute: choose enough bits for ~1% resolution
      maxInt <- as.integer(ceiling((domainMax - domainMin) * 100))
    }
    autoSig <- ceiling(log2(maxInt + 1))
    intScale <- list(min = domainMin, max = domainMax)
  } else if (type == "int" && !is.null(decimals) && decimals > 0 &&
             is.null(range) && dataIsNonInteger) {
    # auto-scaling: decimals on non-integer data without explicit range — use data min/max
    domainMin <- min(var, na.rm = TRUE)
    domainMax <- max(var, na.rm = TRUE)
    maxInt <- as.integer(ceiling((domainMax - domainMin) * 10^decimals))
    autoSig <- ceiling(log2(maxInt + 1))
    intScale <- list(min = domainMin, max = domainMax)
  } else if (type == "int" && !is.null(fields) && !is.null(fields$mant) &&
             is.null(range) && dataIsNonInteger) {
    # auto-scaling: fields on non-integer data without explicit range — use data min/max
    domainMin <- min(var, na.rm = TRUE)
    domainMax <- max(var, na.rm = TRUE)
    autoSig <- fields$mant
    intScale <- list(min = domainMin, max = domainMax)
  } else {
    # int/enum: bits needed to store the max value (existing logic)
    xInt <- var * 10^max(decimals, 0)
    xInt <- as.integer(xInt)
    autoSig <- ceiling(log2(max(xInt, 2, na.rm = TRUE) + 1))
  }


  # determine variable exponent (in powers of 2, not 10)
  # need enough bits to represent the range of exponents
  nonZeroVar <- var[var != 0 & !is.na(var)]
  if (length(nonZeroVar) > 0) {
    expRange <- floor(log2(abs(nonZeroVar)))
    minExp <- min(expRange)
    maxExp <- max(expRange)
    # exponent bits must cover range from minExp to maxExp with bias
    # bias = 2^(expBits-1) - 1, so we need expBits where 2^(expBits-1) - 1 + maxExp fits
    # and minExp + bias >= 0
    expSpan <- maxExp - minExp
    autoExp <- max(ceiling(log2(max(abs(minExp), abs(maxExp)) + 2)) + 1, ceiling(log2(expSpan + 2)), range, 1)
  } else {
    autoExp <- max(range, 1)
  }

  if(type == "bool"){

    sign <- 0
    exp <- 0
    sig <- 1

  } else if(type == "enum"){

    sign <- 0
    exp <- 0
    sig <- autoSig

  } else if(type == "int"){

    sign <- if (!is.null(intScale)) 0L else autoSign  # scaled values are always >= 0
    exp <- 0
    sig <- autoSig

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

      sig <- case_when(format == "half" ~ 10,
                        format == "bfloat16" ~ 7,
                        format == "tensor19" ~ 10,
                        format == "fp24" ~ 16,
                        format == "pxr24" ~ 15,
                        format == "single" ~ 23,
                        format == "double" ~ 52)

    } else {

      sign <- autoSign
      exp <- autoExp
      sig <- autoSig

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
    # quality implications are now recorded in provenance via .assessEncodingQuality()
    exp <- fields$exponent
  }

  if(!is.null(fields) & !is.null(fields$mant)){
    assertIntegerish(x = fields$mant, lower = 0, len = 1, any.missing = FALSE)
    # quality implications are now recorded in provenance via .assessEncodingQuality()
    sig <- fields$mant
  }

  enc <- list(sign = as.integer(sign),
              exponent = as.integer(exp),
              significand = as.integer(sig),
              bias = as.integer(2**(exp-1)-1),
              scale = intScale)

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
  assertList(x = protocol$data)

  # turn test string into function first, before validation
  if (is.character(protocol$test)) {
    protocol$test <- eval(parse(text = protocol$test))
  }

  # protocol$test can be either string or function at this point

  # ensure that glue statements have only names that are also in the data

  # ensure extensions are provided correctly
  if(!is.null(protocol$extends)){
    assertCharacter(x = protocol$extends, pattern = "^([a-zA-Z]+)_(\\d+\\.\\d+\\.\\d+)$")
    matches <- as.character(str_match(protocol$extends, "^([a-zA-Z]+)_(\\d+)\\.(\\d+)\\.(\\d+)$"))
    # assertChoice(x = matches[2], choices = names(bf_pcl))
    if(!testCharacter(x = protocol$extends_note, any.missing = FALSE, min.len = 1)) stop("please provide a short note about what this extension changes.")
  }

  # ensure packages are installed
  if(!is.null(protocol$requires)){
    for(pkg in protocol$requires){
      require(pkg, character.only = TRUE)
    }
  }

  # ensure test is a function for execution
  if (is.character(protocol$test)) {
    protocol$test <- eval(parse(text = protocol$test))
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

  # keep test as function for bf_map execution
  # string conversion only happens in bf_protocol for final storage
  return(protocol)

}

#' Create a Project Metadata Object
#'
#' @param title [`character(1)`][character]\cr project title.
#' @param author [`person(.)`][person]\cr person or organization objects created
#'   with `person()`.
#' @param year [`character(1)`][character]\cr publication year, defaults to
#'   current year.
#' @param publisher [`character(1)`][character]\cr name of the publishing
#'   entity.
#' @param type [`character(1)`][character]\cr resource type, one of "Dataset",
#'   "Software", "Image", "Model", "Text", "Collection", "Other".
#' @param identifier [`character(1)`][character]\cr project identifier (e.g.,
#'   DOI).
#' @param description [`character(1)`][character]\cr abstract or description.
#' @param subject [`character(.)`][character]\cr keywords or classification
#'   codes.
#' @param contributor [`person(.)`][person]\cr additional contributors as person
#'   objects.
#' @param license [`character(1)`][character]\cr license or rights statement.
#' @param funding [`character(.)`][character]\cr funding information.
#' @param language [`character(1)`][character]\cr primary language, defaults to
#'   "en".
#' @param version [`character(1)`][character]\cr version of the resource.
#' @param ... additional metadata elements as name-value pairs.
#' @return An object of class "project" with standardized metadata fields.
#' @examples
#' myProj <- project(title = "example project",
#'                   author = c(person("Jane", "Smith", email = "jane@example.com",
#'                                     role = "aut",
#'                                     comment = c(ORCID = "0000-0001-2345-6789",
#'                                                 affiliation = "University of Example",
#'                                                 ROR = "https://ror.org/05gq02987")),
#'                              person("Robert", "Jones", role = c("aut", "cre"))),
#'                   publisher = "example consortium",
#'                   type = "Dataset",
#'                   identifier = "10.5281/zenodo.1234567",
#'                   description = "A comprehensive explanation",
#'                   subject = c("keyword", "subject"),
#'                   license = "CC-BY-4.0")
#' @importFrom checkmate assertCharacter assertChoice assertList
#' @export

project <- function(title, year = format(Sys.Date(), "%Y"), language = "en",
                    type, author = NULL, publisher = NULL, identifier = NULL,
                    description = NULL, subject = NULL, contributor = NULL,
                    license = NULL, funding = NULL, version = NULL, ...) {

  # Input validation using checkmate
  assertCharacter(title, len = 1, any.missing = FALSE)
  assertCharacter(year, len = 1, pattern = "^[0-9]{4}$")
  assertChoice(x = type, choices = c("Dataset", "Software", "Image", "Model", "Text", "Collection", "Other"))
  assertCharacter(language, len = 1)
  if (!is.null(author)) {
    assertClass(author, classes = "person")
  }
  if (!is.null(publisher)) assertCharacter(publisher, len = 1, any.missing = FALSE)
  if (!is.null(identifier)) assertCharacter(identifier, len = 1, any.missing = FALSE)
  if (!is.null(description)) assertCharacter(description, len = 1, any.missing = FALSE)
  if (!is.null(subject)) assertCharacter(subject, min.len = 1, any.missing = FALSE)
  if (!is.null(contributor)) assertClass(author, classes = "person")
  if (!is.null(license)) assertCharacter(license, len = 1, any.missing = FALSE)
  if (!is.null(funding)) assertCharacter(funding, any.missing = FALSE)
  if (!is.null(version)) assertCharacter(version, len = 1, any.missing = FALSE)

  # Create the project object
  proj <- list(
    title = title,
    author = author,
    year = year,
    publisher = publisher,
    type = type,
    identifier = identifier,
    description = description,
    subject = subject,
    contributor = contributor,
    license = license,
    funding = funding,
    language = language,
    version = version
  )

  # Add any additional metadata elements
  if (length(list(...)) > 0) {
    extra <- list(...)
    proj <- c(proj, extra)
  }

  # Remove NULL elements
  proj <- proj[!sapply(proj, is.null)]

  # Set class
  class(proj) <- c("project", "list")

  return(proj)
}

#' Create DataCite-compliant metadata structure
#'
#' @param registry Registry object
#' @return List with DataCite-compliant structure
#' @importFrom stringr str_detect str_replace_all
#' @importFrom purrr map map_chr

.makeDatacite <- function(registry) {

  # Known license URLs
  known_licenses <- list(
    "GPL-3" = "https://www.gnu.org/licenses/gpl-3.0.en.html",
    "GPL-2" = "https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html",
    "MIT" = "https://opensource.org/licenses/MIT",
    "CC-BY-4.0" = "https://creativecommons.org/licenses/by/4.0/",
    "CC0-1.0" = "https://creativecommons.org/publicdomain/zero/1.0/"
  )

  # Required DataCite elements
  output <- list(
    schemaVersion = "4.5",
    identifiers = list(list(
      identifierType = "DOI",
      identifier = NA_character_  # Placeholder for future DOI
    )),
    titles = list(list(title = registry@name)),
    descriptions = list(list(
      description = registry@description,
      descriptionType = "Abstract"
    )),
    resourceType = list(
      resourceTypeGeneral = "Software",
      resourceType = "Bitfield Registry"
    ),
    publicationYear = format(Sys.Date(), "%Y"),
    language = "en"
  )



  # Add creators from registry metadata
  if (!is.null(registry@attribution$author)) {
    authors <- if (inherits(registry@attribution$author, "person")) {
      list(registry@attribution$author)
    } else {
      registry@attribution$author
    }

    output$creators <- map(authors, function(p) {
      creator <- list(
        givenName = paste(p$given, collapse = " "),
        familyName = paste(p$family, collapse = " "),
        nameType = "Personal"
      )

      # Add ORCID if available
      if (!is.null(p$comment) && "ORCID" %in% names(p$comment)) {
        creator$nameIdentifiers <- list(list(
          nameIdentifier = p$comment[["ORCID"]],
          nameIdentifierScheme = "ORCID",
          schemeURI = "https://orcid.org/"
        ))
      }

      # Add affiliation if available
      if (!is.null(p$comment) && "affiliation" %in% names(p$comment)) {
        affiliation <- list(name = p$comment[["affiliation"]])

        # Add ROR ID if available
        if ("ROR" %in% names(p$comment)) {
          affiliation$affiliationIdentifier <- p$comment[["ROR"]]
          affiliation$affiliationIdentifierScheme <- "ROR"
          affiliation$schemeURI <- "https://ror.org/"
        }

        creator$affiliations <- list(affiliation)
      }

      return(creator)
    })
  }

  if (!is.null(registry@version)) {
    alt_ids <- list()

    # MD5 as separate identifier
    if (!is.na(registry@md5)) {
      alt_ids <- append(alt_ids, list(list(
        alternateIdentifier = registry@md5,
        alternateIdentifierType = "MD5"
      )))
    }

    output$alternateIdentifiers <- alt_ids
  }

  # Determine publisher
  if (!is.null(registry@attribution$project$publisher)) {
    output$publisher <- registry@attribution$project$publisher
  } else if (!is.null(registry@attribution$author) &&
             !is.null(registry@attribution$author$comment) &&
             "affiliation" %in% names(registry@attribution$author$comment)) {
    output$publisher <- registry@attribution$author$comment[["affiliation"]]
  } else {
    output$publisher <- "Individual Researcher"
  }

  # Add project attribution if available
  if (!is.null(registry@attribution$project)) {
    proj <- registry@attribution$project

    # Add subjects/keywords
    if (!is.null(proj$subject)) {
      output$subjects <- map(proj$subject, ~list(subject = .x))
    }

    # Add funding information
    if (!is.null(proj$funding)) {
      output$fundingReferences <- list(list(funderName = proj$funding))
    }

    # Add related identifiers
    if (!is.null(proj$identifier)) {
      output$relatedIdentifiers <- list(list(
        relatedIdentifier = proj$identifier,
        relatedIdentifierType = if (str_detect(proj$identifier, "^10\\.")) "DOI" else "URL",
        relationType = "IsSupplementTo"
      ))
    }
  }

  # Add version information
  if (!is.null(registry@version)) {
    version_id <- sprintf("%s_%s_%s",
                          registry@version$bitfield,
                          registry@version$r,
                          registry@version$date)
    version_id <- str_replace_all(version_id, "[.]", "")
    version_id <- str_replace_all(version_id, "-", "")
    version_id <- str_replace_all(version_id, "_", ".")
    output$version <- version_id

    output$dates <- list(list(
      date = registry@version$date,
      dateType = "Created"
    ))
  }

  # Add rights/license information
  if (!is.null(registry@attribution$license)) {
    rights <- list(rights = registry@attribution$license)
    if (registry@attribution$license %in% names(known_licenses)) {
      rights$rightsURI <- known_licenses[[registry@attribution$license]]
    }
    output$rightsList <- list(rights)
  }

  # Add technical details as additional description
  if (registry@template$width > 0) {
    # Create flag summary
    flag_summary <- map_chr(registry@flags, function(f) {
      pcl <- str_split(f$wasGeneratedBy$useTest, "_", simplify = TRUE)[1]

      base_info <- sprintf("Bit %s (%s encoding)",
                           paste(c(min(f$wasGeneratedBy$assignPosition), max(f$wasGeneratedBy$assignPosition)), collapse="-"),
                           bf_pcl[[pcl]]$encoding_type)

      # Handle multiple cases for enumeration
      if (length(f$comment) > 1) {
        case_descriptions <- map_chr(seq_along(f$comment), function(i) {
          sprintf("  %d: %s", i-1, f$comment[i])
        })
        return(sprintf("%s:\n%s", base_info, paste(case_descriptions, collapse="\n")))
      } else {
        return(sprintf("%s: %s", base_info, f$comment))
      }
    })

    tech_description <- list(
      description = sprintf(
        "Bitfield registry (%s) with %d bits across %d observations, containing %d flags\nFlag legend:\n%s",
        registry@template$type, registry@template$width, registry@template$length, length(registry@flags),
        paste(flag_summary, collapse = "\n")
      ),
      descriptionType = "TechnicalInfo"
    )

    output$descriptions <- c(output$descriptions, list(tech_description))
  }

  return(output)
}
