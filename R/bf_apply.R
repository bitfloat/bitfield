#' Build a bit flag by applying a standard encoding
#'
#' @param x the object to build bit flags for.
#' @param test [`character(1)`][character]\cr the column or layer in \code{x}
#'   that is checked ...
#' @param type [`character(1)`][character]\cr .
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val [`logical(1)`][logical]\cr value that needs to be given, if the
#'   test for this flag results in \code{NA}s.
#' @param description [`character(.)`][character]\cr optional description that
#'   should be used instead of the default function-specific description. This
#'   description is used in the registry legend, so it should have as many
#'   entries as there will be entries per the respective flag in the legend (two
#'   for a binary flag, as many as there are cases for a cases flag and one for
#'   count or numeric flags).
#' @param registry [`registry(1)`][registry]\cr a bitfield registry that has
#'   been defined with \code{\link{bf_registry}}; if it's undefined, an empty
#'   registry will be defined on-the-fly.

bf_standard <- function(x, test, type, pos = NULL, na.val = NULL,
                        description = NULL, registry = NULL){





  # bf_encodings <- list(
  #   distribution_type = list(
  #     bits_required = 3,
  #     encoding_type = "categorical",
  #     description = "Probability distribution type",
  #     values = c("unknown", "normal", "log_normal", "beta", "gamma", "exponential", "uniform", "other"),
  #     bit_values = c("000", "001", "010", "011", "100", "101", "110", "111"),
  #     reference = "bitfield R-package"),
  #   skewness_class = list(
  #     bits_required = 2,
  #     encoding_type = "categorical",
  #     description = "Distribution skewness class",
  #     values = c("unknown", "left_skewed", "symmetric", "right_skewed"),
  #     bit_values = c("00", "01", "10", "11"),
  #     reference = "bitfield R-package"),
  #   size_class = list(
  #     bits_required = 3,
  #     encoding_type = "categorical",
  #     description = "Size classes",
  #     values = c("unknown", "very_small", "small", "medium", "large", "very_large", "enormous", "infinite"),
  #     bit_values = c("000", "001", "010", "011", "100", "101", "110", "111"),
  #     reference = "bitfield R-package"),
  #   p_value_class = list(
  #     bits_required = 3,
  #     encoding_type = "categorical",
  #     description = "Statistical significance class",
  #     values = c("not_tested", "not_significant", "p<0.1", "p<0.05", "p<0.01", "p<0.001", "p<0.0001", "deterministic"),
  #     bit_values = c("000", "001", "010", "011", "100", "101", "110", "111"),
  #     reference = "bitfield R-package"),
  #   temporal_aggregation = list(
  #     bits_required = 3,
  #     encoding_type = "categorical",
  #     description = "Temporal aggregation period",
  #     values = c("minute", "hour", "day", "week", "month", "season", "year", "multi_year"),
  #     bit_values = c("000", "001", "010", "011", "100", "101", "110", "111"),
  #     reference = "bitfield R-package"),
  #   trend_direction = list(
  #     bits_required = 2,
  #     encoding_type = "categorical",
  #     description = "Direction of trend",
  #     values = c("unknown", "decreasing", "stable", "increasing"),
  #     bit_values = c("00", "01", "10", "11"),
  #     reference = "bitfield R-package"),
  #   sample_type = list(
  #     bits_required = 4,
  #     encoding_type = "categorical",
  #     description = "Type of samples",
  #     values = c("field_measurement", "satellite", "aerial", "model_output",
  #                "literature", "statistical_metric", "laboratory", "archive",
  #                "reanalysis", "mixed", "other"),
  #     bit_values = c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
  #                    "1000", "1001", "1010"),
  #     reference = "bitfield R-package"),
  #   collector = list(
  #     bits_required = 2,
  #     encoding_type = "categorical",
  #     description = "Type of person collecting samples",
  #     values = c("expert", "professional", "citizen_scientist", "student"),
  #     bit_values = c("00", "01", "10", "11"),
  #     reference = "bitfield R-package"),
  #   processing_level = list(
  #     bits_required = 3,
  #     encoding_type = "categorical",
  #     description = "Data processing level",
  #     values = c("raw", "calibrated", "derived", "analyzed", "integrated",
  #                "synthesized", "reprocessed", "unknown"),
  #     bit_values = c("000", "001", "010", "011", "100", "101", "110", "111"),
  #     reference = "bitfield R-package"),
  #   algorithm_family = list(
  #     bits_required = 4,
  #     encoding_type = "categorical",
  #     description = "Algorithm family used in processing",
  #     values = c("none", "interpolation", "classification", "regression", "machine-learning",
  #                "physics based", "statistical", "ensemble", "bayesian", "empirical",
  #                "mechanistic", "neural network", "geostatistical", "decision-tree", "other"),
  #     bit_values = c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
  #                    "1000", "1001", "1010", "1011", "1100", "1101", "1111"),
  #     reference = "bitfield R-package"),
  #   edge_effect = list(
  #     bits_required = 1,
  #     encoding_type = "binary",
  #     description = "Presence of edge effects",
  #     values = c("edge_effect_absent", "edge_effect_present"),
  #     bit_values = c("0", "1"),
  #     reference = "bitfield R-package"),
  #   ecosystem_type = list(
  #     bits_required = 3,
  #     encoding_type = "categorical",
  #     description = "Primary ecosystem type",
  #     values = c("forest", "savanna", "shrubland", "grassland", "wetlands", "rocky", "desert",
  #                "artificial"),
  #     bit_values = c("000", "001", "010", "011", "100", "101", "110", "111"),
  #     reference = "IUCN Habitats Classification Scheme"),
  #   hemeroby_type = list(
  #     bits_required = 3,
  #     encoding_type = "categorical",
  #     description = "",
  #     values = c("ahemerob", "oligohemerob", "mesohemerob", "euhemerob", "polyhemerob", "metahemerob"),
  #     bit_values = c("000", "001", "010", "011", "100", "101"),
  #     reference = "Sukopp, H. (1972). Wandel von Flora und Vegetation in Mitteleuropa unter dem Einfluss des Menschen. Berichte über Landwirtschaft, 50, 112-139."),
  #   management_type = list(
  #     bits_required = 3,
  #     encoding_type = "categorical",
  #     description = "Land management type",
  #     values = c("unknown", "unmanaged", "protected", "extensive", "intensive", "restored", "abandoned", "mixed"),
  #     bit_values = c("000", "001", "010", "011", "100", "101", "110", "111"),
  #     reference = "bitfield R-package")
  # )


}
