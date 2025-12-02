## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bitfield)
library(dplyr, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
# Example: Quality assessment for agricultural data
bf_tbl

## -----------------------------------------------------------------------------
reg <- bf_registry(name = "quality_check",
                   description = "Agricultural data quality assessment")

# Detect missing commodities (1 bit)
reg <- bf_map(protocol = "na", data = bf_tbl, x = commodity, registry = reg)

# Check for infinite coordinates (1 bit each)
reg <- bf_map(protocol = "inf", data = bf_tbl, x = x, registry = reg)
reg <- bf_map(protocol = "inf", data = bf_tbl, x = y, registry = reg)

# Check for suspicious zero coordinates (1 bit)
reg <- bf_map(protocol = "matches", data = bf_tbl, x = x, registry = reg, set = 0)

reg

## -----------------------------------------------------------------------------
# Good: Separate protocols for different quality checks
reg2 <- bf_registry(name = "detailed_quality", description = "Detailed quality flags")

reg2 <- bf_map(protocol = "na", data = bf_tbl, x = commodity, registry = reg2)
reg2 <- bf_map(protocol = "range", data = bf_tbl, x = yield, registry = reg2,
               min = 0, max = 20, na.val = 1L)  # Reasonable yield range
reg2 <- bf_map(protocol = "grepl", data = bf_tbl, x = year, registry = reg2,
               pattern = "[0-9]{4}$", na.val = 0L)  # Valid 4-digit year

reg2

## -----------------------------------------------------------------------------
# Encode with sufficient precision for downstream analysis
field <- bf_encode(registry = reg2)
decoded <- bf_decode(field, registry = reg2, verbose = FALSE)

# Combine with original data for downstream use
result <- bf_tbl %>%
  bind_cols(decoded) %>%
  bind_cols(bf_int = field$bf_int1)

head(result, 3)

## -----------------------------------------------------------------------------
# Simple but effective approach
simple_reg <- bf_registry(name = "essential", description = "Essential quality flags")

simple_reg <- bf_map(protocol = "na", data = bf_tbl, x = commodity, registry = simple_reg)
simple_reg <- bf_map(protocol = "numeric", data = bf_tbl, x = yield, registry = simple_reg,
                     format = "half")  # Preserve yield with limited precision

simple_reg

## -----------------------------------------------------------------------------
# Test with the problematic rows from bf_tbl
problematic_rows <- bf_tbl[c(4, 5, 8, 9), ]  # NA, Inf, NaN, zeros
print(problematic_rows)

test_reg <- bf_registry(name = "edge_test", description = "Edge case testing")
test_reg <- bf_map(protocol = "na", data = problematic_rows, x = commodity, registry = test_reg)
test_reg <- bf_map(protocol = "inf", data = problematic_rows, x = x, registry = test_reg)

# Verify encoding/decoding works
test_field <- bf_encode(registry = test_reg)
test_decoded <- bf_decode(test_field, registry = test_reg, verbose = FALSE)
test_decoded

## -----------------------------------------------------------------------------
# Compare different numeric encoding approaches
reg_precise <- bf_registry(name = "precise", description = "High precision encoding")
reg_efficient <- bf_registry(name = "efficient", description = "Efficient encoding")

# High precision (16 bits)
reg_precise <- bf_map(protocol = "numeric", data = bf_tbl, x = yield,
                      registry = reg_precise, format = "half")

# More efficient (using half precision)
reg_efficient <- bf_map(protocol = "numeric", data = bf_tbl, x = yield,
                        registry = reg_efficient, format = "half")

cat("Precise encoding:", reg_precise@width, "bits\n")
cat("Efficient encoding:", reg_efficient@width, "bits\n")

## -----------------------------------------------------------------------------
# Check unique categories first
unique(bf_tbl$commodity)

# Encode categories (automatically determines bit needs)
cat_reg <- bf_registry(name = "categories", description = "Categorical encoding")
cat_reg <- bf_map(protocol = "category", data = bf_tbl, x = commodity,
                  registry = cat_reg, na.val = 0L)

cat_reg

## -----------------------------------------------------------------------------
# Well-documented registry
final_reg <- bf_registry(
  name = "agricultural_qa",
  description = "Agricultural data quality assessment including missing value detection, coordinate validation, and yield encoding with 8-bit precision"
)

final_reg <- bf_map(protocol = "na", data = bf_tbl, x = commodity, registry = final_reg)
final_reg <- bf_map(protocol = "numeric", data = bf_tbl, x = yield, registry = final_reg,
                    format = "half")

final_reg

## -----------------------------------------------------------------------------
# Always test encode/decode cycle
final_field <- bf_encode(registry = final_reg)
final_decoded <- bf_decode(final_field, registry = final_reg, verbose = FALSE)

# Check that important information is preserved
original_na_count <- sum(is.na(bf_tbl$commodity))
decoded_na_count <- sum(final_decoded$na_commodity == "1")

cat("Original NA count:", original_na_count, "\n")
cat("Decoded NA count:", decoded_na_count, "\n")
cat("Information preserved:", original_na_count == decoded_na_count, "\n")

