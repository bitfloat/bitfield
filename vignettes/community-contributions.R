## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bitfield)
library(dplyr)

## ----eval=FALSE---------------------------------------------------------------
# # Generate GitHub token (opens browser)
# usethis::create_github_token()
# 
# # Store token securely in R
# gitcreds::gitcreds_set()
# 
# # Test your setup
# bf_standards(action = "list")

## ----eval=FALSE---------------------------------------------------------------
# usethis::use_git_config(user.name = "Your Name",
#                        user.email = "your.email@example.com")

## -----------------------------------------------------------------------------
# Example: Create a protocol for data freshness
dataAgeProtocol <- bf_protocol(
  name = "dataAgeDays",
  description = paste("Days since data collection, encoded as 8-bit integer",
                     "(0-255 days) where {daysSince} days are encoded"),
  test = "function(daysSince) { pmin(pmax(round(daysSince), 0), 255) }",
  example = list(daysSince = c(1, 15, 30, 90, 200)),
  type = "int",
  bits = 8
)

# Test the protocol with example data
test_data <- data.frame(
  sample_id = 1:9,
  days_old = c(1, 5, 12, 30, 45, 67, 120, 180, 300)  # Include > 255 to test limit
)

test_reg <- bf_registry(name = "freshness_test", description = "Data freshness testing",
                        template = test_data)
test_reg <- bf_map(protocol = "dataAgeProtocol", data = test_data,
                   daysSince = days_old, registry = test_reg)

test_reg

## -----------------------------------------------------------------------------
# Test encoding/decoding cycle
test_field <- bf_encode(registry = test_reg)
test_decoded <- bf_decode(test_field, registry = test_reg, verbose = FALSE)

# Verify protocol handles edge cases correctly
verification <- data.frame(
  original = test_data$days_old,
  decoded = test_decoded$dataAgeProtocol_days_old,
  capped_at_255 = pmin(test_data$days_old, 255)
)

print(verification)

## -----------------------------------------------------------------------------
# Examine the example data
bf_tbl

# Create a protocol for yield reliability based on coordinate quality
yieldReliability <- bf_protocol(
  name = "yieldReliability",
  description = paste("Yield reliability score (0-7) based on coordinate quality.",
                     "Higher scores indicate more reliable yield measurements.",
                     "Coordinates {x}, {y} are evaluated for validity."),
  test = "function(x, y) { ifelse(is.na(x) | is.na(y), 0L, ifelse(x == 0 & y == 0, 1L, ifelse(is.infinite(x) | is.infinite(y), 2L, 3L))) }",
  example = list(x = c(25.3, 0, 259), y = c(59.5, 0, Inf)),
  type = "int",
  bits = 3  # 0-7 range needs 3 bits
)

# Test with the example data
reliability_reg <- bf_registry(name = "yield_reliability_test",
                              description = "Test yield reliability protocol",
                              template = bf_tbl)
reliability_reg <- bf_map(protocol = "yieldReliability", data = bf_tbl,
                         x = x, y = y, registry = reliability_reg)

reliability_field <- bf_encode(registry = reliability_reg)
reliability_decoded <- bf_decode(reliability_field, registry = reliability_reg, verbose = FALSE)

# Check results - first see what columns exist
print("Available columns:")
print(names(reliability_decoded))

result_check <- bf_tbl %>%
  bind_cols(reliability_decoded)

print(result_check)

## -----------------------------------------------------------------------------
# Enhanced version of a basic protocol
enhancedNaCheck <- bf_protocol(
  name = "enhancedNaCheck",
  description = paste("Enhanced missing value detection for {x}.",
                     "Detects NA, empty strings, and placeholder values (-999, -99)."),
  test = "function(x) { as.integer(is.na(x) | (is.character(x) & x == '') | (is.numeric(x) & x %in% c(-999, -99, 9999))) }",
  example = list(x = c("valid", "", NA, -999, "normal")),
  type = "int",
  bits = 1,
  version = "1.1.0",
  extends = "na_1.0.0",
  note = "Enhanced to detect placeholder values and empty strings"
)

## -----------------------------------------------------------------------------
# Check categories in example data
unique(bf_tbl$commodity)

# Create optimized categorical protocol
commodityProtocol <- bf_protocol(
  name = "agriculturalCommodity",
  description = paste("Agricultural commodity classification with",
                     "3-bit encoding (0-7): 1=soybean, 2=maize, 3=honey, 0=NA/unknown"),
  test = "function(commodity) { ifelse(is.na(commodity), 0L, ifelse(commodity == 'soybean', 1L, ifelse(commodity == 'maize', 2L, ifelse(commodity == 'honey', 3L, 0L)))) }",
  example = list(commodity = c("soybean", "maize", "honey", NA, "unknown")),
  type = "int",
  bits = 3
)

# Test the categorical protocol
cat_reg <- bf_registry(name = "commodity_test", description = "Commodity encoding test",
                       template = bf_tbl)
cat_reg <- bf_map(protocol = "commodityProtocol", data = bf_tbl,
                  commodity = commodity, registry = cat_reg)

cat_field <- bf_encode(registry = cat_reg)
cat_decoded <- bf_decode(cat_field, registry = cat_reg, verbose = FALSE)

commodity_check <- bf_tbl %>%
  bind_cols(cat_decoded) %>%
  select(commodity, commodityProtocol_commodity)

print(commodity_check)

## ----eval=FALSE---------------------------------------------------------------
# # Push to community standards
# bf_standards(
#   protocol = data_age_protocol,
#   remote = "environmental/temporal",
#   action = "push",
#   version = "1.0.0",
#   change = "Initial release: data age encoding for environmental monitoring"
# )
# 
# # Push agricultural commodity protocol
# bf_standards(
#   protocol = commodity_protocol,
#   remote = "agricultural/crops",
#   action = "push",
#   version = "1.0.0",
#   change = "Agricultural commodity encoding optimized for common crops"
# )

## ----eval=FALSE---------------------------------------------------------------
# # List available protocols
# available_protocols <- bf_standards(action = "list")
# 
# # Pull a specific protocol
# soil_moisture <- bf_standards(
#   protocol = "soil_moisture",
#   remote = "environmental/soil",
#   action = "pull"
# )
# 
# # Use in your analysis
# soil_reg <- bf_registry(name = "soil_analysis", description = "Soil moisture analysis",
#                         template = my_soil_data)
# soil_reg <- bf_map(protocol = soil_moisture, data = my_soil_data,
#                   valMoisture = moisture_percent, registry = soil_reg)

