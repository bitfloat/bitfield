## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bitfield)

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
  name = "data_age_days",
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

test_reg <- bf_registry(name = "freshness_test", description = "Data freshness testing")
test_reg <- bf_map(protocol = "dataAgeProtocol", data = test_data,
                   daysSince = days_old, registry = test_reg)

test_reg

