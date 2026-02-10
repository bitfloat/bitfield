## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bitfield)
library(dplyr, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
bf_tbl

## -----------------------------------------------------------------------------
set.seed(42)
x <- runif(1000, 0.1, 10)
bf_analyze(x, range = c(0, 15), max_bits = 8, decimals = 1)

## ----eval = FALSE-------------------------------------------------------------
# # After deciding on exp=4, sig=3 based on bf_analyze() output:
# reg <- bf_map(protocol = "numeric", data = my_data, x = sd_values,
#               registry = reg, fields = list(exponent = 4, significand = 3))

## -----------------------------------------------------------------------------
# 1. Create a registry
reg <- bf_registry(
  name = "yield_QA",
  description = "Quality assessment for yield data",
  template = bf_tbl)

# 2. Add boolean flags
reg <- bf_map(protocol = "na", data = bf_tbl, x = commodity, registry = reg)
reg <- bf_map(protocol = "inf", data = bf_tbl, x = x, registry = reg)
reg <- bf_map(protocol = "inf", data = bf_tbl, x = y, registry = reg)

# 3. Add a category
reg <- bf_map(protocol = "category", data = bf_tbl, x = commodity,
              registry = reg, na.val = 0L)

# 4. Add a numeric value with custom float encoding
reg <- bf_map(protocol = "numeric", data = bf_tbl, x = yield,
              registry = reg, format = "half")

reg

## -----------------------------------------------------------------------------
# 5. Encode and decode
field <- bf_encode(registry = reg)
decoded <- bf_decode(field, registry = reg, verbose = FALSE)

head(decoded, 3)

## -----------------------------------------------------------------------------
# input NAs
sum(is.na(bf_tbl$commodity))

# NAs after roundtrip
sum(decoded$na_commodity == 1)

## -----------------------------------------------------------------------------
problematic <- bf_tbl[c(4, 5, 8, 9), ]
print(problematic)

## -----------------------------------------------------------------------------
valueFlag <- bf_protocol(
  name = "valueFlag",
  description = paste("Extracts trailing status flags from {x}.",
                      "0 = none, 1 = r(evised), 2 = p(rovisional),",
                      "3 = e(stimated)"),
  test = "function(x) { suffix <- sub('.*([a-z])$', '\\\\1', x); match(suffix, c('r','p','e'), nomatch = 0L) }",
  example = list(x = c("2020", "2021r", "2019p", "2018e", NA)),
  type = "int",
  bits = 2
)

## -----------------------------------------------------------------------------
valueFlagV2 <- bf_protocol(
  name = "valueFlag",
  description = paste("Extracts trailing status flags from {x}.",
                      "0 = none, 1 = r(evised), 2 = p(rovisional),",
                      "3 = e(stimated). Now also handles uppercase flags."),
  test = "function(x) { suffix <- sub('.*([a-zA-Z])$', '\\\\1', tolower(x)); match(suffix, c('r','p','e'), nomatch = 0L) }",
  example = list(x = c("2020", "2021r", "2019P", "2018e", NA)),
  type = "int",
  bits = 2,
  version = "1.1.0",
  extends = "valueFlag_1.0.0",
  note = "Now handles uppercase flags via case-insensitive matching"
)

## ----eval = FALSE-------------------------------------------------------------
# # List available protocols
# bf_standards(action = "list")
# 
# # Pull a community protocol
# soil_protocol <- bf_standards(
#   protocol = "soil_moisture",
#   remote = "environmental/soil",
#   action = "pull"
# )
# 
# # Push your own protocol
# bf_standards(
#   protocol = dataAgeProtocol,
#   remote = "environmental/temporal",
#   action = "push",
#   version = "1.0.0",
#   change = "Initial release: data age encoding for environmental monitoring"
# )

