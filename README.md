
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bitfield <a href='https://github.com/bitfloat/bitfield/'><img src='man/figures/logo.svg' align="right" height="200" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/bitfield)](https://cran.r-project.org/package=bitfield)
[![](http://cranlogs.r-pkg.org/badges/grand-total/bitfield)](https://cran.r-project.org/package=bitfield)

[![R-CMD-check](https://github.com/bitfloat/bitfield/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bitfloat/bitfield/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/bitfloat/bitfield/graph/badge.svg?token=QZB36RION3)](https://app.codecov.io/gh/bitfloat/bitfield)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Overview

The `bitfield` package enables efficient storage and transmission of
metadata and intermediate results across scientific workflows by
encoding computational decisions into sequences of bits that transform
to integer values. This approach allows storing rich contextual
information - including quality assessments, uncertainty metrics, model
parameters, and processing thresholds - in a single column of a table or
raster layer.

Think of a bit as a switch representing off and on states. A sequence of
n bits can accommodate 2^n states, enabling the encoding of boolean
responses, categorical cases, integers, or even floating-point values
with limited precision. The resulting bitfield creates a ‘computational
footprint’ that preserves the context needed for cross-workflow
integration and downstream reuse.

## Installation

Install the official version from CRAN:

``` r
install.packages("bitfield")
```

Install the latest development version from GitHub:

``` r
devtools::install_github("bitfloat/bitfield")
```

## Getting Started

``` r
library(bitfield)
library(dplyr, warn.conflicts = FALSE)

# Example data with quality issues
bf_tbl
#> # A tibble: 9 × 5
#>       x     y commodity yield year 
#>   <dbl> <dbl> <fct>     <dbl> <chr>
#> 1  25.3  59.5 soybean   11.2  2021 
#> 2  27.9  58.1 maize     12.0  <NA> 
#> 3  27.8  57.8 soybean   13.2  2021r
#> 4  27    59.2 <NA>       4.43 2021 
#> 5 259   Inf   honey     13.0  2021 
#> 6  27.3  59.1 maize      8.55 2021 
#> 7  26.1  58.4 soybean   11.3  2021 
#> 8  26.5 NaN   maize     10.6  2021 
#> 9   0     0   soybean    9.01 2021
```

Create a registry to capture metadata about your workflow:

``` r
reg <- bf_registry(name = "data_quality", 
                   description = "Quality assessment for agricultural data")

# Test for missing values
reg <- bf_map(protocol = "na", data = bf_tbl, x = commodity, registry = reg)

# Encode yield values with limited precision  
reg <- bf_map(protocol = "numeric", data = bf_tbl, x = yield, 
              format = "half", registry = reg)

# View the registry structure
reg
#>   width 17
#>   flags 2  -|----------------
#> 
#>   pos encoding  type    col
#>   1   0.0.1/0   na      commodity
#>   2   1.5.10/15 numeric yield
```

Encode the flags into integer representation:

``` r
(field <- bf_encode(registry = reg))
#> # A tibble: 9 × 1
#>   bf_int1
#>     <int>
#> 1   18840
#> 2   18942
#> 3   19101
#> 4   83054
#> 5   19071
#> 6   18502
#> 7   18851
#> 8   18770
#> 9   18561
```

Decode the bitfield in a downstream application:

``` r
(flags <- bf_decode(x = field, registry = reg, verbose = FALSE))
#> # A tibble: 9 × 2
#>   na_commodity numeric_yield   
#>   <chr>        <chr>           
#> 1 0            0100100110011000
#> 2 0            0100100111111110
#> 3 0            0100101010011101
#> 4 1            0100010001101110
#> 5 0            0100101001111111
#> 6 0            0100100001000110
#> 7 0            0100100110100011
#> 8 0            0100100101010010
#> 9 0            0100100010000001

# -> legend is available in bf_legend

bf_tbl |>
  bind_cols(flags) |>
  knitr::kable()
```

|     x |    y | commodity |     yield | year  | na_commodity | numeric_yield    |
|------:|-----:|:----------|----------:|:------|:-------------|:-----------------|
|  25.3 | 59.5 | soybean   | 11.192915 | 2021  | 0            | 0100100110011000 |
|  27.9 | 58.1 | maize     | 11.986793 | NA    | 0            | 0100100111111110 |
|  27.8 | 57.8 | soybean   | 13.229386 | 2021r | 0            | 0100101010011101 |
|  27.0 | 59.2 | NA        |  4.431376 | 2021  | 1            | 0100010001101110 |
| 259.0 |  Inf | honey     | 12.997422 | 2021  | 0            | 0100101001111111 |
|  27.3 | 59.1 | maize     |  8.548882 | 2021  | 0            | 0100100001000110 |
|  26.1 | 58.4 | soybean   | 11.276921 | 2021  | 0            | 0100100110100011 |
|  26.5 |  NaN | maize     | 10.640715 | 2021  | 0            | 0100100101010010 |
|   0.0 |  0.0 | soybean   |  9.010452 | 2021  | 0            | 0100100010000001 |

The decoded information is also available in the package environment for
programmatic access:

``` r
# access values manually
ls(.GlobalEnv)
#> [1] "bf_legend"     "field"         "flags"         "na_commodity" 
#> [5] "numeric_yield" "reg"

.GlobalEnv[["na_commodity"]]
#> [1] 0 0 0 1 0 0 0 0 0
.GlobalEnv[["numeric_yield"]]
#> [1] 11.187500 11.984375 13.226562  4.429688 12.992188  8.546875 11.273438
#> [8] 10.640625  9.007812

# beware that numeric values are stored with the precision you have specified
bf_tbl$yield
#> [1] 11.192915 11.986793 13.229386  4.431376 12.997422  8.548882 11.276921
#> [8] 10.640715  9.010452
```

## Further Reading

- **[Best Practices](articles/best-practices.html)**: Guidelines for
  effective bitfield design, protocol selection, and common pitfalls
- **[Community Contributions](articles/community-contributions.html)**:
  How to contribute protocols to the community standards repository
- **[Applications and Citations](articles/applications.html)**: Papers
  and projects using the bitfield package

## Getting Help

- Browse the [function reference](reference/index.html) for detailed
  documentation
- Report bugs at
  [github.com/bitfloat/bitfield/issues](https://github.com/bitfloat/bitfield/issues)
- Join discussions about community standards at
  [github.com/bitfloat/standards](https://github.com/bitfloat/standards)
