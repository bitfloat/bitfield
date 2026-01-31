
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bitfield <a href='https://github.com/bitfloat/bitfield/'><img src='man/figures/logo.svg' align="right" height="200" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/bitfield)](https://cran.r-project.org/package=bitfield)

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

### Tabular Data

``` r
library(bitfield)

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
                   description = "Quality assessment for agricultural data",
                   template = bf_tbl)

# Test for missing values (1 bit)
reg <- bf_map(protocol = "na", data = bf_tbl, x = commodity, registry = reg)

# Encode yield values with half precision (16 bits)
reg <- bf_map(protocol = "numeric", data = bf_tbl, x = yield,
              format = "half", registry = reg)

# View the registry structure
reg
#>   type  data.frame
#>   width 17
#>   flags 2  -|----------------
#> 
#>   pos encoding  name    col
#>   1   0.0.1/0   na      commodity
#>   2   1.5.10/15 numeric yield
```

Encode the flags into integer representation:

``` r
field <- bf_encode(registry = reg)
field
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
decoded <- bf_decode(x = field, registry = reg, verbose = FALSE)

# Returns a named list with decoded values
names(decoded)
#> [1] "na_commodity"  "numeric_yield"

# Access individual flags
decoded$na_commodity
#> [1] 0 0 0 1 0 0 0 0 0
decoded$numeric_yield
#> [1] 11.187500 11.984375 13.226562  4.429688 12.992188  8.546875 11.273438
#> [8] 10.640625  9.007812
```

### Raster Data

The same workflow applies to raster data - just use a `SpatRaster` as
the template:

``` r
library(terra)

# Create example raster
bf_rst <- rast(nrows = 3, ncols = 3, vals = bf_tbl$commodity, names = "commodity")
bf_rst$yield <- rast(nrows = 3, ncols = 3, vals = bf_tbl$yield)

# Create registry with raster template
reg_rst <- bf_registry(name = "raster_quality",
                       description = "Quality flags for raster data",
                       template = bf_rst)

reg_rst <- bf_map(protocol = "na", data = bf_rst, x = commodity, registry = reg_rst)
reg_rst <- bf_map(protocol = "numeric", data = bf_rst, x = yield,
                  format = "half", registry = reg_rst)

# Encode returns a SpatRaster
field_rst <- bf_encode(registry = reg_rst)
field_rst
#> class       : SpatRaster 
#> size        : 3, 3, 1  (nrow, ncol, nlyr)
#> resolution  : 120, 60  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84) 
#> source(s)   : memory
#> name        : bf_int1 
#> min value   :   18502 
#> max value   :   83054

# Decode returns a multi-layer SpatRaster
decoded_rst <- bf_decode(x = field_rst, registry = reg_rst, verbose = FALSE)
decoded_rst
#> class       : SpatRaster 
#> size        : 3, 3, 2  (nrow, ncol, nlyr)
#> resolution  : 120, 60  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84) 
#> source(s)   : memory
#> names       : na_commodity, numeric_yield 
#> min values  :            0,      4.429688 
#> max values  :            1,     13.226562
```

## Getting Help

- Browse the [function
  reference](https://bitfloat.github.io/bitfield/reference/index.html)
  for detailed documentation
- Report bugs at
  [github.com/bitfloat/bitfield/issues](https://github.com/bitfloat/bitfield/issues)
