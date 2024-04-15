
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bitfield <a href='https://github.com/ehrmanns/bitfield/'><img src='man/figures/logo.svg' align="right" height="200" /></a>

<!-- badges: start -->
<!-- [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/)](https://cran.r-project.org/package=) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/)](https://doi.org/) -->

[![R-CMD-check](https://github.com/ehrmanns/bitfield/workflows/R-CMD-check/badge.svg)](https://github.com/ehrmanns/bitfield/actions)
[![codecov](https://codecov.io/gh/ehrmanns/bitfield/branch/master/graph/badge.svg?token=hjppymcGr3)](https://codecov.io/gh/ehrmanns/bitfield)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- [![](http://cranlogs.r-pkg.org/badges/grand-total/)](https://cran.r-project.org/package=) -->
<!-- badges: end -->

## Overview

This package is designed to build sequences of bits (i.e.,
[bitfields](https://en.wikipedia.org/wiki/Bit_field)) to capture the
computational footprint of a scientific/modelling workflow. This can be
useful when documenting

- the metadata of any tabular dataset by collecting information
  throughout the dataset creation process,
- a provenance graph that documents how a gridded modelled data product
  was built,
- intermediate data that accrue along a workflow, or
- a set of output metrics or parameters

into a sequence of bits that are encoded as a single integer value that
is stored in a column of a table or a raster layer.

Think of a bit as a switch representing off and on states. A combination
of a pair of bits can store four states, and n bits can accommodate 2^n
states. In R, integers are typically 32-bit values, allowing a single
integer to store 32 switches (called `flags` here) and 2^32 states.
These states could be the outcomes of (simple) tests that document
binary responses, cases or numeric values. Those data could be described
as meta-analytic or meta-algorithmic data, because they can be re-used
to extend the analysis pipeline or algorithm by downstream applications.

## Installation

Install the official version from CRAN:

``` r
# install.packages("bitfield")
```

Install the latest development version from github:

``` r
devtools::install_github("EhrmannS/bitfield")
```

## Examples

``` r
library(bitfield)

library(dplyr, warn.conflicts = FALSE); library(CoordinateCleaner); library(stringr)
```

Let’s first load an example dataset

``` r
bityield$x                                       # invalid (259) and improbable (0) coordinate value
#>  [1]  25.3  27.9  27.8  27.0 259.0  27.3  26.1  26.5   0.0  25.7

bityield$y                                       # Inf and NaN value
#>  [1] 59.5 58.1 57.8 59.2  Inf 59.1 58.4 59.0  0.0  NaN

bityield$commodity                               # NA value or mislabelled term ("honey")
#>  [1] "soybean" "maize"   NA        "maize"   "honey"   "maize"   "soybean"
#>  [8] "maize"   "soybean" "maize"

bityield$yield                                   # correct range?!
#>  [1] 11.192915 11.986793 13.229386  4.431376 12.997422  8.548882 11.276921
#>  [8] 10.640715  9.010452 13.169897

bityield$year                                    # flags (*r)
#>  [1] "2021"  NA      "2021r" "2021"  "2021"  "2021"  "2021"  "2021"  "2021" 
#> [10] "2021"

# and there is a set of valid commodity terms
validComm <- c("soybean", "maize")
```

The first step is in creating what is called `registry` in `bitfield`.
This registry captures all the information required to build the
bitfield

``` r
yieldReg <- bf_registry(name = "yield_QA",
                        description = "this bitfield documents quality assessment in a table of yield data.")
```

Then, individual bit flags need to be grown by specifying the respective
mapping function. These functions create flags for the most common
applications, such as `bf_na()` (to test for missing values),
`bf_case()` (to test what case/class the observations are part
of),`bf_length()` (to count the number of digits of a variable), or
`bf_numeric()` to encode a numeric (floating point) variable as bit
sequence.

``` r
# tests for longitude availability
yieldReg <- 
  bf_na(x = bityield,                            # specify where to determine flags
        test = "x",                              # ... and which variable to test
        pos = 1,                                 # specify at which position to store the flag
        registry = yieldReg)                     # provide the registry to update

# test which case an observation is part of
yieldReg <- 
  bf_case(x = bityield, exclusive = FALSE,
          yield >= 11, yield < 11 & yield > 9, yield < 9 & commodity == "maize",
          registry = yieldReg)

# test the length (number of digits) of values
yieldReg <- 
  bf_length(x = bityield, test = "y",
            registry = yieldReg)
  
# store a simplified (e.g. rounded) numeric value
# yieldReg <- 
#   bf_numeric(x = bityield, source = "yield", precision = 3, 
#              registry = yieldReg)
```

Various derived functions build on these and thus require bits according
to the same rules. Finally, the registry is combined into a bitfield
that is encoded by an integer and can be stored by the usual means. The
resulting data structure is a record of all the things that are grown on
the bitfield.

``` r
yieldReg
```

This is, however, not yet the bitfield. The registry is merely the
instruction manual, so to speak, to create the bitfield and encode it as
integer, with the function `bf_encode()`.

``` r
(intBit <- bf_encode(registry = yieldReg))
#> # A tibble: 10 × 1
#>     int1
#>    <dbl>
#>  1     4
#>  2     4
#>  3     4
#>  4    20
#>  5     3
#>  6    20
#>  7     4
#>  8    10
#>  9     9
#> 10     0
```

The bitfield can be decoded based on the registry with the function
`bf_decode()` at a later point in time, where the metadata contained in
the bitfield can be studied or extended in a downstream application.

``` r
# bitfield <- bf_decode(x = intBit, registry = yieldReg, sep = "-")
# 
# # -> prints legend by default, which is also available in bf_env$legend
# 
# bityield |> 
#   bind_cols(bitfield) |> 
#   kable()
```

~~Together with the rules mentioned above, we can read the binary
representation on step at a time. For example, considering the second
position, with the description
`the values in column 'x' range between [-180,180]`, we see that row
five has the value `0`, which means according to naming-rule 1
(`FALSE == 0`), that the x-value here should be outside of the range of
\[-180, 180\], which we can confirm.~~include some example here

## Bitfields for other data-types

This example here shows how to compute quality bits for tabular data,
but this technique is especially helpful for raster data. To keep this
package as simple as possible, no specific methods for rasters were
developed (so far), they instead need to be converted to tabular form
and joined to the attributes or meta data that should be added to the
QB, for example like this

``` r
library(terra)

raster <- rast(matrix(data = 1:25, nrow = 5, ncol = 5))

input <- values(raster) |> 
  as_tibble() |>  
  rename(values = lyr.1) |> 
  bind_cols(crds(raster), .)

# from here we can continue creating a bitfield and growing bits on it just like shown above...
intBit <- bf_combine(...)

# ... and then converting it back to a raster
QB_rast <- crds(raster) |> 
  bind_cols(intBit) |> 
  rast(type = "xyz", crs = crs(raster), extent = ext(raster))
```

# To Do

- write registry show method
- include MD5 sum for a bitfield and update it each time the bitfield is
  grown further
