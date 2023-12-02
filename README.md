
<!-- README.md is generated from README.Rmd. Please edit that file -->

# queuebee <a href='https://github.com/luckinet/queuebee/'><img src='man/figures/logo.svg' align="right" height="200" /></a>

<!-- badges: start -->
<!-- [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/)](https://cran.r-project.org/package=) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/)](https://doi.org/) -->

[![R-CMD-check](https://github.com/luckinet/queuebee/workflows/R-CMD-check/badge.svg)](https://github.com/luckinet/queuebee/actions)
[![codecov](https://codecov.io/gh/luckinet/queuebee/branch/master/graph/badge.svg?token=hjppymcGr3)](https://codecov.io/gh/luckinet/queuebee)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- [![](http://cranlogs.r-pkg.org/badges/grand-total/)](https://cran.r-project.org/package=) -->
<!-- badges: end -->

## Overview

This package is designed to queue up bits for documenting the quality,
provenance, or other metadata of any tabular dataset by collecting
information throughout the dataset creation process. This can also be
useful in any setting where large amounts of non-complex data have to be
stored in an efficient way. The resulting data structure is referred to
as a “bit flag” or “[bit
field](https://en.wikipedia.org/wiki/Bit_field)”, commonly used in MODIS
dataproducts to document layer quality.

Think of a bit as a switch, having values 0 and 1, representing off and
on states. A pair of bits can store four states, and n bits can
accommodate 2^n states. In R, integers are typically 32-bit values,
allowing a single integer to store 32 switches and 2^32 states.
`queuebee` facilitates combining states into a bit sequence and storing
that as an integer. These states might be the outcomes of functions
returning boolean values (each using one bit) or functions returning a
small set of cases (using the corresponding number of bits).

In essence, `queuebee` allows you to capture a diverse range of
information into a single value, like a column from table or a raster
layer from a data cube. This is beneficial not only for reporting
quality metrics, provenance, or other metadata but also for simplifying
the reuse of ancillary data in script-based workflows.

## Installation

Install the official version from CRAN:

``` r
# install.packages("queuebee")
```

Install the latest development version from github:

``` r
devtools::install_github("EhrmannS/queuebee")
```

## Examples

``` r
library(queuebee)
library(CoordinateCleaner)
library(stringr)
```

Let’s first build an example dataset

``` r
input <- tibble(x = sample(seq(23.3, 28.1, 0.1), 10),
                y = sample(seq(57.5, 59.6, 0.1), 10),
                year = rep(2021, 10),
                commodity = rep(c("soybean", "maize"), 5),
                some_other = rnorm(10))

validComm <- c("soybean", "maize")
```

And make it have false values

``` r
input$x[5] <- 259
input$x[9] <- 0
input$y[10] <- NA_real_
input$y[9] <- 0
input$year[c(2:3)] <- c(NA, "2021r")
input$commodity[c(3, 5)] <- c(NA_character_, "honey")

kable(input)
```

|     x |    y | year  | commodity | some_other |
|------:|-----:|:------|:----------|-----------:|
|  25.8 | 57.7 | 2021  | soybean   | -0.0026519 |
|  23.8 | 58.4 | NA    | maize     |  0.2049487 |
|  27.2 | 58.3 | 2021r | NA        |  0.0539777 |
|  24.6 | 59.0 | 2021  | maize     | -1.0008374 |
| 259.0 | 58.1 | 2021  | honey     | -0.2780210 |
|  25.7 | 58.7 | 2021  | maize     | -2.1653212 |
|  27.3 | 58.9 | 2021  | soybean   | -0.0587031 |
|  27.6 | 59.4 | 2021  | maize     |  0.6484230 |
|   0.0 |  0.0 | 2021  | soybean   | -1.7140368 |
|  25.1 |   NA | 2021  | maize     | -0.3314015 |

The first step in yielding quality bits is in creating a bitfield with

``` r
newBitfield <- qb_create(width = 10, length = dim(input)[1])
```

1.  The `width =` specifies how many bits are in the bitfield.
2.  The `lenght =` specifies how long the output table is. This is
    usually taken from the input, as we want to produce a bit flag for
    every value in out input.
3.  The `name =` specifies the label of the bitfield, which becomes very
    important when publishing, because bitfield and output table are
    stored in different files and it must be possible to unambiguously
    associate them to one another.

Then, individual bits need to be grown by specifying a mapping function
and which position of the bitfield should be modified. When providing a
concise yet expressive description, you will also in the future still
understand what you did here. To help with growing bits, various aspects
are important to keep in mind

1.  if your mapping function returns a boolean value, `FALSE == 0` and
    `TRUE == 1`.
2.  if your mapping function returns cases, they will be assigned a
    sequence of numbers that are encoded by their respective binary
    representation, i.e. if there are 3 cases (which takes up 2 bits),
    the bits will have be `case 1 = 00`, `case 2 == 01` and
    `case 3 == 10`.
3.  chose name and description so that they reflect the outcome of the
    mapping function. If the function tests whether a value is `NA` and
    returns `TRUE` if the value is `NA`, the name and description should
    indicate that the bit flag is `1` when an `NA` value has been found.

``` r
newBitfield <- newBitfield %>%
  # explicit tests for coordinates ...
  qb_grow(bit = qbb_na(x = input, test = "x"), name = "not_na_x",
          desc = c("x-coordinate values do not contain any NAs"),
          pos = 1, bitfield = .) %>%
  qb_grow(bit =  qbb_range(x = input, test = "x", min = -180, max = 180), name = "range_1_x",
          desc = c("x-coordinate values are numeric and within the valid WGS84 range"),
          pos = 2, bitfield = .) %>%
  
  # ... or override NA test
  qb_grow(bit = qbb_range(x = input, test = "y", min = -90, max = 90), name = "range_1_y",
          desc = c("y-coordinate values are numeric and within the valid WGS84 range, NAs are FALSE"),
          pos = 3, na_val = FALSE, bitfield = .) %>%
  
  # it is also possible to use other functions that give flags, such as from CoordinateCleaner ...
  qb_grow(bit = cc_equ(x = input, lon = "x", lat = "y", value = "flagged"), name = "distinct_coords",
          desc = c("x and y coordinates are not identical, NAs are FALSE"),
          pos = 4, na_val = FALSE, bitfield = .) %>%
  
  # ... or stringr ...
  qb_grow(bit = str_detect(input$year, "r"), name = "flag_year",
          desc = c("year values do have a flag, NAs are FALSE"),
          pos = 5, na_val = FALSE, bitfield = .) %>%
  
  # ... or even base R
  qb_grow(bit = !is.na(as.integer(input$year)), name = "valid_year",
          desc = c("year values are valid integers"),
          pos = 6, bitfield = .)  %>%
  
  # test for matches with an external vector
  qb_grow(bit = qbb_match(x = input, test = "commodity", against = validComm), name = "match_commodities",
          desc = c("commodity values are part of 'soybean' or 'maize'"),
          pos = 7, na_val = FALSE, bitfield = .) %>%
  
  # define cases
  qb_grow(bit = qbb_case(x = input, some_other > 0.5, some_other > 0, some_other < 0, exclusive = FALSE), name = "cases_some_other",
          desc = c("some_other values are distinguished into large (. > 0.5), medium (. > 0) and small (. < 0)"),
          pos = 8:9, bitfield = .)
#> Testing equal lat/lon
#> Flagged NA records.
#> Warning in qb_grow(bit = !is.na(as.integer(input$year)), name = "valid_year", :
#> NAs durch Umwandlung erzeugt
```

The resulting strcuture is basically a record of all the things that are
grown on the bitfield, but so far nothing has happened

``` r
newBitfield
```

Finally the bitfield needs to be harvested (note: input datasets have
been stored into the environment `qb_env`). This will result in an
output table (with one column that has the name `QB`).

``` r
(QB_int <- qb_harvest(bitfield = newBitfield))
#> # A tibble: 10 × 1
#>       QB
#>    <int>
#>  1   495
#>  2   335
#>  3   287
#>  4   495
#>  5   429
#>  6   495
#>  7   495
#>  8   367
#>  9   487
#> 10   483
```

As mentioned above, the bitfield is a record of things, which is
required to decode the quality bit (similar to a key). Together with the
legend, the bit flags can then be converted back to human readable text
or used in any downstream workflow.

``` r
QB_chr <- qb_propagate(x = QB_int, bitfield = newBitfield, sep = "|")
#> # A tibble: 8 × 4
#>   name              flags pos   description                                     
#>   <chr>             <int> <chr> <chr>                                           
#> 1 not_na_x              2 1     x-coordinate values do not contain any NAs      
#> 2 range_1_x             2 2     x-coordinate values are numeric and within the …
#> 3 range_1_y             2 3     y-coordinate values are numeric and within the …
#> 4 distinct_coords       2 4     x and y coordinates are not identical, NAs are …
#> 5 flag_year             2 5     year values do have a flag, NAs are FALSE       
#> 6 valid_year            2 6     year values are valid integers                  
#> 7 match_commodities     2 7     commodity values are part of 'soybean' or 'maiz…
#> 8 cases_some_other      3 8:9   some_other values are distinguished into large …

# -> prints legend by default, but it is also available in qb_env$legend

input %>% 
  bind_cols(QB_chr) %>% 
  kable()
```

|     x |    y | year  | commodity | some_other |  QB | QB_flags                |
|------:|-----:|:------|:----------|-----------:|----:|:------------------------|
|  25.8 | 57.7 | 2021  | soybean   | -0.0026519 | 495 | 1\|1\|1\|1\|0\|1\|1\|11 |
|  23.8 | 58.4 | NA    | maize     |  0.2049487 | 335 | 1\|1\|1\|1\|0\|0\|1\|01 |
|  27.2 | 58.3 | 2021r | NA        |  0.0539777 | 287 | 1\|1\|1\|1\|1\|0\|0\|01 |
|  24.6 | 59.0 | 2021  | maize     | -1.0008374 | 495 | 1\|1\|1\|1\|0\|1\|1\|11 |
| 259.0 | 58.1 | 2021  | honey     | -0.2780210 | 429 | 1\|0\|1\|1\|0\|1\|0\|11 |
|  25.7 | 58.7 | 2021  | maize     | -2.1653212 | 495 | 1\|1\|1\|1\|0\|1\|1\|11 |
|  27.3 | 58.9 | 2021  | soybean   | -0.0587031 | 495 | 1\|1\|1\|1\|0\|1\|1\|11 |
|  27.6 | 59.4 | 2021  | maize     |  0.6484230 | 367 | 1\|1\|1\|1\|0\|1\|1\|01 |
|   0.0 |  0.0 | 2021  | soybean   | -1.7140368 | 487 | 1\|1\|1\|0\|0\|1\|1\|11 |
|  25.1 |   NA | 2021  | maize     | -0.3314015 | 483 | 1\|1\|0\|0\|0\|1\|1\|11 |

## Bitfields for other data-types

This example here shows how to compute quality bits for tabular data,
but this technique is especially helpful for raster data. To keep this
package as simple as possible, no specific methods for rasters were
developed (so far), they instead need to be converted to tabular form
and joined to the attributes or meta data that should be added to the
QB, for example like this

``` r
raster <- rast(matrix(data = 1:25, nrow = 5, ncol = 5))

input <- values(raster) %>% 
  as_tibble() %>% 
  rename(values = lyr.1) %>% 
  bind_cols(crds(raster), .)

# from here we can continue creating a bitfield and growing bits on it just like shown above...

# ... and then converting it back to a raster
# 
QB_rast <- crds(raster) %>% 
  bind_cols(QB_int) %>% 
  rast(type="xyz", crs = crs(raster), extent = ext(raster))
```

## Growing bitfields in complex workflows

Complex workflows do not only contain raster data, but a mix of tabular
and raster data, for example when occurrences and/or areal data are
modelled based on drivers that are available as rasters. In this case
the design of `queuebee` allows to use different of its’ functions in
different (distinct) parts of the workflow to build one overall QB.

# To Do

- [ ] write bitfield show method
- [ ] write qb_filter
- [ ] other pre-made quality flag functions?!
