
<!-- README.md is generated from README.Rmd. Please edit that file -->

# queuebee <a href=''><img src='' align="right" height="200" /></a>

<!-- badges: start -->
<!-- badges: end -->

## Overview

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
library(dplyr, warn.conflicts = FALSE)
library(queuebee)
library(CoordinateCleaner)
#> The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, will retire in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> The sp package is now running under evolution status 2
#>      (status 2 uses the sf package in place of rgdal)
#> Please note that rgdal will be retired during October 2023,
#> plan transition to sf/stars/terra functions using GDAL and PROJ
#> at your earliest convenience.
#> See https://r-spatial.org/r/2023/05/15/evolution4.html and https://github.com/r-spatial/evolution
#> rgdal: version: 1.6-7, (SVN revision 1203)
#> Geospatial Data Abstraction Library extensions to R successfully loaded
#> Loaded GDAL runtime: GDAL 3.4.1, released 2021/12/27
#> Path to GDAL shared files: /usr/share/gdal
#> GDAL binary built with GEOS: TRUE 
#> Loaded PROJ runtime: Rel. 8.2.1, January 1st, 2022, [PJ_VERSION: 821]
#> Path to PROJ shared files: /home/se87kuhe/.local/share/proj:/usr/share/proj
#> PROJ CDN enabled: TRUE
#> Linking to sp version:1.6-1
#> To mute warnings of possible GDAL/OSR exportToProj4() degradation,
#> use options("rgdal_show_exportToProj4_warnings"="none") before loading sp or rgdal.
#> rgeos version: 0.6-4, (SVN revision 699)
#>  GEOS runtime version: 3.10.2-CAPI-1.16.0 
#>  Please note that rgeos will be retired during October 2023,
#> plan transition to sf or terra functions using GEOS at your earliest convenience.
#> See https://r-spatial.org/r/2023/05/15/evolution4.html for details.
#>  GEOS using OverlayNG
#>  Linking to sp version: 2.0-0 
#>  Polygon checking: TRUE
library(stringr)
```

Let’s first build an example dataset

``` r
input <- tibble(x = sample(seq(23.3, 28.1, 0.1), 10),
                y = sample(seq(57.5, 59.6, 0.1), 10),
                year = rep(2021, 10),
                commodity = rep(c("soybean", "maize"), 5),
                landuse = sample(c("crop", "grazing", "forest"), size = 10, replace = TRUE),
                some_other = rnorm(10))

# make it have some errors
input$x[5] <- 259
input$x[9] <- 0
input$y[10] <- NA_real_
input$y[9] <- 0
input$year[c(2:3)] <- c(NA, "2021r")
input$commodity[c(3, 5)] <- c(NA_character_, "dog")

# derive valid values for commodities
validComm <- c("soybean", "maize")
```

The first step in yielding quality bits is in creating a bitfield.

1.  The `width =` specifies how many bits are in the bitfield.
2.  The `lenght =` specifies how long the output table is. Any input
    must then have the same length. In case there are inputs with a
    different length, make sure in your pre-processing steps that all
    inputs are joined/merged that empty values or NAs are explicit.
3.  The `name =` specifies the label of the bitfield, which becomes very
    important when publishing, because bitfield and output table are
    stored in different files and it must be possible to unambiguously
    associate them to one another.

``` r
newBitfield <- qb_create(width = 10, length = dim(input)[1])
```

Then, individual bits need to be grown by specifying from which input
and based on which mapping function a particular position of the
bitfield should be modified. When providing a concise yet expressive
description, you will also in the future still understand what you did
here.

``` r
newBitfield <- newBitfield %>%
  # explicit tests for coordinates ...
  qb_grow(bit = qb_na(x = input, test = "x"), name = "is_na_x",
          desc = c("x-coordinate values do not contain any NAs"),
          pos = 1, bitfield = .) %>%
  qb_grow(bit =  qb_range(x = input, test = "x", min = -180, max = 180), name = "range_1_x",
          desc = c("x-coordinate values are numeric and within the valid WGS84 range"),
          pos = 2, bitfield = .) %>%
  # ... or override NA test
  qb_grow(bit = qb_range(x = input, test = "y", min = -90, max = 90), name = "range_1_y",
          desc = c("y-coordinate values are numeric and within the valid WGS84 range, NAs are FALSE"),
          pos = 3, na_val = FALSE, bitfield = .) %>%
  # it is also possible to use other functions that give flags, such as from CoordinateCleaner ...
  qb_grow(bit = cc_equ(x = input, lon = "x", lat = "y", value = "flagged"), name = "equal_coords",
          desc = c("x and y coordinates are not identical"),
          pos = 4, na_val = FALSE, bitfield = .) %>%
  # ... or stringr ...
  qb_grow(bit = str_detect(input$year, "r"), name = "flag_year",
          desc = c("year values do have a flag"),
          pos = 5, na_val = FALSE, bitfield = .) %>%
  # ... or even base R
  qb_grow(bit = is.na(as.integer(input$year)), name = "is_na_year",
          desc = c("year values are valid integers"),
          pos = 6, bitfield = .)  %>%
  # test for matches with an external vector
  qb_grow(bit = qb_match(x = input, test = "commodity", against = validComm), name = "match_commodities",
          desc = c("commodity values are part of 'soybean' or 'maize'"),
          pos = 7, na_val = FALSE, bitfield = .) %>%
  # define cases
  qb_grow(bit = qb_case(x = input, some_other > 0.5, some_other > 0, some_other < 0, exclusive = FALSE), name = "cases_some_other",
          desc = c("some_other values are distinguished into large, medium and small"),
          pos = 8:9, bitfield = .)
```

This strcuture is basically a record of all the things that are grown on
a bitfield, but so far nothing has happened

``` r
newBitfield
```

To make things happen eventually, the bitfield and a(ny) input it shall
be applied to are combined. This will result in an output table (with
one column that has the name `QB`) with the same length as the longest
column in the inputs. When bit flags are grown from inputs with
different length, a join/merge column needs to be provided.

``` r
qb_combine(bitfield = newBitfield)
```

Anybody that wants to either extend the bitfield or analyse the output
data and base their judgement on the quality bit will want to extract
that bit. The bitfield is thereby the key that is required to decode the
quality bit

``` r
qb_extract(x = output, bitfield = newBitfield)
```

## Some background - what is a bitfield, really?

Adding more thoughts about what I had in mind about this code. For
MODIS, this is a so-called “bit flag” (or [bit
field](https://en.wikipedia.org/wiki/Bit_field)). This is a combination
of 0s and 1s that are combined into a binary sequence. It’s called bit
flag because each position in the sequence is coded by a single bit. R
stores each integer as a (huge) 32 bit value, you can check this with
`intToBits(x = 2L)` and see how many 0s are used up for that (or
actually 00s here, it’s just their way of representing a 0) … But I
think we can do way better, by having each single of those 0s be one
“switch” that stores particular information, so in the integer `2L` we
can store 32 pieces of information, we probably don’t even need that
many. Depending on how we arrange that information, we’ll then get
different integers, see the following (32 positions, either 00 or 01):

    intToBits(x = 0L)
     [1] 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    intToBits(x = 1L)
     [1] 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    intToBits(x = 2L)
     [1] 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    intToBits(x = 3L)
     [1] 01 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00

Using the first two of those bits, we can represent 4 different states
of an attribute (00, 01, 10 and 11, or in the R-notation that would be
`'00 00'`, `'00 01'`, `'01 00'` and `'01 01'`), with three bits 8
states, etc (2^n)

We could translate the bit-vector, we create, back to an integer and
store those integers in a raster-tiff, or a vector for point data and
have a giant amount of quality information stored in that single value,
which takes up hardly any volume on the hard disc.

It’s good that you bring up such information that has several values
because I now change the function so that the user can provide the
number of bits that shall encode for the quality of a particular
attribute. So if we have something like precision and we decided that we
want to represent 4 levels, we’d have to specify that we want to use two
bits for that. And yeah, the idea was to encode several attributes to be
able to select downstream what to weigh more!

Ok, I’ll try to implement the country-border check! I am just not sure
about the matching with actual landuse/cover information. Which data
product should we use for that? I guess ESA LC is not trustworthy
enough, right? Would it also make sense to include and propagate the
quality metrics you derive for your first chapter in that data
structure? Maybe we could already implement that for modelling in
LUCKINet, basically, build such a QB-layer based on your metric and use
it for weighing modelling by it?

# To Do

- need functionality to build the sequence actually sequentially. For
  instance, when going through a pipeline, where certain actions are
  carried out but in different scripts, an old QB from a previous script
  could be picked up and additional information can be added to it. -\>
  This should be possible with this set-up, but needs to be described
  explicitly
- needs functionality to have different data structures interact -\> I
  solve this now by transforming rasters to a table that can then be
  dealt with easily without any generics/methods
- [ ] write qb_combine
- [ ] write qb_extract
- [ ] write bitfield show method
- [ ] write qb_case
- [ ] write qb_filter
- [ ] other pre-made quality flag functions?!
- [ ] write and/or improve documentation
