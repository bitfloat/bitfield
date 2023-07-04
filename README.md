# queuebee - Handle bit flags recording quality for various inputs

## To Do

- transform `make_bits()` to a S3 generic and define methods for various data structures, such as tables (and simple features) but also rasters and ...
- adapt `make_bits()` so that it is better readable and that a back-transformation after building the bit sequence is also possible, see https://stackoverflow.com/questions/62333478/from-integer-to-bits-and-back-again
- need functionality to build the sequence actually sequentially. For instance, when going through a pipeline, where certain actions are carried out but in different scripts, an old QB from a previous script could be picked up and additional information can be added to it.
- needs functionality to have different data structures interact
  a) simple case is where attributes of a geometry need to be burned into a raster
  b) more complicated is how information in a table that are used in a model (random forest) to predict into a raster can be stored in that raster
- naming functions probably needs to be revised



# Description

Adding more thoughts about what I had in mind about this code. For MODIS, this is a so-called "bit flag" (or [bit field](https://en.wikipedia.org/wiki/Bit_field)). This is a combination of 0s and 1s that are combined into a binary sequence. It's called bit flag because each position in the sequence is coded by a single bit. R stores each integer as a (huge) 32 bit value, you can check this with `intToBits(x = 2L)` and see how many 0s are used up for that (or actually 00s here, it's just their way of representing a 0) ... But I think we can do way better, by having each single of those 0s be one "switch" that stores particular information, so in the integer `2L` we can store 32 pieces of information, we probably don't even need that many. Depending on how we arrange that information, we'll then get different integers, see the following (32 positions, either 00 or 01):

```
intToBits(x = 0L)
 [1] 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
intToBits(x = 1L)
 [1] 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
intToBits(x = 2L)
 [1] 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
intToBits(x = 3L)
 [1] 01 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
```

Using the first two of those bits, we can represent 4 different states of an attribute (00, 01, 10 and 11, or in the R-notation that would be `'00 00'`, `'00 01'`, `'01 00'` and `'01 01'`), with three bits 8 states, etc (2^n)

We could translate the bit-vector, we create, back to an integer and store those integers in a raster-tiff, or a vector for point data and have a giant amount of quality information stored in that single value, which takes up hardly any volume on the hard disc.

It's good that you bring up such information that has several values because I now change the function so that the user can provide the number of bits that shall encode for the quality of a particular attribute. So if we have something like precision and we decided that we want to represent 4 levels, we'd have to specify that we want to use two bits for that. And yeah, the idea was to encode several attributes to be able to select downstream what to weigh more!

Ok, I'll try to implement the country-border check! I am just not sure about the matching with actual landuse/cover information. Which data product should we use for that? I guess ESA LC is not trustworthy enough, right? Would it also make sense to include and propagate the quality metrics you derive for your first chapter in that data structure? Maybe we could already implement that for modelling in LUCKINet, basically, build such a QB-layer based on your metric and use it for weighing modelling by it?

The following snippet would briefly show how to handle tables

```
library(dplyr)

# make an example dataset
input <- tibble(x = sample(seq(23.3, 28.1, 0.1), 10),
                y = sample(seq(57.5, 59.6, 0.1), 10),
                year = rep(2021, 10),
                commodity = rep(c("soybean", "maize"), 5),
                landuse = sample(c("crop", "grazing", "forest"), size = 10, replace = TRUE),
                some_other = rnorm(10))

# make it have some errors
input$x[5] <- 259
input$y[9] <- NA_real_
input$year[c(2:3)] <- c(NA, "2021r")
input$commodity[c(3, 5)] <- c(NA_character_, "dog")

# build the QB
input %>%
  make_bits(coordinates = c("x", "y"),
            attributes = c("year", "commodity", "landuse", "some_other"),
            year = .bit(fun = function(x) is.na(as.integer(x)), flags = 2),
            commodity = .bit(fun = function(x) !x %in% validComm, flags = 2),
            some_other = .bit(fun = function(x) ifelse(x > 0.5, 0, ifelse(x > 0, 1, 2)),
                              flags = 3),
            sep = "_")

# this would lead to a bit field of 
# pos 1        : coords missing
# pos 2        : coords flawed (various, open for debate)
# pos 3        : year available
# pos 4        : year flawed (not an integer)
# pos 5        : commodity missing
# pos 6        : commodity flawed (not according to accepted values)
# pos 7        : landuse missing
# pos 8        : some_other missing
# pos 9 and 10 : some_other > 0.5 (00), 0.5 > some_other > 0 (10) and some_other < 0 (01) 

# A tibble: 10 × 7
       x     y year  commodity landuse some_other bits            
   <dbl> <dbl> <chr> <chr>     <chr>        <dbl> <chr>         
 1  23.7  59   2021  soybean   grazing     0.998  00_00_00_0_000
 2  23.8  59.6 NA    maize     crop        1.44   00_11_00_0_000
 3  27.6  59.4 2021r NA        forest     -1.19   00_01_11_0_001
 4  25.1  58.6 2021  maize     grazing     0.664  00_00_00_0_000
 5 259    59.1 2021  dog       crop        0.677  01_00_01_0_000
 6  26.4  58.2 2021  maize     forest      1.43   00_00_00_0_000
 7  24    58.4 2021  soybean   grazing     0.866  00_00_00_0_000
 8  27.7  59.2 2021  maize     crop        1.27   00_00_00_0_000
 9  26.9  NA   2021  soybean   grazing    -0.866  10_00_00_0_001
10  27.3  58.1 2021  maize     forest      0.0830 00_00_00_0_010
```
