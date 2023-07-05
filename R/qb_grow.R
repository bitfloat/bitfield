#' Add bits to a bitfield
#'
#' @param bit a function that returns
#' @param desc the description of the bit(s).
#' @param na which value NAs in the output of .bit should have in the bitfield.
#' @param pos the position in the bitfield that should be set.
#' @param bitfield the bitfield in which the bits should be set.
#' @details
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # make an example dataset
#' input <- tibble(x = sample(seq(23.3, 28.1, 0.1), 10),
#'                 y = sample(seq(57.5, 59.6, 0.1), 10),
#'                 year = rep(2021, 10),
#'                 commodity = rep(c("soybean", "maize"), 5),
#'                 landuse = sample(c("crop", "grazing", "forest"), size = 10, replace = TRUE),
#'                 some_other = rnorm(10))
#'
#' # make it have some errors
#' input$x[5] <- 259
#' input$x[9] <- 0
#' input$y[10] <- NA_real_
#' input$y[9] <- 0
#' input$year[c(2:3)] <- c(NA, "2021r")
#' input$commodity[c(3, 5)] <- c(NA_character_, "dog")
#'
#' # derive valid values for commodities
#' validComm <- c("soybean", "maize")
#'
#' # build a new bitfield
#' newBitfield <- qb_create()
#'
#' # specify the quality checks
#' newBitfield %>%
#'   # explicit tests for coordinates ...
#'   qb_grow(bit = qb_na(x = input, test = "x"),
#'           desc = c("x-coordinate values do not contain any NAs"),
#'           pos = 1, bitfield = .) %>%
#'   qb_grow(bit = qb_range(x = input, test = "x", min = -180, max = 180),
#'           desc = c("x-coordinate values are numeric and within the valid WGS84 range"),
#'           pos = 2, bitfield = .) %>%
#'   # ... or override NA test
#'   qb_grow(bit = qb_range(x = input, test = "y", min = -90, max = 90),
#'           desc = c("y-coordinate values are numeric and within the valid WGS84 range, NAs are FALSE"),
#'           pos = 3, na = FALSE, bitfield = .) %>%
#'   # it is also possible to use other functions that give flags, such as from CoordinateCleaner ...
#'   qb_grow(bit = cc_equ(x = input, lon = "x", lat = "y", value = "flagged"),
#'           desc = c("x and y coordinates are not identical"),
#'           pos = 4, bitfield = .) %>%
#'   qb_grow(bit = qb_na(x = input, test = "year"),
#'           desc = c("year values do not contain any NAs"),
#'           pos = 5, bitfield = .) %>%
#'   # ... or stringr
#'   qb_grow(bit = str_detect(input$year, "r"),
#'           desc = c("year values do have a flag"),
#'           pos = 6, bitfield = .)  %>%
#'   # test for matches with an external vector
#'   qb_grow(bit = qb_match(x = input, test = "commodity", against = validComm),
#'           desc = c("commodity values are part of 'soybean' or 'maize'"),
#'           na = FALSE, pos = 7, bitfield = .) %>%
#'   # define cases
#'   qb_grow(bit = qb_case(x = input, some_other > 0.5, some_other > 0, some_other < 0),
#'           desc = c("some_other values are distinguished into large, medium and small"),
#'           pos = 8:9, bitfield = .)
#'
#' @importFrom checkmate assertChoice assertIntegerish
#' @export


qb_grow <- function(bit, desc, na = FALSE, pos, bitfield){

  assertChoice(x = na, choices = unique(bit))
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE)


  # 'intToBase2' <- function(x){
  #   x %>%
  #     intToBits() %>%
  #     rev %>%
  #     as.character() %>%
  #     {sapply(strsplit(., "", fixed = TRUE), '[', 2)} %>%
  #     paste0(.,collapse = '')
  # }
  #
  # base2ToInt <- function(string){
  #   sapply(strsplit(string, ""), function(x) sum(rev(+(x == "1")) * 2^(seq(x)-1)))
  # }


  # number of flags
  flags <- ceiling(sqrt(...)) # of unique values in the bit

  return()

}
