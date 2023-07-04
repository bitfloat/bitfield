#' Test coordinates for plausibility
#'
#' @param input [`data.frame`][data.frame]\cr tidy table that contains
#'   coordinate values in the columns 'x' and 'y'.
#' @importFrom checkmate assertNames
#' @export

testCoords <- function(input){

  assertNames(x = names(input), must.include = c("x", "y"))

  out <- input %>%
    mutate(xAvail = if_else(is.na(x) | x %in% c(""), 1, 0),
           yAvail = if_else(is.na(y) | y %in% c(""), 1, 0),
           avail = if_else(xAvail == 1 | yAvail == 1, 1, 0),
           xRange = if_else(avail == 0, if_else(x < -180 | x > 180, 1, 0), 0),
           yRange = if_else(avail == 0, if_else(y < -90 | y > 90, 1, 0), 0),
           range = if_else(xRange == 1 | yRange == 1, 1, 0),
           overall = if_else(avail == 1 | range == 1, 1, 0))

  return(out)

}
