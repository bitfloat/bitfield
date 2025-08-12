#' Example table
#'
#' A 9 × 5 tibble with a range of example data to showcase functionality of
#' this package.
#' @format object of class \code{tibble} has two columns that indicate
#'   coordinates, one column that indicates a crop that is grown there, one
#'   column that indicates the yield of that crop there and one column that
#'   indicates the year of harvest. All columns contain some sort of deviation
#'   that may occur in data.
"bf_tbl"

#' Internal bit-flag protocols
#'
#' @format a list containing bit-flag protocols for the internal tests. Each
#'   protocol is a list itself with the fields \code{"name"}, \code{"version"},
#'   \code{"extends"}, \code{"extends_note"}, \code{"description"},
#'   \code{"encoding_type"}, \code{"bits"}, \code{"requires"}, \code{"test"},
#'   \code{"data"} and \code{"reference"}. For information on how they were set
#'   up and how you can set up additional protocols, go to
#'   \code{\link{bf_protocol}}.
"bf_pcl"
