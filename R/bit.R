#' Derive bit representation
#'
#' @param fun any function that returns either logical values or a sequence of
#'   values starting at 0.
#' @param flags an integer value of the number of flags, can be higher than the
#'   number of unique return values of fun, in case a specific instance of
#'   \code{fun} doesn't cover the whole global standard.
#' @importFrom rlang enquo
#' @importFrom tibble tibble
#' @importFrom dplyr rowwise mutate
#' @export

.bit <- function(fun, flags = NULL){

  temp <- enquo(fun)

  nBits <- ceiling(log2(flags))
  bitFlags <- tibble(flag = 0:(flags-1)) %>%
    rowwise() %>%
    mutate(bits = paste0(as.integer(intToBits(flag))[0:nBits], collapse = ""))

  out <- list(fun = temp, flags = bitFlags)

  return(out)

}
