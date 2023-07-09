#' Build a bit by checking
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains
#'   \code{test}.
#' @param ... \cr any set of (mutually exclusive) statements that results in a
#'   logical return value
#' @param exclusive [`logical(1)`][logical]\cr whether the function should check
#'   that the cases are mutually exclusive, or whether it would allow that cases
#'   defined later in the sequence overwrite cases earlier in the sequence.
#'
#' @importFrom checkmate assertDataFrame assertLogical assertTRUE
#' @importFrom rlang enquos eval_tidy as_label `:=`
#' @importFrom purrr map reduce
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename bind_cols filter if_else
#' @export

qb_case <- function(x, ..., exclusive = TRUE){

  assertDataFrame(x = x)
  assertLogical(x = exclusive, len = 1)

  cases <- enquos(...)
  # return(cases)

  temp <- map(cases, function(ix){
    eval_tidy(expr = ix, data = x) %>%
      as_tibble() %>%
      rename(!!as_label(ix) := value)
  }) %>%
    bind_cols()

  if(exclusive){

    test <- temp %>%
      filter(rowSums(temp) != 1)
    assertTRUE(x = dim(test)[1] == 0, .var.name = "overlapping columns == 0")

  }

  status <- map(seq_along(temp), function(ix){
    temp[[ix]][temp[[ix]]] <- ix
    temp[ix] %>%
      as_tibble()
  }) %>%
    bind_cols()

  out <- reduce(status, function(first, second){
    if_else(second != 0, second, first)
  })

  return(out)

}
