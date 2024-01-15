#' Build a bit flag by checking for cases
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains tests
#'   defined in \code{...}.
#' @param ... \cr any set of (mutually exclusive) statements that results in a
#'   logical return value
#' @param exclusive [`logical(1)`][logical]\cr whether the function should check
#'   that the cases are mutually exclusive, or whether it would allow that cases
#'   defined later in the sequence overwrite cases earlier in the sequence.
#'
#' @importFrom checkmate assertDataFrame assertLogical assertTRUE
#' @importFrom rlang enquos eval_tidy as_label `:=` get_expr quo_get_expr quos
#'   parse_expr quo_set_env quo
#' @importFrom purrr map reduce
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename bind_cols filter if_else
#' @export

bf_case <- function(x, ..., exclusive = TRUE){

  assertDataFrame(x = x)
  assertLogical(x = exclusive, len = 1)

  cases <- enquos(..., .named = TRUE)

  temp <- bind_cols(map(cases, function(ix){
    blubb <- eval_tidy(expr = ix, data = x)
    blubb <- as_tibble(blubb)
    rename(blubb, !!as_label(ix) := value)
  }))


  if(exclusive){

    test <- filter(temp, rowSums(temp) != 1)
    assertTRUE(x = dim(test)[1] == 0, .var.name = "overlapping columns == 0")

  }

  status <- bind_cols(
    map(seq_along(temp), function(ix){
      temp[[ix]][temp[[ix]]] <- ix
      as_tibble(temp[ix])
    })
  )

  out <- reduce(status, function(first, second){
    if_else(second != 0, second, first)
  })

  case_expr <- map(seq_along(cases), function(ix){
    get_expr(cases[[ix]])
  })

  attr(out, which = "name") <- paste0("cases")
  attr(out, which = "desc") <- paste0("the values are split into the following cases [", paste0(paste0(seq_along(cases), ": ", case_expr), collapse = " | "), "]")

  return(out)

}
