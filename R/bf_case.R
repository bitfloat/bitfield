#' Build a bit flag by checking for cases
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains tests
#'   defined in \code{...}.
#' @param ... \cr any set of (mutually exclusive) statements that results in a
#'   logical return value
#' @param exclusive [`logical(1)`][logical]\cr whether the function should check
#'   that the cases are mutually exclusive, or whether it would allow that cases
#'   defined later in the sequence overwrite cases earlier in the sequence.
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param prov description
#' @param registry description
#' @examples
#' registry <- bf_case(x = example_data, exclusive = FALSE,
#'         yield >= 11,
#'         yield < 11 & yield > 9,
#'         yield < 9 & commodity == "maize")
#' @importFrom checkmate assertDataFrame assertLogical assertTRUE assertList
#' @importFrom rlang enquos eval_tidy as_label `:=` get_expr quo_get_expr quos
#'   parse_expr quo_set_env quo
#' @importFrom purrr map reduce
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename bind_cols filter if_else
#' @export

bf_case <- function(x, ..., exclusive = TRUE, pos = NULL, na.val = NULL,
                    prov = NULL, registry = NULL){

  assertDataFrame(x = x)
  assertLogical(x = exclusive, len = 1)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)
  assertList(x = prov, types = "character", any.missing = FALSE, null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "nameless_registry", description = "descriptionless_registry")
  }

  thisName <- paste0("cases")
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
      if(anyNA(temp[[ix]])){
        temp[[ix]][which(is.na(temp[[ix]]))] <- FALSE
      }
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

  if(any(out == 0)){
    case_expr <- c(list("none"), case_expr)
  } else {
    out <- out - min(out)
  }
  nValues <- length(case_expr)
  nBits <- as.integer(ceiling(log2(nValues)))

  # replace NA values
  if(any(is.na(out))){
    out[is.na(out)] <- na.val
  }

  # update position if it's not set
  if(is.null(pos)){
    pos <- (registry@width+1):(registry@width+nBits)
  }

  # assign tentative flags values into the current environment
  env_bind(.env = bf_env, !!thisName := out)

  # update the registry
  registry@width <- registry@width + nBits
  if(registry@length == 0L){
    registry@length <- length(out)
  } else {
    if(registry@length != length(out)){
      stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # store encoding metadata
  enc <- list(sign = 0L,
              exponent = 0L,
              significand = nBits,
              bias = 0L)

  # and store everything in the registry
  temp <- list(description = paste0("the observation has the case [", case_expr, "]."),
               position = pos,
               encoding = enc,
               provenance = prov,
               triple = paste0("{OBS}|encoded|0.0.", nBits,"/0"))

  registry@flags[[thisName]] <- temp

  return(registry)

}
