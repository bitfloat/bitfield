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
#' @param description description
#' @param registry description
#' @examples
#' registry <- bf_case(x = tbl_bityield, exclusive = FALSE,
#'                     yield >= 11,
#'                     yield < 11 & yield > 9,
#'                     yield < 9 & commodity == "maize")
#' @importFrom checkmate assertDataFrame assertLogical assertTRUE assertList
#' @importFrom rlang enquos eval_tidy as_label `:=` get_expr quo_get_expr quos
#'   parse_expr quo_set_env quo
#' @importFrom purrr map reduce
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename bind_cols filter if_else
#' @export

bf_case <- function(x, ..., exclusive = TRUE, pos = NULL, na.val = NULL,
                    description = NULL, registry = NULL){

  assertDataFrame(x = x)
  assertLogical(x = exclusive, len = 1)
  assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)

  if(is.null(registry)){
    registry <- bf_registry(name = "new_registry")
  }

  thisName <- paste0("cases")
  cases <- enquos(..., .named = TRUE)
  # return(cases)

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
    if(is.null(na.val)) stop("there are NA values in the bit representation, please define 'na.val'.")
    out[is.na(out)] <- na.val
    naProv <- paste0("substituteValue: NA->", na.val)
  } else {
    naProv <- NULL
  }

  # update position if it's not set
  if(is.null(pos)){
    pos <- (registry@width+1):(registry@width+nBits)
  } else {
    # include test that checks whether sufficient positions are set, and give an error if not
  }

  # update the registry
  registry@width <- registry@width + nBits
  if(registry@length == 0L){
    registry@length <- length(out)
  } else {
    if(registry@length != length(out)){
      stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
    }
  }

  # update flag metadata ...
  if(is.null(description)){
    for(i in seq_along(case_expr)){
      description <- c(description, paste0("the observation has case ", i," [", case_expr[i], "]."))
    }
  }

  enc <- list(sign = 0L,
              exponent = 0L,
              mantissa = nBits,
              bias = 0L)

  prov <- list(wasDerivedFrom = "{OBS}",
               wasGeneratedBy = c(naProv, paste0("encodingAsBinary: 0.0.", nBits, "/0")))

  # ... and store everything in the registry
  temp <- list(description = description,
               position = pos,
               encoding = enc,
               provenance = prov)

  registry@flags[[thisName]] <- temp

  # assign tentative flags values into the current environment
  env_bind(.env = bf_env, !!thisName := out)

  return(registry)

}
