#' Create a new bitfield
#'
#' @param width [`integerish(1)`][integer]\cr the table that contains
#'   \code{test}.
#' @param length [`integerish(1)`][integer]\cr the column in \code{x} for which
#'   a range is checked.
#' @param name [`character(1)`][character]\cr the minimum allowed value.
#'
#' @importFrom checkmate assertDataFrame assertSubset assertNumeric
#' @importFrom rlang new_environment
#' @importFrom tibble tibble
#' @export

qb_create <- function(width, length, name = NULL){

  assertIntegerish(x = width, len = 1, lower = 2, any.missing = FALSE, )
  assertIntegerish(x = length, len = 1, any.missing = FALSE)
  assertCharacter(x = name, len = 1, null.ok = TRUE)

  # set a name, in case none is provided
  if(is.null(name)){
    name <- paste0("qb_", paste0(sample(c(LETTERS, letters), 7, TRUE), collapse = ""), format(Sys.Date(), "%Y%m%d"))
  }

  # open a new environment
  .GlobalEnv[["qb_env"]] <- new_environment()

  # put together the intial bitfield
  out <- new(Class = "bitfield",
             width = as.integer(width),
             length = as.integer(length),
             name = name,
             desc = tibble(pos = character(), description = character()),
             bits = list())

  # store the environment name in the options
  oldOptions <- options()
  on.exit(options(oldOptions))

  options(qb_env = name)

  return(out)

}
