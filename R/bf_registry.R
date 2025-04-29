#' Initiate a new registry
#'
#' @param name [`character(1)`][character]\cr the name of the bitfield.
#' @param description [`character(1)`][character]\cr the description of the
#'   bitfield.
#' @return an empty registry that captures some metadata of the bitfield, but
#'   doesn't contain any flags yet.
#' @examples
#' reg <- bf_registry(name = "currentWorkflow",
#'                    description = "this is to document my current workflow so
#'                                  that I can share it with my colleagues
#'                                  alongside a publication.")
#' @importFrom checkmate assertIntegerish assertCharacter
#' @importFrom utils packageVersion
#' @importFrom stringr str_replace_all
#' @importFrom rlang new_environment
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

bf_registry <- function(name = NULL, description = NULL){

  assertCharacter(x = name, len = 1, null.ok = TRUE)
  assertCharacter(x = description, len = 1, null.ok = TRUE)

  if(is.null(description)) description <- NA_character_

  # set a name, in case none is provided
  if(is.null(name)){
    name <- paste0("bf_", paste0(sample(c(LETTERS, letters), 12, TRUE), collapse = ""))
  }

  version <- list(bitfield = as.character(packageVersion("bitfield")), r = paste0(version$major, ".", version$minor), date = format(Sys.Date(), "%Y-%m-%d"))

  # put together the initial bitfield
  out <- new(Class = "registry",
             width = 0L,
             length = 0L,
             name = name,
             version = version,
             md5 = NA_character_,
             description = description,
             flags = list())

  return(out)

}
