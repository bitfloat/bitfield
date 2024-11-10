#' Create a new registry
#'
#' @param name [`character(1)`][character]\cr the name of the bitfield.
#' @param description [`character(1)`][character]\cr the description of the
#'   bitfield.
#' @details Besides creating a registry object, this function also opens the
#' environment \code{bf_env}, which is used to store the temporary non-encoded
#' flag values.
#' @return an empty registry that captures some metadata of the bitfield, but
#'   doesn't contain any flags yet.
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

  bf_version <- str_replace_all(string = as.character(packageVersion("bitfield")), pattern = "[.]", replacement = "")
  r_version <- str_replace_all(string = paste0(version$major, version$minor), pattern = "[.]", replacement = "")
  version <- paste0(paste0(c(bf_version, r_version, format(Sys.Date(), "%Y%m%d")), collapse = "."))

  # open a new environment
  .GlobalEnv[["bf_env"]] <- new_environment()

  # put together the intial bitfield
  out <- new(Class = "registry",
             width = 0L,
             length = 0L,
             name = name,
             version = version,
             md5 = NA_character_,
             description = description,
             flags = list())

  # store the environment name in the options
  oldOptions <- options()
  on.exit(options(oldOptions))

  options(bf_env = name)

  return(out)

}
