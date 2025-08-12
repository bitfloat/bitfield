#' Initiate a new registry
#'
#' @param name [`character(1)`][character]\cr the name of the bitfield.
#' @param description [`character(1)`][character]\cr the description of the
#'   bitfield.
#' @param author [`person(.)`][person]\cr the author(s) involved in the creation
#'   of this registry.
#' @param project [`list(1)`][list]\cr object created with the function
#'   \code{\link{project}} that documents the project metadata.
#' @param license [`character(1)`][character]\cr license or rights statement.
#' @return an empty registry that captures some metadata of the bitfield, but
#'   doesn't contain any flags yet.
#' @examples
#' auth <- person(given = "Jane", family = "Smith",
#'                email = "jane@example.com", role = c("cre", "aut"))
#'
#' proj <- project(title = "example project",
#'                 people = c(person("Jane", "Smith", email = "jane@example.com",
#'                                   role = "aut"),
#'                            person("Robert", "Jones", role = c("aut", "cre"))),
#'                 publisher = "example publisher",
#'                 type = "Dataset",
#'                 identifier = "10.5281/zenodo.1234567",
#'                 description = "A comprehensive explanation",
#'                 subject = c("keyword", "subject"),
#'                 license = "CC-BY-4.0")
#'
#' reg <- bf_registry(name = "currentWorkflow",
#'                    description = "the registry to my modelling pipeline",
#'                    author = auth,
#'                    project = proj)
#' @importFrom checkmate assertIntegerish assertCharacter
#' @importFrom utils packageVersion
#' @importFrom methods new
#' @export

bf_registry <- function(name, description, author = NULL, project = NULL,
                        license = "MIT"){

  assertCharacter(x = name, len = 1, null.ok = TRUE)
  assertCharacter(x = description, len = 1, null.ok = TRUE)
  assertClass(author, classes = "person", null.ok = TRUE)

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
             attribution = list(author = author,
                                project = project,
                                license = license),
             flags = list())

  return(out)

}
