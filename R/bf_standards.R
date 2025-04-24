#' Handle community standard protocols
#'
#' This function allows the user to list, pull or push bit-flag protocols to the
#' \href{https://github.com/bitfloat/standards}{bitfloat/standards} repository
#' on github
#' @param protocol [`character(1)`][character]\cr name of the bit-flag protocol
#'   to handle. This is either used to filter the list retrieved from
#'   \code{remote}, the name of the protocol to pull from github, or the name of
#'   the new protocol that should be pushed to github.
#' @param remote [`character(1)`][character]\cr the path in the repo, where the
#'   protocol is stored or shall be stored. For instance, to store a protocol in
#'   \code{https://github.com/bitfloat/standards/distributions/type/distType.yml},
#'   this should be \code{"distributions/type"}.
#' @param action [`character(1)`][character]\cr whether to \code{push} or
#'   \code{pull} a protocol, or \code{list} the \code{remote} contents.
#' @param version [`character(1)`][character]\cr version tag for the protocol,
#'   must have a semantic versioning pattern, i.e., \code{MAJOR.MINOR.PATCH}.
#' @param change [`character(1)`][character]\cr in case you try to push an
#'   updated version of a protocol, you must provide a brief description of what
#'   has changed from the current version to this version.
#' @param token [`character(1)`][character]\cr your github personal access token
#'   (PAT).
#' @details Create a Personal Access Token in your github developer settings (or
#'   by running \code{usethis::create_github_token()}) and store it with
#'   \code{gitcreds::gitcreds_set()}. The token must have the scope 'repo' so
#'   you can authenticate yourself to pull or push community standards, and will
#'   only be accessible to your personal R session.
#' @return description
#' @examples
#' # list all currently available standards
#' bf_standards()
#' @importFrom checkmate assertCharacter assertChoice assertList
#' @importFrom methods functionBody
#' @importFrom httr content
#' @importFrom gh gh
#' @importFrom purrr map map_lgl
#' @importFrom base64enc base64decode base64encode
#' @importFrom yaml yaml.load as.yaml
#' @export

bf_standards <- function(protocol = NULL, remote = NULL, action = "list",
                         version = "latest", change = NULL, token = NULL){

  # assert arguments are valid ----
  assertCharacter(x = protocol, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = remote, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertChoice(x = action, choices = c("push", "pull", "list"))
  assertCharacter(x = version, len = 1, any.missing = FALSE, pattern = "latest|^\\d+\\.\\d+\\.\\d+$")
  assertCharacter(x = change, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = token, len = 1, any.missing = FALSE, null.ok = TRUE)
  if(is.null(protocol) & action != "list") stop("when no protocol is given, I can only list the 'remote' contents.")

  # get protocol ----
  if(!is.null(protocol)){
    if(protocol %in% names(bf_pcl)){
      pcl <- bf_pcl[[protocol]] # load from internal list of protocols
    } else if(exists(protocol)){
      pcl <- get(protocol)
      assertList(x = pcl)
    }
  } else {
    pcl <- NULL
  }

  if(is.null(remote)) remote <- ""

  # user info ----
  token <- .validateToken(token = token)
  userInfo <- tryCatch(
    gh("GET /user", .token = token)$login,
    error = function(e) "unknown-user"
  )

  # list all protocols ----
  remoteContent <- gh("GET /repos/bitfloat/standards/git/trees/main?recursive=1",
                      .token = token)

  # check if directory exists ----
  dirExists <- map_lgl(remoteContent$tree, function(x) {
    grepl(x = x$path, pattern = remote)
  })
  withArchive <- map_lgl(remoteContent$tree, function(x){
    grepl(x = x$path, pattern = paste0(remote, "/archive"))
  })

  protocolNames <- map(remoteContent$tree, function(x) {
    if (x$type == "blob" && grepl("\\.yml$", x$path)) {
      x$path
    }
  })
  protocolNames <- protocolNames[!map_lgl(protocolNames, is.null)]

  # if we want to see the content on remote ----
  if (action == "list") {
    return(protocolNames)
  }

  #  pull protocol from the repository ----
  protocolContent <- tryCatch(
    gh(paste0("GET /repos/bitfloat/standards/contents/", remote, "/", protocol, ".yml"),
       .token = token),
    error = function(e) NULL)

  if(!is.null(protocolContent)){

    # decode to yaml and turn into list ----
    pcl_yaml <- rawToChar(base64decode(protocolContent$content))
    remotePcl <- yaml.load(pcl_yaml)
    remotePcl$test <- eval(parse(text = remotePcl$test))
    remotePcl$data <- eval(parse(text = remotePcl$data))

    # ensure the protocol is valid ----
    remotePcl <- .validateProtocol(remotePcl)

  } else if(action == "pull") {
    stop(paste0("Failed to pull '", remote, "/", protocol, ".yml'"))
  }

  # if we want to pull that protocol ----
  if(action == "pull"){
    return(pcl)
  }

  # if we want to push a (new or updated) file ----
  if(action == "push"){

    assertList(x = pcl)

    # reformat for yaml content
    funArgs <- paste0(formalArgs(pcl$test), collapse = ", ")
    funBody <- paste(deparse(functionBody(pcl$test)), collapse = "\n")
    if (!grepl("\\{", funBody) && !grepl("\\}", funBody)) {
      funBody <- paste0("{\n  ", funBody, "\n}")
    }
    fullFun <- paste0("function(", funArgs, ")\n", funBody)
    exampleStr <- map_chr(seq_along(pcl$data), function(ix){
      paste0(names(pcl$data)[ix], " = c(", paste0(pcl$data[[ix]], collapse = ", "), ")")
    })
    fullExample <- paste0("list(", paste0(exampleStr, collapse = ",\n       "), ")")

    if(is.null(protocolContent)){

      # if no previous version exists, push an initial version
      version <- "1.0.0"
      histVersion <- c(list(list(version = version,
                                 date = format(Sys.Date(), "%Y-%m-%d"),
                                 change = "initial release")))

    } else {

      if(version == "latest" & action == "push") stop("please be explicit about the version you want to push ([version = 'latest'] is not possible).")

      # make sure that the new version is different in version and function body
      assertCharacter(x = change, len = 1, any.missing = FALSE)

      if(!version > remotePcl$version) stop("the current version (", remotePcl$version,") is the same as the version you try to set.")
      if(identical(deparse(remotePcl$test), deparse(eval(parse(text = fullFun))))){
        warning("the current version has the same function specification as the version you try to push.", immediate. = TRUE)
      }

      # reconstruct version history
      histVersion <- c(list(list(version = version,
                                 date = format(Sys.Date(), "%Y-%m-%d"),
                                 change = change)),
                       remotePcl$version_history)

    }

    pcl$version <- version
    pcl <- append(pcl, list(version_history = histVersion), after = 2)
    pcl$bits <- as.integer(pcl$bits)
    pcl$test <- fullFun
    pcl$data <- fullExample
    pcl$reference$url <- paste0("https://github.com/bitfloat/standards/blob/main/", remote, "/", protocol, ".yml")

    pcl_yaml <- as.yaml(pcl)

    cat(pcl_yaml)
    readline(prompt = paste0("please carefully check the above yaml file and confirm to push it to '", remote,"' with any key or abort with [ESC]."))

    itemEncoded <- base64encode(charToRaw(pcl_yaml))

    # create directory for latest versions with README.md
    if (!any(dirExists)) {

      readmeContent <- base64encode(charToRaw(
        "# Bit-flag protocols\n\nThis directory contains the latest versions of bit-flag protocols and a optionally a folder 'archive' with all previous versions of the protocols listed here."
      ))
      readmePath <- paste0(remote, "/README.md")

      tryCatch(
        gh(paste0("PUT /repos/bitfloat/standards/contents/", readmePath),
           .token = token,
           message = paste0("New directory '", remote, "'"),
           content = readmeContent),
        error = function(e) {
          stop(paste0("Failed to create protocol directory: ", e$message))
        }
      )
    }

    # create directory for archived versions with README.md
    if(!any(withArchive)){
      readmeContent <- base64encode(charToRaw(
        "# Bit-flag protocols\n\nThis directory contains the archived previous versions of bit-flag protocols."
      ))
      readmePath <- paste0(remote, "/archive/README.md")

      tryCatch(
        gh(paste0("PUT /repos/bitfloat/standards/contents/", readmePath),
           .token = token,
           message = paste0("New directory '", remote, "/archive'"),
           content = readmeContent),
        error = function(e) {
          stop(paste0("Failed to create protocol directory: ", e$message))
        }
      )
    }

    # push the archived version ----
    if(!is.null(protocolContent)){

      archiveMessage <- sprintf("[%s] Archive bif-flag protocol %s, version %s",
                                userInfo,
                                paste0("'", protocol, "'"),
                                remotePcl$version)
      tryCatch(
        gh(paste0("PUT /repos/bitfloat/standards/contents/", remote, "/archive/", protocol, "_", remotePcl$version, ".yml"),
           .token = token,
           message = archiveMessage,
           content = protocolContent$content),
        error = function(e) {
          stop(paste0("Failed to push protocol: ", e$message))
        }
      )

    }

    # push the new version ----
    commitMessage <- sprintf("[%s] %s bit-flag protocol %s, version %s",
                            userInfo,
                            if(!is.null(protocolContent)) "Update" else "Add",
                            paste0("'", protocol, "'"),
                            pcl$version)
    tryCatch(
      gh(paste0("PUT /repos/bitfloat/standards/contents/", remote, "/", protocol, ".yml"),
         .token = token,
         message = commitMessage,
         content = itemEncoded,
         sha = protocolContent$sha),
      error = function(e) {
        stop(paste0("Failed to push protocol: ", e$message))
      }
    )

    message(paste0("Successfully pushed protocol '", protocol, "' version '", version, "' to the repository"))
    invisible(TRUE)

  }
}
