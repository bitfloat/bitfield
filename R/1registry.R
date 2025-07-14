#' Bit registry class (S4) and methods
#'
#' A \code{registry} stores metadata and flag configuration of a bitfield.
#'
#' @slot width [`integerish(1)`][integer]\cr how many bits is the bitfield wide.
#' @slot length [`integerish(1)`][integer]\cr how many observations are encoded
#'   in the bitfield.
#' @slot name [`character(1)`][character]\cr short name of the bitfield.
#' @slot version [`character(1)`][character]\cr automatically created version
#'   tag of the bitfield. This consists of the package version, the version of R
#'   and the date of creation of the bitfield.
#' @slot md5 [`character(1)`][character]\cr the MD5 checksum of the bitfield as
#'   determined with \link[tools]{md5sum}.
#' @slot description [`character(1)`][character]\cr longer description of the
#'   bitfield.
#' @slot flags [`list(.)`][list]\cr list of flags in the registry.

registry <- setClass(Class = "registry",
                     slots = c(width = "integer",
                               length = "integer",
                               name = "character",
                               version = "list",
                               md5 = "character",
                               description = "character",
                               flags = "list",
                               metadata = "list"
                     )
)

setValidity("registry", function(object){

  errors = character()

  if(!.hasSlot(object = object, name = "width")){
    errors = c(errors, "the registry does not have a 'width' slot.")
  } else {
    if(!is.integer(object@width)){
      errors = c(errors, "the slot 'width' is not a integer.")
    }

  }

  if(!.hasSlot(object = object, name = "length")){
    errors = c(errors, "the registry does not have a 'length' slot.")
  } else {
    if(!is.integer(object@length)){
      errors = c(errors, "the slot 'length' is not a integer.")
    }

  }

  if(!.hasSlot(object = object, name = "name")){
    errors = c(errors, "the registry does not have a 'name' slot.")
  } else {
    if(!is.character(object@name)){
      errors = c(errors, "the slot 'name' is not a character.")
    }

  }

  if(!.hasSlot(object = object, name = "version")){
    errors = c(errors, "the registry does not have a 'version' slot.")
  } else {
    if(!is.list(object@version)){
      errors = c(errors, "the slot 'version' is not a list.")
    }

  }

  if(!.hasSlot(object = object, name = "md5")){
    errors = c(errors, "the registry does not have a 'md5' slot.")
  } else {
    if(!is.character(object@md5)){
      errors = c(errors, "the slot 'md5' is not a character.")
    }

  }

  if(!.hasSlot(object = object, name = "description")){
    errors = c(errors, "the registry does not have a 'description' slot.")
  } else {
    if(!is.character(object@description)){
      errors = c(errors, "the slot 'description' is not a character.")
    }

  }

  if(!.hasSlot(object = object, name = "flags")){
    errors = c(errors, "the registry does not have a 'flags' slot.")
  } else {
    if(!is.list(object@flags)){
      errors = c(errors, "the slot 'flags' is not a list")
    }

  }

  if(!.hasSlot(object = object, name = "metadata")){
    errors = c(errors, "the registry does not have a 'metadata' slot.")
  } else {
    if(!is.list(object@metadata)){
      errors = c(errors, "the slot 'metadata' is not a list")
    }

  }

  if(length(errors) == 0){
    return(TRUE)
  } else {
    return(errors)
  }

})

#' Print registry in the console
#'
#' @param object [`registry(1)`][registry]\cr object to \code{show}.
#' @details This method produces an overview of the registry by printing a
#'   header with information about the setup of the bitfield and a table with
#'   one line for each flag in the bitfield. The table shows the start position
#'   of each flag, the encoding type (see \code{\link{.makeEncoding}}), the
#'   bitfield operator type and the columns that are tested by the flag.
#' @importFrom utils head
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom dplyr last
#' @importFrom stats na.omit

setMethod(f = "show",
          signature = "registry",
          definition = function(object){

            theNames <- map(names(object@flags), function(string){
              paste0(str_split(string, "_")[[1]][1], collapse = "-")
            })
            theVars <- map(names(object@flags), function(string){
              paste0(str_split(string, "_")[[1]][-1], collapse = "-")
            })

            if(length(object@flags) != 0){

              thePos <- map(object@flags, "position")
              minPos <- map(thePos, min) |>
                unlist()
              theProv <- map(object@flags, "provenance")
              theEnc <- map(map(theProv, "wasGeneratedBy"), last)
              theEnc <- map(theEnc, function(ix){
                str_split(ix[[1]], ": ")[[1]][2]
              }) |> unlist()


              colWidth <- c(max(3, max(nchar(minPos), 3)) + 1, max(nchar(theEnc), 8) + 1, max(nchar(theNames), 4) + 1, 2)

              theHeader <- paste0("  ",
                                  "pos", paste0(rep(" ", colWidth[1] - 3), collapse = ""),
                                  "encoding", paste0(rep(" ", colWidth[2] - 8), collapse = ""),
                                  "type", paste0(rep(" ", colWidth[3] - 4), collapse = ""),
                                  "col", "\n")
              barcode <- rep("-", object@width + length(object@flags) - 1)
              bars <- unlist(minPos) + (1:length(object@flags))-1
              barcode[bars[-1]-1] <- "|"
              barcode <- paste0(barcode, collapse = "")

              cat("  width", " ", object@width, "\n", sep = "")
              cat("  flags", " ", length(object@flags), "  ", barcode, "\n\n", sep = "")
              cat(theHeader)
              for(i in seq_along(object@flags)){
                theFlag <- paste0("  ",
                                  minPos[[i]], paste0(rep(" ", colWidth[1] - nchar(minPos[[i]])), collapse = ""),
                                  theEnc[[i]], paste0(rep(" ", colWidth[2] - nchar(theEnc)[i]), collapse = ""),
                                  theNames[[i]], paste0(rep(" ", colWidth[3] - nchar(theNames[[i]])), collapse = ""),
                                  as.character(theVars[[i]]), "\n")
                cat(theFlag)
              }

            } else {
              cat("  empty registry")
            }

          }
)
