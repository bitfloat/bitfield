#' Bit registry class (S4) and methods
#'
#' A \code{registry} stores metadata and flag configuration of a bitfield.
#'
#' @slot name [`character(1)`][character]\cr short name of the bitfield.
#' @slot version [`character(1)`][character]\cr automatically created version
#'   tag of the bitfield. This consists of the package version, the version of R
#'   and the date of creation of the bitfield.
#' @slot md5 [`character(1)`][character]\cr the MD5 checksum of the bitfield as
#'   determined with \link[tools]{md5sum}.
#' @slot description [`character(1)`][character]\cr longer description of the
#'   bitfield.
#' @slot template [`list(.)`][list]\cr structural metadata for encoding/decoding,
#'   including: \code{type} ("data.frame" or "SpatRaster"), \code{width} (total
#'   bits), \code{length} (number of observations/cells), and for rasters:
#'   \code{nrows}, \code{ncols}, \code{extent}, \code{crs}.
#' @slot flags [`list(.)`][list]\cr list of flags in the registry.

registry <- setClass(Class = "registry",
                     slots = c(name = "character",
                               version = "list",
                               md5 = "character",
                               description = "character",
                               template = "list",
                               attribution = "list",
                               flags = "list"
                     )
)

setValidity("registry", function(object){

  errors = character()

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

  if(!.hasSlot(object = object, name = "template")){
    errors = c(errors, "the registry does not have a 'template' slot.")
  } else {
    if(!is.list(object@template)){
      errors = c(errors, "the slot 'template' is not a list.")
    }

  }

  if(!.hasSlot(object = object, name = "flags")){
    errors = c(errors, "the registry does not have a 'flags' slot.")
  } else {
    if(!is.list(object@flags)){
      errors = c(errors, "the slot 'flags' is not a list")
    }

  }

  if(!.hasSlot(object = object, name = "attribution")){
    errors = c(errors, "the registry does not have a 'attribution' slot.")
  } else {
    if(!is.list(object@attribution)){
      errors = c(errors, "the slot 'attribution' is not a list")
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

              # need to adapt position from 'assignPosition'
              gen <- map(object@flags, "wasGeneratedBy")

              thePos <- map(gen, "assignPosition")
              minPos <- map(thePos, min) |>
                unlist()
              theEnc <- map(gen, "encodeAsBinary")
              theEnc <- map(theEnc, function(ix){
                paste0(paste0(ix[1:3], collapse = "."), "/", ix[4])
              }) |> unlist()


              colWidth <- c(max(3, max(nchar(minPos), 3)) + 1, max(nchar(theEnc), 8) + 1, max(nchar(theNames), 4) + 1, 2)

              theHeader <- paste0("  ",
                                  "pos", paste0(rep(" ", colWidth[1] - 3), collapse = ""),
                                  "encoding", paste0(rep(" ", colWidth[2] - 8), collapse = ""),
                                  "name", paste0(rep(" ", colWidth[3] - 4), collapse = ""),
                                  "col", "\n")
              theWidth <- object@template$width
              barcode <- rep("-", theWidth + length(object@flags) - 1)
              bars <- unlist(minPos) + (1:length(object@flags))-1
              barcode[bars[-1]-1] <- "|"
              barcode <- paste0(barcode, collapse = "")

              cat("  type", "  ", object@template$type, "\n", sep = "")
              cat("  width", " ", theWidth, "\n", sep = "")
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
