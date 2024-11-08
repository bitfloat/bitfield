#' Bit registry class (S4) and methods
#'
#' A \code{registry} stores ...
#'
#'
#' @slot width [`integerish(1)`][integer]\cr
#' @slot length [`integerish(1)`][integer]\cr
#' @slot name [`character(1)`][character]\cr
#' @slot version [`character(1)`][character]\cr
#' @slot md5 [`character(1)`][character]\cr
#' @slot description [`character(1)`][character]\cr
#' @slot flags [`list(.)`][list]\cr

registry <- setClass(Class = "registry",
                     slots = c(width = "integer",
                               length = "integer",
                               name = "character",
                               version = "character",
                               md5 = "character",
                               description = "character",
                               flags = "list"
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
    if(!is.character(object@version)){
      errors = c(errors, "the slot 'version' is not a character.")
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

  if(length(errors) == 0){
    return(TRUE)
  } else {
    return(errors)
  }

})

#' Print registry in the console (not working yet)
#'
#' @param object [`registry(1)`][registry]\cr object to \code{show}.
#' @importFrom utils head
#' @importFrom crayon yellow red cyan
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom dplyr last
#' @importFrom stats na.omit

setMethod(f = "show",
          signature = "registry",
          definition = function(object){

            theNames <- map(names(object@flags), str_split, "_") |>
              unlist(recursive = FALSE) |>
              map(`[`, 1)
            # theVars <- map(names(object@flags), str_split, "_") |>
            #   unlist(recursive = FALSE) |>
            #   map(`[`, 2)

            if(length(object@flags) != 0){

              thePos <- map(object@flags, "position")
              minPos <- map(thePos, min) |>
                unlist()
              theProv <- map(object@flags, "provenance")
              theVars <- map(theProv, "wasDerivedFrom")
              theEnc <- map(map(theProv, "wasGeneratedBy"), last)
              theEnc <- map(theEnc, function(ix){
                str_split(ix[[1]], ": ")[[1]][2]
              }) |> unlist()


              colWidth <- c(max(3, max(nchar(minPos))) + 2, max(nchar(theEnc)) + 2, max(nchar(theNames)) + 1, 2)

              theHeader <- paste0("  ",
                                  "pos", paste0(rep(" ", colWidth[1] - 3 + 1), collapse = ""),
                                  "encoding", paste0(rep(" ", colWidth[2] - 8), collapse = ""),
                                  "type", paste0(rep(" ", colWidth[3] - 4 + 1), collapse = ""),
                                  "col", "\n")
              barcode <- rep("-", object@width + length(object@flags) - 1)
              bars <- unlist(minPos) + (1:length(object@flags))-1
              barcode[bars[-1]-1] <- "|"
              barcode <- paste0(barcode, collapse = "")

              cat(yellow("  width"), " ", object@width, "\n", sep = "")
              cat(yellow("  flags"), " ", length(object@flags), "  ", barcode, "\n\n", sep = "")
              cat(yellow(theHeader))
              for(i in seq_along(object@flags)){
                theFlag <- paste0("  ",
                                  minPos[[i]], paste0(rep(" ", colWidth[1] - nchar(minPos[[i]]) + 1), collapse = ""),
                                  theEnc[[i]], paste0(rep(" ", colWidth[2] - nchar(theEnc)[i]), collapse = ""),
                                  theNames[[i]], paste0(rep(" ", colWidth[3] - nchar(theNames[[i]]) + 1), collapse = ""),
                                  na.omit(theVars[[i]]), "\n")
                cat(theFlag)
              }

            } else {
              cat(yellow("  empty registry"))
            }

          }
)
