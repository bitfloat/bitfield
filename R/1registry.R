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

setMethod(f = "show",
          signature = "registry",
          definition = function(object){



          }
)
