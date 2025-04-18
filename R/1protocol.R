#' Bitflag class (S4) and methods
#'
#' A \code{protocol} stores test, documentation and example data to establish a
#' formal method to build a bit flag.
#'
#' @slot meta description
#' @slot test description
#' @slot specs description
#' @slot extends description

protocol <- setClass(Class = "protocol",
                    slots = c(meta = "list",
                              test = "function",
                              specs = "list",
                              extends = "list"
                    )
)

setValidity("protocol", function(object){

  errors = character()

  make sure to also test the elements in the slots

  if(!.hasSlot(object = object, name = "meta")){
    errors = c(errors, "the protocol does not have a 'meta' slot.")
  } else {
    if(!is.list(object@meta)){
      errors = c(errors, "the slot 'meta' is not a list.")
    }

  }

  if(!.hasSlot(object = object, name = "test")){
    errors = c(errors, "the protocol does not have a 'test' slot.")
  } else {
    if(!is.function(object@test)){
      errors = c(errors, "the slot 'test' is not a function.")
    }

  }

  if(!.hasSlot(object = object, name = "specs")){
    errors = c(errors, "the protocol does not have a 'specs' slot.")
  } else {
    if(!is.list(object@specs)){
      errors = c(errors, "the slot 'specs' is not a list.")
    }

  }

  if(!.hasSlot(object = object, name = "extends")){
    errors = c(errors, "the protocol does not have a 'extends' slot.")
  } else {
    if(!is.list(object@extends)){
      errors = c(errors, "the slot 'extends' is not a list.")
    }

  }

  if(length(errors) == 0){
    return(TRUE)
  } else {
    return(errors)
  }

})

#' Print protocol in the console
#'
#' @param object [`protocol(1)`][protocol]\cr object to \code{show}.

setMethod(f = "show",
          signature = "protocol",
          definition = function(object){

            make this (a lot) nicer

            cat("Bitfield Operator:", object@meta$name, "\n")
            cat("Description:", object@meta$description, "\n")
            cat("Version:", object@meta$version, "\n")
            cat("Encoding Type:", object@specs$encoding, "\n")
            cat("Bits:", object@specs$bits, "\n")

            if (!is.null(object@extends$bitflag)) {
              cat("Extends:", object@extends$bitflag, "\n")
            }

            if (!is.null(object@specs$requires)) {
              cat("Requires:", paste(object@specs$requires, collapse = ", "), "\n")
            }

          }
)
