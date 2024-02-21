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

          #   theType <- object@type
          #   thePoints <- getPoints(x = object)
          #   theFeatures <- getFeatures(x = object)
          #   theGroups <- getGroups(x = object)
          #
          #   vertAttribs <- length(thePoints)
          #   featureAttribs <- names(theFeatures)[!names(theFeatures) %in% c("fid")]
          #   groupAttribs <- names(theGroups)[!names(theGroups) %in% c("gid")]
          #
          #   myAttributes <- NULL
          #   points <- feats <- groups <- FALSE
          #
          #   if(is.na(object@crs)){
          #     myCrs <- "cartesian"
          #   } else {
          #     myCrs <- object@crs
          #   }
          #
          #   if(theType == "grid"){
          #     theFeats <- featureAttribs
          #     theLayer <- theGroups
          #     if(!is.null(theLayer)){
          #       if(!all(names(thePoints) %in% c("gid"))){
          #         myAttributes <- c(myAttributes, paste0(" ", ifelse(length(groupAttribs) == 0,
          #                                                            paste0("--\n"),
          #                                                            ifelse(length(groupAttribs) <= 9,
          #                                                                   paste0(paste0(groupAttribs, collapse = ", "), "\n"),
          #                                                                   paste0(paste0(c(head(groupAttribs, 9), "..."), collapse = ", "), "\n"))
          #         )))
          #       }
          #     }
          #
          #     if(length(unique(groupAttribs)) == 1){
          #       myFeat <- "layer"
          #     } else {
          #       myFeat <- "layers"
          #     }
          #     myUnits <- "cells"
          #     geomGroups <- ""
          #
          #   } else {
          #     theGrps <- theGroups$gid
          #     if(length(unique(theGrps)) == 1){
          #       myGrp <- "group"
          #     } else {
          #       myGrp <- "groups"
          #     }
          #     theFeats <- theFeatures$fid
          #     featureAttribs <- featureAttribs[-which(featureAttribs == "gid")]
          #     if(length(unique(theFeats)) == 1){
          #       myFeat <- "feature"
          #     } else {
          #       myFeat <- "features"
          #     }
          #     myUnits <- "points"
          #     geomGroups <- paste0(length(unique(theGrps)), " ", myGrp, " | ")
          #
          #     if(!all(names(thePoints) %in% c("x", "y", "fid"))){
          #       myAttributes <- c(myAttributes, paste0(" (points) ",
          #                                              ifelse(vertAttribs <= 9,
          #                                                     paste0(paste0(names(thePoints)[!names(thePoints) %in% c("x", "y", "fid")], collapse = ", "), "\n"),
          #                                                     paste0(paste0(c(head(names(thePoints)[!names(thePoints) %in% c("x", "y", "fid")], 9), "..."), collapse = ", "), "\n")
          #                                              )))
          #       points <- TRUE
          #     }
          #     if(!all(names(theFeatures) %in% c("fid", "gid"))){
          #       if(points){
          #         featureString <- "           (features) "
          #       } else {
          #         featureString <- " (features) "
          #       }
          #       myAttributes <- c(myAttributes, paste0(featureString,
          #                                              ifelse(length(featureAttribs) <= 9,
          #                                                     paste0(paste0(featureAttribs, collapse = ", "), "\n"),
          #                                                     paste0(paste0(c(head(featureAttribs, 9), "..."), collapse = ", "), "\n")
          #                                              )))
          #       feats <- TRUE
          #     }
          #     if(!all(names(theGroups) %in% c("gid"))){
          #       if(feats | points){
          #         groupString <- "            (groups) "
          #       } else {
          #         groupString <- " (groups) "
          #       }
          #       myAttributes <- c(myAttributes, paste0(groupString,
          #                                              ifelse(length(groupAttribs) <= 9,
          #                                                     paste0(paste0(names(theGroups)[!names(theGroups) %in% c("gid")], collapse = ", "), "\n"),
          #                                                     paste0(paste0(c(head(names(theGroups)[!names(theGroups) %in% c("gid")], 9), "..."), collapse = ", "), "\n")
          #                                              )))
          #     }
          #   }
          #   if(is.null(myAttributes)){
          #     myAttributes <- " --\n"
          #   }
          #
          #
          #   cat(yellow(class(object)), "        ", object@type, "\n", sep = "")
          #   cat("            ", geomGroups, length(unique(theFeats)), " ", myFeat, " | ", length(thePoints$fid), " ", myUnits, "\n", sep = "")
          #   cat(yellow("crs         "), myCrs, "\n", sep = "")
          #   cat(yellow("attributes "), myAttributes, sep = "")
          #   if(!theType == "grid"){
          #     # make a tiny map
          #     tinyMap <- .makeTinyMap(geom = object)
          #     cat(yellow("tiny map  "), tinyMap)
          #   } else {
          #     theRes <- getRes(object)
          #     theExt <- getExtent(object)
          #     cat(yellow("resolution "), as.numeric(theRes), "(x, y)\n")
          #     cat(yellow("extent     "), c(theExt$x, theExt$y), "(xmin, xmax, ymin, ymax)")
          #   }


          }
)
