#' Bitfield class (S4) and methods
#'
#' A \code{bitfield} stores ...
#'
#'
#' @slot width [`integerish(1)`][integer]\cr
#' @slot length [`integerish(1)`][integer]\cr
#' @slot name [`character(.)`][character]\cr
#' @slot desc [`data.frame(.)`][data.frame]\cr
#' @slot bits [`list()`][list]\cr

bitfield <- setClass(Class = "bitfield",
                 slots = c(width = "integer",
                           length = "integer",
                           name = "character",
                           desc = "data.frame",
                           bits = "list"
                 )
)

setValidity("bitfield", function(object){

  errors = character()

  if(!.hasSlot(object = object, name = "width")){
    errors = c(errors, "the bitfield does not have a 'width' slot.")
  } else {
    if(!is.integer(object@width)){
      errors = c(errors, "the slot 'width' is not a integer.")
    }

  }

  if(!.hasSlot(object = object, name = "length")){
    errors = c(errors, "the bitfield does not have a 'length' slot.")
  } else {
    if(!is.integer(object@length)){
      errors = c(errors, "the slot 'length' is not a integer.")
    }

  }

  if(!.hasSlot(object = object, name = "name")){
    errors = c(errors, "the bitfield does not have a 'name' slot.")
  } else {
    if(!is.character(object@name)){
      errors = c(errors, "the slot 'name' is not a character.")
    }

  }

  if(!.hasSlot(object = object, name = "desc")){
    errors = c(errors, "the bitfield does not have a 'desc' slot.")
  } else {
    if(!is.data.frame(object@desc)){
      errors = c(errors, "the slot 'desc' is not a data.frame.")
    }

  }

  if(!.hasSlot(object = object, name = "bits")){
    errors = c(errors, "the bitfield does not have a 'bits' slot.")
  } else {
    if(!is.list(object@bits)){
      errors = c(errors, "the slot 'bits' is not a list")
    }

  }

  # if(!.hasSlot(object = object, name = "type")){
  #   errors = c(errors, "the geom does not have a 'type' slot.")
  # } else {
  #   if(!any(object@type %in% c("point", "line", "polygon", "grid"))){
  #     errors = c(errors, "the geom must either be of type 'point', 'line', 'polygon' or 'grid'.")
  #   } else if(object@type == "line"){
  #     if(dim(object@point)[1] < 2){
  #       errors = c(errors, "a geom of type 'line' must have at least 2 points.")
  #     }
  #   } else if(object@type == "polygon"){
  #     if(dim(object@point)[1] < 3){
  #       errors = c(errors, "a geom of type 'polygon' must have at least 3 points.")
  #     }
  #   } else if(object@type == "grid"){
  #     if(dim(object@point)[1] != 3){
  #       errors = c(errors, "a geom of type 'grid' must have three rows ('origin' and 'cell number' extent and 'cell size').")
  #     }
  #   }
  # }
  #
  # if(!.hasSlot(object = object, name = "point")){
  #   errors = c(errors, "the geom does not have a 'name' slot.")
  # }
  #
  # if(!.hasSlot(object = object, name = "point")){
  #   errors = c(errors, "the geom does not have a 'point' slot.")
  # } else {
  #   if(!is.data.frame(object@point)){
  #     errors = c(errors, "the slot 'point' is not a data.frame.")
  #   }
  #   if(object@type == "grid"){
  #     if(!all(c("x" ,"y") %in% names(object@point))){
  #       errors = c(errors, "the geom must have a grid table with the columns 'x' and 'y'.")
  #     }
  #   } else {
  #     if(!all(c("fid", "x" ,"y") %in% names(object@point))){
  #       errors = c(errors, "the geom must have a point table with the columns 'x', 'y' and 'fid'.")
  #     }
  #   }
  # }
  #
  # if(!.hasSlot(object = object, name = "feature")){
  #   errors = c(errors, "the geom does not have a 'feature' slot.")
  # } else {
  #   if(!is.list(object@feature)){
  #     errors = c(errors, "the slot 'feature' is not a list")
  #   }
  #   if(is.null(names(object@feature))){
  #     errors = c(errors, "the slot 'feature' must contain named lists.")
  #   }
  #   if(object@type != "grid"){
  #     # for(i in seq_along(object@feature)){
  #       if(!all(c("fid", "gid") %in% names(object@feature))){
  #         errors = c(errors, "the geom must have a features table with at least the columns 'fid' and 'gid'.")
  #       }
  #     # }
  #   }
  # }
  #
  # if(!.hasSlot(object = object, name = "group")){
  #   errors = c(errors, "the geom does not have a 'group' slot.")
  # } else {
  #   if(!is.list(object@group)){
  #     errors = c(errors, "the slot 'group' is not a list.")
  #   }
  #   if(is.null(names(object@group))){
  #     errors = c(errors, "the slot 'group' must contain named lists.")
  #   }
  #   # for(i in seq_along(object@group)){
  #     # if(!any(c("value", "gid") %in% names(object@group))){
  #     #   errors = c(errors, "the geom must have a group table with the column 'value'.")
  #     # }
  #   # }
  # }
  #
  # if(!.hasSlot(object = object, name = "window")){
  #   errors = c(errors, "the geom does not have a 'window' slot.")
  # } else {
  #   if(!is.data.frame(object@window)){
  #     errors = c(errors, "the slot 'window' is not a data.frame.")
  #   }
  #   if(!all(c("x" ,"y") %in% names(object@window))){
  #     errors = c(errors, "the geom must have a window table with columns 'x' and 'y'.")
  #   }
  # }
  #
  # if(!.hasSlot(object = object, name = "crs")){
  #   errors = c(errors, "the geom does not have a 'crs' slot.")
  # } else {
  #   if(!is.character(object@crs)){
  #     errors = c(errors, "the slot 'crs' is not a character vector.")
  #   }
  # }
  #
  # if(!.hasSlot(object = object, name = "history")){
  #   errors = c(errors, "the geom does not have a 'history' slot.")
  # } else {
  #   if(!is.list(object@history)){
  #     errors = c(errors, "the slot 'history' is not a list.")
  #   }
  # }

  if(length(errors) == 0){
    return(TRUE)
  } else {
    return(errors)
  }

})

#' Print geom in the console
#'
#' @param object [`bitfield(1)`][bitfield]\cr object to \code{show}.
#' @importFrom utils head
#' @importFrom crayon yellow red cyan

setMethod(f = "show",
          signature = "bitfield",
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
