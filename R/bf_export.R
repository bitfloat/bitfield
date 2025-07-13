#' Export bitfield registries
#'
#' @param registry description
#' @param format description
#' @param file description
#' @return description
#' @examples
#' bf_export(reg, format = "json", file = "yield_QA.json")
#' bf_export(reg, format = "xml", file = "yield_QA.xml")
#' bf_export(reg, format = "yaml", file = "yield_QA.yaml")
#' @importFrom checkmate assertClass assertChoice assertCharacter
#' @export

bf_export <- function(registry, format = NULL, file = NULL){

  assertClass(x = registry, classes = "registry")
  assertChoice(x = format, choices = c("rds", "json", "xml", "datacite"))
  assertCharacter(x = file, len = 1, any.missing = FALSE)

  # continue here

}





# bf_export <- function(registry,
#                       format = c("json", "rds", "xml", "yaml"),
#                       file = NULL) {
# Export Registry to File
#
# @param registry Registry object to export
# @param format Format to export ("rds", "json", "xml", "yaml")
# @param file Optional file path to write the output
#
# @return Exported data (invisibly if file is specified)
# @export
#
#   format <- match.arg(format)
#
#   # Handle different export formats
#   result <- switch(format,
#                    "rds" = registry,
#                    "json" = {
#                      # Create DataCite-compliant JSON structure
#                      output <- create_datacite_structure(registry)
#                      jsonlite::toJSON(output, auto_unbox = TRUE, pretty = TRUE)
#                    },
#                    "xml" = {
#                      # Convert DataCite JSON to XML format
#                      output <- create_datacite_structure(registry)
#
#                      # Basic XML conversion - in real implementation you might
#                      # want to use an XML package to create proper DataCite XML
#                      xml_lines <- c(
#                        '<?xml version="1.0" encoding="UTF-8"?>',
#                        '<resource xmlns="http://datacite.org/schema/kernel-4" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
#                        paste0('  <identifier identifierType="DOI">', ifelse(is.na(output$identifiers[[1]]$identifier), "TBD", output$identifiers[[1]]$identifier), '</identifier>'),
#                        '  <creators>'
#                      )
#
#                      # Add creators
#                      for (creator in output$creators) {
#                        xml_lines <- c(xml_lines,
#                                       '    <creator>',
#                                       paste0('      <creatorName>', creator$familyName, ', ', creator$givenName, '</creatorName>'),
#                                       paste0('      <givenName>', creator$givenName, '</givenName>'),
#                                       paste0('      <familyName>', creator$familyName, '</familyName>')
#                        )
#
#                        # Add ORCID if available
#                        if (!is.null(creator$nameIdentifiers)) {
#                          xml_lines <- c(xml_lines,
#                                         paste0('      <nameIdentifier nameIdentifierScheme="ORCID" schemeURI="https://orcid.org/">',
#                                                creator$nameIdentifiers[[1]]$nameIdentifier, '</nameIdentifier>')
#                          )
#                        }
#
#                        # Add affiliation
#                        if (!is.null(creator$affiliations)) {
#                          xml_lines <- c(xml_lines,
#                                         paste0('      <affiliation>', creator$affiliations[[1]]$name, '</affiliation>')
#                          )
#                        }
#
#                        xml_lines <- c(xml_lines, '    </creator>')
#                      }
#
#                      # Add remaining elements
#                      xml_lines <- c(xml_lines,
#                                     '  </creators>',
#                                     paste0('  <title>', output$titles[[1]]$title, '</title>'),
#                                     paste0('  <publisher>', output$publisher, '</publisher>'),
#                                     paste0('  <publicationYear>', output$publicationYear, '</publicationYear>'),
#                                     '  <resourceType resourceTypeGeneral="Software">Bitfield Registry</resourceType>'
#                      )
#
#                      # Add description
#                      if (!is.null(output$descriptions)) {
#                        xml_lines <- c(xml_lines,
#                                       '  <descriptions>',
#                                       paste0('    <description descriptionType="Abstract">', output$descriptions[[1]]$description, '</description>'),
#                                       '  </descriptions>'
#                        )
#                      }
#
#                      # Add rights/license
#                      if (!is.null(output$rightsList)) {
#                        xml_lines <- c(xml_lines, '  <rightsList>')
#                        for (right in output$rightsList) {
#                          if (!is.null(right$rightsURI)) {
#                            xml_lines <- c(xml_lines,
#                                           paste0('    <rights rightsURI="', right$rightsURI, '">', right$rights, '</rights>')
#                            )
#                          } else {
#                            xml_lines <- c(xml_lines,
#                                           paste0('    <rights>', right$rights, '</rights>')
#                            )
#                          }
#                        }
#                        xml_lines <- c(xml_lines, '  </rightsList>')
#                      }
#
#                      # Close root element
#                      xml_lines <- c(xml_lines, '</resource>')
#
#                      # Return as single string
#                      paste(xml_lines, collapse = "\n")
#                    },
#                    "yaml" = {
#                      # Create DataCite-compliant structure and convert to YAML
#                      output <- create_datacite_structure(registry)
#                      yaml::as.yaml(output)
#                    }
#   )
#
#   # Write to file if specified
#   if (!is.null(file)) {
#     if (format == "rds") {
#       saveRDS(result, file)
#     } else {
#       writeLines(result, file)
#     }
#     return(invisible(result))
#   }
#
#   return(result)
# }

# create_datacite_structure <- function(registry) {
#
# Create DataCite-compliant metadata structure
#
# @param registry Registry object
# @return List with DataCite-compliant structure
# @keywords internal
#   # Build DataCite metadata structure
#   output <- list(
#     schemaVersion = "4.5",
#     identifiers = list(
#       list(
#         identifierType = "DOI",
#         identifier = NA  # Can be filled in later when DOI is minted
#       )
#     ),
#     titles = list(list(title = registry@name)),
#     descriptions = list(list(
#       description = registry@description,
#       descriptionType = "Abstract"
#     ))
#   )
#
#   # Add creators from person objects
#   if (!is.null(registry@metadata$creators)) {
#     output$creators <- lapply(registry@metadata$creators, function(p) {
#       # Extract name components
#       creator <- list(
#         givenName = paste(p$given, collapse = " "),
#         familyName = paste(p$family, collapse = " "),
#         nameType = "Personal"
#       )
#
#       # Extract ORCID if available
#       if (!is.null(p$comment)) {
#         # Look for ORCID in comments
#         if ("ORCID" %in% names(p$comment)) {
#           creator$nameIdentifiers <- list(
#             list(
#               nameIdentifier = p$comment["ORCID"],
#               nameIdentifierScheme = "ORCID",
#               schemeURI = "https://orcid.org/"
#             )
#           )
#         }
#
#         # Look for affiliation in comments
#         if ("affiliation" %in% names(p$comment)) {
#           creator$affiliations <- list(
#             list(name = p$comment["affiliation"])
#           )
#
#           # Add ROR ID if available
#           if ("ROR" %in% names(p$comment)) {
#             creator$affiliations[[1]]$affiliationIdentifier <- p$comment["ROR"]
#             creator$affiliations[[1]]$affiliationIdentifierScheme <- "ROR"
#             creator$affiliations[[1]]$schemeURI <- "https://ror.org/"
#           }
#         }
#       }
#
#       return(creator)
#     })
#   }
#
#   # Add publisher and publication year
#   # Try to infer publisher from creators' affiliation if not explicitly set
#   if (!is.null(registry@metadata$creators) &&
#       !is.null(registry@metadata$creators[[1]]$comment) &&
#       "affiliation" %in% names(registry@metadata$creators[[1]]$comment)) {
#     output$publisher <- registry@metadata$creators[[1]]$comment["affiliation"]
#   } else {
#     # Default publisher
#     output$publisher <- "Unknown"
#   }
#
#   # Publication year (current year)
#   output$publicationYear <- format(Sys.Date(), "%Y")
#
#   # Add project information if available
#   if (!is.null(registry@metadata$project)) {
#     proj <- registry@metadata$project
#
#     # Add subjects if available
#     if (!is.null(proj$subjects)) {
#       output$subjects <- lapply(proj$subjects, function(s) list(subject = s))
#     }
#
#     # Add related identifiers if available
#     if (!is.null(proj$related)) {
#       output$relatedIdentifiers <- proj$related
#     }
#
#     # Add funding information if available
#     if (!is.null(proj$funding)) {
#       output$fundingReferences <- list(
#         list(
#           funderName = proj$funding
#         )
#       )
#     }
#   }
#
#   # Add license information
#   if (!is.null(registry@metadata$license)) {
#     output$rightsList <- list(list(rights = registry@metadata$license))
#
#     # Add license URL if known license
#     known_licenses <- list(
#       "GPL-3" = "https://www.gnu.org/licenses/gpl-3.0.en.html",
#       "GPL-2" = "https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html",
#       "MIT" = "https://opensource.org/licenses/MIT",
#       "CC-BY-4.0" = "https://creativecommons.org/licenses/by/4.0/",
#       "CC0-1.0" = "https://creativecommons.org/publicdomain/zero/1.0/"
#     )
#
#     if (registry@metadata$license %in% names(known_licenses)) {
#       output$rightsList[[1]]$rightsURI <- known_licenses[[registry@metadata$license]]
#     }
#   }
#
#   # Add resource type
#   output$resourceType <- list(
#     resourceTypeGeneral = "Software",
#     resourceType = "Bitfield Registry"
#   )
#
#   # Add version information
#   if (!is.null(registry@version)) {
#     output$version <- registry@version$bitfield
#   }
#
#   return(output)
# }
