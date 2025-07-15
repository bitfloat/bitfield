#' Export bitfield registries
#'
#' Export bitfield registries in DataCite-compliant formats for archiving,
#' sharing, and integration with metadata repositories.
#'
#' @param registry [`registry(1)`][registry]\cr Registry object to export.
#' @param format [`character(1)`][character]\cr Export format. One of "json",
#'   "xml", "yaml", or "rds".
#' @param file [`character(1)`][character]\cr Optional file path to write the
#'   output. If NULL, returns the formatted data.
#' @return Exported data as character string for formatted outputs, or the
#'   registry object for "rds" format. If `file` is specified, returns
#'   invisibly and writes to file.
#' @examples
#' \dontrun{
#' # Create registry with metadata
#' auth <- person("Jane", "Smith", email = "jane@example.com",
#'                comment = c(ORCID = "0000-0000-0000-0000"))
#' reg <- bf_registry(name = "analysis",
#'                    description = "Data quality assessment",
#'                    author = auth)
#'
#' # Export to different formats
#' bf_export(registry = reg, format = "json", file = "metadata.json")
#' bf_export(registry = reg, format = "xml", file = "metadata.xml")
#' yaml_output <- bf_export(registry = reg, format = "yaml")
#' }
#' @importFrom checkmate assertClass assertChoice assertCharacter
#' @importFrom jsonlite toJSON
#' @importFrom yaml as.yaml
#' @importFrom purrr map map_chr
#' @importFrom stringr str_detect
#' @export

bf_export <- function(registry, format, file = NULL) {

  # input validation
  assertClass(x = registry, classes = "registry")
  assertChoice(x = format, choices = c("json", "xml", "yaml", "rds"))
  assertCharacter(x = file, len = 1, any.missing = FALSE, null.ok = TRUE)

  # create DataCite-compliant structure
  output <- .makeDatacite(registry)

  # build output string
  if(format == "json"){
    result <- toJSON(output, auto_unbox = TRUE, pretty = TRUE)
  } else if(format == "yaml"){
    result <- as.yaml(output)
  } else {
    result <- {
      # Create DataCite XML
      xml_lines <- c(
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<resource xmlns="http://datacite.org/schema/kernel-4" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'
      )

      # Add identifier
      xml_lines <- c(xml_lines,
                     sprintf('  <identifier identifierType="%s">%s</identifier>',
                             output$identifiers[[1]]$identifierType,
                             ifelse(is.na(output$identifiers[[1]]$identifier),
                                    "TBD", output$identifiers[[1]]$identifier))
      )

      # Add creators
      if (!is.null(output$creators)) {
        xml_lines <- c(xml_lines, '  <creators>')
        for (creator in output$creators) {
          xml_lines <- c(xml_lines,
                         '    <creator>',
                         sprintf('      <creatorName>%s, %s</creatorName>',
                                 creator$familyName, creator$givenName),
                         sprintf('      <givenName>%s</givenName>', creator$givenName),
                         sprintf('      <familyName>%s</familyName>', creator$familyName)
          )

          # Add ORCID if available
          if (!is.null(creator$nameIdentifiers)) {
            xml_lines <- c(xml_lines,
                           sprintf('      <nameIdentifier nameIdentifierScheme="ORCID" schemeURI="https://orcid.org/">%s</nameIdentifier>',
                                   creator$nameIdentifiers[[1]]$nameIdentifier)
            )
          }

          # Add affiliation
          if (!is.null(creator$affiliations)) {
            xml_lines <- c(xml_lines,
                           sprintf('      <affiliation>%s</affiliation>',
                                   creator$affiliations[[1]]$name)
            )
          }

          xml_lines <- c(xml_lines, '    </creator>')
        }
        xml_lines <- c(xml_lines, '  </creators>')
      }

      # Add remaining required elements
      xml_lines <- c(xml_lines,
                     sprintf('  <title>%s</title>', output$titles[[1]]$title),
                     sprintf('  <publisher>%s</publisher>', output$publisher),
                     sprintf('  <publicationYear>%s</publicationYear>', output$publicationYear),
                     sprintf('  <resourceType resourceTypeGeneral="%s">%s</resourceType>',
                             output$resourceType$resourceTypeGeneral,
                             output$resourceType$resourceType),
                     '</resource>'
      )

      paste(xml_lines, collapse = "\n")
    }
  }

  # write to file if specified
  if (!is.null(file)) {
    if(format == "rds"){
      saveRDS(registry, file = file)
    } else {
      writeLines(result, con = file)
    }
    invisible(result)
  } else {
    return(result)
  }

}
