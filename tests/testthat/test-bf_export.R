# Tests for bf_export.R

test_that("bf_export returns JSON string", {

  reg <- bf_registry(name = "export_json",
                     description = "Test JSON export",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  result <- bf_export(registry = reg, format = "json")
  expect_type(result, "character")
  # should be valid JSON (parseable)
  parsed <- jsonlite::fromJSON(result)
  expect_type(parsed, "list")
  expect_true("titles" %in% names(parsed))

})

test_that("bf_export returns YAML string", {

  reg <- bf_registry(name = "export_yaml",
                     description = "Test YAML export",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  result <- bf_export(registry = reg, format = "yaml")
  expect_type(result, "character")
  expect_true(grepl("schemaVersion", result))

})

test_that("bf_export returns XML string", {

  reg <- bf_registry(name = "export_xml",
                     description = "Test XML export",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  result <- bf_export(registry = reg, format = "xml")
  expect_type(result, "character")
  expect_true(grepl('<?xml version', result, fixed = TRUE))
  expect_true(grepl('datacite', result))
  expect_true(grepl('</resource>', result))

})

test_that("bf_export writes JSON to file", {

  reg <- bf_registry(name = "file_test",
                     description = "Test file writing",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  tmpfile <- tempfile(fileext = ".json")
  on.exit(unlink(tmpfile))

  bf_export(registry = reg, format = "json", file = tmpfile)
  expect_true(file.exists(tmpfile))

  content <- readLines(tmpfile)
  expect_true(length(content) > 0)

})

test_that("bf_export writes RDS to file", {

  reg <- bf_registry(name = "rds_test",
                     description = "Test RDS export",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  tmpfile <- tempfile(fileext = ".rds")
  on.exit(unlink(tmpfile))

  bf_export(registry = reg, format = "rds", file = tmpfile)
  expect_true(file.exists(tmpfile))

  loaded <- readRDS(tmpfile)
  expect_s4_class(loaded, "registry")
  expect_equal(loaded@name, "rds_test")

})

test_that("bf_export validates inputs", {

  reg <- bf_registry(name = "val_test",
                     description = "Test validation",
                     template = bf_tbl)

  # invalid format
  expect_error(bf_export(registry = reg, format = "csv"))

  # invalid registry
  expect_error(bf_export(registry = "not_a_registry", format = "json"))

})

test_that("bf_export includes DataCite required elements", {

  auth <- person("Jane", "Smith", email = "jane@example.com", role = "aut",
                 comment = c(ORCID = "0000-0001-2345-6789",
                             affiliation = "University of Example"))

  reg <- bf_registry(name = "datacite_test",
                     description = "Test DataCite compliance",
                     template = bf_tbl,
                     author = auth)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  result <- jsonlite::fromJSON(bf_export(registry = reg, format = "json"))

  expect_true("identifiers" %in% names(result))
  expect_true("titles" %in% names(result))
  expect_true("resourceType" %in% names(result))
  expect_true("publicationYear" %in% names(result))
  expect_true("creators" %in% names(result))

})

test_that("bf_export includes technical description for populated registry", {

  reg <- bf_registry(name = "tech_test",
                     description = "Test tech details",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)
  reg <- bf_map(protocol = "range", data = bf_tbl, registry = reg,
                x = yield, min = 5, max = 11)

  result <- jsonlite::fromJSON(bf_export(registry = reg, format = "json"))

  # should have both Abstract and TechnicalInfo descriptions
  desc_types <- result$descriptions$descriptionType
  expect_true("Abstract" %in% desc_types)
  expect_true("TechnicalInfo" %in% desc_types)

})

test_that("bf_export writes YAML to file", {

  reg <- bf_registry(name = "yaml_file_test",
                     description = "Test YAML file writing",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  tmpfile <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmpfile))

  bf_export(registry = reg, format = "yaml", file = tmpfile)
  expect_true(file.exists(tmpfile))

  content <- readLines(tmpfile)
  expect_true(length(content) > 0)
  expect_true(any(grepl("schemaVersion", content)))

})

test_that("bf_export writes XML to file", {

  reg <- bf_registry(name = "xml_file_test",
                     description = "Test XML file writing",
                     template = bf_tbl)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  tmpfile <- tempfile(fileext = ".xml")
  on.exit(unlink(tmpfile))

  bf_export(registry = reg, format = "xml", file = tmpfile)
  expect_true(file.exists(tmpfile))

  content <- readLines(tmpfile)
  expect_true(any(grepl("<?xml", content, fixed = TRUE)))

})

test_that("bf_export XML includes ORCID and affiliation when provided", {

  auth <- person("Jane", "Smith", role = "aut",
                 comment = c(ORCID = "0000-0001-2345-6789",
                             affiliation = "University of Example"))

  reg <- bf_registry(name = "xml_orcid_test",
                     description = "Test XML ORCID",
                     template = bf_tbl,
                     author = auth)
  reg <- bf_map(protocol = "na", data = bf_tbl, registry = reg, x = y)

  result <- bf_export(registry = reg, format = "xml")

  expect_true(grepl("ORCID", result))
  expect_true(grepl("0000-0001-2345-6789", result))
  expect_true(grepl("University of Example", result))

})

test_that("bf_export handles empty registry (no flags)", {

  reg <- bf_registry(name = "empty_test",
                     description = "Empty registry",
                     template = bf_tbl)

  # should still export without error
  result <- bf_export(registry = reg, format = "json")
  expect_type(result, "character")
  parsed <- jsonlite::fromJSON(result)
  expect_true("titles" %in% names(parsed))

})
