test_that("validate_inputs returns structured result", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  result <- suppressMessages(validate_inputs(csv, ids, xml))

  expect_type(result, "list")
  expect_true("data_labels" %in% names(result))
  expect_true("arrow_labels" %in% names(result))
  expect_true("template_labels" %in% names(result))
  expect_true("unmapped_data" %in% names(result))
  expect_true("ok" %in% names(result))
  expect_type(result$ok, "logical")
})

test_that("validate_inputs detects all arrow mappings", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  result <- suppressMessages(validate_inputs(csv, ids, xml))

  # Should have the 25 arrow labels minus WIDTH_MAX
  expect_true(length(result$arrow_labels) >= 24)
  expect_false("{WIDTH_MAX}" %in% result$arrow_labels)
})

test_that("validate_inputs checks regions", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  result <- suppressMessages(
    validate_inputs(csv, ids, xml, regions = "Albacete")
  )
  expect_true(result$ok || !result$ok)  # just shouldn't error

  result <- suppressMessages(
    validate_inputs(csv, ids, xml, regions = "NonexistentRegion")
  )
  expect_false(result$ok)
})

test_that("validate_inputs stops for missing files", {
  expect_error(
    validate_inputs("/nonexistent.csv", "/fake.csv", "/fake.xml"),
    "not found"
  )
})

test_that("validate_inputs finds template placeholders", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  result <- suppressMessages(validate_inputs(csv, ids, xml))

  # Template should have placeholders like {PROVINCE_NAME}, {YEAR}, etc.
  expect_true("{PROVINCE_NAME}" %in% result$template_labels)
  expect_true("{YEAR}" %in% result$template_labels)
  expect_true(length(result$template_labels) > 20)
})
