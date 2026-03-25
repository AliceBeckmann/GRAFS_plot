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

test_that("validate_inputs stops for missing CSV", {
  expect_error(
    validate_inputs("/nonexistent.csv", "/fake.csv", "/fake.xml"),
    "Input CSV not found"
  )
})

test_that("validate_inputs stops for missing arrows CSV", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  expect_error(
    validate_inputs(csv, "/nonexistent_arrows.csv", "/fake.xml"),
    "Arrow IDs CSV not found"
  )
})

test_that("validate_inputs stops for missing XML template", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
  expect_error(
    validate_inputs(csv, ids, "/nonexistent.xml"),
    "XML template not found"
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

test_that("validate_inputs reports unmapped arrows", {
  # Create data with only one label, but arrow mapping has two
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_ids <- tempfile(fileext = ".csv")
  on.exit(unlink(c(tmp_csv, tmp_ids)), add = TRUE)

  writeLines(c(
    "province,year,label,data,align,arrowColor",
    "A,2000,{FLOW_A},100,L,NA"
  ), tmp_csv)

  writeLines(c(
    "label,id",
    "{FLOW_A},arrow1",
    "{FLOW_B},arrow2"
  ), tmp_ids)

  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  result <- suppressMessages(validate_inputs(tmp_csv, tmp_ids, xml))

  expect_true("{FLOW_B}" %in% result$unmapped_arrows)
  expect_false(result$ok)
})

test_that("validate_inputs reports arrows missing from template", {
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_ids <- tempfile(fileext = ".csv")
  on.exit(unlink(c(tmp_csv, tmp_ids)), add = TRUE)

  writeLines(c(
    "province,year,label,data,align,arrowColor",
    "A,2000,{FAKE_LABEL},100,L,NA"
  ), tmp_csv)

  # Arrow mapping for a label that doesn't exist in the template
  writeLines(c(
    "label,id",
    "{FAKE_LABEL},fake_id"
  ), tmp_ids)

  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  result <- suppressMessages(validate_inputs(tmp_csv, tmp_ids, xml))

  expect_true("{FAKE_LABEL}" %in% result$missing_in_template)
  expect_false(result$ok)
})

test_that("validate_inputs warns about non-numeric data", {
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_ids <- tempfile(fileext = ".csv")
  on.exit(unlink(c(tmp_csv, tmp_ids)), add = TRUE)

  writeLines(c(
    "province,year,label,data,align,arrowColor",
    "A,2000,{FLOW_A},not_a_number,L,NA",
    "A,2000,{FLOW_A},also_bad,L,NA"
  ), tmp_csv)

  writeLines(c(
    "label,id",
    "{FLOW_A},arrow1"
  ), tmp_ids)

  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")

  msgs <- character()
  withCallingHandlers(
    validate_inputs(tmp_csv, tmp_ids, xml),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  all_msgs <- paste(msgs, collapse = " ")
  expect_true(grepl("non-numeric", all_msgs))
})

test_that("validate_inputs reports many available regions on missing", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  msgs <- character()
  withCallingHandlers(
    validate_inputs(csv, ids, xml, regions = "ZZZ_NoSuchRegion"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  all_msgs <- paste(msgs, collapse = " ")
  # Should show available regions with "..." for truncation
  expect_true(grepl("Available:", all_msgs))
  expect_true(grepl("total", all_msgs))
})
