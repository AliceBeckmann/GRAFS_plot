test_that("create_GRAFS stops with clear error for missing drawio", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  expect_error(
    create_GRAFS(
      csv_inputs = csv,
      path_outputs = tempdir(),
      xml_base = xml,
      arrows_csv = ids,
      drawio_exe = "/nonexistent/drawio",
      regions = "Albacete",
      periods = list(list(years = 1930:1931))
    ),
    "draw.io executable not found"
  )
})

test_that("create_GRAFS produces XML output when drawio is available", {
  drawio <- find_drawio()
  skip_if(is.null(drawio), "draw.io not installed")

  csv <- system.file("extdata", "GRAFS_spain_data.csv", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
  out <- file.path(tempdir(), paste0("grafs_integration_", Sys.getpid()))
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  create_GRAFS(
    csv_inputs = csv,
    path_outputs = out,
    xml_base = xml,
    arrows_csv = ids,
    drawio_exe = drawio,
    regions = "Albacete",
    periods = list(list(years = 1930:1931))
  )

  xml_files <- list.files(file.path(out, "xml"), pattern = "\\.xml$")
  png_files <- list.files(file.path(out, "png"), pattern = "\\.png$")
  expect_length(xml_files, 1)
  expect_length(png_files, 1)
  expect_match(xml_files[1], "GRAFS_Albacete_P1_MEAN_1930-1931\\.xml")
})

test_that("create_GRAFS respects overwrite=FALSE", {
  drawio <- find_drawio()
  skip_if(is.null(drawio), "draw.io not installed")

  csv <- system.file("extdata", "GRAFS_spain_data.csv", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
  out <- file.path(tempdir(), paste0("grafs_overwrite_", Sys.getpid()))
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  # First run
  create_GRAFS(
    csv_inputs = csv, path_outputs = out, xml_base = xml,
    arrows_csv = ids, drawio_exe = drawio,
    regions = "Albacete",
    periods = list(list(years = 1930:1931))
  )
  xml_file <- list.files(file.path(out, "xml"), full.names = TRUE)[1]
  mtime1 <- file.mtime(xml_file)

  Sys.sleep(1)

  # Second run with overwrite=FALSE should skip
  create_GRAFS(
    csv_inputs = csv, path_outputs = out, xml_base = xml,
    arrows_csv = ids, drawio_exe = drawio,
    regions = "Albacete",
    periods = list(list(years = 1930:1931)),
    overwrite = FALSE
  )
  mtime2 <- file.mtime(xml_file)
  expect_equal(mtime1, mtime2)
})
