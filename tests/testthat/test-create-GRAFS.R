# --- Input validation ---

test_that("create_GRAFS stops for missing CSV", {
  expect_error(
    create_GRAFS(
      csv_inputs = "/nonexistent.csv",
      path_outputs = tempdir(),
      xml_base = system.file("templates", "grafs_auto_v18.xml", package = "GRAFS"),
      arrows_csv = system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS"),
      drawio_exe = "/usr/bin/drawio",
      regions = "spain",
      periods = list(list(years = 1990:1991))
    ),
    "Input CSV file not found"
  )
})

test_that("create_GRAFS stops for missing XML template", {
  expect_error(
    create_GRAFS(
      csv_inputs = system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS"),
      path_outputs = tempdir(),
      xml_base = "/nonexistent.xml",
      arrows_csv = system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS"),
      drawio_exe = "/usr/bin/drawio",
      regions = "spain",
      periods = list(list(years = 1990:1991))
    ),
    "XML template not found"
  )
})

test_that("create_GRAFS stops for missing arrows CSV", {
  expect_error(
    create_GRAFS(
      csv_inputs = system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS"),
      path_outputs = tempdir(),
      xml_base = system.file("templates", "grafs_auto_v18.xml", package = "GRAFS"),
      arrows_csv = "/nonexistent.csv",
      drawio_exe = "/usr/bin/drawio",
      regions = "spain",
      periods = list(list(years = 1990:1991))
    ),
    "Arrow IDs CSV not found"
  )
})

test_that("create_GRAFS stops for missing drawio", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  expect_error(
    create_GRAFS(
      csv_inputs = csv,
      path_outputs = tempdir(),
      xml_base = xml,
      arrows_csv = ids,
      drawio_exe = "/nonexistent/drawio",
      regions = "spain",
      periods = list(list(years = 1990:1991))
    ),
    "draw.io executable not found"
  )
})

test_that("create_GRAFS stops for empty regions", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  expect_error(
    create_GRAFS(
      csv_inputs = csv, path_outputs = tempdir(), xml_base = xml,
      arrows_csv = ids, drawio_exe = "/nonexistent/drawio",
      regions = character(0),
      periods = list(list(years = 1990:1991))
    ),
    "non-empty character vector"
  )
})

test_that("create_GRAFS stops for empty periods", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  expect_error(
    create_GRAFS(
      csv_inputs = csv, path_outputs = tempdir(), xml_base = xml,
      arrows_csv = ids, drawio_exe = "/nonexistent/drawio",
      regions = "spain",
      periods = list()
    ),
    "non-empty list"
  )
})

test_that("create_GRAFS stops for period missing years", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  expect_error(
    create_GRAFS(
      csv_inputs = csv, path_outputs = tempdir(), xml_base = xml,
      arrows_csv = ids, drawio_exe = "/nonexistent/drawio",
      regions = "spain",
      periods = list(list(prev_years = 1990:1991))
    ),
    "missing required 'years'"
  )
})

# --- Integration tests ---

test_that("create_GRAFS produces XML output when drawio is available", {
  drawio <- find_drawio()
  skip_if(is.null(drawio), "draw.io not installed")

  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
  out <- file.path(tempdir(), paste0("grafs_integration_", Sys.getpid()))
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  suppressWarnings(create_GRAFS(
    csv_inputs = csv,
    path_outputs = out,
    xml_base = xml,
    arrows_csv = ids,
    drawio_exe = drawio,
    regions = "spain",
    periods = list(list(years = 1990:1991))
  ))

  xml_files <- list.files(file.path(out, "xml"), pattern = "\\.xml$")
  png_files <- list.files(file.path(out, "png"), pattern = "\\.png$")
  expect_length(xml_files, 1)
  expect_length(png_files, 1)
  expect_match(xml_files[1], "GRAFS_spain_P1_MEAN_1990-1991\\.xml")
})

# The bundled example data has a single region, so the multi-region path gets
# a synthetic two-region copy of it.
two_region_csv <- function() {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  d <- readr::read_csv(csv, show_col_types = FALSE)
  north <- d
  north$province <- "north"
  south <- d
  south$province <- "south"

  path <- tempfile(fileext = ".csv.gz")
  readr::write_csv(rbind(north, south), path)
  path
}

test_that("create_GRAFS handles multiple regions", {
  drawio <- find_drawio()
  skip_if(is.null(drawio), "draw.io not installed")

  csv <- two_region_csv()
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
  out <- file.path(tempdir(), paste0("grafs_multi_", Sys.getpid()))
  on.exit(unlink(c(out, csv), recursive = TRUE), add = TRUE)

  suppressWarnings(create_GRAFS(
    csv_inputs = csv, path_outputs = out, xml_base = xml,
    arrows_csv = ids, drawio_exe = drawio,
    regions = c("north", "south"),
    periods = list(list(years = 1990:1991))
  ))

  xml_files <- list.files(file.path(out, "xml"), pattern = "\\.xml$")
  expect_length(xml_files, 2)
  expect_match(xml_files[1], "GRAFS_north_P1_MEAN_1990-1991\\.xml")
  expect_match(xml_files[2], "GRAFS_south_P1_MEAN_1990-1991\\.xml")
})

test_that("create_GRAFS respects overwrite=FALSE", {
  drawio <- find_drawio()
  skip_if(is.null(drawio), "draw.io not installed")

  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
  out <- file.path(tempdir(), paste0("grafs_overwrite_", Sys.getpid()))
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  suppressWarnings(create_GRAFS(
    csv_inputs = csv, path_outputs = out, xml_base = xml,
    arrows_csv = ids, drawio_exe = drawio,
    regions = "spain",
    periods = list(list(years = 1990:1991))
  ))
  xml_file <- list.files(file.path(out, "xml"), full.names = TRUE)[1]
  mtime1 <- file.mtime(xml_file)

  Sys.sleep(1)

  suppressWarnings(create_GRAFS(
    csv_inputs = csv, path_outputs = out, xml_base = xml,
    arrows_csv = ids, drawio_exe = drawio,
    regions = "spain",
    periods = list(list(years = 1990:1991)),
    overwrite = FALSE
  ))
  mtime2 <- file.mtime(xml_file)
  expect_equal(mtime1, mtime2)
})

test_that("create_GRAFS produces comparison diagram with change bubbles", {
  drawio <- find_drawio()
  skip_if(is.null(drawio), "draw.io not installed")

  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
  out <- file.path(tempdir(), paste0("grafs_compare_", Sys.getpid()))
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  suppressWarnings(create_GRAFS(
    csv_inputs = csv, path_outputs = out, xml_base = xml,
    arrows_csv = ids, drawio_exe = drawio,
    regions = "spain",
    periods = list(list(years = 2011:2015, prev_years = 1990:1994))
  ))

  xml_files <- list.files(file.path(out, "xml"), pattern = "\\.xml$")
  png_files <- list.files(file.path(out, "png"), pattern = "\\.png$")
  expect_length(xml_files, 1)
  expect_length(png_files, 1)

  # Verify the output XML contains ellipse bubbles (not removed)
  xml_content <- xml2::read_xml(
    list.files(file.path(out, "xml"), full.names = TRUE)[1]
  )
  bubbles <- xml2::xml_find_all(xml_content, ".//mxCell[contains(@style, 'ellipse')]")
  expect_gt(length(bubbles), 0)
})

test_that("create_GRAFS handles multiple periods", {
  drawio <- find_drawio()
  skip_if(is.null(drawio), "draw.io not installed")

  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
  out <- file.path(tempdir(), paste0("grafs_multiperiod_", Sys.getpid()))
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  suppressWarnings(create_GRAFS(
    csv_inputs = csv, path_outputs = out, xml_base = xml,
    arrows_csv = ids, drawio_exe = drawio,
    regions = "spain",
    periods = list(
      list(years = 1990:1991),
      list(years = 2000:2001)
    )
  ))

  xml_files <- list.files(file.path(out, "xml"), pattern = "\\.xml$")
  expect_length(xml_files, 2)
  expect_true(any(grepl("P1", xml_files)))
  expect_true(any(grepl("P2", xml_files)))
})

test_that("create_GRAFS returns output PNG paths", {
  drawio <- find_drawio()
  skip_if(is.null(drawio), "draw.io not installed")

  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
  out <- file.path(tempdir(), paste0("grafs_retval_", Sys.getpid()))
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  result <- suppressWarnings(create_GRAFS(
    csv_inputs = csv, path_outputs = out, xml_base = xml,
    arrows_csv = ids, drawio_exe = drawio,
    regions = "spain",
    periods = list(list(years = 1990:1991))
  ))

  expect_type(result, "character")
  expect_length(result, 1)
  expect_true(grepl("\\.png$", result))
  expect_true(file.exists(result))
})

test_that("create_GRAFS warns for nonexistent region", {
  drawio <- find_drawio()
  skip_if(is.null(drawio), "draw.io not installed")

  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")
  out <- file.path(tempdir(), paste0("grafs_noregion_", Sys.getpid()))
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  expect_warning(
    create_GRAFS(
      csv_inputs = csv, path_outputs = out, xml_base = xml,
      arrows_csv = ids, drawio_exe = drawio,
      regions = "ZZZ_NoSuchPlace",
      periods = list(list(years = 1930:1931))
    ),
    "No data found for region"
  )

  # No files should be produced
  xml_files <- list.files(file.path(out, "xml"), pattern = "\\.xml$")
  expect_length(xml_files, 0)
})

test_that("create_GRAFS validates non-character regions", {
  csv <- system.file("extdata", "GRAFS_spain_data.csv.gz", package = "GRAFS")
  xml <- system.file("templates", "grafs_auto_v18.xml", package = "GRAFS")
  ids <- system.file("extdata", "GRAFS_arrows_ids.csv", package = "GRAFS")

  expect_error(
    create_GRAFS(
      csv_inputs = csv, path_outputs = tempdir(), xml_base = xml,
      arrows_csv = ids, drawio_exe = "/nonexistent/drawio",
      regions = 123,
      periods = list(list(years = 1930:1931))
    ),
    "non-empty character vector"
  )
})
