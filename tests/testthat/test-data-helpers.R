test_that("select_region_data filters by province and years", {
  d <- data.frame(
    province = c("A", "A", "B", "B"),
    year = c(2000, 2001, 2000, 2001),
    label = c("{X}", "{X}", "{X}", "{X}"),
    data = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  result <- GRAFS:::select_region_data(d, "A", 2000:2001)
  expect_equal(nrow(result), 2)
  expect_true(all(result$province == "A"))

  result <- GRAFS:::select_region_data(d, "B", 2000)
  expect_equal(nrow(result), 1)
  expect_equal(result$data, 3)
})

test_that("select_region_data returns empty for unknown region", {
  d <- data.frame(
    province = c("A"),
    year = c(2000),
    label = c("{X}"),
    data = c(1),
    stringsAsFactors = FALSE
  )

  result <- GRAFS:::select_region_data(d, "MISSING", 2000)
  expect_equal(nrow(result), 0)
})

test_that("select_region_data handles NULL years", {
  d <- data.frame(
    province = c("A", "A"),
    year = c(2000, 2001),
    label = c("{X}", "{X}"),
    data = c(1, 2),
    stringsAsFactors = FALSE
  )

  result <- GRAFS:::select_region_data(d, "A", NULL)
  expect_equal(nrow(result), 0)
})

test_that("prepare_directories creates xml and png subdirs", {
  tmp <- file.path(tempdir(), paste0("grafs_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  GRAFS:::prepare_directories(tmp)
  expect_true(dir.exists(file.path(tmp, "xml")))
  expect_true(dir.exists(file.path(tmp, "png")))
})

test_that("prepare_data reads CSV and removes WIDTH_MAX rows", {
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)

  writeLines(c(
    "province,year,label,data,align,arrowColor",
    "A,2000,{FLOW},100,L,NA",
    "A,2000,{FLOW},100,L,NA",
    "A,2000,{WIDTH_MAX},999,L,NA"
  ), tmp_csv)

  result <- GRAFS:::prepare_data(tmp_csv)
  expect_equal(nrow(result), 1)
  expect_false(any(grepl("WIDTH_MAX", result$label)))
})
