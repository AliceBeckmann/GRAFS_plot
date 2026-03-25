test_that("find_drawio returns a string or NULL", {
  result <- find_drawio()
  expect_true(is.null(result) || is.character(result))
  if (!is.null(result)) {
    expect_true(file.exists(result))
  }
})

test_that("check_setup returns logical invisibly", {
  result <- suppressMessages(check_setup())
  expect_type(result, "logical")
})

test_that("check_setup returns FALSE for missing drawio path", {
  result <- suppressMessages(check_setup(drawio_path = "/nonexistent/drawio"))
  expect_false(result)
})
