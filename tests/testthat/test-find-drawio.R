test_that("find_drawio returns a string or NULL", {
  result <- find_drawio()
  expect_true(is.null(result) || is.character(result))
  if (!is.null(result)) {
    expect_true(file.exists(result))
  }
})

test_that("find_drawio returns a path that exists on this system", {
  result <- find_drawio()
  skip_if(is.null(result), "draw.io not installed")
  expect_true(file.exists(result))
  # Should be a single path, not a vector
  expect_length(result, 1)
})

test_that("check_setup returns logical invisibly", {
  result <- suppressMessages(check_setup())
  expect_type(result, "logical")
})

test_that("check_setup returns FALSE for missing drawio path", {
  result <- suppressMessages(check_setup(drawio_path = "/nonexistent/drawio"))
  expect_false(result)
})

test_that("check_setup provides OS-specific install guidance", {
  msgs <- character()
  withCallingHandlers(
    check_setup(drawio_path = "/nonexistent/drawio"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  all_msgs <- paste(msgs, collapse = " ")
  # Should mention at least one OS-specific install method
  expect_true(
    grepl("snap|brew|homebrew|winget|installer|deb|rpm", all_msgs,
          ignore.case = TRUE)
  )
})
