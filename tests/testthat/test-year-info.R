test_that("year_info formats single year", {
  expect_equal(GRAFS:::year_info(2015), "2015")
})

test_that("year_info formats contiguous range", {
  expect_equal(GRAFS:::year_info(2011:2015), "2011-2015")
})

test_that("year_info formats two-year range", {
  expect_equal(GRAFS:::year_info(1930:1931), "1930-1931")
})

test_that("year_info formats non-contiguous years", {
  expect_equal(GRAFS:::year_info(c(1930, 1935)), "1930_1935")
})

test_that("year_info formats mixed contiguous and non-contiguous", {
  result <- GRAFS:::year_info(c(1990, 1991, 1995, 1996))
  expect_equal(result, "1990-1991_1995-1996")
})
