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

test_that("year_info handles empty vector", {
  expect_equal(GRAFS:::year_info(c()), "")
})

test_that("year_info handles long contiguous range", {
  expect_equal(GRAFS:::year_info(1860:1870), "1860-1870")
})

test_that("year_info handles three non-contiguous years", {
  result <- GRAFS:::year_info(c(2000, 2005, 2010))
  expect_equal(result, "2000_2005_2010")
})
