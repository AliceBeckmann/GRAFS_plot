test_that("parse_style parses semicolon-delimited key=value pairs", {
  style <- "strokeWidth=5;fillColor=#FF0000;fontSize=12"
  result <- GRAFS:::parse_style(style)

  expect_equal(result[["strokeWidth"]], "5")
  expect_equal(result[["fillColor"]], "#FF0000")
  expect_equal(result[["fontSize"]], "12")
})

test_that("parse_style handles keys without values", {
  style <- "rounded;strokeWidth=3;dashed"
  result <- GRAFS:::parse_style(style)

  expect_equal(result[["rounded"]], "")
  expect_equal(result[["strokeWidth"]], "3")
  expect_equal(result[["dashed"]], "")
})

test_that("parse_style ignores trailing semicolons", {
  style <- "strokeWidth=5;fillColor=#FF0000;"
  result <- GRAFS:::parse_style(style)

  expect_equal(length(result), 2)
  expect_equal(result[["strokeWidth"]], "5")
  expect_equal(result[["fillColor"]], "#FF0000")
  expect_false("" %in% names(result))
})

test_that("parse_style ignores leading semicolons", {
  style <- ";strokeWidth=5;fillColor=#FF0000"
  result <- GRAFS:::parse_style(style)

  expect_equal(length(result), 2)
  expect_false("" %in% names(result))
})

test_that("parse_style handles multiple consecutive semicolons", {
  style <- "strokeWidth=5;;fillColor=#FF0000"
  result <- GRAFS:::parse_style(style)

  expect_equal(length(result), 2)
  expect_equal(result[["strokeWidth"]], "5")
  expect_equal(result[["fillColor"]], "#FF0000")
})

test_that("parse_style handles single property", {
  result <- GRAFS:::parse_style("strokeWidth=10")
  expect_equal(length(result), 1)
  expect_equal(result[["strokeWidth"]], "10")
})

test_that("build_style reconstructs semicolon-delimited string", {
  style_list <- c(strokeWidth = "5", fillColor = "#FF0000")
  result <- GRAFS:::build_style(style_list)

  expect_equal(result, "strokeWidth=5;fillColor=#FF0000")
})

test_that("parse_style and build_style round-trip correctly", {
  original <- "strokeWidth=5;fillColor=#FF0000;fontSize=12"
  round_tripped <- GRAFS:::build_style(GRAFS:::parse_style(original))

  expect_equal(round_tripped, original)
})

test_that("round-trip preserves trailing-semicolon styles (minus trailing ;)", {
  # draw.io often has trailing semicolons; our parse drops them but that's OK
  original <- "strokeWidth=5;fillColor=#FF0000;"
  parsed <- GRAFS:::parse_style(original)
  rebuilt <- GRAFS:::build_style(parsed)

  expect_equal(rebuilt, "strokeWidth=5;fillColor=#FF0000")
  # The values are preserved even if trailing ; is dropped
  expect_equal(parsed[["strokeWidth"]], "5")
  expect_equal(parsed[["fillColor"]], "#FF0000")
})

test_that("build_style preserves bare keys without adding =", {
  # draw.io uses bare keys like "rounded", "dashed", "html" without values
  style <- "rounded;strokeWidth=5;html=1;dashed"
  parsed <- GRAFS:::parse_style(style)
  rebuilt <- GRAFS:::build_style(parsed)

  # Bare keys should NOT get "=" appended
  expect_equal(rebuilt, "rounded;strokeWidth=5;html=1;dashed")
  expect_false(grepl("rounded=", rebuilt, fixed = TRUE))
  expect_false(grepl("dashed=", rebuilt, fixed = TRUE))
})

test_that("build_style handles all bare keys", {
  style_list <- c(rounded = "", dashed = "")
  result <- GRAFS:::build_style(style_list)
  expect_equal(result, "rounded;dashed")
})

test_that("build_style handles mix of bare and valued keys", {
  style_list <- c(rounded = "", strokeWidth = "5", dashed = "", fillColor = "#FF0000")
  result <- GRAFS:::build_style(style_list)
  expect_equal(result, "rounded;strokeWidth=5;dashed;fillColor=#FF0000")
})
