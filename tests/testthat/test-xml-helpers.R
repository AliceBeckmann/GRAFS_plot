make_test_doc <- function() {
  xml <- '
  <mxGraphModel>
    <root>
      <mxCell id="0"/>
      <mxCell id="1" parent="0"/>
      <mxCell id="arrow1" value="{FLOW_A}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
      <mxCell id="arrow2" value="{FLOW_B}" style="strokeWidth=10;fillColor=#0000FF" parent="1"/>
      <mxCell id="label1" value="Value: {FLOW_A}" style="fontSize=12" parent="1"/>
      <mxCell id="bubble1" value="{FLOW_A}" style="ellipse;fontSize=8;width=20;height=20" parent="1"/>
    </root>
  </mxGraphModel>'
  xml2::read_xml(xml)
}

test_that("remove_id removes matching cells", {
  doc <- make_test_doc()
  cells_before <- xml2::xml_find_all(doc, ".//mxCell[contains(@value, '{FLOW_A}')]")
  expect_gt(length(cells_before), 0)

  doc <- GRAFS:::remove_id(doc, "{FLOW_A}")
  cells_after <- xml2::xml_find_all(doc, ".//mxCell[contains(@value, '{FLOW_A}')]")
  expect_equal(length(cells_after), 0)
})

test_that("remove_id warns for missing id", {
  doc <- make_test_doc()
  expect_warning(
    GRAFS:::remove_id(doc, "{NONEXISTENT}"),
    "No mxCell found"
  )
})

test_that("remove_change_bubbles removes ellipse cells", {
  doc <- make_test_doc()
  bubbles_before <- xml2::xml_find_all(doc, ".//mxCell[contains(@style, 'ellipse')]")
  expect_equal(length(bubbles_before), 1)

  doc <- GRAFS:::remove_change_bubbles(doc)
  bubbles_after <- xml2::xml_find_all(doc, ".//mxCell[contains(@style, 'ellipse')]")
  expect_equal(length(bubbles_after), 0)
})

test_that("replace_label_in_value substitutes label text", {
  doc <- make_test_doc()
  doc <- GRAFS:::replace_label_in_value(doc, "{FLOW_A}", "42")

  label_cell <- xml2::xml_find_first(doc, ".//mxCell[@id='label1']")
  expect_equal(xml2::xml_attr(label_cell, "value"), "Value: 42")
})

test_that("replace_label_in_value warns for missing label", {
  doc <- make_test_doc()
  expect_warning(
    GRAFS:::replace_label_in_value(doc, "{MISSING}", "99"),
    "No mxCell found"
  )
})

test_that("change_bubble_size modifies width, height, and fontSize", {
  doc <- make_test_doc()
  doc <- GRAFS:::change_bubble_size(doc, "{FLOW_A}", 50, 14)

  bubble <- xml2::xml_find_first(doc, ".//mxCell[contains(@style, 'ellipse')]")
  style <- GRAFS:::parse_style(xml2::xml_attr(bubble, "style"))
  expect_equal(style[["width"]], "50")
  expect_equal(style[["height"]], "50")
  expect_equal(style[["fontSize"]], "14")
})

test_that("change_style modifies arrow strokeWidth proportionally", {
  doc <- make_test_doc()
  t_id <- data.frame(
    label = "{FLOW_A}",
    id = "arrow1",
    stringsAsFactors = FALSE
  )

  doc <- GRAFS:::change_style(
    doc, "{FLOW_A}", "strokeWidth", 400,
    t_id, max_width_arrows = 25, val_max_width = 1000
  )

  arrow <- xml2::xml_find_first(doc, ".//mxCell[@id='arrow1']")
  style <- GRAFS:::parse_style(xml2::xml_attr(arrow, "style"))
  # 400 * 25 / 1000 = 10
  expect_equal(style[["strokeWidth"]], "10")
})

test_that("change_style caps at max width", {
  doc <- make_test_doc()
  t_id <- data.frame(
    label = "{FLOW_A}",
    id = "arrow1",
    stringsAsFactors = FALSE
  )

  doc <- GRAFS:::change_style(
    doc, "{FLOW_A}", "strokeWidth", 5000,
    t_id, max_width_arrows = 25, val_max_width = 1000
  )

  arrow <- xml2::xml_find_first(doc, ".//mxCell[@id='arrow1']")
  style <- GRAFS:::parse_style(xml2::xml_attr(arrow, "style"))
  expect_equal(style[["strokeWidth"]], "25")
})

test_that("change_style sets fillColor directly", {
  doc <- make_test_doc()
  t_id <- data.frame(
    label = "{FLOW_A}",
    id = "arrow1",
    stringsAsFactors = FALSE
  )

  doc <- GRAFS:::change_style(
    doc, "{FLOW_A}", "fillColor", "#FF0000",
    t_id, max_width_arrows = 25, val_max_width = 1000
  )

  arrow <- xml2::xml_find_first(doc, ".//mxCell[@id='arrow1']")
  style <- GRAFS:::parse_style(xml2::xml_attr(arrow, "style"))
  expect_equal(style[["fillColor"]], "#FF0000")
})
