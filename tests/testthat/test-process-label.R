make_test_doc <- function() {
  xml <- '
  <mxGraphModel>
    <root>
      <mxCell id="0"/>
      <mxCell id="1" parent="0"/>
      <mxCell id="arrow1" value="{FLOW_A}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
      <mxCell id="label1" value="Value: {FLOW_A}" style="fontSize=12" parent="1"/>
      <mxCell id="prov_cell" value="{PROVINCE_NAME}" style="fontSize=14" parent="1"/>
      <mxCell id="year_cell" value="{YEAR}" style="fontSize=14" parent="1"/>
    </root>
  </mxGraphModel>'
  xml2::read_xml(xml)
}

test_that("process_label handles {PROVINCE_NAME} without calling change_style", {
  doc <- make_test_doc()
  x <- data.frame(
    label = "{PROVINCE_NAME}", data = "Albacete",
    arrowColor = NA, stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     stringsAsFactors = FALSE)

  # Should not warn about missing arrow mapping for PROVINCE_NAME
  expect_silent(
    doc <- GRAFS:::process_label(
      doc, x, NULL, "{PROVINCE_NAME}", 2000:2001, NULL,
      0, FALSE, "#97cde5", "#a9d77f", t_id, 25, 1000
    )
  )

  cell <- xml2::xml_find_first(doc, ".//mxCell[@id='prov_cell']")
  expect_equal(xml2::xml_attr(cell, "value"), "Albacete")
})

test_that("process_label handles {YEAR} without calling change_style", {
  doc <- make_test_doc()
  x <- data.frame(
    label = "{YEAR}", data = "2000",
    arrowColor = NA, stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     stringsAsFactors = FALSE)

  expect_silent(
    doc <- GRAFS:::process_label(
      doc, x, NULL, "{YEAR}", 2000:2005, NULL,
      0, FALSE, "#97cde5", "#a9d77f", t_id, 25, 1000
    )
  )

  cell <- xml2::xml_find_first(doc, ".//mxCell[@id='year_cell']")
  expect_equal(xml2::xml_attr(cell, "value"), "2000-2005")
})

test_that("process_label formats single year without 'mean_' prefix", {
  doc <- make_test_doc()
  x <- data.frame(
    label = "{YEAR}", data = "2015",
    arrowColor = NA, stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     stringsAsFactors = FALSE)

  expect_silent(
    doc <- GRAFS:::process_label(
      doc, x, NULL, "{YEAR}", 2015, NULL,
      0, FALSE, "#97cde5", "#a9d77f", t_id, 25, 1000
    )
  )

  cell <- xml2::xml_find_first(doc, ".//mxCell[@id='year_cell']")
  expect_equal(xml2::xml_attr(cell, "value"), "2015")
})

test_that("process_label computes mean for numeric labels", {
  doc <- make_test_doc()
  x <- data.frame(
    label = rep("{FLOW_A}", 3),
    data = c("100", "200", "300"),
    arrowColor = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     stringsAsFactors = FALSE)

  doc <- suppressWarnings(GRAFS:::process_label(
    doc, x, NULL, "{FLOW_A}", 2000:2002, NULL,
    0, FALSE, "#97cde5", "#a9d77f", t_id, 25, 1000
  ))

  label_cell <- xml2::xml_find_first(doc, ".//mxCell[@id='label1']")
  expect_equal(xml2::xml_attr(label_cell, "value"), "Value: 200")
})

test_that("process_label respects decimals parameter", {
  doc <- make_test_doc()
  x <- data.frame(
    label = "{FLOW_A}", data = "123.456",
    arrowColor = NA, stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     stringsAsFactors = FALSE)

  doc <- suppressWarnings(GRAFS:::process_label(
    doc, x, NULL, "{FLOW_A}", 2000, NULL,
    2, FALSE, "#97cde5", "#a9d77f", t_id, 25, 1000
  ))

  label_cell <- xml2::xml_find_first(doc, ".//mxCell[@id='label1']")
  expect_equal(xml2::xml_attr(label_cell, "value"), "Value: 123.46")
})

test_that("process_label colors bubble with increase_color on positive change", {
  doc <- xml2::read_xml('
  <mxGraphModel>
    <root>
      <mxCell id="0"/>
      <mxCell id="1" parent="0"/>
      <mxCell id="arrow1" value="{FLOW_A}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
      <mxCell id="bubble1" value="{XFLOW_A}" style="ellipse;fontSize=8;width=20;height=20;fillColor=#dae8fc" parent="1"/>
    </root>
  </mxGraphModel>')
  x <- data.frame(label = "{FLOW_A}", data = "200", arrowColor = NA,
                  stringsAsFactors = FALSE)
  xch <- data.frame(label = "{FLOW_A}", data = "100", arrowColor = NA,
                    stringsAsFactors = FALSE)
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     labelchange = "XFLOW_A", stringsAsFactors = FALSE)

  doc <- GRAFS:::process_label(
    doc, x, xch, "{FLOW_A}", 2001, 2000,
    0, TRUE, "#green", "#blue", t_id, 25, 1000
  )

  bubble <- xml2::xml_find_first(doc, ".//mxCell[@id='bubble1']")
  style <- GRAFS:::parse_style(xml2::xml_attr(bubble, "style"))
  expect_equal(style[["fillColor"]], "#green")
  # Arrow fillColor must NOT be changed
  arrow <- xml2::xml_find_first(doc, ".//mxCell[@id='arrow1']")
  arrow_style <- GRAFS:::parse_style(xml2::xml_attr(arrow, "style"))
  expect_equal(arrow_style[["fillColor"]], "#000000")
})

test_that("process_label colors bubble with decrease_color on negative change", {
  doc <- xml2::read_xml('
  <mxGraphModel>
    <root>
      <mxCell id="0"/>
      <mxCell id="1" parent="0"/>
      <mxCell id="arrow1" value="{FLOW_A}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
      <mxCell id="bubble1" value="{XFLOW_A}" style="ellipse;fontSize=8;width=20;height=20;fillColor=#dae8fc" parent="1"/>
    </root>
  </mxGraphModel>')
  x <- data.frame(label = "{FLOW_A}", data = "50", arrowColor = NA,
                  stringsAsFactors = FALSE)
  xch <- data.frame(label = "{FLOW_A}", data = "100", arrowColor = NA,
                    stringsAsFactors = FALSE)
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     labelchange = "XFLOW_A", stringsAsFactors = FALSE)

  doc <- GRAFS:::process_label(
    doc, x, xch, "{FLOW_A}", 2001, 2000,
    0, TRUE, "#green", "#blue", t_id, 25, 1000
  )

  bubble <- xml2::xml_find_first(doc, ".//mxCell[@id='bubble1']")
  style <- GRAFS:::parse_style(xml2::xml_attr(bubble, "style"))
  expect_equal(style[["fillColor"]], "#blue")
  # Arrow fillColor must NOT be changed
  arrow <- xml2::xml_find_first(doc, ".//mxCell[@id='arrow1']")
  arrow_style <- GRAFS:::parse_style(xml2::xml_attr(arrow, "style"))
  expect_equal(arrow_style[["fillColor"]], "#000000")
})

test_that("process_label handles zero baseline without error", {
  doc <- make_test_doc()
  x <- data.frame(
    label = "{FLOW_A}", data = "200", arrowColor = NA,
    stringsAsFactors = FALSE
  )
  xch <- data.frame(
    label = "{FLOW_A}", data = "0", arrowColor = NA,
    stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     stringsAsFactors = FALSE)

  # Division by zero case: should not error, value_change stays NA
  expect_no_error(suppressWarnings(
    GRAFS:::process_label(
      doc, x, xch, "{FLOW_A}", 2001, 2000,
      0, TRUE, "#blue", "#green", t_id, 25, 1000
    )
  ))
})

test_that("process_label applies arrowColor when present", {
  doc <- make_test_doc()
  x <- data.frame(
    label = "{FLOW_A}", data = "500",
    arrowColor = "#FF0000", stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     stringsAsFactors = FALSE)

  doc <- suppressWarnings(GRAFS:::process_label(
    doc, x, NULL, "{FLOW_A}", 2000, NULL,
    0, FALSE, "#97cde5", "#a9d77f", t_id, 25, 1000
  ))

  arrow <- xml2::xml_find_first(doc, ".//mxCell[@id='arrow1']")
  style <- GRAFS:::parse_style(xml2::xml_attr(arrow, "style"))
  expect_equal(style[["fillColor"]], "#FF0000")
})

test_that("process_label replaces bubble label with percentage", {
  doc <- xml2::read_xml('
  <mxGraphModel>
    <root>
      <mxCell id="0"/>
      <mxCell id="1" parent="0"/>
      <mxCell id="arrow1" value="{FLOW_A}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
      <mxCell id="bubble1" value="{XFLOW_A}" style="ellipse;fontSize=8;width=20;height=20" parent="1"/>
    </root>
  </mxGraphModel>')
  x <- data.frame(label = "{FLOW_A}", data = "150", arrowColor = NA,
                  stringsAsFactors = FALSE)
  xch <- data.frame(label = "{FLOW_A}", data = "100", arrowColor = NA,
                    stringsAsFactors = FALSE)
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     labelchange = "XFLOW_A", stringsAsFactors = FALSE)

  doc <- GRAFS:::process_label(
    doc, x, xch, "{FLOW_A}", 2001, 2000,
    0, TRUE, "#blue", "#green", t_id, 25, 1000
  )

  bubble <- xml2::xml_find_first(doc, ".//mxCell[@id='bubble1']")
  expect_equal(xml2::xml_attr(bubble, "value"), "50%")
})

test_that("process_label removes orphaned bubble when value_change is NA (zero baseline)", {
  doc <- xml2::read_xml('
  <mxGraphModel>
    <root>
      <mxCell id="0"/>
      <mxCell id="1" parent="0"/>
      <mxCell id="arrow1" value="{FLOW_A}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
      <mxCell id="bubble1" value="{XFLOW_A}" style="ellipse;fontSize=8;width=20;height=20" parent="1"/>
    </root>
  </mxGraphModel>')
  x <- data.frame(label = "{FLOW_A}", data = "150", arrowColor = NA,
                  stringsAsFactors = FALSE)
  xch <- data.frame(label = "{FLOW_A}", data = "0", arrowColor = NA,
                    stringsAsFactors = FALSE)
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     labelchange = "XFLOW_A", stringsAsFactors = FALSE)

  doc <- GRAFS:::process_label(
    doc, x, xch, "{FLOW_A}", 2001, 2000,
    0, TRUE, "#blue", "#green", t_id, 25, 1000
  )

  bubble <- xml2::xml_find_first(doc, ".//mxCell[@id='bubble1']")
  expect_true(inherits(bubble, "xml_missing"))
})

test_that("process_label skips bubble when labelchange is NA", {
  doc <- xml2::read_xml('
  <mxGraphModel>
    <root>
      <mxCell id="0"/>
      <mxCell id="1" parent="0"/>
      <mxCell id="arrow1" value="{FLOW_A}" style="strokeWidth=5;fillColor=#000000" parent="1"/>
    </root>
  </mxGraphModel>')
  x <- data.frame(label = "{FLOW_A}", data = "150", arrowColor = NA,
                  stringsAsFactors = FALSE)
  xch <- data.frame(label = "{FLOW_A}", data = "100", arrowColor = NA,
                    stringsAsFactors = FALSE)
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     labelchange = NA_character_, stringsAsFactors = FALSE)

  expect_no_warning(GRAFS:::process_label(
    doc, x, xch, "{FLOW_A}", 2001, 2000,
    0, TRUE, "#blue", "#green", t_id, 25, 1000
  ))
})

test_that("process_label uses extra decimal when mean rounds to zero", {
  doc <- make_test_doc()
  x <- data.frame(
    label = "{FLOW_A}", data = "0.04",
    arrowColor = NA, stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     stringsAsFactors = FALSE)

  doc <- suppressWarnings(GRAFS:::process_label(
    doc, x, NULL, "{FLOW_A}", 2000, NULL,
    0, FALSE, "#97cde5", "#a9d77f", t_id, 25, 1000
  ))

  # round(0.04, 0) = 0, so change_style removes the cell (strokeWidth=0).
  # The label cells are removed too since they contain {FLOW_A}.
  cells <- xml2::xml_find_all(doc, ".//mxCell[contains(@value, '{FLOW_A}')]")
  expect_equal(length(cells), 0)
})

test_that("process_label handles large values correctly", {
  doc <- make_test_doc()
  x <- data.frame(
    label = "{FLOW_A}", data = "99999",
    arrowColor = NA, stringsAsFactors = FALSE
  )
  t_id <- data.frame(label = "{FLOW_A}", id = "arrow1",
                     stringsAsFactors = FALSE)

  doc <- suppressWarnings(GRAFS:::process_label(
    doc, x, NULL, "{FLOW_A}", 2000, NULL,
    0, FALSE, "#97cde5", "#a9d77f", t_id, 25, 1000
  ))

  label_cell <- xml2::xml_find_first(doc, ".//mxCell[@id='label1']")
  expect_equal(xml2::xml_attr(label_cell, "value"), "Value: 99999")
})
